//
// C++ Interface: textstyle
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005-2009
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef _TEXTSTYLE_H_
#define _TEXTSTYLE_H_

#include <string>
#include <vector>
#include <map>
#include <boost/regex.hpp>

namespace srchilite {

#define STYLE_VAR_TEXT "$style" // the text of the style variable
#define TEXT_VAR_TEXT "$text" // the text of the text variable
#define STYLE_VAR "\\" STYLE_VAR_TEXT // the name of the style variable as regexp
#define TEXT_VAR "\\" TEXT_VAR_TEXT // the name of the text variable as regexp
/// map for substitutions
typedef std::map<std::string, std::string> SubstitutionMapping;

/**
 * Represents a formatting template where there can be some
 * variables (starting with $, e.g., $style, $text, etc.) that will be replaced
 * with specific elements, e.g., the actual style for the formatting, the
 * text to format, etc.
 */
class TextStyle {
private:
    typedef std::vector<std::string> StringVector;
    typedef std::vector<int> IndexVector;
    typedef std::map<std::string, IndexVector> SubstitutionIndexes;

    /// the regular expression to find variable occurrences
    boost::regex var_exp;

    std::string repr;

    /// contains all the string parts of this TextStyle.
    StringVector parts;

    /// contains the indexes of parts where to substitute $vars.
    SubstitutionIndexes substitutions;

    /// whether to rebuild the vectors
    bool invalid;

    void build_vectors();

public:
    /**
     * @param s the representation
     * @param vars an array of string representing the variables in this TextStyle
     * (e.g., {"linenum", "infilename", "infile", "outfile", 0}), the last element must
     * be 0
     */
    TextStyle(const std::string &s = "", const char **vars = 0);
    ~TextStyle();

    /**
     * substitutes $text with text and $style with style
     * @param text
     * @param style
     * @return the string after substitutions
     */
    std::string output(const std::string &text, const std::string &style = "");

    /**
     * for each i substitutes: subst_map[i].first occurrence with subst_map[i].second
     * @param subst_map
     * @return the string after substitutions
     */
    std::string output(SubstitutionMapping &subst_map);

    /**
     * substitutes $style with style
     * @param style
     * @return the string after substitutions
     */
    std::string subst_style(const std::string &style = "");

    /**
     * @return the string representation
     */
    const std::string &toString() const {
        return repr;
    }

    /**
     * substitutes $text with the string representation of inner
     * e.g., if this is <b>$text</b> and inner is <i>$text</i>
     * this will return <b><i>$text</i></b>
     * @param inner
     * @return a new TextStyle after substitution
     */
    TextStyle compose(const TextStyle &inner);

    /**
     * as compose, but acts on this instance
     * @param inner
     */
    void update(const TextStyle &inner);

    /**
     * as compose, but acts on this instance
     * @param inner
     */
    void update(const std::string &inner);

    /**
     * as output, but acts on this instance
     * @param text
     * @param style
     */
    void update(const std::string &text, const std::string &style);

    /**
     * @return whether this TextStyle contains the $style variable
     */
    bool containsStyleVar() const;

    /**
     * @return whether it is only $style or $text
     */
    bool empty() const;
};

}

#endif /*_TEXTSTYLE_H_*/
