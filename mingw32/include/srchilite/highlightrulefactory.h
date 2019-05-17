//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef HIGHLIGHTRULEFACTORY_H_
#define HIGHLIGHTRULEFACTORY_H_

#include <list>
#include <string>

namespace srchilite {

typedef std::list<std::string> WordList;
typedef std::list<std::string> ElemNameList;

class HighlightRule;

/**
 * Abstract factory for highlighting rules
 */
class HighlightRuleFactory {
public:
    HighlightRuleFactory();
    virtual ~HighlightRuleFactory();

    /**
     * Creates a generic highlighting rule
     * @param name the element name represented by the rule
     * @param the string representation
     * @return the generated rule
     */
    virtual HighlightRule *createSimpleRule(const std::string &name,
            const std::string &s) = 0;

    /**
     * Creates a rule for detecting a list of specific words, i.e., isolated
     * from other parts by a space or a delimiter (e.g., if "class" is considered as
     * a word, then it will not match the substring "class" in "myclass")
     * @param name the element name represented by the rule
     * @param list the list of words to detect
     * @param caseSensitive if the characters in the expression must be interpreted
     * case sensitive
     * @return the generated rule
     */
    virtual HighlightRule *createWordListRule(const std::string &name,
            const WordList &list, bool caseSensitve = true) = 0;

    /**
     * Creates a rule for detecting a list of specific expressions
     * @param name the element name represented by the rule
     * @param list the list of words to detect
     * @param caseSensitive if the characters in the expression must be interpreted
     * case sensitive
     * @return the generated rule
     */
    virtual HighlightRule *createListRule(const std::string &name,
            const WordList &list, bool caseSensitve = true) = 0;

    /**
     * Creates a rule for matching a delimited string (spanning a single line)
     * @param name the element name represented by the rule
     * @param start the string determining the start of the sequence
     * @param end the string determining the end of the sequence
     * @param escape the string the escape sequence (typically a char, e.g., \)
     * @param nested whether the delimiters can be nested
     * @return the generated rule
     */
    virtual HighlightRule *createLineRule(const std::string &name,
            const std::string &start, const std::string &end,
            const std::string &escape, bool nested) = 0;

    /**
     * Creates a rule for matching a delimited string (possibly spanning more than one line)
     * @param name the element name represented by the rule
     * @param start the string determining the start of the sequence
     * @param end the string determining the end of the sequence
     * @param escape the string the escape sequence (typically a char, e.g., \)
     * @param nested whether the delimiters can be nested
     * @return the generated rule
     */
    virtual HighlightRule *createMultiLineRule(const std::string &name,
            const std::string &start, const std::string &end,
            const std::string &escape, bool nested) = 0;

    /**
     * Creates a rule for matching many element names, each represented by a
     * subexpression (the number of subexpression must be equal to the size of the
     * nameList)
     * @param nameList the list of element names represented by the rule
     * @param rep the string representation of the rule
     * @return the generated rule
     */
    virtual HighlightRule *createCompoundRule(const ElemNameList &nameList,
            const std::string &rep) = 0;

};

}

#endif /*HIGHLIGHTRULEFACTORY_H_*/
