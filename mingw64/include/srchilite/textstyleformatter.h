/**
 * C++ class: textstyleformatter.h
 *
 * Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005-2008
 * Copyright: See COPYING file that comes with this distribution
 */

#ifndef _TEXTSTYLEFORMATTER_H_
#define _TEXTSTYLEFORMATTER_H_

#include "textstyle.h"
#include "formatter.h"

namespace srchilite {

class BufferedOutput;
class PreFormatter;
class CTagsFormatter;

/**
 * An implementation of Formatter, based on source-highlight TextStyle
 */
class TextStyleFormatter : public Formatter {
private:
    /// the TextStyle for formatting
    TextStyle textstyle;

    /// where to output the formatting
    BufferedOutput *output;

    /// the possible pre formatter for text
    PreFormatter *preFormatter;

    /// the possible formatter for ctags (references and anchors)
    CTagsFormatter *ctagsFormatter;

public:
    TextStyleFormatter(const TextStyle &style, BufferedOutput *o = 0);
            TextStyleFormatter(const std::string &repr = "$text",
                    BufferedOutput *o = 0);

    virtual ~TextStyleFormatter() {
    }

    virtual void
            format(const std::string &s, const FormatterParams *params = 0);

    BufferedOutput *getBufferedOutput() const {
        return output;
    }

    CTagsFormatter *getCTagsFormatter() const {
        return ctagsFormatter;
    }

    void setBufferedOutput(BufferedOutput *o) {
        output = o;
    }

    void setPreFormatter(PreFormatter *p) {
        preFormatter = p;
    }

    void setCTagsFormatter(CTagsFormatter *f) {
        ctagsFormatter = f;
    }

    /**
     * @return a string representation of this formatter
     */
    const std::string &toString() const {
        return textstyle.toString();
    }

protected:
    /**
     * Actually performs formatting (no reference formatting); this is used
     * internally
     * @param s the string to format.
     * @param preformat whether to perform preformatting
     */
    void doFormat(const std::string &s, bool preformat = true);

    /**
     * Performs reference formatting (by relying on the CTagsFormatter)
     * @param s the string to format.
     * @param params additional parameters
     * @return true if an anchor or reference was found and formatted
     */
    bool formatReferences(const std::string &s, const FormatterParams *params);

};

}

#endif /* _TEXTSTYLEFORMATTER_H_ */
