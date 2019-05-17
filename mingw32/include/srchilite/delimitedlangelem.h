//
// C++ Interface: delimitedlangelem
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2009
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef DELIMITEDLANGELEM_H
#define DELIMITEDLANGELEM_H

#include "statestartlangelem.h"

namespace srchilite {

class StringDef;

/**
 A language element that is delimited by specific strings
 (regular expressions)

 */
// doublecpp: forward declarations, DO NOT MODIFY
class HighlightState; // file: highlightstate.h
class HighlightStateBuilder; // file: highlightstatebuilder.h
// doublecpp: end, DO NOT MODIFY

class DelimitedLangElem : public StateStartLangElem
{
private:
    StringDef *start;
    StringDef *end;
    StringDef *escape;
    bool multiline;
    bool nested;

public:
    /**
     * @param n the name of the element
     * @param s the starting regular expression for the element
     * @param e the ending regular expression
     * @param es the escaping sequence (if any)
     * @param multi whether this element spans multiple lines
     * @param nes whether this element can be nested
     */
    DelimitedLangElem(const std::string &n, StringDef *s, StringDef *e,
            StringDef *es, bool multi, bool nes);

    ~DelimitedLangElem();

    /**
     * return the string representation (with preprocessing)
     * @return the string representation
     */
    virtual const std::string toString() const;

    /**
     * return the original representation (without any preprocessing);
     * this is useful for printing errors
     * @return the original representation
     */
    virtual const std::string toStringOriginal() const;

    /**
     * @return the starting string
     */
    StringDef *getStart() const {
        return start;
    }

    /**
     * @return the ending string
     */
    StringDef *getEnd() const {
        return end;
    }

    /**
     * @return the escape string
     */
    StringDef *getEscape() const {
        return escape;
    }

    /**
     * @return whether this element can span multiple lines
     */
    bool isMultiline() const {
        return multiline;
    }

    /**
     * @return whether this element can be nested
     */
    bool isNested() const {
        return nested;
    }
// doublecpp: dispatch methods, DO NOT MODIFY
public:
virtual void dispatch_build(HighlightStateBuilder *, HighlightState * state);
// doublecpp: end, DO NOT MODIFY
};

}

#endif
