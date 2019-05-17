//
// C++ Interface: stringlistlangelem
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2009
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef STRINGLISTLANGELEM_H
#define STRINGLISTLANGELEM_H

#include "statestartlangelem.h"

namespace srchilite {

class StringDefs;

/**
 * @class StringListLangElem
 A language element consisting of a list of strings
 */
// doublecpp: forward declarations, DO NOT MODIFY
class HighlightState; // file: highlightstate.h
class HighlightStateBuilder; // file: highlightstatebuilder.h
// doublecpp: end, DO NOT MODIFY

class StringListLangElem : public StateStartLangElem
{
private:
    StringDefs *alternatives;
    bool nonsensitive;

public:
    /**
     * @param n the name for this element
     * @param defs the alternatives for this element
     * @param nons whether the alternatives are to be considered case unsensitive
     */
    StringListLangElem(const std::string &n, StringDefs *defs, bool nons);

    virtual ~StringListLangElem();

    virtual const std::string toString() const;

    virtual const std::string toStringOriginal() const;

    /**
     * @return the collection of all the alternatives of this element
     */
    StringDefs *getAlternatives() const {
        return alternatives;
    }

    /**
     * @return whether the alternatives are case sensitive
     */
    bool isCaseSensitive() const {
        return !nonsensitive;
    }
// doublecpp: dispatch methods, DO NOT MODIFY
public:
virtual void dispatch_build(HighlightStateBuilder *, HighlightState * state);
// doublecpp: end, DO NOT MODIFY
};

}

#endif
