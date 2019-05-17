//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 1999-2009
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef LANGELEM_H
#define LANGELEM_H

#include "parserinfo.h"

namespace srchilite {

/**
 * Represents a generic language definition element
 */
// doublecpp: forward declarations, DO NOT MODIFY
class HighlightState; // file: highlightstate.h
class HighlightStateBuilder; // file: highlightstatebuilder.h
class LangElemsPrinter; // file: langelemsprinter.h
// doublecpp: end, DO NOT MODIFY

class LangElem : public ParserInfo
{
private:
    /// the name for this language element
    const std::string name;
    /// whether this redefs an existing language element
    bool redef;
    // whether this substitutes an existing language element
    bool subst;

public:
    LangElem(const std::string &n) :
        name(n), redef(false), subst(false) {
    }

    virtual ~LangElem() {
    }

    const std::string getName() const {
        return name;
    }

    /**
     * return the string representation (with preprocessing)
     * @return the string representation
     */
    virtual const std::string toString() const {
        return name;
    }

    /**
     * return the original representation (without any preprocessing);
     * this is useful for printing errors
     * @return the original representation
     */
    virtual const std::string toStringOriginal() const = 0;

    bool isRedef() const {
        return redef;
    }
    void setRedef() {
        redef = true;
    }
    bool isSubst() const {
        return subst;
    }
    void setSubst() {
        subst = true;
    }

    /**
     * @return a string representation of the ParserInfo struct
     */
    const std::string toStringParserInfo() const;

// doublecpp: dispatch methods, DO NOT MODIFY
public:
virtual void dispatch_build(HighlightStateBuilder *, HighlightState * state);
virtual void dispatch_collect_const(LangElemsPrinter *);
// doublecpp: end, DO NOT MODIFY
};    

}

#endif
