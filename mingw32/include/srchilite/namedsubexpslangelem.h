//
// C++ Interface: NamedSubExpsLangElem
//
// Description: represents a regular expression made by many marked groups
// and each marked group represents a different language element
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2007-2009
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef NAMEDSUBEXPSLANGELEM_H_
#define NAMEDSUBEXPSLANGELEM_H_

#include "statestartlangelem.h"

#include <list>

namespace srchilite {

class StringDef;

/// collection of element names
typedef std::list<std::string> ElementNames;

/**
 * @class NamedSubExpsLangElem
 * An element with subparts (subexpressions), each with a possible
 * different name.
 */
// doublecpp: forward declarations, DO NOT MODIFY
class HighlightState; // file: highlightstate.h
class HighlightStateBuilder; // file: highlightstatebuilder.h
class LangElemsPrinter; // file: langelemsprinter.h
// doublecpp: end, DO NOT MODIFY

class NamedSubExpsLangElem : public StateStartLangElem
{
    /// the element names
    const ElementNames *elementNames;

    /// the whole regular expression defiition
    StringDef *regexpDef;
public:
    /**
     * @param names the element names (one for each subexpression)
     * @param def the whole definition
     * @param exit whether to exit one state
     * @param all whether to exit all states
     */
	NamedSubExpsLangElem(const ElementNames *names, StringDef *def, bool exit = false, bool all = false);
	virtual ~NamedSubExpsLangElem();

	/**
	 * @return a string representation
	 */
    virtual const std::string toString() const;

    /**
     * @return a string representatio of the original expression (without
     * any preprocessing)
     */
    virtual const std::string toStringOriginal() const;

    /**
     * @return the list of all the element names
     */
    const ElementNames *getElementNames() const { return elementNames; }

    /**
     * @return the complete expression for this element
     */
    const StringDef *getRegexpDef() const { return regexpDef; }

// doublecpp: dispatch methods, DO NOT MODIFY
public:
virtual void dispatch_build(HighlightStateBuilder *, HighlightState * state);
virtual void dispatch_collect_const(LangElemsPrinter *);
// doublecpp: end, DO NOT MODIFY
};

}

#endif /*NAMEDSUBEXPSLANGELEM_H_*/
