//
// C++ Interface: %{MODULE}
//
// Description:
//
//
// Author: %{AUTHOR} <%{EMAIL}>, (C) %{YEAR}
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef STATELANGELEM_H
#define STATELANGELEM_H

#include "statestartlangelem.h"
#include "langelems.h"

namespace srchilite {

class StringDef;
class StringDefs;
class StateStartLangElem;

/**
 * @class StateLangElem
a language element that introduces a new state
pattern Composite

@author Lorenzo Bettini
*/
// doublecpp: forward declarations, DO NOT MODIFY
class HighlightState; // file: highlightstate.h
class HighlightStateBuilder; // file: highlightstatebuilder.h
class LangElemsPrinter; // file: langelemsprinter.h
// doublecpp: end, DO NOT MODIFY

class StateLangElem : public LangElem
{
  private:
    StateStartLangElem *statestartlangelem;
    LangElems *langelems;
    bool state;

public:
  StateLangElem(const std::string &n, StateStartLangElem *start, LangElems *elems, bool st = false);

    ~StateLangElem();

    void set_elems(LangElems *elems) { langelems = elems; }
    void set_state() { state = true; }

    virtual const std::string toString() const;

    virtual const std::string toStringOriginal() const;

    StateStartLangElem *getStateStart() const { return statestartlangelem; }
    bool isState() const { return state; }
    LangElems *getElems() const { return langelems; }
// doublecpp: dispatch methods, DO NOT MODIFY
public:
virtual void dispatch_build(HighlightStateBuilder *, HighlightState * state);
virtual void dispatch_collect_const(LangElemsPrinter *);
// doublecpp: end, DO NOT MODIFY
};

}

#endif
