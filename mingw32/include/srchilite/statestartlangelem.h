//
// C++ Interface: statestartlangelem
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef STATESTARTLANGELEM_H
#define STATESTARTLANGELEM_H

#include "langelem.h"

namespace srchilite {

class StringDef;
class StateLangElem;

/**
 A language element that may start a new state/environment
 */
class StateStartLangElem: public LangElem {
private:
    /// the exit level
    unsigned int exit;

    /// whether to exit all states
    bool exit_all;

    /// the possible State of which we represent the start.
    StateLangElem *statelangelem;

public:
    /**
     * @param names the element names (one for each subexpression)
     * @param exit whether to exit a number of states (default 0)
     * @param all whether to exit all states
     */
    StateStartLangElem(const std::string &n, unsigned int exit = 0, bool all =
            false);

    virtual ~StateStartLangElem();

    virtual const std::string toString() const;

    /**
     * Sets the "exit" property of this element (i.e., if the element is match
     * then exit one state)
     * @param level the exit level (default to 1)
     */
    void setExit(unsigned int level = 1) {
        exit = level;
    }

    /**
     * Sets the "exit all" property of this element (i.e., if the element is match
     * then exit all states and get back to the main initial state)
     */
    void setExitAll() {
        exit_all = true;
    }

    /**
     * @return whether the "exit" property is set
     */
    bool exitAll() const {
        return exit_all;
    }

    /**
     * @return whether the "exit all" property is set
     */
    unsigned int getExit() const {
        return exit;
    }

    /**
     * @return the state for which this element represents the start
     */
    StateLangElem *getStateLangElem() const {
        return statelangelem;
    }

    /**
     * Sets the state for which this element represents the start
     * @param s the state for which this element represents the start
     */
    void setStateLangElem(StateLangElem *s) {
        statelangelem = s;
    }
};

}

#endif
