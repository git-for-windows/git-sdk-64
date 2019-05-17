//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef DEBUGLISTENER_H_
#define DEBUGLISTENER_H_

#include <iostream>

#include "highlighteventlistener.h"

namespace srchilite {

struct HighlightEvent;

/**
 * Implementation of highlight events that prints debug information.
 * In interactive mode, after each event, it will wait for the user
 * to press ENTER to go on.
 */
class DebugListener: public HighlightEventListener {
    /// where to output debug info (default stdout)
    std::ostream &os;

    /// whether to act in step mode (wait for the user ENTER after each step)
    bool interactive;
public:
    /**
     * @param os the output stream to print debug information (default std::cout)
     */
    DebugListener(std::ostream &_os = std::cout);
    virtual ~DebugListener();

    virtual void notify(const HighlightEvent &event);

    void setInteractive(bool i = true) {
        interactive = i;
    }

    /**
     * Waits for the user to press ENTER (in case of interactive debugging)
     */
    void step();
};

}

#endif /*DEBUGLISTENER_H_*/
