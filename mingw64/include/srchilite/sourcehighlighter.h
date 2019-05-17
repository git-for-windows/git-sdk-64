//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef SOURCEHIGHLIGHTER_H_
#define SOURCEHIGHLIGHTER_H_

#include <string>
#include <stack>
#include <sstream>
#include <boost/shared_ptr.hpp>

#include "highlightstate.h"
#include "eventgenerator.h"

namespace srchilite {

class FormatterManager;
struct HighlightToken;
struct FormatterParams;
class HighlightEventListener;
struct HighlightEvent;

typedef std::stack<HighlightStatePtr> HighlightStateStack;
typedef boost::shared_ptr<HighlightStateStack> HighlightStateStackPtr;

/**
 * The main class performing the highlighting of a single line.  It relies on a HighlightState
 * (and its HighlightRule objects).
 *
 * It provides the method highlightParagraph() to highlight a single line.
 *
 * The current highlighting state can be retrieved with getCurrentState().
 *
 * The highlighting state is not reset after highlighting a line, thus, the
 * same object can be used to highlight, for instance, an entire file, by
 * calling highlightParagraph on each line.
 *
 * An example of use of this class is in infoformatter-main.cpp
 */
class SourceHighlighter: public EventGenerator<HighlightEventListener,
        HighlightEvent> {
    /// the main (and initial) highlight state
    HighlightStatePtr mainHighlightState;

    /// the current highlight state
    HighlightStatePtr currentHighlightState;

    /// the stack for the highlight states
    HighlightStateStackPtr stateStack;

    /// the formatter manager, used to format element strings
    const FormatterManager *formatterManager;

    /**
     * Whether to optimize output (e.g., adjacent text parts belonging
     * to the same element will be buffered and generated as a single text part)
     */
    bool optimize;

    /**
     * Whether formatting is currently suspended.  Note that matching for
     * regular expressions is not suspended: only the actual output of formatted
     * code is suspended.
     */
    bool suspended;

    /**
     * Additional parameters for the formatters
     */
    FormatterParams *formatterParams;

    /**
     * The current element being formatted (used for optmization and buffering)
     */
    std::string currentElement;

    /**
     * The buffer for the text for the current element
     */
    std::ostringstream currentElementBuffer;

    /**
     * Enters a new state (using the stack)
     * @param state
     */
    void enterState(HighlightStatePtr state);

    /**
     * Exits level states (-1 means exit all states)
     * @param level
     */
    void exitState(int level);

    /**
     * Exits all states in the stack (and thus go back to the initial main state)
     */
    void exitAll();

    /**
     * Computes the (possible) next state for the given rule (if required, also
     * performs reference replacement)
     * @param token
     */
    HighlightStatePtr getNextState(const HighlightToken &token);

    /**
     * Formats the given string as the specified element
     * @param elem
     * @param s
     */
    void format(const std::string &elem, const std::string &s);

    /**
     * Makes sure to flush the possible buffer of the current element
     * (e.g., during optimizations)
     */
    void flush();

public:
    /**
     * @param mainState the main and initial state for highlighting
     */
    SourceHighlighter(HighlightStatePtr mainState);
    ~SourceHighlighter();

    /**
     * Highlights a paragraph (a line actually)
     * @param paragraph
     */
    void highlightParagraph(const std::string &paragraph);

    HighlightStatePtr getCurrentState() const {
        return currentHighlightState;
    }

    void setCurrentState(HighlightStatePtr state) {
        currentHighlightState = state;
    }

    HighlightStateStackPtr getStateStack() {
        return stateStack;
    }

    void setStateStack(HighlightStateStackPtr state) {
        stateStack = state;
    }

    /**
     * Clears the statck of states
     */
    void clearStateStack();

    HighlightStatePtr getMainState() const {
        return mainHighlightState;
    }

    const FormatterManager *getFormatterManager() const {
        return formatterManager;
    }

    void setFormatterManager(const FormatterManager *_formatterManager) {
        formatterManager = _formatterManager;
    }

    bool getOptimize() const {
        return optimize;
    }

    void setOptimize(bool b = true) {
        optimize = b;
    }

    void setFormatterParams(FormatterParams *p) {
        formatterParams = p;
    }

    bool isSuspended() const {
        return suspended;
    }

    void setSuspended(bool b = true) {
        suspended = b;
    }
};

}

#endif /*SOURCEHIGHLIGHTER_H_*/
