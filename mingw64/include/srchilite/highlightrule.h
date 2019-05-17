//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef HIGHLIGHTRULE_H_
#define HIGHLIGHTRULE_H_

#include <string>
#include <boost/shared_ptr.hpp>

#include "highlightstate.h"

namespace srchilite {

struct HighlightToken;
struct MatchingParameters;

/// list representing the names of parts of a program
typedef std::deque<std::string> ElemList;

/**
 * Base class for highlight rules.  This abstracts from the actual
 * implementation for matching.
 */
class HighlightRule {
    /// the list of program elements detected by this rule
    ElemList elemList;

    /// if set, it represents the state to enter if this rule matches
    /// this class does not delete nextState
    HighlightStatePtr nextState;

    /// additional information about this rule
    std::string additionalInfo;

    /// how many state must we exit if we match this rule:
    /// 0: none, -1: all, otherwise the number of states to exit
    int exitLevel;

    /// tells that if this rule matches we must enter the same state once again
    bool nested;

    /// whether this rule has dynamic references to be replaced
    bool needsReferenceReplacement;

    /// whether this rule can match subexpressions
    bool hasSubexpressions;

public:
    HighlightRule();
    /**
     * Creates a rule for the given element (Although each rule can concern
     * more than one program element, we provide only this convenience constructor with
     * only one name: if the rule concerns more than one element one can use
     * addElem method)
     * @param name the element name of this rule
     */
    HighlightRule(const std::string &name);
    virtual ~HighlightRule();

    /**
     * Try to match this rule against the passed string (implemented by calling
     * the pure virtual function tryToMatch below).  The passed token is assumed to be
     * reset (i.e., no existing matching information is stored in it when passing it to
     * this method).
     *
     * @param s the string for trying to match the rule
     * @param token where results will be inserted, if the rule matched
     * @param params additional arguments for the matching
     * @return the result of this matching
     */
    virtual bool tryToMatch(const std::string &s, HighlightToken &token,
            const MatchingParameters &params);

    /**
     * Try to match this rule against the passed string
     *
     * @param start the beginning of the string for trying to match the rule
     * @param end the beginning of the string for trying to match the rule
     * @param token where results will be inserted, if the rule matched
     * @param params additional arguments for the matching
     * @return the result of this matching
     */
    virtual bool tryToMatch(std::string::const_iterator start,
            std::string::const_iterator end, HighlightToken &token,
            const MatchingParameters &params) = 0;

    virtual const std::string toString() const = 0;

    /**
     * Performs replacement of references in this rule.
     * @param the list of values for the replacement; the first element
     * is the value for replacing the first dynamic back reference, and so on.
     * (it should contain 9 possibly empty elements)
     */
    virtual void replaceReferences(const ReplacementList &rep) = 0;

    /**
     * @return a copy of this rule.
     */
    virtual HighlightRule *clone() = 0;

    const HighlightStatePtr getNextState() const {
        return nextState;
    }
    void setNextState(HighlightStatePtr _nextState) {
        nextState = _nextState;
    }

    /**
     * Adds an element name to the list of this rule
     * @param name the name to add
     */
    void addElem(const std::string &name);

    const ElemList &getElemList() const {
        return elemList;
    }

    int getExitLevel() const {
        return exitLevel;
    }
    void setExitLevel(int l) {
        exitLevel = l;
    }

    bool isNested() const {
        return nested;
    }
    void setNested(bool n) {
        nested = n;
    }

    bool getNeedsReferenceReplacement() const {
        return needsReferenceReplacement;
    }

    void setNeedsReferenceReplacement(bool b = true) {
        needsReferenceReplacement = b;
    }

    bool getHasSubexpressions() const {
        return hasSubexpressions;
    }

    void setHasSubexpressions(bool b = true) {
        hasSubexpressions = b;
    }

    std::string getAdditionalInfo() const {
        return additionalInfo;
    }

    void setAdditionalInfo(const std::string &info) {
        additionalInfo = info;
    }
};

}

#endif /*HIGHLIGHTRULE_H_*/
