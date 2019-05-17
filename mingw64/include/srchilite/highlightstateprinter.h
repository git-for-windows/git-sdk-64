//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//


#ifndef HIGHLIGHTSTATEPRINTER_H_
#define HIGHLIGHTSTATEPRINTER_H_

#include <set>
#include <iostream>

namespace srchilite {

class HighlightState;
class HighlightRule;
struct HighlightToken;

typedef std::set<int> StateIdSet;

/**
 * Prints an HighlightState (and all its rules)
 */
class HighlightStatePrinter {
private:
    /// the indentation level
    int indent;
    /// to avoid infinite loops due to recursive states
    StateIdSet stateidset;
    /// the stream to print the state (default cout)
    std::ostream &stream;

public:
    HighlightStatePrinter(std::ostream &s = std::cout);
    ~HighlightStatePrinter();

    void printHighlightState(const HighlightState *state);
    void printHighlightRule(const HighlightRule *rule);
    void printHighlightToken(const HighlightToken *token);
};

}

#endif /*HIGHLIGHTSTATEPRINTER_H_*/
