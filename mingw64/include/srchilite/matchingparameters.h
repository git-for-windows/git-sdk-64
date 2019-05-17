//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef MATCHINGPARAMETERS_H_
#define MATCHINGPARAMETERS_H_

namespace srchilite {

/**
 * Structure for passing parameters to the matching algorithm,
 * for instance, "not beginning of line", etc.
 */
struct MatchingParameters {
    /// whether we are inspecting the beginning of a line
    bool beginningOfLine;

    MatchingParameters() :
        beginningOfLine(true) {
    }
};

}

#endif /*MATCHINGPARAMETERS_H_*/
