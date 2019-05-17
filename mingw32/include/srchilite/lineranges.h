/*
 * lineranges.h
 *
 *  Created on: Sep 17, 2008
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef LINERANGES_H_
#define LINERANGES_H_

#include <set>
#include <string>

namespace srchilite {

/// a possible error in specifying a range
enum RangeError {
    NO_ERROR = 0, INVALID_RANGE_NUMBER
};

/// result for a check whether a number is in a range (or in a context)
enum RangeResult {
    NOT_IN_RANGE = 0, CONTEXT_RANGE, IN_RANGE
};

/**
 * Functionalities for detecting whether a line is in one of the
 * stored line ranges (or in the context of a range).
 *
 * This also performs some optimization: ranges are stored in a set
 * in ascending order; if we have ranges, e.g., 1-20, 50-70, 100-200, and
 * we check whether 23 is in range, we first check if it's in 1-20, then,
 * since it's not there, we check whether it's in 50-70; it is not, and since the
 * range is 50-70 it makes no sense searching for it in other ranges.  The next search
 * will start from range 50-70, since we assume that line numbers are always increasing.
 */
class LineRanges {
public:
    LineRanges(unsigned int contextLines = 0);
    ~LineRanges();

    typedef int RangeElemType;
    typedef std::pair<RangeElemType, RangeElemType> RangeType;

    typedef std::set<RangeType> LineRangeSet;

    /**
     * Adds a range to the set.
     * The argument can be:
     *
     * - a single element (means only one line)
     * - a complete range (e.g., 20-35)
     * - a partial range (e.g., 10- : from line 10 to the end,
     *   -20 : from the beginning to line 20)
     *
     * @param range the string representing the range.
     * @return code specifying a possible error
     */
    RangeError addRange(const std::string &range);

    const LineRangeSet &getLineRangeSet() const {
        return lineRangeSet;
    }

    /**
     * The next isInRange search will start from the first element of
     * the set.  This should be called before searching for lines of a file,
     * that we started to process.
     */
    void reset() {
        searchFromTheStart = true;
    }

    /**
     * Checks whether the passed element is in a range of this set.
     * If it's not in the range it might be in the surrounding context.
     *
     * @param e
     * @return whether the passed element is in a range or in the surrounding context.
     */
    RangeResult isInRange(const RangeElemType e);

    void setContextLines(unsigned int context) {
        contextLines = context;
    }

private:
    LineRangeSet lineRangeSet;

    /**
     * whether to perform the search from the first element of the set
     */
    bool searchFromTheStart;

    /**
     * The current range for performing the search of isInRange.
     */
    LineRangeSet::const_iterator currentRange;

    /**
     * The number of lines making the context (i.e., the number of lines
     * that are not part of a range but are in the specified line number context)
     */
    int contextLines;
};

}

#endif /* LINERANGES_H_ */
