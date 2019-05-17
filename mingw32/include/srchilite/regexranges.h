/*
 * regexranges.h
 *
 *  Created on: Apr 11, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef REGEXRANGES_H_
#define REGEXRANGES_H_

#include <string>
#include <list>
#include <boost/regex.hpp>

namespace srchilite {

/**
 * Stores possible separators implemented as regular expressions and
 * provides functionalities to search for such occurrences in lines.
 */
class RegexRanges {
public:
    RegexRanges();
    ~RegexRanges();

    typedef std::list<boost::regex> RegexRangesType;

    /**
     * Adds a regular expression range, specified by the passed string.
     *
     * @param s the string representation of the regular expression
     * @return true if the passed string is a valid regular expression, false
     * otherwise
     */
    bool addRegexRange(const std::string &s);

    /**
     * Removes all the added expressions
     */
    void clear() {
        ranges.clear();
    }

    /**
     * Checks whether one of the stored regular expression can be found in the
     * passed string line.
     * @param line
     * @return the matched regular expression or 0 if none was found
     */
    const boost::regex *matches(const std::string &line);

    /**
     * @param line the line to inspect
     * @return whether the passed line is in range
     */
    bool isInRange(const std::string &line);

    /**
     * The next isInRange search will start from the first element of
     * the list.  This should be called before searching for lines of a file,
     * that we started to process.
     */
    void reset() {
        currentRegex = 0;
    }

private:
    /// the actual collection of regular expressions for ranges
    RegexRangesType ranges;

    /**
     * if set, it represents the matched regular expression up to now.
     * This is used internally: if we're searching for a range, the first time
     * we find a matching expression, we found the beginning of the range.
     * The end of the range will be found when we match again the previously
     * matched expression.
     */
    const boost::regex *currentRegex;
};

}

#endif /* REGEXRANGES_H_ */
