//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef HIGHLIGHTTOKEN_H_
#define HIGHLIGHTTOKEN_H_

#include <string>
#include <list>
#include <vector>
#include <algorithm>

namespace srchilite {

class HighlightRule;

/**
 * The matched element information by a rule. Each element of the collection is:
 * first = the element name, second = the actual program string
 */
typedef std::list<std::pair<std::string, std::string> > MatchedElements;

/**
 * The matched subexpressions (if the original rule had subexpressions)
 */
typedef std::vector<std::string> MatchedSubExps;

/**
 * Token containing information for performing the highlight
 */
struct HighlightToken {
    /// the possible prefix (part before the matched string)
    std::string prefix;

    /// true if the prefix is empty or contains only spaces
    bool prefixOnlySpaces;

    /// the possible suffix (part after the matched string)
    std::string suffix;

    /// the matched elements information
    MatchedElements matched;

    /** the size of the whole matched sequence (this is computed automatically
     when matched elements are set or added) */
    unsigned int matchedSize;

    /**
     * The matched subexpressions (in case the rule had subexpressions)
     */
    MatchedSubExps matchedSubExps;

    /// the matching rule
    const HighlightRule *rule;

    HighlightToken(const HighlightRule *_rule = 0);
    HighlightToken(const std::string &elem, const std::string &matched,
            const std::string &_prefix, const HighlightRule *_rule = 0);
    ~HighlightToken();

    /**
     * Copy from the passed toke
     * @param token
     */
    void copyFrom(const HighlightToken &token) {
        prefix = token.prefix;
        suffix = token.suffix;
        matched = token.matched;
        matchedSize = token.matchedSize;
        matchedSubExps = token.matchedSubExps;
        rule = token.rule;
    }

    /**
     * Resets the matched related fields (i.e., matched, matchedSize)
     */
    void clearMatched();

    /**
     * Adds information about a matched element
     * @param elem the element name
     * @param s the matched string
     */
    void addMatched(const std::string &elem, const std::string &s);
};

}

#endif /*HIGHLIGHTTOKEN_H_*/
