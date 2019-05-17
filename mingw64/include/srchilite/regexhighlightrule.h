//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef REGEXHIGHLIGHTRULE_H_
#define REGEXHIGHLIGHTRULE_H_

#include "highlightrule.h"

#include <boost/regex.hpp>

namespace srchilite {

/**
 * An implementation of HighlightRule using Boost regex library
 */
class RegexHighlightRule : public HighlightRule {
    /// the regular expression for the rule
    boost::regex regExp;
public:
    /**
     * @param s the string representing the regular expression
     */
    RegexHighlightRule(const std::string &s);

    /**
     * @param name the element name represented by this rule
     * @param s the string representing the regular expression
     */
    RegexHighlightRule(const std::string &name, const std::string &s);
    virtual ~RegexHighlightRule();

    virtual bool tryToMatch(std::string::const_iterator start,
            std::string::const_iterator end, HighlightToken &token,
            const MatchingParameters &params);

    virtual const std::string toString() const;

    virtual void replaceReferences(const ReplacementList &rep);

    virtual HighlightRule *clone();

    void setRegExp(const std::string &s);
};

}

#endif /*REGEXHIGHLIGHTRULE_H_*/
