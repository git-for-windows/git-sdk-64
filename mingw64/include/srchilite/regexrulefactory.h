//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef REGEXRULEFACTORY_H_
#define REGEXRULEFACTORY_H_

#include "highlightrulefactory.h"

namespace srchilite {

/**
 * Implementation of the abstract factory, creating highlighting rules
 * based on boost::regex regular expressions.
 */
class RegexRuleFactory : public HighlightRuleFactory {
public:
    RegexRuleFactory();
    virtual ~RegexRuleFactory();

    virtual HighlightRule *createSimpleRule(const std::string &name,
            const std::string &s);

    virtual HighlightRule *createWordListRule(const std::string &name,
                const WordList &list, bool caseSensitve = true);

    virtual HighlightRule *createListRule(const std::string &name,
                const WordList &list, bool caseSensitve = true);

    virtual HighlightRule *createLineRule(const std::string &name,
            const std::string &start, const std::string &end,
            const std::string &escape = "", bool nested = false);

    virtual HighlightRule *createMultiLineRule(const std::string &name,
            const std::string &start, const std::string &end,
            const std::string &escape, bool nested);

    virtual HighlightRule *createCompoundRule(const ElemNameList &nameList,
            const std::string &rep);
};

}

#endif /*REGEXRULEFACTORY_H_*/
