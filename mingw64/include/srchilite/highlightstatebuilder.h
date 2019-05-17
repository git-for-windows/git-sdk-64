//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef HIGHLIGHTSTATEBUILDER_H_
#define HIGHLIGHTSTATEBUILDER_H_

#include "highlightstate.h"

namespace srchilite {

class LangElems;
class HighlightRuleFactory;

/**
 * Builds an HighlightState from the language definition file collected structures.
 * This class uses dynamic overloading and it must be processed by doublecpp,
 * http://doublecpp.sf.net, in case you need to modify it.
 */
class HighlightStateBuilder {
    /// the factory for creating rules
    HighlightRuleFactory *highlightRuleFactory;
public:
    HighlightStateBuilder(HighlightRuleFactory *_highlightRuleFactory);
    virtual ~HighlightStateBuilder();

    /**
     * Builds all the rules into the passed mainState as specified in the
     * passed LangElems
     *
     * @param elems the element definitions retrieved from the lang file
     * @param mainState the main state where to store all the rules
     */
    void build(LangElems *elems, HighlightStatePtr mainState);

    /// the following is a multi-method that needs to be processed by doublecpp
    branches build
    void (LangElem *elem, HighlightState *state);
    void (StringListLangElem *elem, HighlightState *state);
    void (DelimitedLangElem *elem, HighlightState *state);
    void (NamedSubExpsLangElem *elem, HighlightState *state);
    void (StateLangElem *elem, HighlightState *state);
    endbranches
};

}

#endif /*HIGHLIGHTSTATEBUILDER_H_*/
