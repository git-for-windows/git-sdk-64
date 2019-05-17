//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef LANGDEFMANAGER_H_
#define LANGDEFMANAGER_H_

#include <map>

#include "highlightstate.h"

namespace srchilite {

/// store already generated HighlightState specific to a given file name
typedef std::map<std::string, HighlightStatePtr> HighlightStateCache;

class HighlightRuleFactory;
class LangElems;

/**
 * Handles langdef language definition files
 */
class LangDefManager {
    /// the factory for creating highlight rules
    HighlightRuleFactory *ruleFactory;

    /// store already generated HighlightState specific to a given file name
    HighlightStateCache highlightStateCache;
public:
    /**
     * @param ruleFactory the factory for creating highlight rules
     */
    LangDefManager(HighlightRuleFactory *ruleFactory);
    ~LangDefManager();

    /**
     * Builds the HighlightState corresponding to the
     * language definition by the specified file, using the specified path.
     * @param path the path that is used for searching for the file and included files
     * @param file the name of the language definition file
     * @return the HighlightState built using the language definitions
     */
    HighlightStatePtr buildHighlightState(const std::string &path,
            const std::string &file);

    /**
     * Gets the HighlightState corresponding to the
     * language definition file, using the specified
     * path.  If the language definition file was already inspected, then it
     * won't rebuild the HighlightState, but it will reuse the previously built one.
     * @param path the path that is used for searching for the file and included files
     * @param file the name of the language definition file
     * @return the HighlightState built using the language definitions
     */
    HighlightStatePtr getHighlightState(const std::string &path,
            const std::string &file);

    /**
     * Gets the HighlightState corresponding to the
     * language definition by the specified file, using the default path
     * for language definition files (either stored in the user home setting
     * configuration file or the default hardcoded one).
     * @param file the name of the language definition file
     * @return the HighlightState built using the language definitions
     */
    HighlightStatePtr getHighlightState(const std::string &file);

    /**
     * Returns the language elements of the specified language definition file.
     * @param path
     * @param file
     * @return the language elements of the specified language definition file.
     */
    LangElems *getLangElems(const std::string &path, const std::string &file);

    const HighlightRuleFactory *getRuleFactory() const {
        return ruleFactory;
    }
};

}

#endif /*LANGDEFMANAGER_H_*/
