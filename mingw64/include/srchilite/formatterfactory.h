//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef FORMATTERFACTORY_H_
#define FORMATTERFACTORY_H_

#include <string>
#include <list>
#include <boost/shared_ptr.hpp>

using std::string;

namespace srchilite {

/// constants indicating boldface, italics, etc.
enum StyleConstant {
    ISBOLD = 1, ///< bold
    ISITALIC, ///< italics
    ISUNDERLINE,  ///< underline
    ISFIXED, ///< fixed size
    ISNOTFIXED,  ///< non fixed size
    ISNOREF ///< must not contain references
};

/// collection of StyleConstant objects
typedef std::list<StyleConstant> StyleConstants;

/// shared pointer for StyleConstants
typedef boost::shared_ptr<StyleConstants> StyleConstantsPtr;

/// iterator for StyleConstants
typedef StyleConstants::const_iterator StyleConstantsIterator;

/**
 * The generic abstract factory to create Formatter objects, during the
 * parsing of style files.  Note that creation methods do not return a specific
 * object, so that the implementation of where to store the Formatter objects
 * themselves is left completely to the programmer (and does not couple the factory
 * to a specific collection).
 */
class FormatterFactory {
public:
    virtual ~FormatterFactory() {
    }

    /**
     * Creates a formatter for the specific language element (identified by
     * key) with the passed style parameters
     *
     * @param key the language element represented
     * @param color the color
     * @param bgcolor the background color
     * @param styleconstants additional formatting information (e.g., bold, italics, etc.)
     * @return false if a formatter for the specific key is already present
     */
    virtual bool createFormatter(const string &key, const string &color,
            const string &bgcolor, StyleConstantsPtr styleconstants) = 0;

};

}

#endif /*FORMATTERFACTORY_H_*/
