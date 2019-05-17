//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef FORMATTERMANAGER_H_
#define FORMATTERMANAGER_H_

#include <string>
#include <map>

#include "formatter.h"

namespace srchilite {

/// the map for formatters
typedef std::map<std::string, FormatterPtr> FormatterMap;

/**
 * Associates to an element name the corresponding formatter.
 * This class uses shared pointers for formatters
 */
class FormatterManager {
    /// the map associating to each element name a formatter
    mutable FormatterMap formatterMap;

    /// the default formatter, i.e., the one that is used when there's no
    /// formatter associated to an element name
    FormatterPtr defaultFormatter;
public:
    /**
     * @param _defaultFormatter the default formatter, i.e., the one that is used when there's no
     * formatter associated to an element name
     */
    FormatterManager(FormatterPtr _defaultFormatter);
    ~FormatterManager();

    /**
     * Returns the formatter for the specific element (this function always returns
     * a valid pointer, since if no formatter is found for the specified element, it will
     * return the default formatter)
     * @param elem
     * @return the formatter for the specific element
     */
    FormatterPtr getFormatter(const std::string &elem) const;

    FormatterPtr getDefaultFormatter() const {
        return defaultFormatter;
    }

    void setDefaultFormatter(FormatterPtr def) {
        defaultFormatter = def;
    }

    /**
     * Returns the formatter for the specific element or an empty pointer if there's
     * no such formatter
     * @param elem
     * @return the formatter for the specific element or an empty pointer
     */
    FormatterPtr hasFormatter(const std::string &elem) const;

    /**
     * Associates the formatter to the element name (possible previous associated formatter
     * is discarded).
     * @param elem
     * @param formatter
     */
    void addFormatter(const std::string &elem, FormatterPtr formatter);

    /**
     * Resets this formatter manager: it removes all the current associations.
     */
    void reset() {
        formatterMap.clear();
    }

    /**
     * @return the formatter map of this formatter manager
     */
    const FormatterMap &getFormatterMap() const {
        return formatterMap;
    }
};

}

#endif /*FORMATTERMANAGER_H_*/
