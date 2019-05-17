//
// C++ Interface: parsestyles
//
// Description: declaration of function for parsing style files
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2007
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef PARSESTYLES_H
#define PARSESTYLES_H

#include <string>

#include "formatterfactory.h"

namespace srchilite {

/**
 * Parses the specified style file, and creates the corresponding formatters,
 * using the passed FormatterFactory.
 *
 * @param path the path for searching for style files
 * @param name the style file name
 * @param formatterFactory
 * @param bodyBgColor the background color for the document (can be an empty string)
 */
extern void parseStyles(const std::string &path, const std::string &name,
        FormatterFactory *formatterFactory, std::string &bodyBgColor);

/**
 * Parses the specified style file, and creates the corresponding formatters,
 * using the passed FormatterFactory.  (For the default searching path, it uses
 * the hardcoded data dir).
 *
 * @param name the style file name
 * @param formatterFactory
 * @param bodyBgColor the background color for the document (output parameter)
 */
extern void parseStyles(const std::string &name, FormatterFactory *formatterFactory,
        std::string &bodyBgColor);

extern void parseStyleError(const std::string &error);

/// for css style files
extern void parseCssStyles(const std::string &path, const std::string &name,
        FormatterFactory *formatterFactory, std::string &bodyBgColor);

}

#endif
