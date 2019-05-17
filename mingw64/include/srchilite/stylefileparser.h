/*
 * stylefileparser.h
 *
 *  Created on: Dec 9, 2008
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef STYLEFILEPARSER_H_
#define STYLEFILEPARSER_H_

#include <string>

#include "formatterfactory.h"

namespace srchilite {

/**
 * A utility class with static methods to parse style files
 */
class StyleFileParser {
public:
    /**
     * Parses the specified style file, and creates the corresponding formatters,
     * using the passed FormatterFactory.  (For the default searching path, it uses
     * the hardcoded data dir).
     *
     * @param name the style file name
     * @param formatterFactory
     * @param bodyBgColor the background color for the document (output parameter)
     * @throws ParserException in case of parsing error
     */
    static void parseStyleFile(const std::string &name,
            FormatterFactory *formatterFactory, std::string &bodyBgColor);

    /**
     * Parses the specified css style file, and creates the corresponding formatters,
     * using the passed FormatterFactory.  (For the default searching path, it uses
     * the hardcoded data dir).
     *
     * @param name the css style file name
     * @param formatterFactory
     * @param bodyBgColor the background color for the document (output parameter)
     * @throws ParserException in case of parsing error
     */
    static void parseCssStyleFile(const std::string &name,
            FormatterFactory *formatterFactory, std::string &bodyBgColor);

    /**
     * Parses the specified style file, in the specified path, and creates the corresponding formatters,
     * using the passed FormatterFactory.
     *
     * @param path where to look for the file
     * @param name the style file name
     * @param formatterFactory
     * @param bodyBgColor the background color for the document (output parameter)
     * @throws ParserException in case of parsing error
     */
    static void parseStyleFile(const std::string &path, const std::string &name,
            FormatterFactory *formatterFactory, std::string &bodyBgColor);

    /**
     * Parses the specified css style file, in the specified path, and creates the corresponding formatters,
     * using the passed FormatterFactory.
     *
     * @param path where to look for the file
     * @param name the css style file name
     * @param formatterFactory
     * @param bodyBgColor the background color for the document (output parameter)
     * @throws ParserException in case of parsing error
     */
    static void parseCssStyleFile(const std::string &path, const std::string &name,
            FormatterFactory *formatterFactory, std::string &bodyBgColor);
};

}

#endif /* STYLEFILEPARSER_H_ */
