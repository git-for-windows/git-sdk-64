/*
 * sourcehighlightutils.h
 *
 *  Created on: May 19, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef SOURCEHIGHLIGHTUTILS_H_
#define SOURCEHIGHLIGHTUTILS_H_

#include <set>
#include <string>

namespace srchilite {

/// a set of strings
typedef std::set<std::string> StringSet;

/**
 * Class with some utility static methods.
 *
 * @since 3.1
 */
class SourceHighlightUtils {
public:
    /**
     * @param path the path to search for files
     * @param fileExtension the file extension for files (without the dot)
     * @return a set of string with file names matching the specified file extension in
     * the specified path
     * @throw IOException if it cannot open the specified path
     */
    static StringSet getFileNames(const std::string path, const std::string fileExtension);

    /**
     * @param path the path to search for files (if left empty, it uses the standard data dir path)
     * @return a set of string with file names of style files
     * @throw IOException if it cannot open the specified path
     */
    static StringSet getStyleFileNames(const std::string path = "");

    /**
     * @param path the path to search for files (if left empty, it uses the standard data dir path)
     * @return a set of string with file names of css style files
     * @throw IOException if it cannot open the specified path
     */
    static StringSet getCssStyleFileNames(const std::string path = "");

    /**
     * @param path the path to search for files (if left empty, it uses the standard data dir path)
     * @return a set of string with file names of lang definition files
     * @throw IOException if it cannot open the specified path
     * @since 3.1.1
     */
    static StringSet getLangFileNames(const std::string path = "");

    /**
     * @param path the path to search for files (if left empty, it uses the standard data dir path)
     * @return a set of string with file names of outlang definition files
     * @throw IOException if it cannot open the specified path
     * @since 3.1.1
     */
    static StringSet getOutLangFileNames(const std::string path = "");
};

}

#endif /* SOURCEHIGHLIGHTUTILS_H_ */
