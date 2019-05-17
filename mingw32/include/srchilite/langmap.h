//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef LANGMAP_H
#define LANGMAP_H

#include <string>
#include <map>
#include <set>

namespace srchilite {

/**
 * A map stored in a file with the format
 * key = elem.
 *
 * Comment line start with a #
 *
 * This is used for lang.map, outlang.map and style.defaults (and for any other
 * files that have this simple syntax).
 *
 * Methods of this class can throw ParserException if they can't parse
 * the map file because of a syntax error.
 *
 */
class LangMap {
    typedef std::map<std::string, std::string> Map;

    Map langmap;

    /// whether the corresponding file is opened
    bool isOpen;

    /// the path for searching for the map file name
    std::string path;

    /// the map file name
    std::string filename;

public:
    /**
     * A LangMap object based on the passed map file (using the specified
     * path).
     * @param path the path where to search for the filename
     * @param filename the map file
     */
    LangMap(const std::string &path, const std::string &filename);

    /**
     * A LangMap object based on the passed map file (using the
     * path stored in the settings or the default hardcoded one).
     * @param filename the map file
     */
    LangMap(const std::string &filename);

    ~LangMap();

    typedef Map::const_iterator const_iterator;

    const_iterator begin() {
        return langmap.begin();
    }

    const_iterator end() {
        return langmap.end();
    }

    /**
     * Prints the contents on the map to the standard output.
     * This is basically for testing purposes.
     */
    void print();

    /**
     * Open the corresponding file (if it is not already opened) and read
     * and parse its contents.  If the map file has been already opened
     * (and parsed) this does nothing.
     * @throws ParserException in case the lang map file is not correct.
     */
    void open();

    /**
     * Returns the lang file name corresponding to the passed language name.
     * The lang map file must have already been opened (if you want to be sure
     * of that, use getMappedFileName instead).
     *
     * @param lang the language name whose lang definition file name we want
     * to retrieve
     * @return the file name of the lang definition file corresponding to
     * the specified language (or the empty string if there's no such
     * association)
     */
    const std::string getFileName(const std::string &lang) {
        return langmap[lang];
    }

    /**
     * Returns the lang file name corresponding to the passed language name.
     * The lang map file is opened and parsed if it has not been inspected yet.
     *
     * @param lang the language name whose lang definition file name we want
     * to retrieve
     * @return the file name of the lang definition file corresponding to
     * the specified language (or the empty string if there's no such
     * association)
     * @throws ParserException in case the lang map file is not correct.
     */
    const std::string getMappedFileName(const std::string &lang);

    /**
     * Tries to detect the corresponding lang file name, from the file name
     * passed as parameter.
     * In order to do so it proceeds as follows (until it succeeds):
     * -# Use the file extension if any (for instance, if the passed file name
     * is <em>foo.cpp</em> it looks for the lang def file associated to <em>cpp</em>)
     * -# Use the lower case file extension if any
     * -# Use the file name if it has no extension (for instance, if the
     * passed file name is <em>changelog</em>, then looks for the lang file associated
     * to <em>changelog</em>)
     * -# Use the file name in lowercase (for instance, if the
     * passed file name is <em>ChangeLog</em>, then looks for the lang file to <em>changelog</em>)
     *
     * This is useful, e.g., for highlighting a file using its file name
     * for detecting its source language.  Similarly, for choosing the
     * output format by using the name of the output file.
     *
     * This method opens the lang map file if it hasn't been parsed yet.
     *
     * @param fileName the language name whose lang definition file name we want
     * to retrieve
     * @return the file name of the lang definition file corresponding to
     * the specified file name (or the empty string if there's no such
     * association)
     * @throws ParserException in case the lang map file is not correct.
     */
    const std::string getMappedFileNameFromFileName(const std::string &fileName);

    /**
     * Returns a set (i.e., an ordered list) of all the lang names of this
     * map.
     * @return the lang names of this map
     */
    std::set<std::string> getLangNames() const;

    /**
     * Returns a set (i.e., an ordered list) of all the mapped lang file names of this
     * map.
     * @return the lang file names of this map
     */
    std::set<std::string> getMappedFileNames() const;

    /**
     * Reloads the contents of this map, using the (new) path and
     * filename
     * @param path the path where to search for the filename
     * @param filename the map file
     */
    void reload(const std::string &path, const std::string &filename);

};

}

#endif
