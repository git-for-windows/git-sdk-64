//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef UTILS_H
#define UTILS_H

#include <string>

namespace srchilite {

/**
generic utils functions (e.g., string manipulation)
*/
class Utils{
public:
    /**
     * Capitalizes the passed string
     * @param s the string to capitalize
     */
    static void toUpper(std::string &s);

    /**
     * Lowerizes the passed string
     * @param s the string to lowerize
     */
    static void toLower(std::string &s);

    /**
     * returns a capitalized version of the passed string
     * @param s the string to capitalize
     * @return a capitalized version of the passed string
     */
    static std::string toupper(const std::string &s);

    /**
     * returns a lowerized version of the passed string
     * @param s the string to lowerize
     * @return a lowerized version of the passed string
     */
    static std::string tolower(const std::string &s);

};

}

#endif
