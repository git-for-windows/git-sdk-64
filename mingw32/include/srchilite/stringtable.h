//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef STRINGTABLE_H_
#define STRINGTABLE_H_

#include <list>
#include <string>

namespace srchilite {

/**
 * Stores dynamically allocated strings (used by the scanners), so that,
 * later on, we can easily free all the memory for these strings.
 */
class StringTable : public std::list<std::string *> {
public:
    StringTable();
    ~StringTable();

    std::string *newString(const std::string &s);
};

}

#endif /*STRINGTABLE_H_*/
