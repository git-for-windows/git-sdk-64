//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef VARDEFINITIONS_H
#define VARDEFINITIONS_H

#include <map>
#include <string>

namespace srchilite {

class StringDefs;

/**
store definitions of strings and regular expressions representing language elements
*/
class VarDefinitions : protected std::map<std::string, std::string>
{
public:
    VarDefinitions();

    ~VarDefinitions();

    void add(const std::string &var, const StringDefs *value);
    const std::string &getVar(const std::string &name);
    bool contains(const std::string &name);
};

}

#endif
