//
// C++ Interface: parserinfo
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005-2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef PARSERINFO_H
#define PARSERINFO_H

#include <string>

namespace srchilite {

/**
 Stores information about the file name and the line number of a generic element created
 during parsing
 */
struct ParserInfo {
    std::string filename; // including path
    unsigned int line;

    ParserInfo() :
        line(0) {
    }
    ParserInfo(const std::string &n) :
        filename(n), line(0) {
    }

    void setParserInfo(const std::string &name, unsigned int l) {
        filename = name;
        line = l;
    }

    void setParserInfo(const ParserInfo *p) {
        filename = p->filename;
        line = p->line;
    }
};

}

#endif
