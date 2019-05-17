//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef PARSEREXCEPTION_H_
#define PARSEREXCEPTION_H_

#include <ostream>
#include <string>
#include <exception>

namespace srchilite {

struct ParseStruct;

/**
 * Represents an exception during parsing, for instance,
 * syntax errors.
 */
struct ParserException : public std::exception {
    /// detailed message for this exception
    std::string message;

    /// additional explaining message
    std::string additional;

    /// filename of the element that caused this exception
    std::string filename;

    /// line of the element that caused this exception
    unsigned int line;

    ParserException(const std::string &_message,
            const ParseStruct *parserinfo);
    ParserException(const std::string &_message,
            const std::string &filename = "", unsigned int line = 0);
    virtual ~ParserException() throw ();

    virtual const char* what ( ) const throw ();
};

std::ostream& operator<<(std::ostream& os, const ParserException &entry);

}

#endif /*PARSEREXCEPTION_H_*/
