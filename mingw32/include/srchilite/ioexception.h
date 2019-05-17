//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef IOEXCEPTION_H_
#define IOEXCEPTION_H_

#include <ostream>
#include <exception>
#include <string>

namespace srchilite {

/**
 * Exception representing an error in an input/output operation.
 */
struct IOException : public std::exception {
    /// detailed message for this exception
    std::string message;

    /// filename of the element that caused this exception
    std::string filename;

    IOException(const std::string &_message,
            const std::string &_filename);
    virtual ~IOException() throw() ;

    virtual const char* what ( ) const throw ();
};

std::ostream& operator<<(std::ostream& os, const IOException &entry);

}

#endif /*IOEXCEPTION_H_*/
