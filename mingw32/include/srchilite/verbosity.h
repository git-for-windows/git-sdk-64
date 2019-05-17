/*
 * verbosity.h
 *
 *  Created on: Apr 19, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef VERBOSITY_H_
#define VERBOSITY_H_

#include <iostream>

namespace srchilite {

/**
 * Utility class to output messages in case of verbose option is requested.
 * This class provides static methods, so that it can be used globally
 * from within the application using the library, so that all
 * classes that wants to output verbose messages will do that consistently.
 */
class Verbosity {
    static bool verbosity;
public:
    static void setVerbosity(bool b = true) {
        verbosity = b;
    }
    static bool getVerbosity() {
        return verbosity;
    }
};

/**
 * This is an optimization in the sense that the string is not even
 * created if verbosity is false
 */
#define VERBOSE(s) if (Verbosity::getVerbosity()) std::cerr << s;

/**
 * This is an optimization in the sense that the string is not even
 * created if verbosity is false; it also generates a newline
 */
#define VERBOSELN(s) if (Verbosity::getVerbosity()) std::cerr << s << std::endl;

}

#endif /* VERBOSITY_H_ */
