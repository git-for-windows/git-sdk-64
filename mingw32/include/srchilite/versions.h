/*
 *  Created on: Apr 18, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef VERSIONS_H_
#define VERSIONS_H_

#include <string>

namespace srchilite {

/**
 * Utility functions for version numbers
 */
class Versions {
public:
    /**
     * @return the version of Source-highlight
     */
    static const std::string getVersion();

    /**
     * @return the version of Source-highlight library
     */
    static const std::string getLibraryVersion();

    /**
     * @return a string with the version and the library version
     */
    static const std::string getCompleteVersion();

};

}

#endif /* VERSIONS_H_ */
