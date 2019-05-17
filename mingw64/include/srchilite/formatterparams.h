//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef FORMATTERPARAMS_H_
#define FORMATTERPARAMS_H_

#include "parserinfo.h"
#include "fileutil.h"

namespace srchilite {

/**
 * Additional parameters that can be passed to a formatter
 */
struct FormatterParams: public ParserInfo {
    /// file name without path
    std::string fileNameNoPath;

    /**
     * The start position of the string to format within the whole string (line).
     * Note that this makes sense only for strings that are not to be formatted
     * as normal.
     * A negative value means "not specified"
     * IMPORTANT: do not use this if formatting optimization is on.
     */
    int start;

    FormatterParams() :
        start(-1) {
    }

    /**
     * @param the file name (possible path will be stripped)
     */
    FormatterParams(const std::string &n) :
        ParserInfo(n), fileNameNoPath(strip_file_path(n)), start(-1) {
    }
};

}

#endif /*FORMATTERPARAMS_H_*/
