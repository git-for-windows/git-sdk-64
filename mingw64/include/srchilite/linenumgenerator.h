//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef LINENUMGENERATOR_H_
#define LINENUMGENERATOR_H_

#include <string>

#include "textstyle.h"

namespace srchilite {

/**
 * Generates line numbers in the output.
 */
class LineNumGenerator {
    /// number of digits to represent line number
    unsigned int digitNum;

    /// character to use for padding the line number
    char padding;

    /// for actually formatting the line number
    TextStyle lineStyle;

    /// for possible line anchor generation (can be null)
    TextStyle anchorStyle;

    /// when generating an anchor for a line, use this prefix for the anchor name
    std::string anchorLinePrefix;

public:
    LineNumGenerator(const TextStyle &lineStyle, unsigned int digitNum, char padding = '0');
    ~LineNumGenerator();

    void setAnchorStyle(const TextStyle &_anchorStyle) {
        anchorStyle = _anchorStyle;
    }

    void setAnchorPrefix(const std::string &_anchorLinePrefix) {
        anchorLinePrefix = _anchorLinePrefix;
    }

    void setDigitNum(unsigned int _digitNum) {
        digitNum = _digitNum;
    }

    /**
     * Generates a string representing the formatting of the passed line number
     * @param line
     * @return the formatted line
     */
    const std::string generateLine(unsigned int line);
};

}

#endif /*LINENUMGENERATOR_H_*/
