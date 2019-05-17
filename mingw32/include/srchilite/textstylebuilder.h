/**
 * Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005
 * Copyright: See COPYING file that comes with this distribution
 */

#ifndef _TEXTSTYLEBUILDER_H_
#define _TEXTSTYLEBUILDER_H_

#include "textstyle.h"

namespace srchilite {

/**
 * Given TextStyle objects build a new one,
 * adding a starting part, and ending part, and separating
 * them.
 */
class TextStyleBuilder {
    std::string start_, separator_;

    TextStyle buffer; ///< where we store intermediate results
    bool added; ///< whether we've already added something

public:
    TextStyleBuilder(const std::string &st = "", const std::string &sep = "");
    void start();
    void add(const TextStyle &textStyle);
    TextStyle end();
};

}

#endif /*_TEXTSTYLEBUILDER_H_*/
