/*
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2009
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef STYLEFORMATTER_H_
#define STYLEFORMATTER_H_

#include <string>
#include <iostream>

#include <boost/shared_ptr.hpp>

#include "srchilite/formatter.h"
#include "srchilite/formatterparams.h"

/**
 * A specialization of srchilite::Formatter so that it prints information
 * about the language element that is being highlighted, and the format (that is,
 * bold, italics, etc).  To avoid get/set methods, let's make all members
 * public, it is just an example anyway :-)
 *///> TEXINFO
struct StyleFormatter: public srchilite::Formatter {
    /// the language element represented by this formatter
    std::string elem;

    bool bold, italic, underline, fixed, not_fixed;

    std::string color;

    std::string bgColor;

    StyleFormatter(const std::string &elem_ = "normal") :
        elem(elem_), bold(false), italic(false), underline(false),
                fixed(false), not_fixed(false) {
    }

    virtual void format(const std::string &s,
            const srchilite::FormatterParams *params = 0) {
        // do not print anything if normal or string to format is empty
        if (elem != "normal" || !s.size()) {
            std::cout << elem << ": \"" << s << "\"" << std::endl;
            std::cout << "formatted as: " << (bold ? "bold " : "")
                    << (italic ? "italic " : "") << (underline ? "underline "
                    : "");
            std::cout << (color.size() ? "color: " + color + " " : "");
            std::cout << (bgColor.size() ? "bgcolor: " + bgColor : "")
                    << std::endl;
        }
    }
};

/// shared pointer for StyleFormatter
typedef boost::shared_ptr<StyleFormatter> StyleFormatterPtr;

//> TEXINFO
#endif /* STYLEFORMATTER_H_ */
