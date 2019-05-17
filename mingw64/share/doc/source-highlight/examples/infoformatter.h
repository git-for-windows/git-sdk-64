/*
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2009
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef INFOFORMATTER_H_
#define INFOFORMATTER_H_

#include <string>
#include <iostream>

#include <boost/shared_ptr.hpp>

#include "srchilite/formatter.h"
#include "srchilite/formatterparams.h"

/**
 * A specialization of srchilite::Formatter so that it prints information
 * about the language element that is being highlighted.
 *///> TEXINFO
class InfoFormatter: public srchilite::Formatter {
    /// the language element represented by this formatter
    std::string elem;

public:
    InfoFormatter(const std::string &elem_ = "normal") :
        elem(elem_) {
    }

    virtual void format(const std::string &s,
            const srchilite::FormatterParams *params = 0) {
        // do not print anything if normal or string to format is empty
        if (elem != "normal" || !s.size()) {
            std::cout << elem << ": " << s;
            if (params)
                std::cout << ", start: " << params->start;
            std::cout << std::endl;
        }
    }
};

/// shared pointer for InfoFormatter
typedef boost::shared_ptr<InfoFormatter> InfoFormatterPtr;

//> TEXINFO
#endif /* INFOFORMATTER_H_ */
