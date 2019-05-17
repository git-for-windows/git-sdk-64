/**
 * C++ class: doctemplate.h
 *
 * Description: The template for a document containing the output of
 * highlighting
 *
 * Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005-2007
 * Copyright: See COPYING file that comes with this distribution
 */

#ifndef _DOCTEMPLATE_H_
#define _DOCTEMPLATE_H_

#include <string>

namespace srchilite {

class DocTemplate {
    std::string begin_repr, end_repr;
public:
    DocTemplate(const std::string &begin = "", const std::string &end = "");

    std::string output_begin(const std::string &title, const std::string &cs,
            const std::string &add, const std::string &header,
            const std::string &footer, const std::string &background,
	    const std::string &input_lang);
    std::string output_end(const std::string &title, const std::string &cs,
            const std::string &add, const std::string &header,
            const std::string &footer, const std::string &background,
	    const std::string &input_lang);

    const std::string &toStringBegin() const {
        return begin_repr;
    }
    const std::string &toStringEnd() const {
        return end_repr;
    }
};

}

#endif /*_DOCTEMPLATE_H_*/
