/*
 ** Copyright (C) 1999-2007 Lorenzo Bettini <http://www.lorenzobettini.it>
 **
 ** This program is free software; you can redistribute it and/or modify
 ** it under the terms of the GNU General Public License as published by
 ** the Free Software Foundation; either version 3 of the License, or
 ** (at your option) any later version.
 **
 ** This program is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied warranty of
 ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ** GNU General Public License for more details.
 **
 ** You should have received a copy of the GNU General Public License
 ** along with this program; if not, write to the Free Software
 ** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 **
 */

// docgenerator.h : Document generator class

// for preable, header, footer, etc.

#ifndef DOCGENERATOR_H
#define DOCGENERATOR_H

#include <string>
#include <iostream>

#include "doctemplate.h"

using std::string;

namespace srchilite {

/**
 * Given a DocTemplate it generates the start of the document and the end, using
 * variables such as title, file_name, header, etc.
 */
class DocGenerator {
protected:
    string title;
    bool gen_source_highlight_version;
    string input_file_name;
    string doc_header;
    string doc_footer;
    string css_url;
    string doc_background;
    bool entire_doc;
    string input_lang;

    DocTemplate docTemplate;

public:
    DocGenerator(const string &s, const string &i, const string &h,
            const string &f, const string &c, const string &back, bool entire, 
	    const string &inputlang,
            const string &start_tmpl, const string &end_tmpl) :
        title(s), gen_source_highlight_version(true), input_file_name(i),
                doc_header(h), doc_footer(f), css_url(c), doc_background(back),
	  entire_doc(entire), input_lang(inputlang), docTemplate(DocTemplate(start_tmpl,
                        end_tmpl)) {
    }
    DocGenerator(const string &start_tmpl, const string &end_tmpl) :
        gen_source_highlight_version(true), docTemplate(DocTemplate(start_tmpl,
                end_tmpl)) {
    }
    DocGenerator() {
    }
    ~DocGenerator() {
    }

    /**
     * Generates the start of the document into the passed ostream
     *
     * @param sout the stream for generating the output
     */
    void generate_start_doc(std::ostream *sout);

    /**
     * Generates the end of the document into the passed ostream
     *
     * @param sout the stream for generating the output
     */
    void generate_end_doc(std::ostream *sout);

    /**
     * Sets the version of the generator (i.e., of source-highlight)
     */
    void set_gen_version(bool b) {
        gen_source_highlight_version = b;
    }

    void setInputFileName(const std::string &filename) {
        input_file_name = filename;
    }

    void setTitle(const std::string &_title) {
        title = _title;
    }

    void setInputLang(const std::string &_input_lang) {
      input_lang = _input_lang;
    }

    void setBackgroundColor(const std::string &bg) {
        doc_background = bg;
    }

    void setCss(const std::string &css) {
        css_url = css;
    }

    void setHeader(const std::string &_header) {
        doc_header = _header;
    }

    void setFooter(const std::string &_footer) {
        doc_footer = _footer;
    }
};

}

#endif // DOCGENERATOR_H
