/*
 * Copyright (C) 1999-2009  Lorenzo Bettini, http://www.lorenzobettini.it
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef _CHARTRANSLATOR_H
#define _CHARTRANSLATOR_H

#define NUM_OF_CHARS 256

#include <string>
#include <boost/regex.hpp>

#include "preformatter.h"

namespace srchilite {

/**
 * Translates specific character sequences into corresponding ones;
 * it can also use regular expression for the characters to be translated.
 * This is useful for translating some characters in the input which
 * are special characters in the output formats, e.g., & in LaTeX, or
 * < in HTML.
 */
class CharTranslator: public PreFormatter {
protected:
    /// keeps track of the translation patterns added
    unsigned int counter;
    /// the translation regular expression (for buffering)
    std::string translation_exp;
    /// the corresponding translated regular expression (for buffering)
    std::string translation_format;
    /// the actual regular expression
    boost::regex *reg_exp;
    /// whether we are at the beginning of a new line
    bool bol;

    /**
     * The actual preformatting (char translation)
     * @param text what to translate
     * @return the translated string
     */
    virtual const std::string doPreformat(const std::string &text);

public:
    /**
     * @param f the decorated preformatter
     */
    CharTranslator(PreFormatterPtr f = PreFormatterPtr());
    virtual ~CharTranslator();

    /**
     * Adds a translation pair
     * @param s1 what to translate
     * @param s2 the translated expression
     */
    void set_translation(const std::string &s1, const std::string &s2);

    /**
     * returns a string representation: what we translate
     * and into what we translate
     */
    const std::string toString() const {
        return translation_exp + " -> " + translation_format;
    }
};

typedef boost::shared_ptr<CharTranslator> CharTranslatorPtr;

}

#endif // _CHARTRANSLATOR_H
