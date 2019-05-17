/**
 * C++ class: textstyles.h
 *
 * Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005-2009
 * Copyright: See COPYING file that comes with this distribution
 */

#ifndef _TEXTSTYLES_H_
#define _TEXTSTYLES_H_

#include "textstyle.h"
#include "colormap.h"
#include "chartranslator.h"
#include "doctemplate.h"

namespace srchilite {

/**
 * Contains TextStyle objects for all formats (e.g., bold, italics, etc), and
 * other templates (e.g., for the document header, etc.); it also contains the color
 * map and the char translator.
 */
struct TextStyles {
    /// the TextStyle for bold
    TextStyle bold;

    /// the TextStyle for italics
    TextStyle italics;

    /// the TextStyle for underline
    TextStyle underline;

    /// the TextStyle for non fixed width font
    TextStyle notfixed;

    /// the TextStyle for fixed width font
    TextStyle fixed;

    /// the TextStyle for color specification
    TextStyle color;

    /// the TextStyle for background color specification
    TextStyle bg_color;

    /// the TextStyle used when there's only one specification for all formats
    TextStyle onestyle;

    /// the TextStyle for line numbering
    TextStyle linenum;

    /**
     * The TextStyle objects for reference formatting
     */
    struct RefTextStyle {
        TextStyle anchor, inline_reference, postline_reference,
                postdoc_reference;
    } refstyle;

    std::string starting_template, style_separator, file_extension, line_prefix;

    CharTranslatorPtr charTranslator;

    ColorMapPtr colorMap;

    /// template used when generating entire document
    DocTemplate docTemplate;

    /// template used when not generating entire document
    DocTemplate noDocTemplate;

    TextStyles() :
        charTranslator(new CharTranslator), colorMap(new ColorMap) {
    }
};

/// shared pointer
typedef boost::shared_ptr<TextStyles> TextStylesPtr;

}

#endif /*_TEXTSTYLES_H_*/
