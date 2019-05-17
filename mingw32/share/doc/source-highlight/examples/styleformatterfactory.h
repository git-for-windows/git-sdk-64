/*
 * styleformatterfactory.h
 *
 *  Created on: Jun 7, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef STYLEFORMATTERFACTORY_H_
#define STYLEFORMATTERFACTORY_H_

#include <map>

#include "srchilite/formatterfactory.h"
#include "styleformatter.h"

typedef std::map<std::string, StyleFormatterPtr> StyleFormatterMap;

/**
 * Specialization of FormatterFactory to create StyleFormatter objects
 * when reading a source-highlight style or css style file.
 * To avoid get/set methods, let's make all members
 * public, it is just an example anyway :-)
 *///> TEXINFO
struct StyleFormatterFactory: public srchilite::FormatterFactory {
    StyleFormatterMap formatterMap;

    bool hasFormatter(const string &key) const {
        return formatterMap.find(key) != formatterMap.end();
    }

    bool createFormatter(const string &key, const string &color,
            const string &bgcolor,
            srchilite::StyleConstantsPtr styleconstants) {

        if (hasFormatter(key))
            return false;

        StyleFormatter *formatter = new StyleFormatter(key);
        formatterMap[key] = StyleFormatterPtr(formatter);

        if (styleconstants.get()) {
            for (srchilite::StyleConstantsIterator it =
                    styleconstants->begin(); it
                    != styleconstants->end(); ++it) {
                switch (*it) {
                case srchilite::ISBOLD:
                    formatter->bold = true;
                    break;
                case srchilite::ISITALIC:
                    formatter->italic = true;
                    break;
                case srchilite::ISUNDERLINE:
                    formatter->underline = true;
                    break;
                case srchilite::ISFIXED:
                    formatter->fixed = true;
                    break;
                case srchilite::ISNOTFIXED:
                    formatter->not_fixed = true;
                    break;
                case srchilite::ISNOREF: // ignore references here
                    break;
                }
            }
        }

        formatter->color = color;
        formatter->bgColor = bgcolor;

        return true;
    }
};

//> TEXINFO
#endif /* STYLEFORMATTERFACTORY_H_ */
