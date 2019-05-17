/*
 * source-highlight-console.cpp
 *
 *  Example of use of source-highlight library:
 *  prints formatting information to the console: what is going to
 *  be formatted and how it is formatted
 *
 *  Created on: May 7, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 *///> TEXINFO

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream>
#include "srchilite/langdefmanager.h"
#include "srchilite/regexrulefactory.h"
#include "srchilite/sourcehighlighter.h"
#include "srchilite/formattermanager.h"
#include "srchilite/stylefileparser.h" // for parsing style files
#include "styleformatterfactory.h"

using namespace std;

#ifndef DATADIR
#define DATADIR ""
#endif

int main() {
    srchilite::RegexRuleFactory ruleFactory;
    srchilite::LangDefManager langDefManager(&ruleFactory);

    // we highlight C++ code for simplicity
    srchilite::SourceHighlighter highlighter(langDefManager.getHighlightState(
            DATADIR, "cpp.lang"));

    // our factory for our formatters
    StyleFormatterFactory factory;

    // the background color for the document (not used here)
    string docBgColor;

    // let's parse the default.style and create our formatters
    srchilite::StyleFileParser::parseStyleFile(DATADIR, "default.style",
            &factory, docBgColor);

    // now we need to fill up the formatter manager with our formatters
    srchilite::FormatterManager formatterManager(StyleFormatterPtr(
            new StyleFormatter));
    for (StyleFormatterMap::const_iterator it = factory.formatterMap.begin(); it
            != factory.formatterMap.end(); ++it) {
        formatterManager.addFormatter(it->first, it->second);
    }
    highlighter.setFormatterManager(&formatterManager);

    string line;
    // we now highlight a line a time
    while (getline(cin, line)) {
        highlighter.highlightParagraph(line);
    }

    return 0;
}
