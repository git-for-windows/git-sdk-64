/*
 * infoformatter-main.cpp
 *
 *  Example of use of source-highlight library:
 *  prints formatting information to the console: what is going to
 *  be formatted and its position within the line
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
#include "infoformatter.h"

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

    srchilite::FormatterManager formatterManager(InfoFormatterPtr(
            new InfoFormatter));
    InfoFormatterPtr keywordFormatter(new InfoFormatter("keyword"));

    formatterManager.addFormatter("keyword", keywordFormatter);
    formatterManager.addFormatter("string", InfoFormatterPtr(new InfoFormatter(
            "string")));
    // for "type" we use the same formatter as for "keyword"
    formatterManager.addFormatter("type", keywordFormatter);
    formatterManager.addFormatter("comment", InfoFormatterPtr(
            new InfoFormatter("comment")));
    formatterManager.addFormatter("symbol", InfoFormatterPtr(new InfoFormatter(
            "symbol")));
    formatterManager.addFormatter("number", InfoFormatterPtr(new InfoFormatter(
            "number")));
    formatterManager.addFormatter("preproc", InfoFormatterPtr(
            new InfoFormatter("preproc")));
    highlighter.setFormatterManager(&formatterManager);

    // make sure it uses additional information
    srchilite::FormatterParams params;
    highlighter.setFormatterParams(&params);

    string line;
    // we now highlight a line a time
    while (getline(cin, line)) {
        // reset position counter within a line
        params.start = 0;

        highlighter.highlightParagraph(line);
    }

    return 0;
}
