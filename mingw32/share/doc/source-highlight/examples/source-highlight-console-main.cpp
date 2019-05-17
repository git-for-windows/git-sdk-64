/*
 * source-highlight-console.cpp
 *
 *  Example of use of source-highlight library:
 *  highlights a source to the console
 *
 *  Created on: May 7, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */ //> TEXINFO

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream>
#include "srchilite/sourcehighlight.h"
#include "srchilite/langmap.h"

using namespace std;

#ifndef DATADIR
#define DATADIR ""
#endif

int main(int argc, char *argv[]) {
    // we highlight to the console, through ANSI escape sequences
    srchilite::SourceHighlight sourceHighlight("esc.outlang");

    // make sure we find the .lang and .outlang files
    sourceHighlight.setDataDir(DATADIR);

    // by default we highlight C++ code
    string inputLang = "cpp.lang";

    if (argc > 1) {
        // we have a file name so we detect the input source language
        srchilite::LangMap langMap(DATADIR, "lang.map");
        string lang = langMap.getMappedFileNameFromFileName(argv[1]);
        if (lang != "") {
            inputLang = lang;
        } // otherwise we default to C++

        // output file name is empty => cout
        sourceHighlight.highlight(argv[1], "", inputLang);
    } else {
        // input file name is empty => cin
        sourceHighlight.highlight("", "", inputLang);
    }

    return 0;
}
