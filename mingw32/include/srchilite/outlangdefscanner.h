//
// C++ Interface: outlangdefscanner
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef OUTLANGDEFSCANNER_H
#define OUTLANGDEFSCANNER_H

#include "parsestruct.h"

extern int outlangdef_lex() ;
extern FILE *outlangdef_in;
extern srchilite::ParseStructPtr outlang_parsestruct;

namespace srchilite {

void open_outlang_file_to_scan(const std::string &path, const std::string &file);

/**
 * Closes the input file.  This is required only in case of errors during
 * parsing (otherwise the file is closed automatically when the scanner
 * reaches the end of file).
 */
void close_outlangdefinputfile();

/**
 * Releases resources allocated by the scanner
 */
void clear_outlangdefscanner();

}

#endif // OUTLANGDEFSCANNER_H
