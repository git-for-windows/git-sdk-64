//
// C++ Interface: langdefscanner
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef LANGDEFSCANNER_H
#define LANGDEFSCANNER_H

#include "parsestruct.h"

namespace srchilite {

void open_file_to_scan(const std::string &path, const std::string &file);

/**
 * Releases resources allocated by the scanner
 */
void clear_langdefscanner();

/**
 * Closes the input file.  This is required only in case of errors during
 * parsing (otherwise the file is closed automatically when the scanner
 * reaches the end of file).
 */
void close_langdefinputfile();

}

#endif // LANGDEFSCANNER_H
