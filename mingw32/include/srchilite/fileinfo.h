//
// C++ Interface: fileinfo
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef FILEINFO_H
#define FILEINFO_H

#include "parserinfo.h"

namespace srchilite {

/**
 Information about the file we are processing.

 */
struct FileInfo: public ParserInfo {
    /// the input file name (without path)
    std::string input_file_name;

    /// the output file name
    std::string output_file_name;

    /// the output file extension
    std::string output_file_extension;

    FileInfo(const std::string &input, const std::string &output);

    ~FileInfo();
};

}

#endif
