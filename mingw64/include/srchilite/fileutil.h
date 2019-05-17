//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef _FILEUTIL_H
#define _FILEUTIL_H

#include <iostream>
#include <string>
#include <cstdio>
#include "ioexception.h"

using std::istream;
using std::string;
using std::FILE;

namespace srchilite {

extern std::string start_path;

/**
 * Reads the contents of the file into a string and returns it
 *
 * @param fileName
 * @return the contents of the file
 * @throw IOException
 */
string readFile(const string &fileName) throw (IOException);

//char *read_file(const string &fileName);

/**
 * Creates the output file name as outputDir + input file name + ext
 *
 * @param inputFileName
 * @param outputDir
 * @param ext
 * @return the output file (including the path)
 */
string createOutputFileName(const string &inputFileName,
        const string &outputDir, const string &ext);

/**
 * @param input
 * @return returns the number of lines of the passed input stream
 */
unsigned int get_line_count(istream &input);

/**
 * @param filename
 * @return returns the file extension (without the dot)
 */
string get_file_extension(const string &filename);

FILE * open_file_stream(const string &input_file_name);
istream * open_file_istream(const string &filename);
istream * open_file_istream_or_error(const string &filename);
istream * open_data_file_istream(const string &path, const string &filename,
        const string &start = start_path);
FILE * open_data_file_stream(const string &path, const string &filename,
        const string &start = start_path);
bool read_line(istream *in, string &line);
string get_file_path(const string &s);
bool contains_path(const string &);
string strip_file_path(const string &);
string get_input_file_name(const string &file_name);

}

#endif //_FILEUTIL_H
