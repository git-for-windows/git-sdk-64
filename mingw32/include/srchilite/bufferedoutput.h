//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef BUFFEREDOUTPUT_H_
#define BUFFEREDOUTPUT_H_

#include <ostream>
#include <string>
#include <set>

namespace srchilite {

/// the contents to be put after a line or after the document
typedef std::set<std::string> PostContents;

/**
 * The main class for writing into the output.  It wraps an ostream object
 * and can perform buffering.  Moreover, it provides functionalities
 * to write something after a line or after an entire document (these contents
 * are buffered so that they can be inserted at any time, with
 * postLineInsert and postDocInsert
 * and will be actually inserted in the output
 * with writePostLine and writePostDoc).
 */
class BufferedOutput {
    /// the stream used to output strings
    std::ostream &outputBuff;

    /// whether to flush the output stream at each output operation
    bool alwaysFlush;

    /// the contents to be output after each line
    PostContents postLineContents;

    /// the contents to be output after the entire document
    PostContents postDocContents;

    /**
     * Writes all the (buffered) elements (and clear the buffer)
     * @param post the buffered elements
     * @param prefix the string to prefix all the elements
     */
    void writePostInfo(PostContents &post, const std::string &prefix = "");

public:
    /**
     * @param os the ostream where data will be written
     */
    BufferedOutput(std::ostream &os);
    ~BufferedOutput();

    /**
     * Whether to flush the output stream at each output operation
     * @param a
     */
    void setAlwaysFlush(bool a = true) {
        alwaysFlush = a;
    }

    /**
     * Writes the passed string into the output
     * @param s
     */
    void output(const std::string &s);

    /**
     * Writes the passed string into the contents to be output after the current line
     * @param s
     */
    void postLineInsert(const std::string &s);

    /**
     * Writes the passed string into the contents to be output after the entire document
     * @param s
     */
    void postDocInsert(const std::string &s);

    /**
     * Writes the elements of the passed generic collection
     * into the contents to be output after the current line
     * @param s
     */
    template<typename T> void postLineInsertFrom(const T &s) {
        for (typename T::const_iterator it = s.begin(); it != s.end(); ++it)
            postLineInsert(*it);
    }

    /**
     * Writes the elements of the passed generic collection
     * into the contents to be output after the entire document
     * @param s
     */
    template<typename T> void postDocInsertFrom(const T &s) {
        for (typename T::const_iterator it = s.begin(); it != s.end(); ++it)
            postDocInsert(*it);
    }

    /**
     * Writes all the (buffered) elements after the current line (and clear the buffer)
     * @param prefix the string to prefix all the elements
     */
    void writePostLine(const std::string &prefix = "");

    /**
     * Writes all the (buffered) elements after the current document (and clear the buffer)
     * @param prefix the string to prefix all the elements
     */
    void writePostDoc(const std::string &prefix = "");

};

}

#endif /*BUFFEREDOUTPUT_H_*/
