//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef LINEBUFFER_H
#define LINEBUFFER_H

#include <string>
#include <set>
#include <boost/shared_ptr.hpp>

#include <sstream>

namespace srchilite {

/**
 A buffer for a line to be generated
 */
class LineBuffer {
public:
    /// Stores contents to be printed after the line
    typedef std::set<std::string> PostContents;

private:
    ostringstream buffer; ///< the line contents
    PostContents post; ///< to be generated after the line

public:
    LineBuffer() {
    }
    ~LineBuffer() {
    }

    /**
     * Puts something in the buffer
     */
    void output(const std::string &s) {
        buffer << s;
    }

    /**
     * Stores something to be generated after the line
     */
    void output_post(const std::string &s) {
        post.insert(s);
    }

    /**
     * @return the contents of the buffer
     */
    const std::string getContents() const {
        return buffer.str();
    }

    /**
     * @return what to generated after the line
     */
    const PostContents &getPostContents() const {
        return post;
    }

    /**
     * @return whether both the buffer and the post line contens are empty
     */
    bool empty() const {
        return (buffer.str().size() == 0 && post.size() == 0);
    }
};

/// shared pointer for LineBuffer
typedef boost::shared_ptr<LineBuffer> LineBufferPtr;

}

#endif
