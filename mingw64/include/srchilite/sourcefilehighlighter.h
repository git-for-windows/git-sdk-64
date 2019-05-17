//
// C++ Interface: SourceFileHighlighter
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef SOURCEFILEHIGHLIGHTER_H_
#define SOURCEFILEHIGHLIGHTER_H_

#include <string>
#include <istream>

namespace srchilite {

class SourceHighlighter;
class BufferedOutput;
class PreFormatter;
class LineNumGenerator;
class LineRanges;
class RegexRanges;
class Formatter;

/// debugging policy
enum FileHighlighterDebug {
    NO_DEBUG = 0, DEBUG, DEBUG_INTERACTIVE
};

/**
 * Highlights the contents of a file relying on a SourceHighlighter
 */
class SourceFileHighlighter {
    /// the name of the file to highlight
    const std::string fileName;

    /// the initial regexp state
    SourceHighlighter *sourceHighlighter;

    /// the output object
    BufferedOutput *output;

    /// debugging policy
    FileHighlighterDebug debugPolicy;

    /// the preformatter for characters, e.g., for \n
    PreFormatter *preformatter;

    /// for generation of line numbers (if not null)
    LineNumGenerator *lineNumGenerator;

    /// if not null, it detects whether a line is in the stored ranges
    LineRanges *lineRanges;

    /// if not null, it detects whether a line is in the stored ranges
    RegexRanges *regexRanges;

    /// the formatter for possible context lines
    Formatter *contextFormatter;

    /// a prefix to be generated in front of each formatted code line
    std::string linePrefix;

    /// if specified, a string to be printed between two ranges
    std::string rangeSeparator;

public:
    /**
     * @param file the name of the file to highlight;
     * if the filename is empty then we will process the standard input
     * @param sourceHighlighter the highlighter to highlight paragraphs
     * @param output the output object used to write
     */
    SourceFileHighlighter(const std::string &file,
            SourceHighlighter *sourceHighlighter, BufferedOutput *output);

    void setDebugPolicy(FileHighlighterDebug p) {
        debugPolicy = p;
    }

    void setPreformatter(PreFormatter *pf) {
        preformatter = pf;
    }

    void setContextFormatter(Formatter *cf) {
        contextFormatter = cf;
    }

    void setLineNumGenerator(LineNumGenerator *lG) {
        lineNumGenerator = lG;
    }

    void setLinePrefix(const std::string &prefix) {
        linePrefix = prefix;
    }

    /**
     * The range separator will be automatically preformatted to deal with
     * the specific output format.
     *
     * @param rangeSep
     */
    void setRangeSeparator(const std::string &rangeSep);

    const LineRanges *getLineRanges() const {
        return lineRanges;
    }

    void setLineRanges(LineRanges *lr) {
        lineRanges = lr;
    }

    void setRegexRanges(RegexRanges *rr) {
        regexRanges = rr;
    }

    /**
     * Performs the highlighting; Once the highlighting is finished, this
     * object can be re-used.
     */
    void highlight();

    /**
     * Performs the highlighting of the contents of the given input stream;
     * Once the highlighting is finished, this object can be re-used.
     * @param is
     */
    void highlight(std::istream &is);

    /**
     * Performs the highlighting of the contents of the given string;
     * Once the highlighting is finished, this object can be re-used.
     * @param is
     */
    void highlight(const std::string &is);

};

}

#endif /*SOURCEFILEHIGHLIGHTER_H_*/
