//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef CTAGSFORMATTER_H_
#define CTAGSFORMATTER_H_

#include <string>
#include <list>

#include "textstyles.h"

namespace srchilite {

class PreFormatter;
class CTagsCollector;
struct FormatterParams;

/// collection of strings to be written after (post) a line or a doc
typedef std::list<std::string> postResults;

/**
 * Stores the result of the CTagsFormatter
 */
struct CTagsFormatterResults {
    /// the result for inline reference or anchor
    std::string inlineResult;

    /// the result for postline references
    postResults postLineResult;

    /// the result for postdoc references
    postResults postDocResult;

    /**
     * Clears all the results.
     */
    void clear() {
        inlineResult.clear();
        postLineResult.clear();
        postDocResult.clear();
    }
};

/**
 * Formatter for information gathered from ctags
 */
class CTagsFormatter {
    /// the input file name
    std::string inputFile;

    /// the input file name (without path)
    std::string inputFileName;

    /// the output file name
    std::string outputFile;

    /// the output file name extension
    std::string outputFileExtension;

    /// the preformatter
    PreFormatter *preFormatter;

    /// for actually formatting anchors and references
    TextStyles::RefTextStyle refstyle;

    /// collect information about the tags
    CTagsCollector *ctagsCollector;

public:
    CTagsFormatter(PreFormatter *pre, const TextStyles::RefTextStyle &r,
            CTagsCollector *ctagsC);
    ~CTagsFormatter();

    void setPreFormatter(PreFormatter *pre) {
        preFormatter = pre;
    }

    /**
     * Sets the information about input file and output file
     * @param input input file (with path)
     * @param output output file (with path)
     */
    void setFileInfo(const std::string &input, const std::string &output);

    /**
     * Formats an anchor or references concerning the passed word
     *
     * @param word
     * @param result where formatting results will be stored
     * @param params additional parameters for the formatter
     * @return true if some ctags about the word were found and formatted
     */
    bool formatCTags(const std::string &word, CTagsFormatterResults &result,
            const FormatterParams *params);
};

}

#endif /*CTAGSFORMATTER_H_*/
