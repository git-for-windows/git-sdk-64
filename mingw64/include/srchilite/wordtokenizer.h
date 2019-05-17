//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef WORDTOKENIZER_H_
#define WORDTOKENIZER_H_

#include <string>
#include <algorithm>
#include <list>

namespace srchilite {

/**
 * Tokenizes a paragraph separating words from spaces
 */
class WordTokenizer {
public:
    /**
     * Results of the tokenizer; each element is a pair where the first
     * string represents a possible space and the second string a possible word.
     * The two elements are mutually exclusive
     */
    typedef std::list<std::pair<std::string, std::string> > WordTokenizerResults;

    /**
     * Tokenizes the passed string and stores the results.
     * @param s the string to tokenize
     * @param results where to store the results
     */
    static void tokenize(const std::string &s, WordTokenizerResults &results);
};

}

#endif /*WORDTOKENIZER_H_*/
