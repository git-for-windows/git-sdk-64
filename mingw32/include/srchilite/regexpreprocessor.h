//
// C++ Interface: RegexPreProcessor
//
// Description: performs operations or inspections on a string representing
// a valid regular expression
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 1999-2007
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef REGEXPREPROCESSOR_H
#define REGEXPREPROCESSOR_H

#include <string>
#include <list>
#include <utility>
#include <vector>
#include <boost/regex.hpp>

namespace srchilite {

/**
 * @internal
 * info about subexpressions
 */
struct subexpressions_info {
    const static std::string ERR_OUTER_UNMARKED;
    const static std::string ERR_NESTED_SUBEXP;
    const static std::string ERR_UNBALANCED_PAREN;
    const static std::string ERR_OUTSIDE_SUBEXP;

    /// num of marked subexpressions
    unsigned int marked;
    /// error specifications, if any
    std::string errors;

    subexpressions_info() :
        marked(0) {
    }
};

/**
 * all the marked subexpressions in a list
 */
typedef std::list<std::string> subexpressions_strings;

/**
 * Information about backreferences; the first elem contains the
 * number of backreferences and the second one contains the highest
 * backreference info
 */
typedef std::pair<int, int> backreference_info;

/**
 * What to replace to backreferences in a regular expression
 */
typedef std::vector<std::string> backreference_replacements;

/**
 * The result of boost::regex_search
 */
typedef boost::match_results<std::string::const_iterator> regex_match_results;

/**
 preprocess a regular expression, e.g., transform "()" into "(?:)"
 */
class RegexPreProcessor {
public:
    RegexPreProcessor();

    ~RegexPreProcessor();

    /**
     * translates marked subexpressions (...) into non marked subexpressions (?: )
     * @return the translated string
     */
    static const std::string preprocess(const std::string &s);

    /**
     * translates the expression into a case nonsensitive expression, i.e.,
     * foo is translated into [Ff][Oo][Oo]
     * @return the translated string
     */
    static const std::string make_nonsensitive(const std::string &s);

    /**
     * counts the number of marked subexpressions (...)
     *
     * @param s
     * @return the number of marked subexpressions
     */
    static unsigned int num_of_subexpressions(const std::string &s);

    /**
     * check that the expressions is made up of marked subexpressions (...)
     * and no nested subexpressions and no char outside subexpressions (unless
     * allow_outer_char is true).
     *
     * Non-marked groups are allowed only if allow_outer_nonmarked is true.
     *
     * @param s
     * @param allow_outer_char whether we allow characters outside marked subexps
     * @param allow_outer_nonmarked whether we allow outer nonmarked subexps
     * @return the struct containing the number of marked subexpressions
     * and possible errors
     */
    static subexpressions_info num_of_marked_subexpressions(
            const std::string &s, bool allow_outer_char = false,
            bool allow_outer_nonmarked = false);

    /**
     * Splits the marked subexpressions of a regular expression made up of only
     * marked subexpressions and no nested subexpressions and char outside subexpressions
     * (thus, before calling this, you must make sure that num_of_marked_subexpressions
     * did not return an error.
     *
     * @return the subexpressions in a collection (this is allocated on the heap, so
     * it is up to the caller to delete it)
     */
    static const subexpressions_strings *split_marked_subexpressions(
            const std::string &s);

    /**
     * Checks whether the passed regular expression string contains
     * a backreference (e.g., either \1 or a conditional with a backreference
     * (?(1)...)
     *
     * @return true if the passed regular expression string contains
     * a backreference
     */
    static bool contains_backreferences(const std::string &s);

    /**
     * counts the number of backreferences (also in conditionals)
     *
     * @param s
     * @return the number of backreferences and the highest backreference
     * number
     */
    static backreference_info num_of_backreferences(const std::string &s);

    /**
     * counts the number of references (i.e., reference to a matched subexpression
     * of another regular expression)
     *
     * @param s
     * @return the number of backreferences and the highest backreference
     * number
     */
    static backreference_info num_of_references(const std::string &s);

    /**
     * Replace into the original string occurrences of backreferences
     * with the corresponding string in the replace parameter, i.e.,
     * \n backreference will be replaced with replace[n-1]
     *
     * @warning For the moment this is never used but in testing
     *
     * @param original
     * @param replace
     * @return the result of the replacement
     */
    static const std::string replace_backreferences(
            const std::string &original,
            const backreference_replacements &replace);

    /**
     * Replace into the original string occurrences of backreferences
     * with the corresponding subexpressions that matched in the results.
     *
     * Notice that we assume that the results come from a regular expression
     * without nested subexpressions.
     *
     * @warning For the moment this is never used but in testing
     *
     * @param original
     * @param results
     * @return the result of the replacement
     */
    static const std::string replace_backreferences(
            const std::string &original, const regex_match_results &results);

    /**
     * Replace into the original string occurrences of backreferences
     * with the corresponding string in the replace parameter, i.e.,
     * @{n} backreference will be replaced with replace[n-1]
     *
     * @param original
     * @param replace
     * @return the result of the replacement
     */
    static const std::string replace_references(
            const std::string &original,
            const backreference_replacements &replace);

    /**
     * Replace into the original string occurrences of backreferences
     * with the corresponding subexpressions that matched in the results.
     *
     * Notice that we assume that the results come from a regular expression
     * without nested subexpressions.
     *
     * @param original
     * @param results
     * @return the result of the replacement
     */
    static const std::string replace_references(
            const std::string &original, const regex_match_results &results);

};

}

#endif
