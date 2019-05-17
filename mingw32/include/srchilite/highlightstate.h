//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef HIGHLIGHTSTATE_H_
#define HIGHLIGHTSTATE_H_

#include <deque>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>

namespace srchilite {

class HighlightRule;
struct HighlightToken;
struct MatchingParameters;

/**
 * Shared pointer for HighlightRule.
 *
 * We need to use shared pointers because if we need to substitute some variables
 * in a rule, we need to create a brand new copy of it, while we keep on using
 * the same rules that do not need substitutions.  Using shared pointers will
 * automatically keep track of those that need to be removed.
 */
typedef boost::shared_ptr<HighlightRule> HighlightRulePtr;

/// List of rules.
typedef std::deque<HighlightRulePtr> RuleList;

/// the values for replacing references (it should contain 9 possibly empty elements, because
/// 9 is usually the limit for backreferences in regular expressions)
typedef std::vector<std::string> ReplacementList;

class HighlightState;

/// the reference to an HighlightState
typedef boost::shared_ptr<HighlightState> HighlightStatePtr;

/**
 * Represents a state during the highlighting (e.g., comment state,
 * string state, etc.)
 */
class HighlightState {
    /// the global counter to assign unique state ids
    static unsigned int global_id;

    /// the identifier of the state
    const unsigned int id;

    /// the name of the element for strings when no rule matches (default: "normal")
    /// for states this should always be "normal" while for environments it should
    /// be the same element that represents the new state (e.g., "comment", "string", etc.)
    std::string defaultElement;

    /// the list of rules of this state
    RuleList ruleList;

    /// whether one of the contained rules has dynamic references to be replaced
    bool needsReferenceReplacement;

    /**
     * In case this state is a copy of another state, in this field we
     * store the original state
     */
    HighlightStatePtr originalState;

public:
    /**
     * @param e the element for strings when no rule matches (default: "normal")
     */
    HighlightState(const std::string &e = "normal");
    /**
     * Copies all the elements of the highlight state, except for the id: a new
     * id will be used.
     * @param copy
     */
    HighlightState(const HighlightState &copy);
    ~HighlightState();

    /**
     * Adss a rule to this state
     * @param rule the rule to add at the end of the list
     */
    void addRule(HighlightRulePtr rule);

    /**
     * Substitutes the rule at the specified position.  The rule must exit at the
     * specified position.
     * @param index
     * @param rule the new rule
     * @return the old rule
     */
    HighlightRulePtr replaceRule(RuleList::size_type index,
            HighlightRulePtr rule);

    unsigned int getId() const {
        return id;
    }
    const RuleList &getRuleList() const {
        return ruleList;
    }

    const std::string &getDefaultElement() const {
        return defaultElement;
    }
    void setDefaultElement(const std::string &e) {
        defaultElement = e;
    }

    /**
     * Tries to find the rule that matches best (and first): the first rule with
     * the smallest prefix and longest matching sequence
     * @param s the string for trying to match the rule
     * @param token where results will be inserted, if the rule matched
     * @param params additional arguments for the matching
     * @return whether a matching rule was found
     */
    bool findBestMatch(const std::string &s, HighlightToken &token,
            const MatchingParameters &params) const;

    /**
     * Tries to find the rule that matches best (and first): the first rule with
     * the smallest prefix and longest matching sequence
     * @param start the beginning of the string for trying to match the rule
     * @param end the beginning of the string for trying to match the rule
     * @param token where results will be inserted, if the rule matched
     * @param params additional arguments for the matching
     * @return whether a matching rule was found
     */
    bool findBestMatch(std::string::const_iterator start,
            std::string::const_iterator end, HighlightToken &token,
            const MatchingParameters &params) const;

    /**
     * Whether t1 is better than t2: t1 has a non-longer prefix and a longer matching string
     * (this makes sense only if a string was matched, and in fact this should be used
     * only after having matched something)
     * @param t1
     * @param t2
     * @return
     */
    static bool betterThan(const HighlightToken &t1, const HighlightToken &t2);

    /**
     * Performs replacement of references in the rules of this state.
     * For each rule that needs replacement, it creates a copy of the rule.
     * @param the list of values for the replacement; the first element
     * is the value for replacing the first dynamic back reference, and so on.
     * (it should contain 9 possibly empty elements)
     */
    void replaceReferences(const ReplacementList &rep);

    bool getNeedsReferenceReplacement() const {
        return needsReferenceReplacement;
    }

    void setNeedsReferenceReplacement(bool b = true) {
        needsReferenceReplacement = b;
    }

    HighlightStatePtr getOriginalState() const {
        return originalState;
    }

    void setOriginalState(HighlightStatePtr orig) {
        originalState = orig;
    }

};

}

#endif /*HIGHLIGHTSTATE_H_*/
