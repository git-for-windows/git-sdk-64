//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef TEXTSTYLEFORMATTERFACTORY_H_
#define TEXTSTYLEFORMATTERFACTORY_H_

#include <string>
#include <list>
#include <boost/shared_ptr.hpp>

#include "formatterfactory.h"
#include "textstyles.h"
#include "textstyleformattercollection.h"

namespace srchilite {

using std::string;

class PreFormatter;
class CTagsFormatter;
class FormatterManager;
class TextStyleFormatter;

/**
 * Generates TextStyleFormatters based on TextStyles
 */
class TextStyleFormatterFactory : public FormatterFactory {
    /// contains all the styles for formatting
    TextStylesPtr textStyles;

    /// to preformat text
    PreFormatter *preformatter;

    /// for ctgas reference generation (can be null)
    CTagsFormatter *ctagsFormatter;

    /// the formatter manager (associating a formatter to an element)
    FormatterManager *formatterManager;

    /// all the formatters that are created by this factory
    TextStyleFormatterCollection formatterCollection;

public:
    TextStyleFormatterFactory(TextStylesPtr textStyles,
            PreFormatter *preformatter, CTagsFormatter *ctagsFormatter,
            FormatterManager *formatterManager);
    ~TextStyleFormatterFactory();

    /**
     * Creates a formatter for the specific language element (identified by
     * key) with the passed style parameters
     *
     * @param key
     * @param color
     * @param bgcolor
     * @param styleconstants
     * @return false if a formatter for the specific key is already present
     */
    bool createFormatter(const string &key, const string &color,
            const string &bgcolor, StyleConstantsPtr styleconstants);

    /**
     * Check whether the color must be translated with the color map or
     * left as it is (in that case, remove the ")
     * @param color
     * @return
     */
    string preprocessColor(const string &color);

    /**
     * Creates a formatter for key1, if not already present, that has the same
     * style as the formatter for key2
     *
     * @return false if the formatter for key1 is already present, or there's
     * no formatter for key2
     */
    bool createMissingFormatter(const string &key1, const string &key2);

    /**
     * Adds the formatter for the normal style if not already present.
     *
     * This must be called after all the formatter for the language elements were generated
     */
    void addDefaultFormatter();

    const TextStyleFormatterCollection &getFormatterCollection() const {
        return formatterCollection;
    }
};

}

#endif /*TEXTSTYLEFORMATTERFACTORY_H_*/
