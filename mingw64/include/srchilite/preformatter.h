//
// C++ Interface: preformatter
//
// Description:
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2005
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef PREFORMATTER_H
#define PREFORMATTER_H

#include <string>
#include <boost/shared_ptr.hpp>

namespace srchilite {

class PreFormatter;

/// shared pointer for PreFormatter
typedef boost::shared_ptr<PreFormatter> PreFormatterPtr;

/**
 Preformats text to be generated.  This implements the
 Decorator pattern, thus yielding a chain of preformatters

 @author Lorenzo Bettini
 */
class PreFormatter {
    /// the possible nested preformatter (Decorator pattern)
    PreFormatterPtr decorator;

public:
    /**
     * @param f the nested decorated preformatter
     */
    PreFormatter(PreFormatterPtr f = PreFormatterPtr());

    virtual ~PreFormatter();

    /**
     * Sets the nested preformatter
     * @param f the nested (decorated) preformatter
     */
    void setPreFormatter(PreFormatterPtr f);

    /**
     * Preformats the passed string (public version)
     * @param text the string to be preformatted
     * @return the preformatted string
     */
    const std::string preformat(const std::string &text);

protected:
    /**
     * Preformats the passed string (protected version).  The subclasses must
     * redefine this method to perform the preformatting.  The default implementation
     * does not perform any preformatting.
     * @param text the string to be preformatted
     * @return the preformatted string
     */
    virtual const std::string doPreformat(const std::string &text);

};

}

#endif
