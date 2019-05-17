//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef FORMATTER_H_
#define FORMATTER_H_

#include <string>

#include <boost/shared_ptr.hpp>

namespace srchilite {

struct FormatterParams;

/**
 * The base abstract class for formatting strings.  Derived classes must
 * implement the abstract method notify.  Note that only the string to
 * format is passed (and some additional information in a FormatterParams object);
 * everything else (e.g., what kind of elements this formatter handles) must
 * be part of the Formatter itself (i.e., of the subclass).
 */
class Formatter
{
public:
	Formatter();
	virtual ~Formatter();

	/**
	 * Formats the passed string.
	 *
	 * @param the string to format
	 * @param params possible additional parameters for the formatter
	 */
	virtual void format(const std::string &s, const FormatterParams *params = 0) = 0;
};

/// shared pointer for Formatter
typedef boost::shared_ptr<Formatter> FormatterPtr;

}

#endif /*FORMATTER_H_*/
