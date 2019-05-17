//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef HIGHLIGHTEVENTLISTENER_H_
#define HIGHLIGHTEVENTLISTENER_H_

namespace srchilite {

struct HighlightEvent;

/**
 * Base class for listeners of HighlightEvent.  Subclasses must implement notify()
 */
class HighlightEventListener
{
public:
	HighlightEventListener() {}
	virtual ~HighlightEventListener() {}

	/**
	 * The method called to notify the listener about an event.
	 * This must be implemented by subclasses.
	 * @param event the generated event
	 */
	virtual void notify(const HighlightEvent &event) = 0;
};

}

#endif /*HIGHLIGHTEVENTLISTENER_H_*/
