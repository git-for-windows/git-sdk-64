//
// Description: given a collection generates a string representation
//
//
// Author: Lorenzo Bettini, 1999-2007 <http://www.lorenzobettini.it>
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef TOSTRINGCOLLECTION_H
#define TOSTRINGCOLLECTION_H

#include <string>
#include <sstream>

namespace srchilite {

/**
 * Converts a collection of objects with method toString into a string,
 * using the passed separator to separate the elements.
 *
 * @param collection
 * @param sep
 */
template <class T>
const std::string toStringCollection(const T *collection, char sep = ' ')
{
  std::ostringstream buf;

  for (typename T::const_iterator it = collection->begin();
       it != collection->end(); )
  {
    buf << (*it)->toString();
    if (++it != collection->end())
      buf << sep;
  }

  return buf.str();
}

/**
 * Converts a collection of objects into a string (relying on its stream representation),
 * using the passed separator to separate the elements.
 *
 * @param collection
 * @param sep
 */
template <class T>
const std::string toStringCollection(const T &collection, char sep = ' ')
{
  std::ostringstream buf;

  for (typename T::const_iterator it = collection.begin();
       it != collection.end(); )
  {
    buf << (*it);
    if (++it != collection.end())
      buf << sep;
  }

  return buf.str();
}

/**
 * Converts a collection of objects with method toStringOriginal into a string,
 * using the passed separator to separate the elements.
 *
 * @param collection
 * @param sep
 */
template <class T>
const std::string toStringOriginalCollection(const T *collection, char sep = ' ')
{
  std::ostringstream buf;

  for (typename T::const_iterator it = collection->begin();
       it != collection->end(); )
  {
    buf << (*it)->toStringOriginal();
    if (++it != collection->end())
      buf << sep;
  }

  return buf.str();
}

/**
 * Converts a collection of objects into a string,
 * using the passed separator to separate the elements.
 *
 * @param collection
 * @param sep
 */
template <class T>
const std::string collectionToString(const T *collection, char sep = ' ')
{
  std::ostringstream buf;

  for (typename T::const_iterator it = collection->begin();
       it != collection->end(); )
  {
    buf << (*it);
    if (++it != collection->end() && sep)
      buf << sep;
  }

  return buf.str();
}

/**
 * Converts a collection of objects into a string,
 * using the passed separator to separate the elements.
 *
 * @param collection
 * @param sep
 */
template <class T>
const std::string collectionRefToString(const T &collection, char sep = ' ')
{
  std::ostringstream buf;

  for (typename T::const_iterator it = collection.begin();
       it != collection.end(); )
  {
    buf << (*it);
    if (++it != collection.end() && sep)
      buf << sep;
  }

  return buf.str();
}

}

#endif // TOSTRINGCOLLECTION_H
