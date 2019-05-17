//
// Author: Lorenzo Bettini, (C) 1999-2009
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef LANGELEMS_H
#define LANGELEMS_H

#include "langelem.h"

#include <list>
#include <map>
#include <string>

using std::list;
using std::map;
using std::string;

namespace srchilite {

/// the base class for LangElems
typedef list<LangElem *> LangElemsBase;

/**
 * @class LangElems
 * collection of LangElem objects
*/
// doublecpp: forward declarations, DO NOT MODIFY
class LangElemsPrinter; // file: langelemsprinter.h
// doublecpp: end, DO NOT MODIFY

class LangElems : protected list<LangElem *>
{
  typedef LangElemsBase base;
  typedef base::iterator Pointer;
  typedef list<Pointer> PointerList;
  typedef map<string, PointerList> ElemMap;

  ElemMap elem_map;

  public:
    using base::const_iterator;
    using base::begin;
    using base::end;
    using base::size;

    LangElems();

    virtual ~LangElems();

    /**
     * Adds a new element at the end of this collection
     * @param el
     */
    void add(LangElem *el);

    /**
     * Redefines all the possible occurrences of elements (which will be removed) with the
     * same name with the new element (which will be added at the end of this collection)
     * @param el the new element
     */
    void redef(LangElem *el);

    /**
     * Replaces the first occurrence of element (with the same name) with the new one;
     * all the other possible occurrences of elements will be removed
     * @param el the new element
     */
    void subst(LangElem *el);

    /**
     * return the string representation (with preprocessing) of all the elements
     * @return the string representation
     */
    const std::string toString() const;

    /**
     * return the original representation (without any preprocessing) of all the elements;
     * this is useful for printing errors
     * @return the original representation
     */
    const std::string toStringOriginal() const;
// doublecpp: dispatch methods, DO NOT MODIFY
public:
virtual void dispatch_collect_const(LangElemsPrinter *);
// doublecpp: end, DO NOT MODIFY
};

}

#endif
