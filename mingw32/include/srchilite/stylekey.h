//
// C++ Interface: stylekey
//
// Description: Structure for storing the keys of a style file.
//
//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2007
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef STYLEKEY_H
#define STYLEKEY_H

#include <string>
#include <list>

using std::list;
using std::string;

namespace srchilite {

// used to store a key
typedef string KeyType;

// used to store the keys of styles
typedef list<KeyType> KeyList;

}

#endif
