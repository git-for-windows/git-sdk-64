/*
 * Copyright (C) 1999-2009  Lorenzo Bettini, http://www.lorenzobettini.it
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef COLORMAP_H
#define COLORMAP_H

#include <map>
#include <string>
#include <boost/shared_ptr.hpp>
#include <sstream>

using std::map;
using std::string;
using std::ostringstream;

namespace srchilite {

/**
 * Simple map for colors (maps a color constant string to the
 * corresponding color of the output format)
 */
class ColorMap: public map<string, string> {
protected:
    /// when no color corresponds to the requested one
    string default_color;

public:
    /**
     * Sets the default color
     * @param d the default color
     */
    void setDefault(const string &d) {
        default_color = d;
    }

    /**
     * @param key the color we're looking for
     * @return the color corresponding to the specified key, or the
     * default color if we don't have the requested key
     */
    const string getColor(const string &key) {
        const_iterator it = find(key);
        if (it == end())
            return default_color;
        else
            return it->second;
    }

    /**
     * Returns a string representation of the map.
     */
    const string toString() const {
        ostringstream s;
        for (const_iterator it = begin(); it != end(); ++it)
            s << "[" << it->first << "]=" << it->second << "\n";
        s << "default=" << default_color;
        return s.str();
    }
};

/// shared pointer for ColorMap
typedef boost::shared_ptr<ColorMap> ColorMapPtr;

}

#endif // COLORMAP_H
