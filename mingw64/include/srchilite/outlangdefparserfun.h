//
// C++ Interface: %{MODULE}
//
// Description:
//
//
// Author: %{AUTHOR} <%{EMAIL}>, (C) %{YEAR}
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef OUTLANGDEFPARSERFUN_H
#define OUTLANGDEFPARSERFUN_H

#include "textstyles.h"

namespace srchilite {

TextStylesPtr parse_outlang_def();
TextStylesPtr parse_outlang_def(const char *path, const char *name);

}

#endif
