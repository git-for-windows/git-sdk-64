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

#ifndef LANGDEFPARSERFUN_H
#define LANGDEFPARSERFUN_H

#include "langelems.h"

namespace srchilite {

LangElems *parse_lang_def();
LangElems *parse_lang_def(const char *path, const char *name);

}

#endif
