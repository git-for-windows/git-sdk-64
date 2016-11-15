/* <!-- copyright */
/*
 * libmetalink
 *
 * Copyright (c) 2012 Tatsuhiro Tsujikawa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
/* copyright --> */
#ifndef _D_METALINKVER_H_
#define _D_METALINKVER_H_

#ifdef __cplusplus
extern "C" {
#endif

#define LIBMETALINK_COPYRIGHT "2008-2012 Tatsuhiro Tsujikawa"

#define LIBMETALINK_VERSION "0.1.3"

#define LIBMETALINK_VERSION_MAJOR 0
#define LIBMETALINK_VERSION_MINOR 1
#define LIBMETALINK_VERSION_PATCH 3

/*
 *Version concatenated in one integer. In this hexadecimal notation,
 * after first 0x prefix, each 2 digits group represents major, minor
 * and patch version in this order from most significant byte.
 */
#define LIBMETALINK_VERSION_NUMBER 0x000103

#ifdef __cplusplus
}
#endif

#endif /* _D_METALINKVER_H_ */
