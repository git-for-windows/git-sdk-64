/* <!-- copyright */
/*
 * libmetalink
 *
 * Copyright (c) 2008 Tatsuhiro Tsujikawa
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
#ifndef _D_METALINK_ERROR_H_
#define _D_METALINK_ERROR_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef enum metalink_error_e {
	/* 9xx: fatal error */
	METALINK_ERR_BAD_ALLOC = 901,

	METALINK_ERR_CANNOT_OPEN_FILE = 902,

	/* 1xx: XML semantic error */
	METALINK_ERR_MISSING_REQUIRED_ATTR = 101,

	METALINK_ERR_NAMESPACE_ERROR = 102,

	/* 2xx: parser error */
	METALINK_ERR_PARSER_ERROR = 201,

	/* 3xx transaction error */
	METALINK_ERR_NO_FILE_TRANSACTION = 301,

	METALINK_ERR_NO_RESOURCE_TRANSACTION = 302,

	METALINK_ERR_NO_CHECKSUM_TRANSACTION = 303,

	METALINK_ERR_NO_CHUNK_CHECKSUM_TRANSACTION = 304,

	METALINK_ERR_NO_PIECE_HASH_TRANSACTION = 305,

        METALINK_ERR_NO_SIGNATURE_TRANSACTION = 306
} metalink_error_t;

#ifdef __cplusplus
}
#endif

#endif /* _D_METALINK_ERROR_H_ */
