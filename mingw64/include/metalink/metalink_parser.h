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
#ifndef _D_METALINK_PARSER_H_
#define _D_METALINK_PARSER_H_

#include <stdio.h>

#include <metalink/metalink_types.h>
#include <metalink/metalink_error.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Parses metalink XML file.
 * @param filename path to Metalink XML file to be parsed.
 * @param res a dynamically allocated metalink_t structure as a result of
 * parsing.
 * @return 0 for success, non-zero for error. See metalink_error.h for
 * the meaning of error code.
 */
metalink_error_t metalink_parse_file(const char* filename, metalink_t** res);

/*
 * Parses metalink XML file.
 * @param docfp file stream for Metalink XML file to be parsed.
 * @param res a dynamically allocated metalink_t structure as a result of
 * parsing.
 * @return 0 for success, non-zero for error. See metalink_error.h for
 * the meaning of error code.
 */
metalink_error_t metalink_parse_fp(FILE *docfp, metalink_t** res);

/*
 * Parses metalink XML from a file descriptor.
 * @param docfd file descriptor of Metalink XML file to be parsed.
 * @param res a dynamically allocated metalink_t structure as a result of
 * parsing.
 * @return 0 for success, non-zero for error. See metalink_error.h for
 * the meaning of error code.
 */
metalink_error_t metalink_parse_fd(int docfd, metalink_t** res);

/*
 * Parses metalink XML stored in buf and its length is len.
 * @param buf a pointer to the XML data.
 * @param len length of XML data in byte.
 * @param res a dynamically allocated metalink_t structure as a result of
 * parsing.
 * @return 0 for success, non-zero for error. See metalink_error.h for
 * the meaning of error code.
 */
metalink_error_t metalink_parse_memory(const char* buf, size_t len, metalink_t** res);

/**
 * a parser context to keep current progress of XML parser.
 */
typedef struct _metalink_parser_context metalink_parser_context_t;

/*
 * Allocates, initializes and returns a parser context.
 * @return a parser context on success, otherwise NULL.
 */
metalink_parser_context_t* metalink_parser_context_new(void);

/**
 * Deallocates a parser context ctx.
 * @param ctx a parser context to deallocate. If ctx is NULL, this function does
 * nothing.
 */
void metalink_parser_context_delete(metalink_parser_context_t* ctx);

/**
 * Processes len bytes of data at buf. This function can be called several times
 * to parse entire XML data.
 * @param ctx a parser context.
 * @param buf a pointer to the XML data.
 * @param len length of XML data in bytes.
 * @return 0 on success, non-zero for error. See metalink_error.h for the
 * meaning of error code.
 */
metalink_error_t metalink_parse_update(metalink_parser_context_t* ctx,
			  const char* buf, size_t len);

/**
 * Processes len bytes of data at buf and places metalink_t to res.
 * Inside this function, ctx is cleaned up, so you don't need to call
 * delete_metalink_parser_context. len can be 0.
 * @param ctx a parser context.
 * @param buf a pointer to the XML data.
 * @param len length of XML data in bytes.
 * @param res a dynamically allocated metalink_t structure as a result of
 * parsing.
 * @return 0 on success, non-zero for error. See metalink_error_h for
 * the meaning of error code.
 */
metalink_error_t metalink_parse_final(metalink_parser_context_t* ctx,
			 const char* buf, size_t len, metalink_t** res);

#ifdef __cplusplus
}
#endif

#endif /* _D_METALINK_PARSER_H_ */
