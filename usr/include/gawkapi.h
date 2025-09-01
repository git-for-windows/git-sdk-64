/*
 * gawkapi.h -- Definitions for use by extension functions calling into gawk.
 */

/*
 * copyright (c) 2012-2019, 2021-2024, the free software foundation, inc.
 *
 * This file is part of GAWK, the GNU implementation of the
 * AWK Programming Language.
 *
 * GAWK is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * GAWK is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

/*
 * The following types and/or macros and/or functions are referenced
 * in this file.  For correct use, you must therefore include the
 * corresponding standard header file BEFORE including this file.
 *
 * FILE			- <stdio.h>
 * NULL			- <stddef.h>
 * memset(), memcpy()	- <string.h>
 * size_t		- <sys/types.h>
 * struct stat		- <sys/stat.h>
 *
 * Due to portability concerns, especially to systems that are not
 * fully standards-compliant, it is your responsibility to include
 * the correct files in the correct way. This requirement is necessary
 * in order to keep this file clean, instead of becoming a portability
 * hodge-podge as can be seen in the gawk source code.
 *
 * To pass reasonable integer values for ERRNO, you will also need to
 * include <errno.h>.
 */

#ifndef _GAWK_API_H
#define _GAWK_API_H

/*
 * General introduction:
 *
 * This API purposely restricts itself to ISO C 90 features.  In particular, no
 * bool, no // comments, no use of the restrict keyword, or anything else,
 * in order to provide maximal portability.
 *
 * Exception: the "inline" keyword is used below in the "constructor"
 * functions. If your compiler doesn't support it, you should either
 * -Dinline='' on your command line, or use the autotools and include a
 * config.h in your extensions.
 *
 * Additional important information:
 *
 * 1. ALL string values in awk_value_t objects need to come from api_malloc().
 * Gawk will handle releasing the storage if necessary.  This is slightly
 * awkward, in that you can't take an awk_value_t that you got from gawk
 * and reuse it directly, even for something that is conceptually pass
 * by value.
 *
 * 2. Due to gawk internals, after using sym_update() to install an array
 * into gawk, you have to retrieve the array cookie from the value
 * passed in to sym_update().  Like so:
 *
 *	new_array = create_array();
 *	val.val_type = AWK_ARRAY;
 *	val.array_cookie = new_array;
 *	sym_update("array", & val);	// install array in the symbol table
 *
 *	new_array = val.array_cookie;	// MUST DO THIS
 *
 *	// fill in new array with lots of subscripts and values
 *
 * Similarly, if installing a new array as a subarray of an existing
 * array, you must add the new array to its parent before adding any
 * elements to it.
 *
 * You must also retrieve the value of the array_cookie after the call
 * to set_element().
 *
 * Thus, the correct way to build an array is to work "top down".
 * Create the array, and immediately install it in gawk's symbol table
 * using sym_update(), or install it as an element in a previously
 * existing array using set_element().
 *
 * Thus the new array must ultimately be rooted in a global symbol. This is
 * necessary before installing any subarrays in it, due to gawk's
 * internal implementation.  Strictly speaking, this is required only
 * for arrays that will have subarrays as elements; however it is
 * a good idea to always do this.  This restriction may be relaxed
 * in a subsequent revision of the API.
 *
 * 3. While each routine in the API has a few lines of summary for it
 * in this header, said summaries are not standalone, adequate documentation. You
 * should read the chapter in the gawk manual on writing extensions. Find it online
 * at https://www.gnu.org/software/gawk/manual/html_node/Dynamic-Extensions.html,
 * or in the Info files distributed with gawk.
 */

/* Allow use in C++ code.  */
#ifdef __cplusplus
extern "C" {
#endif

/* This is used to keep extensions from modifying certain fields in some structs. */
#ifdef GAWK
#define awk_const
#else
#define awk_const const
#endif

typedef enum awk_bool {
	awk_false = 0,
	awk_true
} awk_bool_t;	/* we don't use <stdbool.h> on purpose */

/*
 * If an input parser would like to specify the field positions in the input
 * record, it may populate an awk_fieldwidth_info_t structure to indicate
 * the location of each field. The use_chars boolean controls whether the
 * field lengths are specified in terms of bytes or potentially multi-byte
 * characters. Performance will be better if the values are supplied in
 * terms of bytes. The fields[0].skip value indicates how many bytes (or
 * characters) to skip before $1, and fields[0].len is the length of $1, etc.
 */

typedef struct {
	awk_bool_t	use_chars;	/* false ==> use bytes */
	size_t		nf;
	struct awk_field_info {
		size_t	skip;	/* amount to skip before field starts */
		size_t	len;	/* length of field */
	} fields[1];		/* actual dimension should be nf */
} awk_fieldwidth_info_t;

/*
 * This macro calculates the total struct size needed. This is useful when
 * calling malloc or realloc.
 */
#define awk_fieldwidth_info_size(NF) (sizeof(awk_fieldwidth_info_t) + \
			(((NF)-1) * sizeof(struct awk_field_info)))

/* The information about input files that input parsers need to know: */
typedef struct awk_input {
	const char *name;	/* filename */
	int fd;			/* file descriptor */
#define INVALID_HANDLE (-1)
	void *opaque;           /* private data for input parsers */

	/*
	 * The get_record function is called to read the next record of data.
	 *
	 * It should return the length of the input record or EOF, and it
	 * should set *out to point to the contents of $0. The rt_start
	 * and rt_len arguments should be used to return RT to gawk.
	 * If EOF is not returned, the parser must set *rt_len (and
	 * *rt_start if *rt_len is non-zero).
	 *
	 * Note that gawk will make a copy of the record in *out, so the
	 * parser is responsible for managing its own memory buffer.
	 * Similarly, gawk will make its own copy of RT, so the parser
	 * is also responsible for managing this memory.
	 *
	 * It is guaranteed that errcode is a valid pointer, so there is
	 * no need to test for a NULL value.  Gawk sets *errcode to 0,
	 * so there is no need to set it unless an error occurs.
	 *
	 * If an error does occur, the function should return EOF and set
	 * *errcode to a positive value.  In that case, if *errcode is greater
	 * than zero, gawk will automatically update the ERRNO variable based
	 * on the value of *errcode (e.g., setting *errcode = errno should do
	 * the right thing).
	 *
	 * If field_width is non-NULL, then *field_width will be initialized
	 * to NULL, and the function may set it to point to a structure
	 * supplying field width information to override the default
	 * gawk field parsing mechanism. Note that this structure will not
	 * be copied by gawk; it must persist at least until the next call
	 * to get_record or close_func. Note also that field_width will
	 * be NULL when getline is assigning the results to a variable, thus
	 * field parsing is not needed.
	 */
	int (*get_record)(char **out, struct awk_input *iobuf, int *errcode,
			char **rt_start, size_t *rt_len,
			const awk_fieldwidth_info_t **field_width);

	/*
	 * This replaces the POSIX read() system call. Use it if you want to
	 * manage reading raw bytes yourself, and let gawk parse the record.
	 */
	ssize_t (*read_func)(int, void *, size_t);

	/*
	 * The close_func is called to allow the parser to free private data.
	 * Gawk itself will close the fd unless close_func first sets it to
	 * INVALID_HANDLE.
	 */
	void (*close_func)(struct awk_input *iobuf);

	/* put last, for alignment. bleah */
	struct stat sbuf;       /* stat buf */

} awk_input_buf_t;

typedef struct awk_input_parser {
	const char *name;	/* name of parser */

	/*
	 * The can_take_file function should return true if the parser
	 * would like to parse this file.  It should not change any gawk
	 * state!
	 */
	awk_bool_t (*can_take_file)(const awk_input_buf_t *iobuf);

	/*
	 * If this parser is selected, then take_control_of will be called.
	 * It can assume that a previous call to can_take_file was successful,
	 * and no gawk state has changed since that call.  It should populate
	 * the awk_input_buf_t's get_record, close_func, and opaque values as needed.
	 * It should return true if successful.
	 */
	awk_bool_t (*take_control_of)(awk_input_buf_t *iobuf);

	awk_const struct awk_input_parser *awk_const next;	/* for use by gawk */
} awk_input_parser_t;

/*
 * Similar for output wrapper.
 */

/* First the data structure */
typedef struct awk_output_buf {
	const char *name;	/* name of output file */
	const char *mode;	/* mode argument to fopen */
	FILE *fp;		/* stdio file pointer */
	awk_bool_t redirected;	/* true if a wrapper is active */
	void *opaque;		/* for use by output wrapper */

	/*
	 * Replacement functions for I/O.  Just like the regular
	 * versions but also take the opaque pointer argument.
	 */
	size_t (*gawk_fwrite)(const void *buf, size_t size, size_t count,
				FILE *fp, void *opaque);
	int (*gawk_fflush)(FILE *fp, void *opaque);
	int (*gawk_ferror)(FILE *fp, void *opaque);
	int (*gawk_fclose)(FILE *fp, void *opaque);
} awk_output_buf_t;

/* Next the output wrapper registered with gawk */
typedef struct awk_output_wrapper {
	const char *name;	/* name of the wrapper */

	/*
	 * The can_take_file function should return true if the wrapper
	 * would like to process this file.  It should not change any gawk
	 * state!
	 */
	awk_bool_t (*can_take_file)(const awk_output_buf_t *outbuf);

	/*
	 * If this wrapper is selected, then take_control_of will be called.
	 * It can assume that a previous call to can_take_file was successful,
	 * and no gawk state has changed since that call.  It should populate
	 * the awk_output_buf_t function pointers and opaque pointer as needed.
	 * It should return true if successful.
	 */
	awk_bool_t (*take_control_of)(awk_output_buf_t *outbuf);

	awk_const struct awk_output_wrapper *awk_const next;  /* for use by gawk */
} awk_output_wrapper_t;

/* A two-way processor combines an input parser and an output wrapper. */
typedef struct awk_two_way_processor {
	const char *name;	/* name of the two-way processor */

	/*
	 * The can_take_file function should return true if the two-way
	 * processor would like to parse this file.  It should not change
	 * any gawk state!
	 */
	awk_bool_t (*can_take_two_way)(const char *name);

	/*
	 * If this processor is selected, then take_control_of will be called.
	 * It can assume that a previous call to can_take_file was successful,
	 * and no gawk state has changed since that call.  It should populate
	 * the awk_input_buf_t and awk_otuput_buf_t structures as needed.
	 * It should return true if successful.
	 */
	awk_bool_t (*take_control_of)(const char *name, awk_input_buf_t *inbuf,
					awk_output_buf_t *outbuf);

	awk_const struct awk_two_way_processor *awk_const next;  /* for use by gawk */
} awk_two_way_processor_t;

#define gawk_api_major_version 4
#define gawk_api_minor_version 0

/* Current version of the API. */
enum {
	GAWK_API_MAJOR_VERSION = gawk_api_major_version,
	GAWK_API_MINOR_VERSION = gawk_api_minor_version
};

/* A number of typedefs related to different types of values. */

/*
 * A mutable string. Gawk owns the memory pointed to if it supplied
 * the value. Otherwise, it takes ownership of the memory pointed to.
 *
 * The API deals exclusively with regular chars; these strings may
 * be multibyte encoded in the current locale's encoding and character
 * set. Gawk will convert internally to wide characters if necessary.
 *
 * Note that a string provided by gawk will always be terminated
 * with a '\0' character.
 */
typedef struct awk_string {
	char *str;	/* data */
	size_t len;	/* length thereof, in chars */
} awk_string_t;

enum AWK_NUMBER_TYPE {
	AWK_NUMBER_TYPE_DOUBLE,
	AWK_NUMBER_TYPE_MPFR,
	AWK_NUMBER_TYPE_MPZ
};

/*
 * When type is AWK_NUMBER_MPFR or AWK_NUMBER_MPZ, the memory pointed to
 * by the ptr member belongs to gawk if it came from gawk.  Otherwise the
 * memory belongs to the extension and gawk copies it when its received.
 * See the manual for further discussion.
 */

typedef struct awk_number {
	double d;	/* always populated in data received from gawk */
	enum AWK_NUMBER_TYPE type;
	void *ptr;	/* either NULL or mpfr_ptr or mpz_ptr */
} awk_number_t;

/* Arrays are represented as an opaque type. */
typedef void *awk_array_t;

/* Scalars can be represented as an opaque type. */
typedef void *awk_scalar_t;

/* Any value can be stored as a cookie. */
typedef void *awk_value_cookie_t;

/*
 * This tag defines the type of a value.
 *
 * Values are associated with regular variables and with array elements.
 * Since arrays can be multidimensional (as can regular variables)
 * it's valid to have a "value" that is actually an array.
 */
typedef enum {
	AWK_UNDEFINED,
	AWK_NUMBER,
	AWK_STRING,
	AWK_REGEX,
	AWK_STRNUM,
	AWK_ARRAY,
	AWK_SCALAR,		/* opaque access to a variable */
	AWK_VALUE_COOKIE,	/* for updating a previously created value */
	AWK_BOOL
} awk_valtype_t;

/*
 * An awk value. The val_type tag indicates what
 * is in the union.
 */
typedef struct awk_value {
	awk_valtype_t	val_type;
	union {
		awk_string_t	s;
		awk_number_t	n;
		awk_array_t	a;
		awk_scalar_t	scl;
		awk_value_cookie_t vc;
		awk_bool_t      b;
	} u;
#define str_value	u.s
#define strnum_value	str_value
#define regex_value	str_value
#define num_value	u.n.d
#define num_type	u.n.type
#define num_ptr		u.n.ptr
#define array_cookie	u.a
#define scalar_cookie	u.scl
#define value_cookie	u.vc
#define bool_value	u.b
} awk_value_t;

/*
 * A "flattened" array element. Gawk produces an array of these
 * inside the awk_flat_array_t.
 * ALL memory pointed to belongs to gawk. Individual elements may
 * be marked for deletion. New elements must be added individually,
 * one at a time, using the separate API for that purpose.
 */

typedef struct awk_element {
	/* convenience linked list pointer, not used by gawk */
	struct awk_element *next;
	enum {
		AWK_ELEMENT_DEFAULT = 0,	/* set by gawk */
		AWK_ELEMENT_DELETE = 1		/* set by extension if
						   should be deleted */
	} flags;
	awk_value_t	index;
	awk_value_t	value;
} awk_element_t;

/*
 * A "flattened" array. See the description above for how
 * to use the elements contained herein.
 */
typedef struct awk_flat_array {
	awk_const void *awk_const opaque1;	/* private data for use by gawk */
	awk_const void *awk_const opaque2;	/* private data for use by gawk */
	awk_const size_t count;			/* how many elements */
	awk_element_t elements[1];		/* will be extended */
} awk_flat_array_t;

/*
 * A record describing an extension function. Upon being
 * loaded, the extension should pass in one of these to gawk for
 * each C function.
 *
 * Each called function must fill in the result with either a scalar
 * (number, string, or regex). Gawk takes ownership of any string memory.
 *
 * The called function must return the value of `result'.
 * This is for the convenience of the calling code inside gawk.
 *
 * Each extension function may decide what to do if the number of
 * arguments isn't what it expected.  Following awk functions, it
 * is likely OK to ignore extra arguments.
 *
 * 'min_required_args' indicates how many arguments MUST be passed.
 * The API will throw a fatal error if not enough are passed.
 *
 * 'max_expected_args' is more benign; if more than that are passed,
 * the API prints a lint message (IFF lint is enabled, of course).
 *
 * In any case, the extension function itself need not compare the
 * actual number of arguments passed to those two values if it does
 * not want to.
 */
typedef struct awk_ext_func {
	const char *name;
	awk_value_t *(*const function)(int num_actual_args,
					awk_value_t *result,
					struct awk_ext_func *finfo);
	const size_t max_expected_args;
	const size_t min_required_args;
	awk_bool_t suppress_lint;
	void *data;		/* opaque pointer to any extra state */
} awk_ext_func_t;

typedef void *awk_ext_id_t;	/* opaque type for extension id */

/*
 * The API into gawk. Lots of functions here. We hope that they are
 * logically organized.
 *
 * !!! If you make any changes to this structure, please remember to bump !!!
 * !!! gawk_api_major_version and/or gawk_api_minor_version.              !!!
 */
typedef struct gawk_api {
	/* First, data fields. */

	/* These are what gawk thinks the API version is. */
	awk_const int major_version;
	awk_const int minor_version;

	/* GMP/MPFR versions, if extended-precision is available */
	awk_const int gmp_major_version;
	awk_const int gmp_minor_version;
	awk_const int mpfr_major_version;
	awk_const int mpfr_minor_version;

	/*
	 * These can change on the fly as things happen within gawk.
	 * Currently only do_lint is prone to change, but we reserve
	 * the right to allow the others to do so also.
	 *
	 * N.B. If we ever again need to add an additional do_flags value,
	 * it would be wise to convert this from an array to a bitmask. If
	 * we add a new do_flags value and bump DO_FLAGS_SIZE, then it requires
	 * us to increment the ABI version. If we use a bitmask instead, then
	 * we will be free to add new flags without breaking ABI compatibility.
	 */
#define DO_FLAGS_SIZE	7
	awk_const int do_flags[DO_FLAGS_SIZE];
/* Use these as indices into do_flags[] array to check the values */
#define gawk_do_lint		0
#define gawk_do_traditional	1
#define gawk_do_profile		2
#define gawk_do_sandbox		3
#define gawk_do_debug		4
#define gawk_do_mpfr		5
#define gawk_do_csv		6

	/* Next, registration functions: */

	/*
	 * Add a function to the interpreter, returns true upon success.
	 * Gawk does not modify what func points to, but the extension
	 * function itself receives this pointer and can modify what it
	 * points to, thus it's not const.
	 */
	awk_bool_t (*api_add_ext_func)(awk_ext_id_t id, const char *name_space,
			awk_ext_func_t *func);

	/* Register an input parser; for opening files read-only */
	void (*api_register_input_parser)(awk_ext_id_t id,
					awk_input_parser_t *input_parser);

	/* Register an output wrapper, for writing files */
	void (*api_register_output_wrapper)(awk_ext_id_t id,
					awk_output_wrapper_t *output_wrapper);

	/* Register a processor for two way I/O */
	void (*api_register_two_way_processor)(awk_ext_id_t id,
				awk_two_way_processor_t *two_way_processor);

	/*
	 * Add an exit call back.
	 *
	 * arg0 is a private data pointer for use by the extension;
	 * gawk saves it and passes it into the function pointed
	 * to by funcp at exit.
	 *
	 * Exit callback functions are called in LIFO order.
	 */
	void (*api_awk_atexit)(awk_ext_id_t id,
			void (*funcp)(void *data, int exit_status),
			void *arg0);

	/* Register a version string for this extension with gawk. */
	void (*api_register_ext_version)(awk_ext_id_t id, const char *version);

	/* Functions to print messages */
	void (*api_fatal)(awk_ext_id_t id, const char *format, ...);
	void (*api_warning)(awk_ext_id_t id, const char *format, ...);
	void (*api_lintwarn)(awk_ext_id_t id, const char *format, ...);
	void (*api_nonfatal)(awk_ext_id_t id, const char *format, ...);

	/* Functions to update ERRNO */
	void (*api_update_ERRNO_int)(awk_ext_id_t id, int errno_val);
	void (*api_update_ERRNO_string)(awk_ext_id_t id, const char *string);
	void (*api_unset_ERRNO)(awk_ext_id_t id);

	/*
	 * All of the functions that return a value from inside gawk
	 * (get a parameter, get a global variable, get an array element)
	 * behave in the same way.
	 *
	 * For a function parameter, the return is false if the argument
	 * count is out of range, or if the actual parameter does not match
	 * what is specified in wanted. In that case,  result->val_type
	 * will hold the actual type of what was passed.
	 *
	 * Similarly for symbol table access to variables and array elements,
	 * the return is false if the actual variable or array element does
	 * not match what was requested, and result->val_type will hold
	 * the actual type.

	Table entry is type returned:


	                        +----------------------------------------------------------------+
	                        |                        Type of Actual Value:                   |
	                        +--------+--------+--------+--------+--------+-------+-----------+
	                        | String | Strnum | Number | Regex  | Bool   | Array | Undefined |
	+-----------+-----------+--------+--------+--------+--------+--------+-------+-----------+
	|           | String    | String | String | String | String | String | false | false     |
	|           +-----------+--------+--------+--------+--------+--------+-------+-----------+
	|           | Strnum    | false  | Strnum | Strnum | false  | false  | false | false     |
	|           +-----------+--------+--------+--------+--------+--------+-------+-----------+
	|           | Number    | Number | Number | Number | false  | Number | false | false     |
	|           +-----------+--------+--------+--------+--------+--------+-------+-----------+
	|           | Regex     | false  | false  | false  | Regex  | false  | false | false     |
	|           +-----------+--------+--------+--------+--------+--------+-------+-----------+
	|   Type    | Bool      | false  | false  | false  | false  | Bool   | false | false     |
	| Requested +-----------+--------+--------+--------+--------+--------+-------+-----------+
	|           | Array     | false  | false  | false  | false  | false  | Array | false     |
	|           +-----------+--------+--------+--------+--------+--------+-------+-----------+
	|           | Scalar    | Scalar | Scalar | Scalar | Scalar | Scalar | false | false     |
	|           +-----------+--------+--------+--------+--------+--------+-------+-----------+
	|           | Undefined | String | Strnum | Number | Regex  | Bool   | Array | Undefined |
	|           +-----------+--------+--------+--------+--------+--------+-------+-----------+
	|           | Value     | false  | false  | false  | false  | false  | false | false     |
	|           | Cookie    |        |        |        |        |        |       |           |
	+-----------+-----------+--------+--------+--------+--------+--------+-------+-----------+
	*/

	/* Functions to handle parameters passed to the extension. */

	/*
	 * Get the count'th parameter, zero-based.
	 * Returns false if count is out of range, or if actual parameter
	 * does not match what is specified in wanted. In that case,
	 * result->val_type is as described above.
	 */
	awk_bool_t (*api_get_argument)(awk_ext_id_t id, size_t count,
					  awk_valtype_t wanted,
					  awk_value_t *result);

	/*
	 * Convert a parameter that was undefined into an array
	 * (provide call-by-reference for arrays).  Returns false
	 * if count is too big, or if the argument's type is
	 * not undefined.
	 */
	awk_bool_t (*api_set_argument)(awk_ext_id_t id,
					size_t count,
					awk_array_t array);

	/*
	 * Symbol table access:
	 * 	- Read-only access to special variables (NF, etc.)
	 * 	- One special exception: PROCINFO.
	 *	- Use sym_update() to change a value, including from UNDEFINED
	 *	  to scalar or array.
	 */
	/*
	 * Lookup a variable, fill in value. No messing with the value
	 * returned.
	 * Returns false if the variable doesn't exist or if the wrong type
	 * was requested.  In the latter case, vaule->val_type will have
	 * the real type, as described above.
	 *
	 * 	awk_value_t val;
	 * 	if (! api->sym_lookup(id, name, wanted, & val))
	 * 		error_code_here();
	 *	else {
	 *		// safe to use val
	 *	}
	 */
	awk_bool_t (*api_sym_lookup)(awk_ext_id_t id,
				const char *name_space,
				const char *name,
				awk_valtype_t wanted,
				awk_value_t *result);

	/*
	 * Update a value. Adds it to the symbol table if not there.
	 * Changing types (scalar <--> array) is not allowed.
	 * In fact, using this to update an array is not allowed, either.
	 * Such an attempt returns false.
	 */
	awk_bool_t (*api_sym_update)(awk_ext_id_t id,
				const char *name_space,
				const char *name,
				awk_value_t *value);

	/*
	 * A ``scalar cookie'' is an opaque handle that provide access
	 * to a global variable or array. It is an optimization that
	 * avoids looking up variables in gawk's symbol table every time
	 * access is needed.
	 *
	 * This function retrieves the current value of a scalar cookie.
	 * Once you have obtained a scalar_cookie using sym_lookup, you can
	 * use this function to get its value more efficiently.
	 *
	 * Return will be false if the value cannot be retrieved.
	 *
	 * Flow is thus
	 *	awk_value_t val;
	 * 	awk_scalar_t cookie;
	 * 	api->sym_lookup(id, "variable", AWK_SCALAR, & val);	// get the cookie
	 *	cookie = val.scalar_cookie;
	 *	...
	 *	api->sym_lookup_scalar(id, cookie, wanted, & val);	// get the value
	 */
	awk_bool_t (*api_sym_lookup_scalar)(awk_ext_id_t id,
				awk_scalar_t cookie,
				awk_valtype_t wanted,
				awk_value_t *result);

	/*
	 * Update the value associated with a scalar cookie.
	 * Flow is
	 * 	sym_lookup with wanted == AWK_SCALAR
	 * 	if returns false
	 * 		sym_update with real initial value to install it
	 * 		sym_lookup again with AWK_SCALAR
	 *	else
	 *		use the scalar cookie
	 *
	 * Return will be false if the new value is not one of
	 * AWK_STRING, AWK_NUMBER, AWK_REGEX.
	 *
	 * Here too, the built-in variables may not be updated.
	 */
	awk_bool_t (*api_sym_update_scalar)(awk_ext_id_t id,
				awk_scalar_t cookie, awk_value_t *value);

	/* Cached values */

	/*
	 * Create a cached string,regex, or numeric value for efficient later
	 * assignment. This improves performance when you want to assign
	 * the same value to one or more variables repeatedly.  Only
	 * AWK_NUMBER, AWK_STRING, AWK_REGEX and AWK_STRNUM values are allowed.
	 * Any other type is rejected.  We disallow AWK_UNDEFINED since that
	 * case would result in inferior performance.
	 */
	awk_bool_t (*api_create_value)(awk_ext_id_t id, awk_value_t *value,
		    awk_value_cookie_t *result);

	/*
	 * Release the memory associated with a cookie from api_create_value.
	 * Please call this to free memory when the value is no longer needed.
	 */
	awk_bool_t (*api_release_value)(awk_ext_id_t id, awk_value_cookie_t vc);

	/* Array management */

	/*
	 * Retrieve total number of elements in array.
	 * Returns false if some kind of error.
	 */
	awk_bool_t (*api_get_element_count)(awk_ext_id_t id,
			awk_array_t a_cookie, size_t *count);

	/*
	 * Return the value of an element - read only!
	 * Use set_array_element() to change it.
	 * Behavior for value and return is same as for api_get_argument
	 * and sym_lookup.
	 */
	awk_bool_t (*api_get_array_element)(awk_ext_id_t id,
			awk_array_t a_cookie,
			const awk_value_t *const index,
			awk_valtype_t wanted,
			awk_value_t *result);

	/*
	 * Change (or create) element in existing array with
	 * index and value.
	 *
	 * ARGV and ENVIRON may not be updated.
	 */
	awk_bool_t (*api_set_array_element)(awk_ext_id_t id, awk_array_t a_cookie,
					const awk_value_t *const index,
					const awk_value_t *const value);

	/*
	 * Remove the element with the given index.
	 * Returns true if removed or false if element did not exist.
	 */
	awk_bool_t (*api_del_array_element)(awk_ext_id_t id,
			awk_array_t a_cookie, const awk_value_t* const index);

	/* Create a new array cookie to which elements may be added. */
	awk_array_t (*api_create_array)(awk_ext_id_t id);

	/* Clear out an array. */
	awk_bool_t (*api_clear_array)(awk_ext_id_t id, awk_array_t a_cookie);

	/*
	 * Flatten out an array with type conversions as requested.
	 * This supersedes the earlier api_flatten_array function that
	 * did not allow the caller to specify the requested types.
	 * (That API is still available as a macro, defined below.)
	 */
	awk_bool_t (*api_flatten_array_typed)(awk_ext_id_t id,
			awk_array_t a_cookie,
			awk_flat_array_t **data,
			awk_valtype_t index_type, awk_valtype_t value_type);

	/* When done, delete any marked elements, release the memory. */
	awk_bool_t (*api_release_flattened_array)(awk_ext_id_t id,
			awk_array_t a_cookie,
			awk_flat_array_t *data);

	/*
	 * Hooks to provide access to gawk's memory allocation functions.
	 * This ensures that memory passed between gawk and the extension
	 * is allocated and released by the same library.
	 */
	void *(*api_malloc)(size_t size);
	void *(*api_calloc)(size_t nmemb, size_t size);
	void *(*api_realloc)(void *ptr, size_t size);
	void (*api_free)(void *ptr);

	/*
	 * Obsolete function, should not be used. It remains only
	 * for binary compatibility.  Any value it returns should be
	 * freed via api_free.
	 */
	void *(*api_get_mpfr)(awk_ext_id_t id);

	/*
	 * Obsolete function, should not be used. It remains only
	 * for binary compatibility.  Any value it returns should be
	 * freed via api_free.
	 */
	void *(*api_get_mpz)(awk_ext_id_t id);

        /*
	 * Look up a file.  If the name is NULL or name_len is 0, it returns
	 * data for the currently open input file corresponding to FILENAME
	 * (and it will not access the filetype argument, so that may be
	 * undefined).
	 *
	 * If the file is not already open, try to open it.
	 *
	 * The "filetype" argument should be one of:
	 *
	 *    ">", ">>", "<", "|>", "|<", and "|&"
	 *
	 * If the file is not already open, and the fd argument is non-negative,
	 * gawk will use that file descriptor instead of opening the file
	 * in the usual way.
	 *
	 * If the fd is non-negative, but the file exists already, gawk
	 * ignores the fd and returns the existing file.  It is the caller's
	 * responsibility to notice that the fd in the returned
	 * awk_input_buf_t does not match the requested value.
	 *
	 * Note that supplying a file descriptor is currently NOT supported
	 * for pipes. It should work for input, output, append, and two-way
	 * (coprocess) sockets.  If the filetype is two-way, we assume that
	 * it is a socket!
	 *
	 * Note that in the two-way case, the input and output file descriptors
	 * may differ.  To check for success, one must check that either of
	 * them matches.
	 *
	 * ibufp and obufp point at gawk's internal copies of the
	 * awk_input_buf_t and awk_output_t associated with the open
	 * file.  Treat these data structures as read-only!
	 */
	awk_bool_t (*api_get_file)(awk_ext_id_t id,
			const char *name,
			size_t name_len,
			const char *filetype,
			int fd,
			/*
			 * Return values (on success, one or both should
			 * be non-NULL):
			 */
			const awk_input_buf_t **ibufp,
			const awk_output_buf_t **obufp);

	/* Destroy an array. */
	awk_bool_t (*api_destroy_array)(awk_ext_id_t id, awk_array_t a_cookie);
} gawk_api_t;

#ifndef GAWK	/* these are not for the gawk code itself! */
/*
 * Use these if you want to define "global" variables named api
 * and ext_id to make the code a little easier to read.
 * See the sample boilerplate code, below.
 */
#define do_lint		(api->do_flags[gawk_do_lint])
#define do_traditional	(api->do_flags[gawk_do_traditional])
#define do_profile	(api->do_flags[gawk_do_profile])
#define do_sandbox	(api->do_flags[gawk_do_sandbox])
#define do_debug	(api->do_flags[gawk_do_debug])
#define do_mpfr		(api->do_flags[gawk_do_mpfr])
#define do_csv		(api->do_flags[gawk_do_csv])

#define get_argument(count, wanted, result) \
	(api->api_get_argument(ext_id, count, wanted, result))
#define set_argument(count, new_array) \
	(api->api_set_argument(ext_id, count, new_array))

#define fatal		api->api_fatal
#define nonfatal	api->api_nonfatal
#define warning		api->api_warning
#define lintwarn	api->api_lintwarn

#define register_input_parser(parser)	(api->api_register_input_parser(ext_id, parser))
#define register_output_wrapper(wrapper) (api->api_register_output_wrapper(ext_id, wrapper))
#define register_two_way_processor(processor) \
	(api->api_register_two_way_processor(ext_id, processor))

#define update_ERRNO_int(e)	(api->api_update_ERRNO_int(ext_id, e))
#define update_ERRNO_string(str) \
	(api->api_update_ERRNO_string(ext_id, str))
#define unset_ERRNO()	(api->api_unset_ERRNO(ext_id))

#define add_ext_func(ns, func)	(api->api_add_ext_func(ext_id, ns, func))
#define awk_atexit(funcp, arg0)	(api->api_awk_atexit(ext_id, funcp, arg0))

#define sym_lookup(name, wanted, result) \
	sym_lookup_ns("", name, wanted, result)
#define sym_update(name, value) \
	sym_update_ns("", name, value)

#define sym_lookup_ns(name_space, name, wanted, result) \
	(api->api_sym_lookup(ext_id, name_space, name, wanted, result))
#define sym_update_ns(name_space, name, value) \
	(api->api_sym_update(ext_id, name_space, name, value))

#define sym_lookup_scalar(scalar_cookie, wanted, result) \
	(api->api_sym_lookup_scalar(ext_id, scalar_cookie, wanted, result))
#define sym_update_scalar(scalar_cookie, value) \
	(api->api_sym_update_scalar)(ext_id, scalar_cookie, value)

#define get_array_element(array, index, wanted, result) \
	(api->api_get_array_element(ext_id, array, index, wanted, result))

#define set_array_element(array, index, value) \
	(api->api_set_array_element(ext_id, array, index, value))

#define set_array_element_by_elem(array, elem) \
	(api->api_set_array_element(ext_id, array, & (elem)->index, & (elem)->value))

#define del_array_element(array, index) \
	(api->api_del_array_element(ext_id, array, index))

#define get_element_count(array, count_p) \
	(api->api_get_element_count(ext_id, array, count_p))

#define create_array()		(api->api_create_array(ext_id))

#define destroy_array(array)	(api->api_destroy_array(ext_id, array))

#define clear_array(array)	(api->api_clear_array(ext_id, array))

#define flatten_array_typed(array, data, index_type, value_type) \
	(api->api_flatten_array_typed(ext_id, array, data, index_type, value_type))

#define flatten_array(array, data) \
	flatten_array_typed(array, data, AWK_STRING, AWK_UNDEFINED)

#define release_flattened_array(array, data) \
	(api->api_release_flattened_array(ext_id, array, data))

#define gawk_malloc(size)		(api->api_malloc(size))
#define gawk_calloc(nmemb, size)	(api->api_calloc(nmemb, size))
#define gawk_realloc(ptr, size)		(api->api_realloc(ptr, size))
#define gawk_free(ptr)			(api->api_free(ptr))

#define create_value(value, result) \
	(api->api_create_value(ext_id, value,result))

#define release_value(value) \
	(api->api_release_value(ext_id, value))

#define get_file(name, namelen, filetype, fd, ibuf, obuf) \
	(api->api_get_file(ext_id, name, namelen, filetype, fd, ibuf, obuf))

/* These two are obsolete and should not be used. */
#define get_mpfr_ptr() (api->api_get_mpfr(ext_id))
#define get_mpz_ptr() (api->api_get_mpz(ext_id))

#define register_ext_version(version) \
	(api->api_register_ext_version(ext_id, version))

#define emalloc(pointer, type, size, message) \
	do { \
		if ((pointer = (type) gawk_malloc(size)) == 0) \
			fatal(ext_id, "%s: malloc of %d bytes failed", message, size); \
	} while(0)

#define ezalloc(pointer, type, size, message) \
	do { \
		if ((pointer = (type) gawk_calloc(1, size)) == 0) \
			fatal(ext_id, "%s: calloc of %d bytes failed", message, size); \
	} while(0)

#define erealloc(pointer, type, size, message) \
	do { \
		if ((pointer = (type) gawk_realloc(pointer, size)) == 0) \
			fatal(ext_id, "%s: realloc of %d bytes failed", message, size); \
	} while(0)

/* Constructor functions */

/* r_make_string_type --- make a string or strnum or regexp value in result from the passed-in string */

static inline awk_value_t *
r_make_string_type(const gawk_api_t *api,	/* needed for emalloc */
		   awk_ext_id_t ext_id,		/* ditto */
		   const char *string,
		   size_t length,
		   awk_bool_t duplicate,
		   awk_value_t *result,
		   awk_valtype_t val_type)
{
	char *cp = NULL;

	memset(result, 0, sizeof(*result));

	result->val_type = val_type;
	result->str_value.len = length;

	if (duplicate) {
		emalloc(cp, char *, length + 1, "r_make_string");
		memcpy(cp, string, length);
		cp[length] = '\0';
		result->str_value.str = cp;
	} else {
		result->str_value.str = (char *) string;
	}

	return result;
}

/* r_make_string --- make a string value in result from the passed-in string */

static inline awk_value_t *
r_make_string(const gawk_api_t *api,	/* needed for emalloc */
	      awk_ext_id_t ext_id,	/* ditto */
	      const char *string,
	      size_t length,
	      awk_bool_t duplicate,
	      awk_value_t *result)
{
	return r_make_string_type(api, ext_id, string, length, duplicate, result, AWK_STRING);
}

#define make_const_string(str, len, result)	r_make_string(api, ext_id, str, len, awk_true, result)
#define make_malloced_string(str, len, result)	r_make_string(api, ext_id, str, len, awk_false, result)

#define make_const_regex(str, len, result)	r_make_string_type(api, ext_id, str, len, awk_true, result, AWK_REGEX)
#define make_malloced_regex(str, len, result)	r_make_string_type(api, ext_id, str, len, awk_false, result, AWK_REGEX)

/*
 * Note: The caller may not create a STRNUM, but it can create a string that is
 * flagged as user input that MAY be a STRNUM. Gawk will decide whether it's a
 * STRNUM or a string by checking whether the string is numeric.
 */
#define make_const_user_input(str, len, result)	r_make_string_type(api, ext_id, str, len, 1, result, AWK_STRNUM)
#define make_malloced_user_input(str, len, result)	r_make_string_type(api, ext_id, str, len, 0, result, AWK_STRNUM)

/* make_null_string --- make a null string value */

static inline awk_value_t *
make_null_string(awk_value_t *result)
{
	memset(result, 0, sizeof(*result));
	result->val_type = AWK_UNDEFINED;

	return result;
}

/* make_number --- make a number value in result */

static inline awk_value_t *
make_number(double num, awk_value_t *result)
{
	result->val_type = AWK_NUMBER;
	result->num_value = num;
	result->num_type = AWK_NUMBER_TYPE_DOUBLE;
	return result;
}

/*
 * make_number_mpz --- make an mpz number value in result.
 * The mpz_ptr must be from a call to get_mpz_ptr.
 */

static inline awk_value_t *
make_number_mpz(void *mpz_ptr, awk_value_t *result)
{
	result->val_type = AWK_NUMBER;
	result->num_type = AWK_NUMBER_TYPE_MPZ;
	result->num_ptr = mpz_ptr;
	return result;
}

/*
 * make_number_mpfr --- make an mpfr number value in result.
 * The mpfr_ptr must be from a call to get_mpfr_ptr.
 */

static inline awk_value_t *
make_number_mpfr(void *mpfr_ptr, awk_value_t *result)
{
	result->val_type = AWK_NUMBER;
	result->num_type = AWK_NUMBER_TYPE_MPFR;
	result->num_ptr = mpfr_ptr;
	return result;
}

/* make_bool --- make a bool value in result */

static inline awk_value_t *
make_bool(awk_bool_t boolval, awk_value_t *result)
{
	result->val_type = AWK_BOOL;
	result->bool_value = boolval;
	return result;
}


/*
 * Each extension must define a function with this prototype:
 *
 *	int dl_load(gawk_api_t *api_p, awk_ext_id_t id)
 *
 * The return value should be zero on failure and non-zero on success.
 *
 * For the macros to work, the function should save api_p in a global
 * variable named 'api' and save id in a global variable named 'ext_id'.
 * In addition, a global function pointer named 'init_func' should be
 * defined and set to either NULL or an initialization function that
 * returns non-zero on success and zero upon failure.
 */

extern int dl_load(const gawk_api_t *const api_p, awk_ext_id_t id);

#if 0
/* Boilerplate code: */
int plugin_is_GPL_compatible;

static gawk_api_t *const api;
static awk_ext_id_t ext_id;
static const char *ext_version = NULL; /* or ... = "some string" */

static awk_ext_func_t func_table[] = {
	{ "name", do_name, 1 },
	/* ... */
};

/* EITHER: */

static awk_bool_t (*init_func)(void) = NULL;

/* OR: */

static awk_bool_t
init_my_extension(void)
{
	...
}

static awk_bool_t (*init_func)(void) = init_my_extension;

dl_load_func(func_table, some_name, "name_space_in_quotes")
#endif

#define dl_load_func(func_table, extension, name_space) \
int dl_load(const gawk_api_t *const api_p, awk_ext_id_t id)  \
{ \
	size_t i, j; \
	int errors = 0; \
\
	api = api_p; \
	ext_id = (void **) id; \
\
	if (api->major_version != GAWK_API_MAJOR_VERSION \
	    || api->minor_version < GAWK_API_MINOR_VERSION) { \
		fprintf(stderr, #extension ": version mismatch with gawk!\n"); \
		fprintf(stderr, "\tmy version (API %d.%d), gawk version (API %d.%d)\n", \
			GAWK_API_MAJOR_VERSION, GAWK_API_MINOR_VERSION, \
			api->major_version, api->minor_version); \
		exit(1); \
	} \
\
	check_mpfr_version(extension); \
\
	/* load functions */ \
	for (i = 0, j = sizeof(func_table) / sizeof(func_table[0]); i < j; i++) { \
		if (func_table[i].name == NULL) \
			break; \
		if (! add_ext_func(name_space, & func_table[i])) { \
			warning(ext_id, #extension ": could not add %s", \
					func_table[i].name); \
			errors++; \
		} \
	} \
\
	if (init_func != NULL) { \
		if (! init_func()) { \
			warning(ext_id, #extension ": initialization function failed"); \
			errors++; \
		} \
	} \
\
	if (ext_version != NULL) \
		register_ext_version(ext_version); \
\
	return (errors == 0); \
}

#if defined __GNU_MP_VERSION && defined MPFR_VERSION_MAJOR
#define check_mpfr_version(extension) do { \
	if (api->gmp_major_version != __GNU_MP_VERSION \
	    || api->gmp_minor_version < __GNU_MP_VERSION_MINOR) { \
		fprintf(stderr, #extension ": GMP version mismatch with gawk!\n"); \
		fprintf(stderr, "\tmy version (%d, %d), gawk version (%d, %d)\n", \
			__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR, \
			api->gmp_major_version, api->gmp_minor_version); \
		exit(1); \
	} \
	if (api->mpfr_major_version != MPFR_VERSION_MAJOR \
	    || api->mpfr_minor_version < MPFR_VERSION_MINOR) { \
		fprintf(stderr, #extension ": MPFR version mismatch with gawk!\n"); \
		fprintf(stderr, "\tmy version (%d, %d), gawk version (%d, %d)\n", \
			MPFR_VERSION_MAJOR, MPFR_VERSION_MINOR, \
			api->mpfr_major_version, api->mpfr_minor_version); \
		exit(1); \
	} \
} while (0)
#else
#define check_mpfr_version(extension) /* nothing */
#endif

#endif /* GAWK */

#ifdef __cplusplus
}
#endif	/* C++ */

#endif /* _GAWK_API_H */
