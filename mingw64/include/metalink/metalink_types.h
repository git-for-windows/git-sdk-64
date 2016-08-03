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
#ifndef _D_METALINK_TYPES_H_
#define _D_METALINK_TYPES_H_

#include <time.h>

#include <metalink/metalink_error.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _metalink_resource {
  /* url, null terminated string */
  char* url;
  /* type of resources, like "http", "ftp", null terminated string */
  char* type;
  /* location, this is 2-characther country code, like "JP",
   * null terminated string
   */
  char* location;
  /* preference of this resource, higher value has bigger
     preference. This is used for metalink 3. To keep compatibility
     with it, when reading metalink 4, 100000 - priority is assigned
     to preference. */
  int preference;
  /* priority of this resource, lower value has bigger priority. This
     is used for metalink 4. */
  int priority;
  /* max connections that a client can establish to this resource */
  int maxconnections;
} metalink_resource_t;

metalink_resource_t* metalink_resource_new(void);

void metalink_resource_delete(metalink_resource_t* resource);

/* mutators */
metalink_error_t metalink_resource_set_type(metalink_resource_t* resource, const char* type);

metalink_error_t metalink_resource_set_location(metalink_resource_t* resource,
				   const char* location);

void metalink_resource_set_preference(metalink_resource_t* resource,
				      int preference);

void metalink_resource_set_priority(metalink_resource_t* resource,
				    int priority);

void metalink_resource_set_maxconnections(metalink_resource_t* resource,
					  int maxconnections);

metalink_error_t metalink_resource_set_url(metalink_resource_t* resource, const char* url);

typedef struct _metalink_metaurl {
  /* url, null terminated string */
  char* url;
  /* typef of the media, like "torrent", null terminated string */
  char* mediatype;
  /* name of the metaurl, null terminated string */
  char* name;
  /* priority of this resource */
  int priority;
} metalink_metaurl_t;

/* constructor */
metalink_metaurl_t* metalink_metaurl_new(void);

/* destructor */
void metalink_metaurl_delete(metalink_metaurl_t* metaurl);

/* mutators */
metalink_error_t metalink_metaurl_set_url(metalink_metaurl_t* metaurl,
					  const char* url);

metalink_error_t metalink_metaurl_set_mediatype(metalink_metaurl_t* metaurl,
						const char* mediatype);

metalink_error_t metalink_metaurl_set_name(metalink_metaurl_t* metaurl,
					   const char* name);

void metalink_metaurl_set_priority(metalink_metaurl_t* metaurl,
				   int priority);

typedef struct _metalink_checksum {
  /* message digest algorithm, for example, sha1, null terminated string */
  char* type;
  /* message digest in a ASCII hexadecimal notation, null terminated string */
  char* hash;
} metalink_checksum_t;

metalink_checksum_t* metalink_checksum_new(void);

void metalink_checksum_delete(metalink_checksum_t* checksum);

/* mutators */
metalink_error_t metalink_checksum_set_type(metalink_checksum_t* checksum, const char* type);

metalink_error_t metalink_checksum_set_hash(metalink_checksum_t* checksum, const char* hash);

/**
 * hash value of each piece.
 */
typedef struct _metalink_piece_hash {
  int piece;
  /* hash value in a ASCII hexadecimal notation */
  char* hash;
} metalink_piece_hash_t;

/* constructor */
metalink_piece_hash_t* metalink_piece_hash_new(void);

/* destructor */
void metalink_piece_hash_delete(metalink_piece_hash_t* piece_hash);

/* mutators */
void metalink_piece_hash_set_piece(metalink_piece_hash_t* piece_hash, int piece);

metalink_error_t metalink_piece_hash_set_hash(metalink_piece_hash_t* piece_hash, const char* hash);

/**
 * Piece hash; containing type(hash algorithm) and piece size and
 * hashes.
 */
typedef struct _metalink_chunk_checksum {
  /* message digest algorithm, for example, sha1, null terminated string */
  char* type;
  /* length of piece */
  int length;
  /* list of hash. Iterate until you get NULL */
  metalink_piece_hash_t** piece_hashes;
} metalink_chunk_checksum_t;

/* constructor */
metalink_chunk_checksum_t* metalink_chunk_checksum_new(void);

/* destructor */
void metalink_chunk_checksum_delete(metalink_chunk_checksum_t* chunk_checksum);

/* mutators */
metalink_error_t metalink_chunk_checksum_set_type(metalink_chunk_checksum_t* chunk_checksum,
				     const char* type);

void metalink_chunk_checksum_set_length(metalink_chunk_checksum_t* chunk_checksum,
					int length);

void metalink_chunk_checksum_set_piece_hashes(metalink_chunk_checksum_t* chunk_checksum,
					      metalink_piece_hash_t** piece_hashes);

/**
 *  signature of a file
 */
typedef struct _metalink_signature {
  /* the type of the signature (eg. application/pgp-signature) */
  char* mediatype;
  /* the content of the signature */
  char* signature;
} metalink_signature_t;

/* constructor */
metalink_signature_t* metalink_signature_new(void);

/* destructor */
void metalink_signature_delete(metalink_signature_t* signature);

/* mutators */
metalink_error_t metalink_signature_set_mediatype(metalink_signature_t* signature,
						  const char* mediatype);

metalink_error_t metalink_signature_set_signature(metalink_signature_t* signature,
						  const char* value);

typedef struct _metalink_file {
  /* filename, null terminated string */
  char* name;
  /* file description, null terminated string */
  char* description;
  /* file size */
  long long int size;
  /* version, null terminated string */
  char* version;
  /* copyright, null terminated string */
  char* copyright;
  /* identity, null terminated string */
  char* identity;
  /* logo, null terminated string */
  char* logo;
  /* publisher name, null terminated string */
  char* publisher_name;
  /* publisher url, null terminated string */
  char* publisher_url;
  /* list of language, null terminated list of null terminated string */
  char** languages;
  /* first language, for compatibility with metalink 3 */
  char* language;
  /* list of os, null terminated list of null terminated string */
  char** oses;
  /* first os, for compatibility with metalink 3 */
  char* os;
  /* file signature */
  metalink_signature_t* signature;
  /* maximum number of connections for this file */
  int maxconnections;
  /* list of metalink_resource_t */
  metalink_resource_t** resources;
  /* list of metaurls (metalink_resource_t) */
  metalink_metaurl_t** metaurls;
  /* list of metalink_checksum_t. It is possible to include multiple message
   * digest algorithms
   */
  metalink_checksum_t** checksums;

  /* chunk checksum */
  metalink_chunk_checksum_t* chunk_checksum;

} metalink_file_t;

/* constructor */
metalink_file_t* metalink_file_new(void);

/* destructor */
void metalink_file_delete(metalink_file_t* file);

/* mutators */
metalink_error_t metalink_file_set_name(metalink_file_t* file, const char* name);

metalink_error_t metalink_file_set_description(metalink_file_t* file, const char* description);

void metalink_file_set_size(metalink_file_t* file, long long int size);

metalink_error_t metalink_file_set_version(metalink_file_t* file, const char* version);

metalink_error_t metalink_file_set_copyright(metalink_file_t* file, const char* copyright);

metalink_error_t metalink_file_set_identity(metalink_file_t* file, const char* identity);

metalink_error_t metalink_file_set_logo(metalink_file_t* file, const char* logo);

metalink_error_t metalink_file_set_publisher_name(metalink_file_t* file, const char* name);

metalink_error_t metalink_file_set_publisher_url(metalink_file_t* file, const char* url);

void metalink_file_set_maxconnections(metalink_file_t* file, int maxconnections);

typedef enum metalink_version_e {
  METALINK_VERSION_UNKNOWN,
  METALINK_VERSION_3 = 3,
  METALINK_VERSION_4 = 4
} metalink_version_t;

typedef struct _metalink {
  /* put more descriptable information here... */
  /* date, publisher or something useful */
  
  /* metalink version of this file */
  metalink_version_t version;
  /* generator of this metalink, null terminated string */
  char* generator;
  /* origin of this metalink, null terminated string. In Metalink
     version 4, this is the content of the origin element. In Metalink
     version 3, this is the origin attribute of the metalink
     element. */
  char* origin;
  /* In Metalink version 4, this value is 1 if the dynamic attribute
   * of the origin element is "true". In Metalink version 3, this
   * value is 1 if the type attribute of the metalink element is
   * "dynamic". */
  int origin_dynamic;
  /* timestamp corresponding to the publication of this metalink */
  time_t published;
  /* timestamp corresponding to the last update of this metalink */
  time_t updated;

  /* list of metalink_file_t */
  metalink_file_t** files;
  char* identity;
  char* tags;
} metalink_t;

metalink_error_t metalink_set_identity(metalink_t* metalink, const char* identity);
metalink_error_t metalink_set_tags(metalink_t* metalink, const char* tags);
metalink_error_t metalink_set_generator(metalink_t* metalink, const char* generator);
metalink_error_t metalink_set_origin(metalink_t* metalink, const char* origin);
void metalink_set_origin_dynamic(metalink_t* metalink, int origin_dynamic);
void metalink_set_published(metalink_t* metalink, time_t published);
void metalink_set_updated(metalink_t* metalink, time_t updated);
void metalink_set_version(metalink_t* metalink, metalink_version_t version);

metalink_t* metalink_new(void);

void metalink_delete(metalink_t* metalink);

#ifdef __cplusplus
}
#endif

#endif /* _D_METALINK_TYPES_H_ */
