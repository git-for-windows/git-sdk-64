#ifndef JAS_CONFIG_H
#define JAS_CONFIG_H

#ifndef JAS_DLL
#define JAS_DLL 1
#endif

#include <jasper/jas_compiler.h>
#include <jasper/jas_dll.h>

/* This preprocessor symbol identifies the version of JasPer. */
#define	JAS_VERSION "2.0.32"
#define JAS_VERSION_MAJOR 2
#define JAS_VERSION_MINOR 0
#define JAS_VERSION_PATCH 32

/* #undef JAS_ENABLE_32BIT */

#define JAS_HAVE_FCNTL_H 1
#define JAS_HAVE_IO_H 1
#define JAS_HAVE_UNISTD_H 1
#define JAS_HAVE_SYS_TIME_H 1
#define JAS_HAVE_SYS_TYPES_H 1

/* #undef JAS_HAVE_MKOSTEMP */
#define JAS_HAVE_GETTIMEOFDAY 1
/* #undef JAS_HAVE_GETRUSAGE */

/* #undef JAS_HAVE_GL_GLUT_H */
/* #undef JAS_HAVE_GLUT_H */

#define JAS_INCLUDE_PNM_CODEC 1
#define JAS_INCLUDE_BMP_CODEC 1
#define JAS_INCLUDE_RAS_CODEC 1
#define JAS_INCLUDE_JP2_CODEC 1
#define JAS_INCLUDE_JPC_CODEC 1
#define JAS_INCLUDE_JPG_CODEC 1
#define JAS_INCLUDE_PGX_CODEC 1
#define JAS_INCLUDE_MIF_CODEC 1
/* #undef JAS_ENABLE_DANGEROUS_INTERNAL_TESTING_MODE */

#if defined(JAS_ENABLE_DANGEROUS_INTERNAL_TESTING_MODE)
#define JAS_ENABLE_MIF_CODEC 1
#else
#undef JAS_ENABLE_MIF_CODEC
#endif

/*
#if defined(JAS_INCLUDE_JP2_CODEC) && !defined(JAS_INCLUDE_JPC_CODEC)
#undef JAS_INCLUDE_JPC_CODEC
#endif
*/

#ifdef _MSC_VER
#undef JAS_HAVE_SNPRINTF
#else
#define JAS_HAVE_SNPRINTF 1
#endif

#if !defined(JAS_DEC_DEFAULT_MAX_SAMPLES)
#define JAS_DEC_DEFAULT_MAX_SAMPLES (64 * ((size_t) 1048576))
#endif

#endif
