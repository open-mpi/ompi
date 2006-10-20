/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * This file is included at the bottom of ompi_config.h, and is
 * therefore a) after all the #define's that were output from
 * configure, and b) included in most/all files in Open MPI.
 *
 * Since this file is *only* ever included by ompi_config.h, and
 * ompi_config.h already has #ifndef/#endif protection, there is no
 * need to #ifndef/#endif protection here.
 */

#ifndef OPAL_CONFIG_H 
#error "opal_config_bottom.h should only be included from opal_config.h"
#endif

/*
 * If we build a static library, Visual C define the _LIB symbol. In the
 * case of a shared library _USERDLL get defined.
 *
 * OMPI_BUILDING and _LIB define how ompi_config.h
 * handles configuring all of Open MPI's "compatibility" code.  Both
 * constants will always be defined by the end of ompi_config.h.
 *
 * OMPI_BUILDING affects how much compatibility code is included by
 * ompi_config.h.  It will always be 1 or 0.  The user can set the
 * value before including either mpi.h or ompi_config.h and it will be
 * respected.  If ompi_config.h is included before mpi.h, it will
 * default to 1.  If mpi.h is included before ompi_config.h, it will
 * default to 0.
 */
#ifndef OMPI_BUILDING
#define OMPI_BUILDING 1
#endif

/***********************************************************************
 *
 * code that should be in ompi_config_bottom.h regardless of build
 * status
 *
 **********************************************************************/

/* Do we have thread support? */
#define OMPI_HAVE_THREAD_SUPPORT (OMPI_ENABLE_MPI_THREADS || OMPI_ENABLE_PROGRESS_THREADS)

/***********************************************************************
 *
 * Windows library interface declaration code
 *
 **********************************************************************/
#if !defined(__WINDOWS__)
#  if defined(_WIN32) || defined(WIN32) || defined(WIN64)
#    define __WINDOWS__
#  endif
#endif  /* !defined(__WINDOWS__) */

#if defined(__WINDOWS__)

#  if defined(_USRDLL)    /* building shared libraries (.DLL) */
#    if defined(OPAL_EXPORTS)
#      define OPAL_DECLSPEC        __declspec(dllexport)
#      define OPAL_MODULE_DECLSPEC
#    else
#      define OPAL_DECLSPEC        __declspec(dllimport)
#      if defined(OPAL_MODULE_EXPORTS)
#        define OPAL_MODULE_DECLSPEC __declspec(dllexport)
#      else
#        define OPAL_MODULE_DECLSPEC __declspec(dllimport)
#      endif  /* defined(OPAL_MODULE_EXPORTS) */
#    endif  /* defined(OPAL_EXPORTS) */
#  else          /* building static library */
#    if defined(OPAL_IMPORTS)
#      define OPAL_DECLSPEC        __declspec(dllimport)
#    else
#      define OPAL_DECLSPEC
#    endif  /* defined(OPAL_IMPORTS) */
#    define OPAL_MODULE_DECLSPEC
#  endif  /* defined(_USRDLL) */
#  if OMPI_BUILDING
#    include "opal/win32/win_compat.h"
#  endif  /* OMPI_BUILDING */
#else
   /* On Unix - this define is plain useless */
#  define OPAL_DECLSPEC
#  define OPAL_MODULE_DECLSPEC
#endif  /* defined(__WINDOWS__) */

/*
 * Do we have <stdint.h>?
 */
#ifdef HAVE_STDINT_H
#if defined(__cplusplus) && !defined(__STDC_LIMIT_MACROS)
/* When using a C++ compiler, the max / min value #defines for std
   types are only included if __STDC_LIMIT_MACROS is set before
   including stdint.h */
#define __STDC_LIMIT_MACROS
#endif
#include <stdint.h>
#else
#include "opal_stdint.h"
#endif

/***********************************************************************
 *
 * Code that is only for when building Open MPI or utilities that are
 * using the internals of Open MPI.  It should not be included when
 * building MPI applications
 *
 **********************************************************************/
#if OMPI_BUILDING

#ifndef HAVE_PTRDIFF_T
typedef OMPI_PTRDIFF_TYPE ptrdiff_t;
#endif

/*
 * If we're in C, we may need to bring in the bool type and true/false
 * constants.  OMPI_NEED_C_BOOL will be true if the compiler either
 * needs <stdbool.h> or doesn't define the bool type at all.
 */
#if !(defined(c_plusplus) || defined(__cplusplus))
#    if OMPI_NEED_C_BOOL
#        if OMPI_USE_STDBOOL_H
             /* If we're using <stdbool.h>, there is an implicit
                assumption that the C++ bool is the same size and has
                the same alignment. */
#            include <stdbool.h>
#        else
             /* We need to create a bool type and ensure that it's the
                same size / alignment as the C++ bool size /
                alignment */
#            define false 0
#            define true 1
#            if SIZEOF_BOOL == SIZEOF_CHAR && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_CHAR
typedef unsigned char bool;
#            elif SIZEOF_BOOL == SIZEOF_SHORT && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_SHORT
typedef short bool;
#            elif SIZEOF_BOOL == SIZEOF_INT && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_INT
typedef int bool;
#            elif SIZEOF_BOOL == SIZEOF_LONG && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_LONG
typedef long bool;
#            elif defined(SIZEOF_LONG_LONG) && defined(OMPI_ALIGNMENT_LONG) && SIZEOF_BOOL == SIZEOF_LONG && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_LONG
typedef long long bool;
#            else
#                error Cannot find a C type that corresponds to the size and alignment of C++ bool!
#            endif
#        endif  /* OMPI_USE_STDBOOL_H */
#    endif  /* OMPI_NEED_C_BOOL */
#endif

/*
 * Maximum size of a filename path.
 */
#include <limits.h>
#if defined(PATH_MAX)
#define OMPI_PATH_MAX	(PATH_MAX + 1)
#elif defined(_POSIX_PATH_MAX)
#define OMPI_PATH_MAX	(_POSIX_PATH_MAX + 1)
#else
#define OMPI_PATH_MAX	256
#endif

/*
 * Set the compile-time path-separator on this system and variable separator
 */
#ifdef __WINDOWS__
#define OPAL_PATH_SEP "\\"
#define OPAL_ENV_SEP  ';'
#else
#define OPAL_PATH_SEP "/"
#define OPAL_ENV_SEP  ':'
#endif


/*
 * Do we want memory debugging?
 *
 * A few scenarios:
 *
 * 1. In the OMPI C library: we want these defines in all cases
 * 2. In the OMPI C++ bindings: we do not want them
 * 3. In the OMPI C++ executables: we do want them
 *
 * So for 1, everyone must include <ompi_config.h> first.  For 2, the
 * C++ bindings will never include <ompi_config.h> -- they will only
 * include <mpi.h>, which includes <ompi_config.h>, but after
 * setting OMPI_BUILDING to 0  For 3, it's the same as 1 -- just include
 * <ompi_config.h> first.
 *
 * Give code that needs to include ompi_config.h but really can't have
 * this stuff enabled (like the memory manager code) a way to turn us
 * off
 */
#if OMPI_ENABLE_MEM_DEBUG && !defined(OMPI_DISABLE_ENABLE_MEM_DEBUG)

/* It is safe to include opal/util/malloc.h here because a) it will only
   happen when we are building OMPI and therefore have a full OMPI
   source tree [including headers] available, and b) we guaranteed to
   *not* to include anything else via opal/util/malloc.h, so we won't
   have Cascading Includes Of Death. */
#    include "opal/util/malloc.h"
#    if defined(malloc)
#        undef malloc
#    endif
#    define malloc(size) opal_malloc((size), __FILE__, __LINE__)
#    if defined(calloc)
#        undef calloc
#    endif
#    define calloc(nmembers, size) opal_calloc((nmembers), (size), __FILE__, __LINE__)
#    if defined(realloc)
#        undef realloc
#    endif
#    define realloc(ptr, size) opal_realloc((ptr), (size), __FILE__, __LINE__)
#    if defined(free)
#        undef free
#    endif
#    define free(ptr) opal_free((ptr), __FILE__, __LINE__)

/*
 * If we're mem debugging, make the OMPI_DEBUG_ZERO resolve to memset
 */
#    include <string.h>
#    define OMPI_DEBUG_ZERO(obj) memset(&(obj), 0, sizeof(obj))
#else
#    define OMPI_DEBUG_ZERO(obj)
#endif



/*
 * printf functions for portability (only when building Open MPI)
 */
#if !defined(HAVE_VASPRINTF) || !defined(HAVE_VSNPRINTF)
#include <stdarg.h>
#include <stdlib.h>
#endif

#if !defined(HAVE_ASPRINTF) || !defined(HAVE_SNPRINTF) || !defined(HAVE_VASPRINTF) || !defined(HAVE_VSNPRINTF)
#include "opal/util/printf.h"
#endif

#ifndef HAVE_ASPRINTF
# define asprintf opal_asprintf
#endif

#ifndef HAVE_SNPRINTF
# define snprintf opal_snprintf
#endif

#ifndef HAVE_VASPRINTF
# define vasprintf opal_vasprintf
#endif

#ifndef HAVE_VSNPRINTF
# define vsnprintf opal_vsnprintf
#endif

/*
 * Some platforms (Solaris) have a broken qsort implementation.  Work
 * around by using our own.
 */
#if OMPI_HAVE_BROKEN_QSORT
#ifdef qsort
#undef qsort
#endif

#include "opal/util/qsort.h"
#define qsort opal_qsort
#endif

/*
 * On some homogenous big-iron machines (Sandia's Red Storm), there
 * are no htonl and friends.  If that's the case, provide stubs.  I
 * would hope we never find a platform that doesn't have these macros
 * and would want to talk to the outside world... On other platforms
 * (like Windows) we fail to detect them correctly.
 */
#ifndef __WINDOWS__
#ifndef HAVE_HTONL
static inline uint32_t htonl(uint32_t hostvar) { return hostvar; }
#endif
#ifndef HAVE_NTOHL
static inline uint32_t ntohl(uint32_t netvar) { return netvar; }
#endif
#ifndef HAVE_HTONS
static inline uint16_t htons(uint16_t hostvar) { return hostvar; }
#endif
#ifndef HAVE_NTOHS
static inline uint16_t ntohs(uint16_t netvar) { return netvar; }
#endif
#endif  /* WINDOWS */

/*
 * Define __func__-preprocessor directive if the compiler does not
 * already define it.  Define it to __FILE__ so that we at least have
 * a clue where the developer is trying to indicate where the error is
 * coming from (assuming that __func__ is typically used for
 * printf-style debugging).
 */
#if defined(HAVE_DECL___FUNC__) && !HAVE_DECL___FUNC__
#define __func__ __FILE__
#endif

/**
 * Because of the way we're using the opal_object inside Open MPI (i.e.
 * dynamic resolution at run-time to derive all objects from the basic
 * type), on Windows we have to build everything on C++ mode, simply
 * because the C mode does not support dynamic resolution in DLL. Therefore,
 * no automatic conversion is allowed. All types have to be explicitly casted
 * or the compiler generate an error. This is true even for the void* type. As
 * we use void* to silence others compilers in the resolution of the addr member
 * of the iovec structure, we have now to find a way around. The simplest solution
 * is to define a special type for this field (just for casting). It can be
 * set to void* on all platforms with the exception of windows where it has to be
 * char*.
 */
#if defined(__WINDOWS__)
#define IOVBASE_TYPE  char
#else
#define IOVBASE_TYPE  void
#endif  /* defined(__WINDOWS__) */

/**
 * If we generate our own bool type, we need a special way to cast the result
 * in such a way to keep the compilers silent. Right now, th only compiler who
 * complain about int to bool conversions is the Microsoft compiler.
 */
#if defined(__WINDOWS__)
#  define OPAL_INT_TO_BOOL(VALUE)  ((VALUE) != 0 ? true : false)
#else
#  define OPAL_INT_TO_BOOL(VALUE)  (bool)(VALUE)
#endif  /* defined(__WINDOWS__) */

/**
 * Top level define to check 2 things: a) if we want ipv6 support, and
 * b) the underlying system supports ipv6.  Having one #define for
 * this makes it simpler to check throughout the code base.
 */
#if OPAL_ENABLE_IPV6 && defined(HAVE_STRUCT_SOCKADDR_IN6)
#define OPAL_WANT_IPV6 1
#else
#define OPAL_WANT_IPV6 0
#endif

#endif /* OMPI_BUILDING */
