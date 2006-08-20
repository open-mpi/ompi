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
#ifndef __WINDOWS__
#  if defined(_WIN32) || defined(WIN32)
#    define __WINDOWS__
#  endif
#endif

#if defined(__WINDOWS__)

#  if defined(_USRDLL)    /* building shared libraries (.DLL) */
#    if defined(OPAL_EXPORTS)
#      define OPAL_DECLSPEC __declspec(dllexport)
#    else
#      define OPAL_DECLSPEC __declspec(dllimport)
#    endif
#    if defined(OPAL_MODULE_EXPORTS)
#      define OPAL_MODULE_DECLSPEC __declspec(dllexport)
#    else
#      define OPAL_MODULE_DECLSPEC __declspec(dllimport)
#    endif
#  else          /* building static library */
#    define OPAL_DECLSPEC
#    define OPAL_MODULE_DECLSPEC
#  endif
#  if OMPI_BUILDING
#    include "opal/win32/win_compat.h"
#  endif  /* OMPI_BUILDING */
#else
   /* On Unix - this define is plain useless */
#  define OPAL_DECLSPEC
#  define OPAL_MODULE_DECLSPEC
#endif

/***********************************************************************
 *
 * Code that is only for when building Open MPI or utilities that are
 * using the internals of Open MPI.  It should not be included when
 * building MPI applications
 *
 **********************************************************************/
#if OMPI_BUILDING

/*
 * If we're in C, we may need to bring in the bool type and true/false
 * constants.  OMPI_NEED_C_BOOL will be true if the compiler either
 * needs <stdbool.h> or doesn't define the bool type at all.
 */
#if !defined(__cplusplus)
#    if OMPI_NEED_C_BOOL
#        if OMPI_USE_STDBOOL_H
             /* If we're using <stdbool.h>, there is an implicit
                assumption that the C++ bool is the same size and has
                the same alignment. */
#            include <stdbool.h>
#        elif defined(__WINDOWS__)
#            define bool BOOL
#            define false FALSE
#            define true TRUE
#        else
             /* We need to create a bool type and ensure that it's the
                same size / alignment as the C++ bool size /
                alignment */
#            define false 0
#            define true 1
#            if SIZEOF_BOOL == SIZEOF_CHAR && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_CHAR
typedef char bool;
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
#        endif
#    endif
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
 * Set the compile-time path-separator on this system
 */
#ifdef __WINDOWS__
#define OMPI_PATH_SEP "\\"
#else
#define OMPI_PATH_SEP "/"
#endif

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

#endif /* OMPI_BUILDING */
