/*
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

/* 
 * If we're in C, bring in the bool type and true/false constants.
 */
#ifndef __cplusplus
#if OMPI_USE_STDBOOL_H
/* If we're using <stdbool.h>, there is an implicit assumption that
   the C++ bool is the same size and has the same alignment. */
#include <stdbool.h>
#else
/* We need to create a bool type and ensure that it's the same size /
   alignment as the C++ bool size / alignment */
#define false 0
#define true 1
#if SIZEOF_BOOL == SIZEOF_CHAR && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_CHAR
typedef bool char
#elif SIZEOF_BOOL == SIZEOF_SHORT && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_SHORT
typedef bool short
#elif SIZEOF_BOOL == SIZEOF_INT && OMPI_ALIGNMENT_CXX_BOOL == OMPI_ALIGNMENT_INT
typedef bool int
#else
#error Cannot find a C type that corresponds to the size and alignment of C++ bool!
#endif
#endif
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
 * Do we have thread support?
 */
#define OMPI_HAVE_THREADS (OMPI_HAVE_SOLARIS_THREADS || OMPI_HAVE_POSIX_THREADS)

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
#include "ompi_stdint.h"
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
 * OMPI_MPI_H is defined.  For 3, it's the same as 1 -- just include
 * <ompi_config.h> first.
 */
#if OMPI_ENABLE_MEM_DEBUG && defined(OMPI_BUILDING) && OMPI_BUILDING && !defined(OMPI_MPI_H)

/* It is safe to include util/malloc.h here because a) it will only
   happen when we are building OMPI and therefore have a full OMPI
   source tree [including headers] available, and b) we guaranteed to
   *not* to include anything else via mem/malloc.h, so we won't
   have Cascading Includes Of Death. */
#include "util/malloc.h"
#if defined(malloc)
#undef malloc
#endif
#define malloc(size) ompi_malloc((size), __FILE__, __LINE__)
#if defined(calloc)
#undef calloc
#endif
#define calloc(nmembers, size) ompi_calloc((nmembers), (size), __FILE__, __LINE__)
#if defined(realloc)
#undef realloc
#endif
#define realloc(ptr, size) ompi_realloc((ptr), (size), __FILE__, __LINE__)
#if defined(free)
#undef free
#endif
#define free(ptr) ompi_free((ptr), __FILE__, __LINE__)
#endif


/*
 * Do we want to override debugging controls?
 */
#if defined(OMPI_ENABLE_DEBUG_OVERRIDE) && OMPI_ENABLE_DEBUG_OVERRIDE
#undef OMPI_ENABLE_DEBUG
#define OMPI_ENABLE_DEBUG 1
#endif


/*
 * C type for Fortran COMPLEX
 */
typedef struct {
  ompi_fortran_real_t real;
  ompi_fortran_real_t imag;
} ompi_fortran_complex_t;


/*
 * C type for Fortran DOUBLE COMPLEX
 */
typedef struct {
  ompi_fortran_dblprec_t real;
  ompi_fortran_dblprec_t imag;
} ompi_fortran_dblcomplex_t;

/*
 * printf functions for portability
 */

#if !defined(HAVE_VASPRINTF) || !defined(HAVE_VSNPRINTF)
#include <stdarg.h>
#include <stdlib.h>
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#ifndef HAVE_ASPRINTF
# define asprintf ompi_asprintf
#endif

#ifndef HAVE_SNPRINTF
# define snprintf ompi_snprintf
#endif

#ifndef HAVE_VASPRINTF
# define vasprintf ompi_vasprintf
#endif

#ifndef HAVE_VSNPRINTF
# define vsnprintf ompi_vsnprintf
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
        
#endif

/* For windoze weirdness.  A symbol is not considered part of a DLL
   interface unless it is exported.  Hence these macros.  It needs to
   be prepended to the declaration of every symbol which needs to be
   exported from a DLL.  For us, this would be all MPI_ functions and
   also all function from within Open MPI which are used by other
   execuatables such as ompi_info, mpicc, etc.  Visual studio defines
   WIN32 and is the standard way of knowing whether we are compiling
   using its native compiler (random note: the compiler itself does
   not define this macro -- it's passed as -DWIN32 automatically).
   Also, they simply define the macro as opposed to defining it to
   something. I guess they dont program defensively :-) */

#if defined (WIN32) 
    #define OMPI_EXPORT __declspec(dllexport)
    #define OMPI_IMPORT __declspec(dllimport)
#else
    #define OMPI_EXPORT 
    #define OMPI_IMPORT 
#endif
