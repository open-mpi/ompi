/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef OMPI_CONFIG_H 
#error "ompi_config_bottom.h should only be included from ompi_config.h"
#endif

/*
 * OMPI_BUILDING and OMPI_BUILDING_WIN_DSO define how ompi_config.h
 * handles configuring all of Open MPI's "compatibility" code.  Both
 * constants will always be defined by the end of ompi_config.h.
 *
 * OMPI_BUILDING affects how much compatibility code is included by
 * ompi_config.h.  It will always be 1 or 0.  The user can set the
 * value before including either mpi.h or ompi_config.h and it will be
 * respected.  If ompi_config.h is included before mpi.h, it will
 * default to 1.  If mpi.h is included before ompi_config.h, it will
 * default to 0.
 *
 * If OMPI_BUILDING is 1, ompi_config.h will:
 *   - everything that happens with OMPI_BUILDING set to 0
 *   - include a bunch of header files (stdint.h, stdtypes.h, etc.)
 *   - potentially override malloc, free, etc. for memory debugging
 *   - provide C bool type
 *   - set ompi_building code to import all OMPI interfaces (windows)
 *
 * If OMPI_BUILDING is 0, ompi_config.h will:
 *   - set configuration #defines
 *   - define the fortran complex types
 *   - set the ompi_building code to export all the interfaces
 *     (unless OMPI_BUILDING_WIN_DSO is set to 1) (windows)
 *
 * If OMPI_BUILDING_WIN_DSO is 1, ompi_config.h will:
 *   - configure the OMPI_DECLSPEC defines to assume we are building a
 *     dynamic shared object for a component on Windows.  This will set
 *     all the import/export flags appropriately.
 *
 * If OMPI_BUILDING_WIN_DSO is 0 (or unset), ompi_config.h will:
 *   - configure the OMPI_DECLSPEC defines to assume we are not building
 *     a DSO component for Windows .
 */
#ifndef OMPI_BUILDING
#define OMPI_BUILDING 1
#endif

#ifndef OMPI_BUILDING_WIN_DSO
#define OMPI_BUILDING_WIN_DSO 0
#endif

/***********************************************************************
 *
 * code that should be in ompi_config_bottom.h regardless of build
 * status
 *
 **********************************************************************/

/* MPI_Fint is the same as ompi_fortran_INTEGER_t */
#define MPI_Fint ompi_fortran_integer_t

/* Do we have thread support? */
#define OMPI_HAVE_THREAD_SUPPORT (OMPI_ENABLE_MPI_THREADS || OMPI_ENABLE_PROGRESS_THREADS)

#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
/* * C type for Fortran COMPLEX */
typedef struct {
  ompi_fortran_real_t real;
  ompi_fortran_real_t imag;
} ompi_fortran_complex_t;
#endif

#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
/* * C type for Fortran COMPLEX*8 */
typedef struct {
  ompi_fortran_real4_t real;
  ompi_fortran_real4_t imag;
} ompi_fortran_complex8_t;
#endif

#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
/* * C type for Fortran COMPLEX*16 */
typedef struct {
  ompi_fortran_real8_t real;
  ompi_fortran_real8_t imag;
} ompi_fortran_complex16_t;
#endif

#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
/* * C type for Fortran COMPLEX*32 */
typedef struct {
  ompi_fortran_real16_t real;
  ompi_fortran_real16_t imag;
} ompi_fortran_complex32_t;
#endif

#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
/* * C type for Fortran DOUBLE COMPLEX */
typedef struct {
  ompi_fortran_double_precision_t real;
  ompi_fortran_double_precision_t imag;
} ompi_fortran_double_complex_t;
#endif


/***********************************************************************
 *
 * Windows library interface declaration code
 *
 **********************************************************************/

#if defined(WIN32)
#  if OMPI_BUILDING
#    include "win32/win_compat.h"
#  endif
#  if OMPI_BUILDING_WIN_DSO
     /* building a component - need to import libmpi and export our
        struct */
#    define OMPI_COMP_EXPORT __declspec(dllexport)
#    define OMPI_DECLSPEC __declspec(dllimport)
#  elif OMPI_BUILDING
     /* building libmpi (or something in libmpi) - need to export libmpi
        interface */
#    define OMPI_COMP_EXPORT
#    define OMPI_DECLSPEC __declspec(dllexport)
#  else
     /* building something using libmpi - export the libmpi interface */
#    define OMPI_COMP_EXPORT
#    define OMPI_DECLSPEC __declspec(dllimport)
#  endif
#else
   /* On Unix - get rid of the defines */ 
#  define OMPI_COMP_EXPORT
#  define OMPI_DECLSPEC
#endif


/***********************************************************************
 *
 * Code that is only for when building Open MPI or utilities that are
 * using the internals of Open MPI.  It should not be included when
 * building MPI applicatiosn
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
#        elif defined(WIN32)
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
 * setting OMPI_BUILDING to 0  For 3, it's the same as 1 -- just include
 * <ompi_config.h> first.
 */
#if OMPI_ENABLE_MEM_DEBUG

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
#include "util/printf.h"
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

#if OMPI_HAVE_BROKEN_QSORT
#include "util/qsort.h"
#define qsort ompi_qsort
#endif

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
