/*
 * $HEADER$
 *
 * This file is included at the bottom of ompi_config.h, and is
 * therefore a) after all the #define's that were output from
 * configure, and b) included in most/all files in OMPI/MPI.
 *
 * Since this file is *only* ever included by ompi_config.h, and
 * ompi_config.h already has #ifndef/#endif protection, there is no
 * need to #ifndef/#endif protection here.
 */

/* 
 * If we're in C, bring in the bool type and true/false constants.
 */
#ifndef __cplusplus
#ifdef HAVE_STDBOOL_H
#include <stdbool.h>
#else
typedef enum { false, true } bool;
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

/* parameter indicating if to check MPI arguments */
extern bool ompi_mpi_param_check;

/*
 * Do we have <stdint.h>?
 */
#if HAVE_STDINT_H
#include <stdint.h>
#else
#include "ompi_stdint.h"
#endif

/*
 * Do we want memory debugging?
 */
#if OMPI_ENABLE_MEM_DEBUG && defined(OMPI_BUILDING) && OMPI_BUILDING

/* It is safe to include mem/malloc.h here because a) it will only
   happen when we are building OMPI and therefore have a full OMPI
   source tree [including headers] available, and b) we guaranteed to
   *not* to include anything else via mem/malloc.h, so we won't
   have Cascading Includes Of Death. */
#include "mem/malloc.h"
#define malloc(size) ompi_malloc((size), __FILE__, __LINE__)
#define realloc(ptr, size) ompi_realloc((ptr), (size), __FILE__, __LINE__)
#define free(ptr) ompi_free((ptr), __FILE__, __LINE__)
#endif


/*
 * Do we want to override debugging controls?
 */
#if defined(OMPI_ENABLE_DEBUG_OVERRIDE) && OMPI_ENABLE_DEBUG_OVERRIDE
#undef OMPI_ENABLE_DEBUG
#define OMPI_ENABLE_DEBUG 1
#endif
