/*
 * $HEADER$
 *
 * This file is included at the bottom of lam_config.h, and is
 * therefore a) after all the #define's that were output from
 * configure, and b) included in most/all files in LAM/MPI.
 *
 * Since this file is *only* ever included by lam_config.h, and
 * lam_config.h already has #ifndef/#endif protection, there is no
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
#define LAM_PATH_MAX	(PATH_MAX + 1)
#elif defined(_POSIX_PATH_MAX)
#define LAM_PATH_MAX	(_POSIX_PATH_MAX + 1)
#else
#define LAM_PATH_MAX	256
#endif

/*
 * Do we have thread support?
 */
#define LAM_HAVE_THREADS (LAM_HAVE_SOLARIS_THREADS || LAM_HAVE_POSIX_THREADS)

/*
 * Do we have <stdint.h>?
 */
#if HAVE_STDINT_H
#include <stdint.h>
#endif

/*
 * Do we want memory debugging?
 */
#if LAM_ENABLE_MEM_DEBUG && defined(LAM_BUILDING) && LAM_BUILDING

/* It is safe to include lam/mem/malloc.h here because a) it will only
   happen when we are building LAM and therefore have a full LAM
   source tree [including headers] available, and b) we guaranteed to
   *not* to include anything else via lam/mem/malloc.h, so we won't
   have Cascading Includes Of Death. */
#include "lam/mem/malloc.h"
#define malloc(size) lam_malloc((size), __FILE__, __LINE__)
#define realloc(ptr, size) lam_realloc((ptr), (size), __FILE__, __LINE__)
#define free(ptr) lam_free((ptr), __FILE__, __LINE__)
#endif


/*
 * Do we want to override debugging controls?
 */
#if defined(LAM_ENABLE_DEBUG_OVERRIDE) && LAM_ENABLE_DEBUG_OVERRIDE
#undef LAM_ENABLE_DEBUG
#define LAM_ENABLE_DEBUG 1
#endif
