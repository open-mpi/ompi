/*
 * $HEADER$
 */

/** @file */

#ifndef LAM_MALLOC_H
#define LAM_MALLOC_H

#include "lam_config.h"

#include <stdlib.h>

#include "lam/util/output.h"


/*
 * Set LAM_MALLOC_DEBUG_LEVEL to
 * 0 for no checking
 * 1 for basic error checking
 * 2 for more error checking
 */

#ifndef LAM_MALLOC_DEBUG_LEVEL
#define LAM_MALLOC_DEBUG_LEVEL 2
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  void lam_malloc_init(void);
  void lam_malloc_finalize(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

extern int lam_malloc_debug_level;
extern int lam_malloc_output;

static inline void lam_malloc_debug(int level);
static inline void *lam_malloc(size_t size, char *file, int line);
static inline void lam_free(void *addr, char *file, int line);


/**
 * Used to set the debug level for malloc debug.
 *
 * @param level The level of debugging (0 = none, 1 = some, 2 = more)
 *
 * This value defaults to the LAM_MALLOC_DEBUG_LEVEL.
 */
static inline void lam_malloc_debug(int level)
{
  lam_malloc_debug_level = level;
}

/**
 * \internal
 *
 * Back-end error-checking malloc function for LAM (you should use the
 * LAM_MALLOC() macro instead of this function).
 *
 * @param size The number of bytes to allocate
 * @param file Typically the __FILE__ macro
 * @param line Typically the __LINE__ macro
 */
static inline void *lam_malloc(size_t size, char *file, int line)
{
    void *addr;
    if (lam_malloc_debug_level > 1) {
        if (size <= 0) {
          lam_output(lam_malloc_output, "Request for %ld bytes (%s, %d)", 
                     (long) size, file, line);
        }
    }
    addr = malloc(size);
    if (lam_malloc_debug_level > 0) {
        if (NULL == addr) {
            lam_output(lam_malloc_output, 
                       "Request for %ld bytes failed (%s, %d)",
                       (long) size, file, line);
        }
    }

    return addr;
}


/**
 * \internal
 *
 * Back-end error-checking free function for LAM (you should use the
 * LAM_FREE() macro instead of this function).
 *
 * @param addr Address on the heap to free()
 * @param file Typically the __FILE__ macro
 * @param line Typically the __LINE__ macro
 */
#include <stdio.h>
static inline void lam_free(void *addr, char *file, int line)
{
    if (lam_malloc_debug_level > 1 && NULL == addr) {
      printf("INVALID FREE!\n");
        lam_output(lam_malloc_output, "Invalid free (%s, %d)", file, line);
    } else {
      free(addr);
    }
}

#if LAM_MALLOC_DEBUG_LEVEL > 0

/**
 * Error-checking malloc function for LAM.  
 *
 * @param SIZE Number of bytes to allocate.
 *
 * This macro will invoke lam_malloc() if compile-time debugging was
 * enabled, or just invoke malloc() directly if compile-time debugging
 * is disabled.
 */
#define LAM_MALLOC(SIZE) \
    lam_malloc(SIZE, __FILE__, __LINE__)

/**
 * Error-checking free function for LAM.  
 *
 * @param ADDR Address to free.
 *
 * This macro will invoke lam_free() if compile-time debugging was
 * enabled, or just invoke free() directly if compile-time debugging
 * is disabled.
 *
 * The memory to be freed can be allocated from anywhere (e.g.,
 * strdup()) -- it does not have to be allocated by LAM_MALLOC().
 */
#define LAM_FREE(ADDR) \
    do { \
      lam_free((ADDR), __FILE__, __LINE__); \
      (ADDR) = NULL; \
    } while (0)
#else
#define LAM_MALLOC(SIZE) malloc(SIZE)
#define LAM_FREE(ADDR) free(ADDR)
#endif
#endif
