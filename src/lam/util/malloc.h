/*
 * $HEADER$
 */

/** @file */

#ifndef LAM_MALLOC_H
#define LAM_MALLOC_H

#include <stdlib.h>
#include "lam_config.h"

/*
 * Set LAM_MALLOC_DEBUG_LEVEL to
 * 0 for no checking
 * 1 for basic error checking
 * 2 for more error checking
 */

#ifndef LAM_MALLOC_DEBUG_LEVEL
#define LAM_MALLOC_DEBUG_LEVEL 2
#endif

static inline void *lam_malloc(size_t size, int debug_level, char *file, 
                               int line);
static inline void lam_free(void *addr, int debug_level, char *file, int line);

/**
 * Back-end error-checking malloc function for LAM (you should use the
 * LAM_MALLOC() macro instead of this function).
 *
 * @param size The number of bytes to allocate
 * @param debug_level What debug level to use (0=none, 1=some, 2=more)
 * @param file Typically the __FILE__ macro
 * @param line Typically the __LINE__ macro
 */
static inline void *lam_malloc(size_t size, int debug_level, char *file, 
                               int line)
{
    void *addr = NULL;
    if (debug_level > 1) {
        if (size <= 0) {
#if 0
          /* JMS Replace with logging output */
            lam_set_file_line(file, line);
            lam_warn("Warning: lam_malloc: Request for %ld bytes\n",
                      (long) size);
#endif
        }
    }
    addr = malloc(size);
    if (debug_level > 0) {
        if (NULL == addr) {
#if 0
          /* JMS Replace with logging output */
            lam_set_file_line(file, line);
            lam_err("Error: lam_malloc: Request for %ld bytes failed\n",
                     (long) size);
#endif
        }
    }

    return addr;
}


/**
 * Back-end error-checking free function for LAM (you should use the
 * LAM_FREE() macro instead of this function).
 *
 * @param addr Address previously returned by lam_malloc()
 * @param debug_level What debug level to use (0=none, 1=some, 2=more)
 * @param file Typically the __FILE__ macro
 * @param line Typically the __LINE__ macro
 */
static inline void lam_free(void *addr, int debug_level, char *file, int line)
{
    if (debug_level > 1 && NULL == addr) {
#if 0
          /* JMS Replace with logging output */
        lam_set_file_line(file, line);
        lam_warn("Warning: lam_free: Invalid pointer %p\n", addr);
#endif
        return;
    }
    free(addr);
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
    lam_malloc(SIZE, LAM_MALLOC_DEBUG_LEVEL, __FILE__, __LINE__)

/**
 * Error-checking free function for LAM.  
 *
 * @param ADDR Address to free.
 *
 * This macro will invoke lam_free() if compile-time debugging was
 * enabled, or just invoke free() directly if compile-time debugging
 * is disabled.
 */
#define LAM_FREE(ADDR) \
    do { \
      lam_free((ADDR), LAM_MALLOC_DEBUG_LEVEL, __FILE__, __LINE__); \
      (ADDR) = NULL; \
    } while (0)
#else
#define LAM_MALLOC(SIZE) malloc(SIZE)
#define LAM_FREE(ADDR) free(ADDR)
#endif
#endif
