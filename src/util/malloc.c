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
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "util/malloc.h"
#include "util/output.h"


/*
 * Undefine "malloc" and "free"
 */

#if defined(malloc)
#undef malloc
#endif
#if defined(calloc)
#undef calloc
#endif
#if defined(free)
#undef free
#endif
#if defined(realloc)
#undef realloc
#endif

/*
 * Public variables
 */
int ompi_malloc_debug_level = OMPI_MALLOC_DEBUG_LEVEL;
int ompi_malloc_output = -1;


/*
 * Private variables
 */
static ompi_output_stream_t malloc_stream = {
  /* debugging */
  true,
  /* verbose level */
  5, 
  /* syslog */
  false, 0, NULL,
  /* prefix */
  "malloc_debug: ",
  /* stdout */
  false,
  /* stderr */
  true,
  /* file */
  false, false, NULL
};

/*
 * Initialize the malloc debug interface
 */
void ompi_malloc_init(void)
{
  ompi_malloc_output = ompi_output_open(&malloc_stream);
}


/*
 * Finalize the malloc debug interface
 */
void ompi_malloc_finalize(void)
{
  if (-1 != ompi_malloc_output) {
    ompi_output_close(ompi_malloc_output);
    ompi_malloc_output = -1;
  }
}


/*
 * Debug version of malloc
 */
void *ompi_malloc(size_t size, char *file, int line)
{
    void *addr;
    if (ompi_malloc_debug_level > 1) {
        if (size <= 0) {
          ompi_output(ompi_malloc_output, "Request for %ld bytes (%s, %d)", 
                     (long) size, file, line);
        }
    }
    addr = malloc(size);
    if (ompi_malloc_debug_level > 0) {
        if (NULL == addr) {
            ompi_output(ompi_malloc_output, 
                       "Request for %ld bytes failed (%s, %d)",
                       (long) size, file, line);
        }
    }

    return addr;
}


/*
 * Debug version of calloc
 */
void *ompi_calloc(size_t nmembers, size_t size, char *file, int line)
{
    void *addr;
    if (ompi_malloc_debug_level > 1) {
        if (size <= 0) {
          ompi_output(ompi_malloc_output,
                     "Request for %ld zeroed elements of size %ld (%s, %d)", 
                     (long) nmembers, (long) size, file, line);
        }
    }
    addr = calloc(nmembers, size);
    if (ompi_malloc_debug_level > 0) {
        if (NULL == addr) {
            ompi_output(ompi_malloc_output, 
                       "Request for %ld zeroed elements of size %ld failed (%s, %d)",
                       (long) nmembers, (long) size, file, line);
        }
    }

    return addr;
}


/*
 * Debug version of realloc
 */
void *ompi_realloc(void *ptr, size_t size, char *file, int line)
{
    void *addr;

    if (ompi_malloc_debug_level > 1) {
        if (size <= 0) {
          if (NULL == ptr) {
            ompi_output(ompi_malloc_output, 
                       "Realloc NULL for %ld bytes (%s, %d)", 
                       (long) size, file, line);
          } else {
            ompi_output(ompi_malloc_output, "Realloc %p for %ld bytes (%s, %d)", 
                       ptr, (long) size, file, line);
          }
        }
    }
    addr = realloc(ptr, size);
    if (ompi_malloc_debug_level > 0) {
        if (NULL == addr) {
            ompi_output(ompi_malloc_output, 
                       "Realloc %p for %ld bytes failed (%s, %d)",
                       ptr, (long) size, file, line);
        }
    }

    return addr;
}


/*
 * Debug version of free
 */
void ompi_free(void *addr, char *file, int line)
{
    if (ompi_malloc_debug_level > 1 && NULL == addr) {
      ompi_output(ompi_malloc_output, "Invalid free (%s, %d)", file, line);
    } else {
      free(addr);
    }
}


