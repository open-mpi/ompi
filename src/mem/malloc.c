/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdlib.h>

#include "mem/malloc.h"
#include "util/output.h"


/*
 * Undefine "malloc" and "free"
 */

#if defined(malloc)
#undef malloc
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
int lam_malloc_debug_level = LAM_MALLOC_DEBUG_LEVEL;
int lam_malloc_output = -1;


/*
 * Private variables
 */
static lam_output_stream_t malloc_stream = {
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
void lam_malloc_init(void)
{
  lam_malloc_output = lam_output_open(&malloc_stream);
}


/*
 * Finalize the malloc debug interface
 */
void lam_malloc_finalize(void)
{
  if (-1 != lam_malloc_output) {
    lam_output_close(lam_malloc_output);
    lam_malloc_output = -1;
  }
}


/*
 * Debug version of malloc
 */
void *lam_malloc(size_t size, char *file, int line)
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


/*
 * Debug version of realloc
 */
void *lam_realloc(void *ptr, size_t size, char *file, int line)
{
    void *addr;

    if (lam_malloc_debug_level > 1) {
        if (size <= 0) {
          if (NULL == ptr) {
            lam_output(lam_malloc_output, 
                       "Realloc NULL for %ld bytes (%s, %d)", 
                       (long) size, file, line);
          } else {
            lam_output(lam_malloc_output, "Realloc %p for %ld bytes (%s, %d)", 
                       ptr, (long) size, file, line);
          }
        }
    }
    addr = realloc(ptr, size);
    if (lam_malloc_debug_level > 0) {
        if (NULL == addr) {
            lam_output(lam_malloc_output, 
                       "Realloc %p for %ld bytes failed (%s, %d)",
                       ptr, (long) size, file, line);
        }
    }

    return addr;
}


/*
 * Debug version of free
 */
void lam_free(void *addr, char *file, int line)
{
    if (lam_malloc_debug_level > 1 && NULL == addr) {
      lam_output(lam_malloc_output, "Invalid free (%s, %d)", file, line);
    } else {
      free(addr);
    }
}


