/*
 * $HEADER$
 */

#include "lam_config.h"

#include "lam/mem/malloc.h"
#include "lam/util/output.h"


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

/**
 * Initialize malloc debug output.
 *
 * This function is invoked to setup a dedicated output stream for
 * malloc debug functions.  It does \em not (currently) do anything
 * other than that (i.e., no internal accounting for tracking
 * malloc/free statements, etc.).
 *
 * It is invoked as part of lam_init().  Although this function is not
 * \em necessary for LAM_MALLOC() and LAM_FREE(), it is strong
 * recommended because no output messages -- regardless of the malloc
 * debug level set by lam_malloc_debug() -- will be displayed unless
 * this function is invoked first.
 */
void lam_malloc_init(void)
{
  lam_malloc_output = lam_output_open(&malloc_stream);
}


/**
 * Shut down malloc debug output.
 *
 * This function is invoked as part of lam_finalize() to shut down the
 * output stream for malloc debug messages.
 */
void lam_malloc_finalize(void)
{
  if (-1 != lam_malloc_output) {
    lam_output_close(lam_malloc_output);
    lam_malloc_output = -1;
  }
}

