/*
 * $HEADER$
 */

/** @file **/

#include "lam_config.h"

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "lam/mem/malloc.h"
#include "lam/util/output.h"
#include "lam/threads/mutex.h"

/**
 * First function that must be called in a LAM process.
 *
 * @param argc Passed in from main()
 * @param argv Passed in from main()
 *
 * @retval LAM_SUCCESS Upon success.
 * @retval LAM_ERROR Upon failure.
 *
 * This is the first function that must be called for a LAM process.
 * It sets up the following:
 *
 * - calls lam_output_init(): sets up default verbose/0 stream
 *
 * - calls lam_set_using_threads(): sets value to false (someone else,
 *   such as MPI_INIT, may reset this value later)
 *
 * More will likely be filled in here in the future.  Note that LAM
 * RTE MCA module setup is distinctly \em not covered in this function
 * -- not all LAM processes need LAM RTE MCA modules (e.g., wrapper
 * compilers).  LAM RTE MCA modules are initialized in lam_rte_init().
 */
int lam_init(int argc, char *argv[])
{
  /* Open up the output streams */

  if (!lam_output_init())
    return LAM_ERROR;

  /* For the moment, the LAM library is not multi-threaded.  MPI_INIT
     may reset this value later, but for now, we say that we are not
     using threads. */

  lam_set_using_threads(false);

  /* For malloc debugging */

  lam_malloc_init();

  /* Other things that we'll probably need:

     - session directory setup
     - ...?
  */

  /* All done */

  return LAM_SUCCESS;
}
