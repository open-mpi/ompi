/*
 * $HEADER$
 */

/** @file **/

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "util/malloc.h"
#include "util/sys_info.h"
#include "threads/mutex.h"
#include "event/event.h"


/**
 * First function that must be called in a OMPI process.
 *
 * @param argc Passed in from main()
 * @param argv Passed in from main()
 *
 * @retval OMPI_SUCCESS Upon success.
 * @retval OMPI_ERROR Upon failure.
 *
 * This is the first function that must be called for a OMPI process.
 * It sets up the following:
 *
 * - calls \c ompi_output_init(): sets up default verbose/0 stream
 *
 * - calls \c ompi_set_using_threads(): sets value to false (someone else,
 *   such as MPI_INIT, may reset this value later)
 *
 * - sets up the internal memory debugging infrastructure
 *
 * More will likely be filled in here in the future.  Note that OMPI
 * RTE MCA module setup is distinctly \em not covered in this function
 * -- not all OMPI processes need OMPI RTE MCA modules (e.g., wrapper
 * compilers).  OMPI RTE MCA modules are initialized in \c ompi_rte_init().
 *
 */
int ompi_init(int argc, char *argv[])
{
  /* Open up the output streams */

  if (!ompi_output_init())
    return OMPI_ERROR;

  /* If threads are supported - assume that we are using threads - and reset
   * otherwise.
   */

  ompi_set_using_threads(OMPI_HAVE_THREADS);

  /* For malloc debugging */

  ompi_malloc_init();

  /* Get the local system information and populate the
     ompi_system_info structure */

  ompi_sys_info();

  /* All done */

  return OMPI_SUCCESS;
}
