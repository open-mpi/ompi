/*
 * $HEADER$
 */

#include <stdlib.h>
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include "util/output.h"
#include "errhandler/errhandler.h"
#include "errhandler/errhandler_predefined.h"
#include "communicator/communicator.h"
#include "runtime/runtime.h"


void ompi_mpi_errors_are_fatal_handler(struct ompi_communicator_t **comm,
                                       int *error_code, ...)
{
  va_list arglist;
#if __STDC__
  va_start(arglist, error_code);
#else
  va_start(arglist);
#endif
  ompi_output(0, "*** An error occurred in %s", va_arg(arglist, char *));

  if (comm != NULL && ompi_mpi_initialized && !ompi_mpi_finalized) {
    ompi_output(0, "*** on communicator %s", (*comm)->c_name);
  } else if (!ompi_mpi_initialized) {
    ompi_output(0, "*** before MPI was initialized");
  } else if (ompi_mpi_finalized) {
    ompi_output(0, "*** after MPI was finalized");
  } else if (NULL == comm) {
    ompi_output(0, "*** on a NULL communicator");
  }

  if (NULL != error_code) {
    ompi_output(0, "*** error code: %d\n", *error_code);
  }
  /* JMS: Should print the error string as well */
  ompi_output(0, "*** MPI_ERRORS_ARE_FATAL (goodbye)");
  va_end(arglist);

  /* Should we do something more intelligent here? */

  abort();
}


void ompi_mpi_errors_return_handler(struct ompi_communicator_t **comm,
                                   int *error_code, ...)
{
  /* Don't need anything more -- just need this function to exist */
}
