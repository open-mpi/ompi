/*
 * $HEADER$
 */

#include "lam_config.h"

#include <string.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/util/strncpy.h"
#include "lam/communicator.h"
#include "lam/totalview.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Comm_set_name = MPI_Comm_set_name
#endif


int
MPI_Comm_set_name(MPI_Comm comm, char *name)
{
  if (comm == MPI_COMM_NULL) {
    /* -- Invoke error function -- */
  }
  
  if (name == NULL) {
    /* -- Invoke error function -- */
  }

  /* -- Thread safety entrance -- */

  /* Copy in the name */
  
  lam_strncpy(comm->c_name, name, MPI_MAX_OBJECT_NAME);
  comm->c_name[MPI_MAX_OBJECT_NAME - 1] = 0;

  /* -- Tracing information for new communicator name -- */
  
  /* Force TotalView DLL to take note of this name setting */

  ++lam_tv_comm_sequence_number;

  /* -- Thread safety exit -- */
  
  return MPI_SUCCESS;
}
