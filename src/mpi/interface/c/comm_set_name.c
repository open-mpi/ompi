/*
 * $HEADER$
 * 
 * $Id: comm_set_name.c,v 1.3 2004/01/07 08:08:59 jsquyres Exp $
 */

#include <string.h>

#include "include/mpi.h"
#include "include/communicator.h"
#include "include/totalview.h"


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
  
  strncpy(comm->c_name, name, MPI_MAX_OBJECT_NAME);
  comm->c_name[MPI_MAX_OBJECT_NAME - 1] = 0;

  /* -- Tracing information for new communicator name -- *
  
  /* Force TotalView DLL to take note of this name setting */

  ++lam_tv_comm_sequence_number;

  /* -- Thread safety exit -- */
  
  return MPI_SUCCESS;
}
