/*
 * Copyright (c) 2003 The Trustees of Indiana University.  
 *                    All rights reserved.
 * 
 * This file is part of the CMPI software package.  For license
 * information, see the LICENSE file in the top level directory of the
 * CMPI source distribution.
 * 
 * $Id: comm_set_name.c,v 1.1 2003/11/22 16:36:26 jsquyres Exp $
 */

#include <string.h>

#include <mpi.h>


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

  ++cmpi_tv_comm_sequence_number;

  /* -- Thread safety exit -- */
  
  return MPI_SUCCESS;
}
