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
#include "coll_basic.h"

#include "mpi.h"
#include "include/constants.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "mca/pml/pml.h"
#include "coll_basic.h"


/*
 *	scatterv_intra
 *
 *	Function:	- scatterv operation
 *	Accepts:	- same arguments as MPI_Scatterv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_scatterv_intra(void *sbuf, int *scounts,
                                  int *disps, struct ompi_datatype_t *sdtype,
                                  void *rbuf, int rcount,
                                  struct ompi_datatype_t *rdtype, int root,
                                  struct ompi_communicator_t *comm)
{
  int i;
  int rank;
  int size;
  int err;
  char *ptmp;
  long lb;
  long extent;

  /* Initialize */

  rank = ompi_comm_rank(comm);
  size = ompi_comm_size(comm);

  /* If not root, receive data.  Note that we will only get here if
     rcount > 0 or rank == root. */

  if (rank != root) {
    err = mca_pml.pml_recv(rbuf, rcount, rdtype,
                           root, MCA_COLL_BASE_TAG_SCATTERV, 
                           comm, MPI_STATUS_IGNORE);
    return err;
  }

  /* I am the root, loop sending data. */

  err = ompi_ddt_get_extent(rdtype, &lb, &extent);
  if (OMPI_SUCCESS != err) {
    return OMPI_ERROR;
  }

  for (i = 0; i < size; ++i) {
    if (0 == scounts[i]) {
      continue;
    }
    ptmp = ((char *) sbuf) + (extent * disps[i]);

    /* simple optimization */

    if (i == rank) {
      err = ompi_ddt_sndrcv(ptmp, scounts[i], sdtype, rbuf, rcount, rdtype);
    } else {
      err = mca_pml.pml_send(ptmp, scounts[i], sdtype, i, 
                             MCA_COLL_BASE_TAG_SCATTERV, 
                             MCA_PML_BASE_SEND_STANDARD, comm);
    }
    if (MPI_SUCCESS != err) {
      return err;
    }
  }

  /* All done */

  return MPI_SUCCESS;
}


/*
 *	scatterv_inter
 *
 *	Function:	- scatterv operation
 *	Accepts:	- same arguments as MPI_Scatterv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_scatterv_inter(void *sbuf, int *scounts,
                                  int *disps, struct ompi_datatype_t *sdtype,
                                  void *rbuf, int rcount,
                                  struct ompi_datatype_t *rdtype, int root,
                                  struct ompi_communicator_t *comm)
{
  int i;
  int rank;
  int size;
  int err;
  char *ptmp;
  long lb;
  long extent;
  ompi_request_t **reqs=comm->c_coll_basic_data->mccb_reqs;

  /* Initialize */

  rank = ompi_comm_rank(comm);
  size = ompi_comm_remote_size(comm);

  /* If not root, receive data.  Note that we will only get here if
     rcount > 0 or rank == root. */

  if ( MPI_PROC_NULL == root ) {
      /* do nothing */
      err = OMPI_SUCCESS;
  }
  else if ( MPI_ROOT != root ) {
      /* If not root, receive data. */
      err = mca_pml.pml_recv(rbuf, rcount, rdtype,
                             root, MCA_COLL_BASE_TAG_SCATTERV, 
                             comm, MPI_STATUS_IGNORE);
  }
  else {
      /* I am the root, loop sending data. */
      err = ompi_ddt_get_extent(rdtype, &lb, &extent);
      if (OMPI_SUCCESS != err) {
          return OMPI_ERROR;
      }
      
      for (i = 0; i < size; ++i) {
          if (0 == scounts[i]) {
              continue;
          }

          ptmp = ((char *) sbuf) + (extent * disps[i]);
          err = mca_pml.pml_isend(ptmp, scounts[i], sdtype, i, 
                                  MCA_COLL_BASE_TAG_SCATTERV, 
                                  MCA_PML_BASE_SEND_STANDARD, comm, reqs++);
          if (MPI_SUCCESS != err) {
              return err;
          }
      }

      err = ompi_request_wait_all (size, reqs, MPI_STATUSES_IGNORE);
  }
  
  /* All done */
  return err;
}
