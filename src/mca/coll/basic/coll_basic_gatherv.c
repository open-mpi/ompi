/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "coll_basic.h"


/*
 *	gatherv_intra
 *
 *	Function:	- basic gatherv operation
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_gatherv_intra(void *sbuf, int scount, 
                                 struct ompi_datatype_t *sdtype,
                                 void *rbuf, int *rcounts, int *disps,
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

  size = ompi_comm_size(comm);
  rank = ompi_comm_rank(comm);

  /* Need this test here because this function is invoked by
     allgatherv in addition to the top-level MPI_Gatherv */

  if (0 == scount && rank != root) {
      return MPI_SUCCESS;
  }

  /* Everyone but root sends data and returns.  Note that we will only
     get here if scount > 0 or rank == root. */

  if (rank != root) {
    err = mca_pml.pml_send(sbuf, scount, sdtype, root,
                           MCA_COLL_BASE_TAG_GATHERV, 
                           MCA_PML_BASE_SEND_STANDARD, comm);
    return err;
  }

  /* I am the root, loop receiving data. */

  err = ompi_ddt_get_extent(rdtype, &lb, &extent);
  if (OMPI_SUCCESS != err) {
    return OMPI_ERROR;
  }

  for (i = 0; i < size; ++i) {
    ptmp = ((char *) rbuf) + (extent * disps[i]);
    if (0 == rcounts[i]) {
        continue;
    }

    /* simple optimization */

    if (i == rank) {
      err = ompi_ddt_sndrcv(sbuf, scount, sdtype,
                           ptmp, rcounts[i], rdtype, 
                           MCA_COLL_BASE_TAG_GATHERV, comm);
    } else {
      err = mca_pml.pml_recv(ptmp, rcounts[i], rdtype, i,
                             MCA_COLL_BASE_TAG_GATHERV, 
                             comm, MPI_STATUS_IGNORE);
    }

    if (MPI_SUCCESS != err) {
      return err;
    }
  }

  /* All done */

  return MPI_SUCCESS;
}


/*
 *	gatherv_inter
 *
 *	Function:	- basic gatherv operation
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_gatherv_inter(void *sbuf, int scount,
                                 struct ompi_datatype_t *sdtype,
                                 void *rbuf, int *rcounts, int *disps,
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
  ompi_request_t **reqs= comm->c_coll_basic_data->mccb_reqs;

  size = ompi_comm_remote_size(comm);
  rank = ompi_comm_rank(comm);

  /* If not root, receive data.  Note that we will only get here if
     scount > 0 or rank == root. */

  if ( MPI_PROC_NULL == root ) {
      /* do nothing */
      err = OMPI_SUCCESS;
  }
  else if ( MPI_ROOT != root ) {
      /* Everyone but root sends data and returns. */
      err = mca_pml.pml_send(sbuf, scount, sdtype, root,
                             MCA_COLL_BASE_TAG_GATHERV, 
                             MCA_PML_BASE_SEND_STANDARD, comm);
  }
  else {
      /* I am the root, loop receiving data. */
      err = ompi_ddt_get_extent(rdtype, &lb, &extent);
      if (OMPI_SUCCESS != err) {
          return OMPI_ERROR;
      }

      for (i = 0; i < size; ++i) {
          if (0 == rcounts[i]) {
              continue;
          }

          ptmp = ((char *) rbuf) + (extent * disps[i]);
          err = mca_pml.pml_irecv(ptmp, rcounts[i], rdtype, i,
                                  MCA_COLL_BASE_TAG_GATHERV, 
                                  comm, &reqs[i]);
          if (OMPI_SUCCESS != err) {
              return err;
          }
      }
      
      err = ompi_request_wait_all (size, reqs, MPI_STATUSES_IGNORE);
  }
  
  /* All done */
  return err;
}
