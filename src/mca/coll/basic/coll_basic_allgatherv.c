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
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	allgatherv_intra
 *
 *	Function:	- allgatherv using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgatherv_intra(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void * rbuf, int *rcounts, int *disps, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm)
{
  int i, size;
  int err;

  /* Collect all values at each process, one at a time. */

  size = ompi_comm_size(comm);
  for (i = 0; i < size; ++i) {
    err = comm->c_coll.coll_gatherv(sbuf, scount, sdtype, rbuf,
                                    rcounts, disps, rdtype, i, comm);
    if (MPI_SUCCESS != err) {
      return err;
    }
  }

  return MPI_SUCCESS;
}


/*
 *	allgatherv_inter
 *
 *	Function:	- allgatherv using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgatherv_inter(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void * rbuf, int *rcounts, int *disps, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm)
{
    int size, rsize;
    int err, i;
    int *scounts=NULL;
    int *sdisps=NULL;
    
    rsize = ompi_comm_remote_size (comm);
    size  = ompi_comm_size (comm);

    scounts = (int *) malloc (rsize * sizeof(int) );
    sdisps  = (int *) calloc (rsize, sizeof(int));
    if ( NULL == scounts || NULL == sdisps ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    for ( i=0; i<rsize; i++) {
        scounts[i] = scount;    
    }

    err = comm->c_coll.coll_alltoallv (sbuf, scounts, sdisps, sdtype,
                                       rbuf, rcounts, disps, rdtype,
                                       comm );

    if (NULL != sdisps  ) {
        free (sdisps);
    }
    if ( NULL != scounts ) {
        free (scounts);
    }
    
    return err;
}
