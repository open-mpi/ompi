/*
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
 *	allgather_intra
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgather_intra(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int rcount, struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm)
{
  int size;
  int err;
  
  /* Gather and broadcast. */

  size = ompi_comm_size(comm);

  err = comm->c_coll.coll_gather(sbuf, scount, sdtype, rbuf, rcount, 
                                 rdtype, 0, comm);
  if (MPI_SUCCESS != err)
    return err;
  
  err = comm->c_coll.coll_bcast(rbuf, rcount * size, rdtype, 0, comm);
  return err;
}


/*
 *	allgather_inter
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgather_inter(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, 
                                   void *rbuf, int rcount, 
                                   struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm)
{
    int rank;
    int root=0;
    int size, rsize;
    int err;
    int i;
    char *tmpbuf=NULL, *ptmp;
    long rlb, slb, rextent, sextent;
    long incr;
    ompi_request_t *req;
    ompi_request_t **reqs = comm->c_coll_basic_data->mccb_reqs;

    rank  = ompi_comm_rank ( comm );
    size  = ompi_comm_size (comm);
    rsize = ompi_comm_remote_size (comm);
    
    /* Algorithm:
       - a gather to the root in remote group (simultaniously executed,
         thats why we cannot use coll_gather).
       - exchange the temp-results between two roots 
       - inter-bcast (again simultanious).
    */

    /* Step one: gather operations: */
    if ( rank != root ) {
        /* send your data to root */
        err = mca_pml.pml_send(sbuf, scount, sdtype, root, 
                               MCA_COLL_BASE_TAG_ALLGATHER, 
                               MCA_PML_BASE_SEND_STANDARD, comm);
        if ( OMPI_SUCCESS != err ) {
            return err;
        }
    }
    else {
        /* Do a send-recv between the two root procs. to avoid deadlock */
        err = mca_pml.pml_isend (sbuf, scount, sdtype, 0, 
                                 MCA_COLL_BASE_TAG_ALLGATHER,
                                 MCA_PML_BASE_SEND_STANDARD, 
                                 comm, &req );
        if ( OMPI_SUCCESS != err ) {
            return err;
        }

        err = mca_pml.pml_recv(rbuf, rcount, rdtype, 0,
                               MCA_COLL_BASE_TAG_ALLGATHER, comm, 
                               MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err) {
            return err;
        }
        
        err = mca_pml.pml_wait_all(1, &req, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err ) {
            return err;
        }

        /* receive a msg. from all other procs.*/
        err = ompi_ddt_get_extent(rdtype, &rlb, &rextent);
        if (OMPI_SUCCESS != err) {
            return err;
        }
        err = ompi_ddt_get_extent(sdtype, &slb, &sextent);
        if (OMPI_SUCCESS != err) {
            return err;
        }

        incr = rextent * rcount;
        ptmp = (char *) rbuf + incr;
        for (i = 1; i < rsize; ++i, ptmp += incr) {
	    err = mca_pml.pml_irecv(ptmp, rcount, rdtype, i,
                                    MCA_COLL_BASE_TAG_ALLGATHER, 
                                    comm, &reqs[i-1]);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
        
        err = mca_pml.pml_wait_all (rsize-1, reqs, MPI_STATUSES_IGNORE);
        if ( OMPI_SUCCESS != err ) {
            return err;
        }

        /* Step 2: exchange the resuts between the root processes */
        tmpbuf = (char *) malloc (scount * size *sextent);
        if ( NULL == tmpbuf ) {
            return err;
        }

        err = mca_pml.pml_isend (rbuf, rsize*rcount, rdtype, 0, 
                                 MCA_COLL_BASE_TAG_ALLGATHER,
                                 MCA_PML_BASE_SEND_STANDARD, 
                                 comm, &req );
        if ( OMPI_SUCCESS != err ) {
            goto exit;
        }

        err = mca_pml.pml_recv(tmpbuf, size *scount, sdtype, 0,
                               MCA_COLL_BASE_TAG_ALLGATHER, comm, 
                               MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }
        
        err = mca_pml.pml_wait_all(1, &req, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err ) {
            goto exit;
        }
    }

    
    /* Step 3: bcast the data to the remote group. This 
       happens in both groups simultaniously, thus we can 
       not use coll_bcast (this would deadlock). 
    */
    if ( rank != root ) {
        /* post the recv */
        err = mca_pml.pml_recv (rbuf, size*rcount, rdtype, 0,
                                MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                MPI_STATUS_IGNORE);
        if ( OMPI_SUCCESS != err ) {
            goto exit;
        }
    }
    else {
        /* Send the data to every other process in the remote group
           except to rank zero. which has it already. */
        for ( i=1; i<rsize; i++ ) {
            err = mca_pml.pml_isend(tmpbuf, size*scount, sdtype, i,
                                    MCA_COLL_BASE_TAG_ALLGATHER, 
                                    MCA_PML_BASE_SEND_STANDARD,
                                    comm, &reqs[i-1] );
            if ( OMPI_SUCCESS != err ) {
                goto exit;
            }
        }
        
        err = mca_pml.pml_wait_all (rsize-1, reqs, MPI_STATUSES_IGNORE);
        if ( OMPI_SUCCESS != err ) {
            goto exit;
        }
    }

 exit:
    if ( NULL != tmpbuf ) {
        free ( tmpbuf);
    }
    
    return err;
}
