/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "mpi.h"
#include "include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_tuned.h"
#include "mca/pml/pml.h"
#include "opal/util/bit_ops.h"

#include "coll_tuned.h"

/*
 *	alltoall_intra_dec 
 *
 *	Function:	- seletects alltoall algorithm to use
 *	Accepts:	- same arguments as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or error code (passed from the bcast implementation)
 */

int mca_coll_tuned_alltoall_intra_dec_fixed(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm)
{
    int i;
    int size;
    int rank;
    int err;
    int contig;
    int dsize;
    MPI_Aint sext;
    long lb;

    OPAL_OUTPUT((mca_coll_tuned_stream, "mca_coll_tuned_alltoall_intra_dec_fixed"));

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* special case */
    if (size==2) {
        return mca_coll_tuned_alltoall_intra_two_procs (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }

    /* else we need data size for decision function */
    err = ompi_ddt_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { 
            OPAL_OUTPUT((mca_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,__LINE__,err,rank));
        return (err);
    }

    dsize = sext * scount * size;   /* needed for decision */

    if (size >= 12 && dsize <= 768) {
        return mca_coll_tuned_alltoall_intra_bruck (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }
    else if (dsize <= 131072) {
        return mca_coll_tuned_alltoall_intra_basic_linear (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }
    else {
        return mca_coll_tuned_alltoall_intra_pairwise (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }
}


