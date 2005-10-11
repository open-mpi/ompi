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
 *	bcast_intra_dec 
 *
 *	Function:	- seletects broadcast algorithm to use
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code (passed from the bcast implementation)
 */
int mca_coll_tuned_bcast_intra_dec_fixed(void *buff, int count,
                                   struct ompi_datatype_t *datatype, int root,
                                   struct ompi_communicator_t *comm)
{
    int i;
    int size;
    int rank;
    int err;
    int contig;
    int dsize;

    OPAL_OUTPUT((mca_coll_tuned_stream,"mca_coll_tuned_bcast_intra_dec_fixed"));

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

/*     err = mca_coll_tuned_bcast_intra_linear (buff, count, datatype, root, comm); */
/*     err = mca_coll_tuned_bcast_intra_pipeline (buff, count, datatype, root, comm, (0)); */
/*     err = mca_coll_tuned_bcast_intra_chain (buff, count, datatype, root, comm, (0), 1); */
/*     err = mca_coll_tuned_bcast_intra_bmtree (buff, count, datatype, root, comm, (8192)); */
    err = mca_coll_tuned_bcast_intra_split_bintree (buff, count, datatype, root, comm, (100));
/*     err = mca_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, (100)); */

    return err;
}


