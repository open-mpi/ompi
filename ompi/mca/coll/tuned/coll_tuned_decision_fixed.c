/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "opal/util/bit_ops.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/op/op.h"
#include "coll_tuned.h"

/*
 * The new default fixed decision functions were generated based off of
 * results that were gathered using the ompi-collectives-tuning package.
 * These results were submitted by multiple OMPI developers on their clusters
 * and were subsequently averaged to generate the algorithm switch points
 * seen below.
 */

/*
 *  allreduce_intra
 *
 *  Function:   - allreduce using other MPI collectives
 *  Accepts:    - same as MPI_Allreduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_allreduce_intra_dec_fixed(const void *sbuf, void *rbuf, int count,
                                          struct ompi_datatype_t *dtype,
                                          struct ompi_op_t *op,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    size_t dsize, total_dsize;
    int communicator_size, alg;
    communicator_size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_allreduce_intra_dec_fixed"));

    ompi_datatype_type_size(dtype, &dsize);
    total_dsize = dsize * (ptrdiff_t)count;

    /** Algorithms:
     *  {1, "basic_linear"},
     *  {2, "nonoverlapping"},
     *  {3, "recursive_doubling"},
     *  {4, "ring"},
     *  {5, "segmented_ring"},
     *  {6, "rabenseifner"
     *
     * Currently, ring, segmented ring, and rabenseifner do not support
     * non-commutative operations.
     */
    if( !ompi_op_is_commute(op) ) {
        if (communicator_size < 4) {
            if (total_dsize < 131072) {
                alg = 3;
            } else {
                alg = 1;
            }
        } else if (communicator_size < 8) {
            alg = 3;
        } else if (communicator_size < 16) {
            if (total_dsize < 1048576) {
                alg = 3;
            } else {
                alg = 2;
            }
        } else if (communicator_size < 128) {
            alg = 3;
        } else if (communicator_size < 256) {
            if (total_dsize < 131072) {
                alg = 2;
            } else if (total_dsize < 524288) {
                alg = 3;
            } else {
                alg = 2;
            }
        } else if (communicator_size < 512) {
            if (total_dsize < 4096) {
                alg = 2;
            } else if (total_dsize < 524288) {
                alg = 3;
            } else {
                alg = 2;
            }
        } else {
            if (total_dsize < 2048) {
                alg = 2;
            } else {
                alg = 3;
            }
        }
    } else {
        if (communicator_size < 4) {
            if (total_dsize < 8) {
                alg = 4;
            } else if (total_dsize < 4096) {
                alg = 3;
            } else if (total_dsize < 8192) {
                alg = 4;
            } else if (total_dsize < 16384) {
                alg = 3;
            } else if (total_dsize < 65536) {
                alg = 4;
            } else if (total_dsize < 262144) {
                alg = 5;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 8) {
            if (total_dsize < 16) {
                alg = 4;
            } else if (total_dsize < 8192) {
                alg = 3;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 16) {
            if (total_dsize < 8192) {
                alg = 3;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 32) {
            if (total_dsize < 64) {
                alg = 5;
            } else if (total_dsize < 4096) {
                alg = 3;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 64) {
            if (total_dsize < 128) {
                alg = 5;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 128) {
            if (total_dsize < 262144) {
                alg = 3;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 256) {
            if (total_dsize < 131072) {
                alg = 2;
            } else if (total_dsize < 262144) {
                alg = 3;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 512) {
            if (total_dsize < 4096) {
                alg = 2;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 2048) {
            if (total_dsize < 2048) {
                alg = 2;
            } else if (total_dsize < 16384) {
                alg = 3;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 4096) {
            if (total_dsize < 2048) {
                alg = 2;
            } else if (total_dsize < 4096) {
                alg = 5;
            } else if (total_dsize < 16384) {
                alg = 3;
            } else {
                alg = 6;
            }
        } else {
            if (total_dsize < 2048) {
                alg = 2;
            } else if (total_dsize < 16384) {
                alg = 5;
            } else if (total_dsize < 32768) {
                alg = 3;
            } else {
                alg = 6;
            }
        }
    }

    return ompi_coll_tuned_allreduce_intra_do_this (sbuf, rbuf, count, dtype, op,
                                                    comm, module, alg, 0, 0);
}

/*
 *	alltoall_intra_dec
 *
 *	Function:	- selects alltoall algorithm to use
 *	Accepts:	- same arguments as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or error code
 */

int ompi_coll_tuned_alltoall_intra_dec_fixed(const void *sbuf, int scount,
                                             struct ompi_datatype_t *sdtype,
                                             void* rbuf, int rcount,
                                             struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module)
{
    int communicator_size, alg;
    size_t dsize, total_dsize;

    communicator_size = ompi_comm_size(comm);
    if (MPI_IN_PLACE != sbuf) {
        ompi_datatype_type_size(sdtype, &dsize);
    } else {
        ompi_datatype_type_size(rdtype, &dsize);
    }
    total_dsize = dsize * (ptrdiff_t)scount;

    /** Algorithms:
     *  {1, "linear"},
     *  {2, "pairwise"},
     *  {3, "modified_bruck"},
     *  {4, "linear_sync"},
     *  {5, "two_proc"},
     */
    if (communicator_size == 2) {
        if (total_dsize < 2) {
            alg = 2;
        } else if (total_dsize < 4) {
            alg = 5;
        } else if (total_dsize < 16) {
            alg = 2;
        } else if (total_dsize < 64) {
            alg = 5;
        } else if (total_dsize < 256) {
            alg = 2;
        } else if (total_dsize < 4096) {
            alg = 5;
        } else if (total_dsize < 32768) {
            alg = 2;
        } else if (total_dsize < 262144) {
            alg = 4;
        } else if (total_dsize < 1048576) {
            alg = 5;
        } else {
            alg = 2;
        }
    } else if (communicator_size < 8) {
        if (total_dsize < 8192) {
            alg = 4;
        } else if (total_dsize < 16384) {
            alg = 1;
        } else if (total_dsize < 65536) {
            alg = 4;
        } else if (total_dsize < 524288) {
            alg = 1;
        } else if (total_dsize < 1048576) {
            alg = 2;
        } else {
            alg = 1;
        }
    } else if (communicator_size < 16) {
        if (total_dsize < 262144) {
            alg = 4;
        } else {
            alg = 1;
        }
    } else if (communicator_size < 32) {
        if (total_dsize < 4) {
            alg = 4;
        } else if (total_dsize < 512) {
            alg = 3;
        } else if (total_dsize < 8192) {
            alg = 4;
        } else if (total_dsize < 32768) {
            alg = 1;
        } else if (total_dsize < 262144) {
            alg = 4;
        } else if (total_dsize < 524288) {
            alg = 1;
        } else {
            alg = 4;
        }
    } else if (communicator_size < 64) {
        if (total_dsize < 512) {
            alg = 3;
        } else if (total_dsize < 524288) {
            alg = 1;
        } else {
            alg = 4;
        }
    } else if (communicator_size < 128) {
        if (total_dsize < 1024) {
            alg = 3;
        } else if (total_dsize < 2048) {
            alg = 1;
        } else if (total_dsize < 4096) {
            alg = 4;
        } else if (total_dsize < 262144) {
            alg = 1;
        } else {
            alg = 2;
        }
    } else if (communicator_size < 256) {
        if (total_dsize < 1024) {
            alg = 3;
        } else if (total_dsize < 2048) {
            alg = 4;
        } else if (total_dsize < 262144) {
            alg = 1;
        } else {
            alg = 2;
        }
    } else if (communicator_size < 512) {
        if (total_dsize < 1024) {
            alg = 3;
        } else if (total_dsize < 8192) {
            alg = 4;
        } else if (total_dsize < 32768) {
            alg = 1;
        } else {
            alg = 2;
        }
    } else if (communicator_size < 1024) {
        if (total_dsize < 512) {
            alg = 3;
        } else if (total_dsize < 8192) {
            alg = 4;
        } else if (total_dsize < 16384) {
            alg = 1;
        } else if (total_dsize < 131072) {
            alg = 4;
        } else if (total_dsize < 262144) {
            alg = 1;
        } else {
            alg = 2;
        }
    } else if (communicator_size < 2048) {
        if (total_dsize < 512) {
            alg = 3;
        } else if (total_dsize < 1024) {
            alg = 4;
        } else if (total_dsize < 2048) {
            alg = 1;
        } else if (total_dsize < 16384) {
            alg = 4;
        } else if (total_dsize < 262144) {
            alg = 1;
        } else {
            alg = 4;
        }
    } else if (communicator_size < 4096) {
        if (total_dsize < 1024) {
            alg = 3;
        } else if (total_dsize < 4096) {
            alg = 4;
        } else if (total_dsize < 8192) {
            alg = 1;
        } else if (total_dsize < 131072) {
            alg = 4;
        } else {
            alg = 1;
        }
    } else {
        if (total_dsize < 2048) {
            alg = 3;
        } else if (total_dsize < 8192) {
            alg = 4;
        } else if (total_dsize < 16384) {
            alg = 1;
        } else if (total_dsize < 32768) {
            alg = 4;
        } else if (total_dsize < 65536) {
            alg = 1;
        } else {
            alg = 4;
        }
    }

    return ompi_coll_tuned_alltoall_intra_do_this (sbuf, scount, sdtype,
                                                   rbuf, rcount, rdtype,
                                                   comm, module,
                                                   alg, 0, 0, ompi_coll_tuned_alltoall_max_requests);
}

/*
 *      Function:       - selects alltoallv algorithm to use
 *      Accepts:        - same arguments as MPI_Alltoallv()
 *      Returns:        - MPI_SUCCESS or error code
 */
int ompi_coll_tuned_alltoallv_intra_dec_fixed(const void *sbuf, const int *scounts, const int *sdisps,
                                              struct ompi_datatype_t *sdtype,
                                              void *rbuf, const int *rcounts, const int *rdisps,
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    int communicator_size, alg;
    communicator_size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_alltoallv_intra_dec_fixed com_size %d",
                 communicator_size));
    /** Algorithms:
     *  {1, "basic_linear"},
     *  {2, "pairwise"},
     *
     * We can only optimize based on com size
     */
    if (communicator_size < 4) {
		alg = 2;
    } else if (communicator_size < 64) {
		alg = 1;
    } else if (communicator_size < 128) {
		alg = 2;
    } else if (communicator_size < 256) {
		alg = 1;
    } else if (communicator_size < 1024) {
		alg = 2;
    } else {
		alg = 1;
    }

    return ompi_coll_tuned_alltoallv_intra_do_this (sbuf, scounts, sdisps, sdtype,
                                                    rbuf, rcounts, rdisps, rdtype,
                                                    comm, module,
                                                    alg);
}


/*
 *	barrier_intra_dec
 *
 *	Function:	- selects barrier algorithm to use
 *	Accepts:	- same arguments as MPI_Barrier()
 *	Returns:	- MPI_SUCCESS or error code (passed from the barrier implementation)
 */
int ompi_coll_tuned_barrier_intra_dec_fixed(struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    int communicator_size, alg;
    communicator_size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_barrier_intra_dec_fixed com_size %d",
                 communicator_size));
    /** Algorithms:
     *  {1, "linear"},
     *  {2, "double_ring"},
     *  {3, "recursive_doubling"},
     *  {4, "bruck"},
     *  {5, "two_proc"},
     *  {6, "tree"},
     *
     * We can only optimize based on com size
     */
    if (communicator_size < 4) {
        alg = 3;
    } else if (communicator_size < 8) {
        alg = 1;
    } else if (communicator_size < 64) {
        alg = 3;
    } else if (communicator_size < 256) {
        alg = 4;
    } else if (communicator_size < 512) {
        alg = 6;
    } else if (communicator_size < 1024) {
        alg = 4;
    } else if (communicator_size < 4096) {
        alg = 6;
    } else {
        alg = 4;
    }

    return ompi_coll_tuned_barrier_intra_do_this (comm, module,
                                                  alg, 0, 0);
}


/*
 *	bcast_intra_dec
 *
 *	Function:	- selects broadcast algorithm to use
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code (passed from the bcast implementation)
 */
int ompi_coll_tuned_bcast_intra_dec_fixed(void *buff, int count,
                                          struct ompi_datatype_t *datatype, int root,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    size_t total_dsize, dsize;
    int communicator_size, alg;
	communicator_size = ompi_comm_size(comm);

    ompi_datatype_type_size(datatype, &dsize);
    total_dsize = dsize * (unsigned long)count;

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_bcast_intra_dec_fixed"
                 " root %d rank %d com_size %d",
                 root, ompi_comm_rank(comm), communicator_size));

    /** Algorithms:
     *  {1, "basic_linear"},
     *  {2, "chain"},
     *  {3, "pipeline"},
     *  {4, "split_binary_tree"},
     *  {5, "binary_tree"},
     *  {6, "binomial"},
     *  {7, "knomial"},
     *  {8, "scatter_allgather"},
     *  {9, "scatter_allgather_ring"},
     */
    if (communicator_size < 4) {
        if (total_dsize < 32) {
            alg = 3;
        } else if (total_dsize < 256) {
            alg = 5;
        } else if (total_dsize < 512) {
            alg = 3;
        } else if (total_dsize < 1024) {
            alg = 7;
        } else if (total_dsize < 32768) {
            alg = 1;
        } else if (total_dsize < 131072) {
            alg = 5;
        } else if (total_dsize < 262144) {
            alg = 2;
        } else if (total_dsize < 524288) {
            alg = 1;
        } else if (total_dsize < 1048576) {
            alg = 6;
        } else {
            alg = 5;
        }
    } else if (communicator_size < 8) {
        if (total_dsize < 64) {
            alg = 5;
        } else if (total_dsize < 128) {
            alg = 6;
        } else if (total_dsize < 2048) {
            alg = 5;
        } else if (total_dsize < 8192) {
            alg = 6;
        } else if (total_dsize < 1048576) {
            alg = 1;
        } else {
            alg = 2;
        }
    } else if (communicator_size < 16) {
        if (total_dsize < 8) {
            alg = 7;
        } else if (total_dsize < 64) {
            alg = 5;
        } else if (total_dsize < 4096) {
            alg = 7;
        } else if (total_dsize < 16384) {
            alg = 5;
        } else if (total_dsize < 32768) {
            alg = 6;
        } else {
            alg = 1;
        }
    } else if (communicator_size < 32) {
        if (total_dsize < 4096) {
            alg = 7;
        } else if (total_dsize < 1048576) {
            alg = 6;
        } else {
            alg = 8;
        }
    } else if (communicator_size < 64) {
        if (total_dsize < 2048) {
            alg = 6;
        } else {
            alg = 7;
        }
    } else if (communicator_size < 128) {
        alg = 7;
    } else if (communicator_size < 256) {
        if (total_dsize < 2) {
            alg = 6;
        } else if (total_dsize < 16384) {
            alg = 5;
        } else if (total_dsize < 32768) {
            alg = 1;
        } else if (total_dsize < 65536) {
            alg = 5;
        } else {
            alg = 7;
        }
    } else if (communicator_size < 1024) {
        if (total_dsize < 16384) {
            alg = 7;
        } else if (total_dsize < 32768) {
            alg = 4;
        } else {
            alg = 7;
        }
    } else if (communicator_size < 2048) {
        if (total_dsize < 524288) {
            alg = 7;
        } else {
            alg = 8;
        }
    } else if (communicator_size < 4096) {
        if (total_dsize < 262144) {
            alg = 7;
        } else {
            alg = 8;
        }
    } else {
        if (total_dsize < 8192) {
            alg = 7;
        } else if (total_dsize < 16384) {
            alg = 5;
        } else if (total_dsize < 262144) {
            alg = 7;
        } else {
            alg = 8;
        }
    }

    return ompi_coll_tuned_bcast_intra_do_this (buff, count, datatype, root,
                                                comm, module,
                                                alg, 0, 0);
}

/*
 *	reduce_intra_dec
 *
 *	Function:	- selects reduce algorithm to use
 *	Accepts:	- same arguments as MPI_reduce()
 *	Returns:	- MPI_SUCCESS or error code (passed from the reduce implementation)
 *
 */
int ompi_coll_tuned_reduce_intra_dec_fixed( const void *sendbuf, void *recvbuf,
                                            int count, struct ompi_datatype_t* datatype,
                                            struct ompi_op_t* op, int root,
                                            struct ompi_communicator_t* comm,
                                            mca_coll_base_module_t *module)
{
    int communicator_size, alg;
    size_t total_dsize, dsize;

    communicator_size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_reduce_intra_dec_fixed "
                 "root %d rank %d com_size %d", root, ompi_comm_rank(comm), communicator_size));

    ompi_datatype_type_size(datatype, &dsize);
    total_dsize = dsize * (ptrdiff_t)count;   /* needed for decision */

    /** Algorithms:
     *  {1, "linear"},
     *  {2, "chain"},
     *  {3, "pipeline"},
     *  {4, "binary"},
     *  {5, "binomial"},
     *  {6, "in-order_binary"},
     *  {7, "rabenseifner"},
     *
     * Currently, only linear and in-order binary tree algorithms are
     * capable of non commutative ops.
     */
    if( !ompi_op_is_commute(op) ) {
        if (communicator_size < 4) {
            if (total_dsize < 8) {
                alg = 6;
            } else {
                alg = 1;
            }
        } else if (communicator_size < 8) {
            alg = 1;
        } else if (communicator_size < 16) {
            if (total_dsize < 1024) {
                alg = 6;
            } else if (total_dsize < 8192) {
                alg = 1;
            } else if (total_dsize < 16384) {
                alg = 6;
            } else if (total_dsize < 262144) {
                alg = 1;
            } else {
                alg = 6;
            }
        } else if (communicator_size < 128) {
            alg = 6;
        } else if (communicator_size < 256) {
            if (total_dsize < 512) {
                alg = 6;
            } else if (total_dsize < 1024) {
                alg = 1;
            } else {
                alg = 6;
            }
        } else {
            alg = 6;
        }
    } else {
        if (communicator_size < 4) {
            if (total_dsize < 8) {
                alg = 7;
            } else if (total_dsize < 16) {
                alg = 4;
            } else if (total_dsize < 32) {
                alg = 3;
            } else if (total_dsize < 262144) {
                alg = 1;
            } else if (total_dsize < 524288) {
                alg = 3;
            } else if (total_dsize < 1048576) {
                alg = 2;
            } else {
                alg = 3;
            }
        } else if (communicator_size < 8) {
            if (total_dsize < 4096) {
                alg = 4;
            } else if (total_dsize < 65536) {
                alg = 2;
            } else if (total_dsize < 262144) {
                alg = 5;
            } else if (total_dsize < 524288) {
                alg = 1;
            } else if (total_dsize < 1048576) {
                alg = 5;
            } else {
                alg = 1;
            }
        } else if (communicator_size < 16) {
            if (total_dsize < 8192) {
                alg = 4;
            } else {
                alg = 5;
            }
        } else if (communicator_size < 32) {
            if (total_dsize < 4096) {
                alg = 4;
            } else {
                alg = 5;
            }
        } else if (communicator_size < 256) {
            alg = 5;
        } else if (communicator_size < 512) {
            if (total_dsize < 8192) {
                alg = 5;
            } else if (total_dsize < 16384) {
                alg = 6;
            } else {
                alg = 5;
            }
        } else if (communicator_size < 2048) {
            alg = 5;
        } else if (communicator_size < 4096) {
            if (total_dsize < 512) {
                alg = 5;
            } else if (total_dsize < 1024) {
                alg = 6;
            } else if (total_dsize < 8192) {
                alg = 5;
            } else if (total_dsize < 16384) {
                alg = 6;
            } else {
                alg = 5;
            }
        } else {
            if (total_dsize < 16) {
                alg = 5;
            } else if (total_dsize < 32) {
                alg = 6;
            } else if (total_dsize < 1024) {
                alg = 5;
            } else if (total_dsize < 2048) {
                alg = 6;
            } else if (total_dsize < 8192) {
                alg = 5;
            } else if (total_dsize < 16384) {
                alg = 6;
            } else {
                alg = 5;
            }
        }
    }

    return  ompi_coll_tuned_reduce_intra_do_this (sendbuf, recvbuf, count, datatype,
                                                  op, root, comm, module,
                                                  alg, 0, 0, 0);
}

/*
 *	reduce_scatter_intra_dec
 *
 *	Function:	- selects reduce_scatter algorithm to use
 *	Accepts:	- same arguments as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code (passed from
 *                        the reduce scatter implementation)
 */
int ompi_coll_tuned_reduce_scatter_intra_dec_fixed( const void *sbuf, void *rbuf,
                                                    const int *rcounts,
                                                    struct ompi_datatype_t *dtype,
                                                    struct ompi_op_t *op,
                                                    struct ompi_communicator_t *comm,
                                                    mca_coll_base_module_t *module)
{
    int communicator_size, i, alg;
    size_t total_dsize, dsize;

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_reduce_scatter_intra_dec_fixed"));

    communicator_size = ompi_comm_size(comm);
    ompi_datatype_type_size(dtype, &dsize);
    total_dsize = 0;
    for (i = 0; i < communicator_size; i++) {
        total_dsize += rcounts[i];
    }
    total_dsize *= dsize;

    /** Algorithms:
     *  {1, "non-overlapping"},
     *  {2, "recursive_halving"},
     *  {3, "ring"},
     *  {4, "butterfly"},
     *
     * Non commutative algorithm capability needs re-investigation.
     * Defaulting to non overlapping for non commutative ops.
     */
    if (!ompi_op_is_commute(op)) {
        alg = 1;
    } else {
        if (communicator_size < 4) {
            if (total_dsize < 65536) {
                alg = 3;
            } else if (total_dsize < 131072) {
                alg = 4;
            } else {
                alg = 3;
            }
        } else if (communicator_size < 8) {
            if (total_dsize < 8) {
                alg = 1;
            } else if (total_dsize < 262144) {
                alg = 2;
            } else {
                alg = 3;
            }
        } else if (communicator_size < 32) {
            if (total_dsize < 262144) {
                alg = 2;
            } else {
                alg = 3;
            }
        } else if (communicator_size < 64) {
            if (total_dsize < 64) {
                alg = 1;
            } else if (total_dsize < 2048) {
                alg = 2;
            } else if (total_dsize < 524288) {
                alg = 4;
            } else {
                alg = 3;
            }
        } else if (communicator_size < 128) {
            if (total_dsize < 256) {
                alg = 1;
            } else if (total_dsize < 512) {
                alg = 2;
            } else if (total_dsize < 2048) {
                alg = 4;
            } else if (total_dsize < 4096) {
                alg = 2;
            } else {
                alg = 4;
            }
        } else if (communicator_size < 256) {
            if (total_dsize < 256) {
                alg = 1;
            } else if (total_dsize < 512) {
                alg = 2;
            } else {
                alg = 4;
            }
        } else if (communicator_size < 512) {
            if (total_dsize < 256) {
                alg = 1;
            } else if (total_dsize < 1024) {
                alg = 2;
            } else {
                alg = 4;
            }
        } else if (communicator_size < 1024) {
            if (total_dsize < 512) {
                alg = 1;
            } else if (total_dsize < 2048) {
                alg = 2;
            } else if (total_dsize < 8192) {
                alg = 4;
            } else if (total_dsize < 16384) {
                alg = 2;
            } else {
                alg = 4;
            }
        } else if (communicator_size < 2048) {
            if (total_dsize < 512) {
                alg = 1;
            } else if (total_dsize < 4096) {
                alg = 2;
            } else if (total_dsize < 16384) {
                alg = 4;
            } else if (total_dsize < 32768) {
                alg = 2;
            } else {
                alg = 4;
            }
        } else if (communicator_size < 4096) {
            if (total_dsize < 512) {
                alg = 1;
            } else if (total_dsize < 4096) {
                alg = 2;
            } else {
                alg = 4;
            }
        } else {
            if (total_dsize < 1024) {
                alg = 1;
            } else if (total_dsize < 8192) {
                alg = 2;
            } else {
                alg = 4;
            }
        }
    }

    return  ompi_coll_tuned_reduce_scatter_intra_do_this (sbuf, rbuf, rcounts, dtype,
                                                          op, comm, module,
                                                          alg, 0, 0);
}

/*
 *	reduce_scatter_block_intra_dec
 *
 *	Function:	- selects reduce_scatter_block algorithm to use
 *	Accepts:	- same arguments as MPI_Reduce_scatter_block()
 *	Returns:	- MPI_SUCCESS or error code (passed from
 *                        the reduce scatter implementation)
 */
int ompi_coll_tuned_reduce_scatter_block_intra_dec_fixed(const void *sbuf, void *rbuf,
                                                         int rcount,
                                                         struct ompi_datatype_t *dtype,
                                                         struct ompi_op_t *op,
                                                         struct ompi_communicator_t *comm,
                                                         mca_coll_base_module_t *module)
{
    int communicator_size, alg;
    size_t dsize, total_dsize;

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_reduce_scatter_block_intra_dec_fixed"));


    ompi_datatype_type_size(dtype, &dsize);
    total_dsize = dsize * (ptrdiff_t)rcount;

    communicator_size = ompi_comm_size(comm);

    /** Algorithms:
     *  {1, "basic_linear"},
     *  {2, "recursive_doubling"},
     *  {3, "recursive_halving"},
     *  {4, "butterfly"},
     *
     * Non commutative algorithm capability needs re-investigation.
     * Defaulting to basic linear for non commutative ops.
     */
    if( !ompi_op_is_commute(op) ) {
        alg = 1;
    } else {
        if (communicator_size < 4) {
            if (total_dsize < 4) {
                alg = 2;
            } else if (total_dsize < 8) {
                alg = 4;
            } else if (total_dsize < 16) {
                alg = 3;
            } else if (total_dsize < 128) {
                alg = 4;
            } else if (total_dsize < 256) {
                alg = 3;
            } else if (total_dsize < 4096) {
                alg = 4;
            } else if (total_dsize < 8192) {
                alg = 3;
            } else if (total_dsize < 131072) {
                alg = 4;
            } else {
                alg = 1;
            }
        } else if (communicator_size < 8) {
            if (total_dsize < 8) {
                alg = 3;
            } else if (total_dsize < 32) {
                alg = 2;
            } else if (total_dsize < 256) {
                alg = 4;
            } else if (total_dsize < 8192) {
                alg = 3;
            } else if (total_dsize < 16384) {
                alg = 4;
            } else if (total_dsize < 1048576) {
                alg = 3;
            } else {
                alg = 1;
            }
        } else if (communicator_size < 16) {
            if (total_dsize < 4) {
                alg = 1;
            } else if (total_dsize < 32) {
                alg = 3;
            } else if (total_dsize < 128) {
                alg = 4;
            } else if (total_dsize < 524288) {
                alg = 3;
            } else if (total_dsize < 4194304) {
                alg = 1;
            } else {
                alg = 4;
            }
        } else if (communicator_size < 32) {
            if (total_dsize < 32) {
                alg = 1;
            } else if (total_dsize < 524288) {
                alg = 3;
            } else if (total_dsize < 2097152) {
                alg = 1;
            } else if (total_dsize < 4194304) {
                alg = 3;
            } else {
                alg = 4;
            }
        } else {
            if (total_dsize < 4) {
                alg = 3;
            } else if (total_dsize < 16) {
                alg = 1;
            } else if (total_dsize < 65536) {
                alg = 4;
            } else if (total_dsize < 262144) {
                alg = 1;
            } else {
                alg = 4;
            }
        }
    }

    return  ompi_coll_tuned_reduce_scatter_block_intra_do_this (sbuf, rbuf, rcount, dtype,
                                                                op, comm, module,
                                                                alg, 0, 0);
}

/*
 *	allgather_intra_dec
 *
 *	Function:	- selects allgather algorithm to use
 *	Accepts:	- same arguments as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code, passed from corresponding
 *                        internal allgather function.
 */

int ompi_coll_tuned_allgather_intra_dec_fixed(const void *sbuf, int scount,
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, int rcount,
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    int communicator_size, alg;
    size_t dsize, total_dsize;
    if (MPI_IN_PLACE != sbuf) {
        ompi_datatype_type_size(sdtype, &dsize);
    } else {
        ompi_datatype_type_size(rdtype, &dsize);
    }
    total_dsize = dsize * (ptrdiff_t)scount;

    communicator_size = ompi_comm_size(comm);
    /** Algorithms:
     *  {1, "linear"},
     *  {2, "bruck"},
     *  {3, "recursive_doubling"},
     *  {4, "ring"},
     *  {5, "neighbor"},
     *  {6, "two_proc"}
     */
    if (communicator_size == 2) {
        alg = 6;
    } else if (communicator_size < 32) {
        alg = 3;
    } else if (communicator_size < 64) {
        if (total_dsize < 1024) {
            alg = 3;
        } else if (total_dsize < 65536) {
            alg = 5;
        } else {
            alg = 4;
        }
    } else if (communicator_size < 128) {
        if (total_dsize < 512) {
            alg = 3;
        } else if (total_dsize < 65536) {
            alg = 5;
        } else {
            alg = 4;
        }
    } else if (communicator_size < 256) {
        if (total_dsize < 512) {
            alg = 3;
        } else if (total_dsize < 131072) {
            alg = 5;
        } else if (total_dsize < 524288) {
            alg = 4;
        } else if (total_dsize < 1048576) {
            alg = 5;
        } else {
            alg = 4;
        }
    } else if (communicator_size < 512) {
        if (total_dsize < 32) {
            alg = 3;
        } else if (total_dsize < 128) {
            alg = 2;
        } else if (total_dsize < 1024) {
            alg = 3;
        } else if (total_dsize < 131072) {
            alg = 5;
        } else if (total_dsize < 524288) {
            alg = 4;
        } else if (total_dsize < 1048576) {
            alg = 5;
        } else {
            alg = 4;
        }
    } else if (communicator_size < 1024) {
        if (total_dsize < 64) {
            alg = 3;
        } else if (total_dsize < 256) {
            alg = 2;
        } else if (total_dsize < 2048) {
            alg = 3;
        } else {
            alg = 5;
        }
    } else if (communicator_size < 2048) {
        if (total_dsize < 4) {
            alg = 3;
        } else if (total_dsize < 8) {
            alg = 2;
        } else if (total_dsize < 16) {
            alg = 3;
        } else if (total_dsize < 32) {
            alg = 2;
        } else if (total_dsize < 256) {
            alg = 3;
        } else if (total_dsize < 512) {
            alg = 2;
        } else if (total_dsize < 4096) {
            alg = 3;
        } else {
            alg = 5;
        }
    } else if (communicator_size < 4096) {
        if (total_dsize < 32) {
            alg = 2;
        } else if (total_dsize < 128) {
            alg = 3;
        } else if (total_dsize < 512) {
            alg = 2;
        } else if (total_dsize < 4096) {
            alg = 3;
        } else {
            alg = 5;
        }
    } else {
        if (total_dsize < 2) {
            alg = 3;
        } else if (total_dsize < 8) {
            alg = 2;
        } else if (total_dsize < 16) {
            alg = 3;
        } else if (total_dsize < 512) {
            alg = 2;
        } else if (total_dsize < 4096) {
            alg = 3;
        } else {
            alg = 5;
        }
    }

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_allgather_intra_dec_fixed"
                 " rank %d com_size %d", ompi_comm_rank(comm), communicator_size));

    return ompi_coll_tuned_allgather_intra_do_this(sbuf, scount, sdtype,
                                                   rbuf, rcount, rdtype,
                                                   comm, module, alg, 0, 0);
}

/*
 *	allgatherv_intra_dec
 *
 *	Function:	- selects allgatherv algorithm to use
 *	Accepts:	- same arguments as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code, passed from corresponding
 *                        internal allgatherv function.
 */

int ompi_coll_tuned_allgatherv_intra_dec_fixed(const void *sbuf, int scount,
                                               struct ompi_datatype_t *sdtype,
                                               void* rbuf, const int *rcounts,
                                               const int *rdispls,
                                               struct ompi_datatype_t *rdtype,
                                               struct ompi_communicator_t *comm,
                                               mca_coll_base_module_t *module)
{
    int communicator_size, alg, i;
    size_t dsize, total_dsize, per_rank_dsize;

    communicator_size = ompi_comm_size(comm);

    if (MPI_IN_PLACE != sbuf) {
        ompi_datatype_type_size(sdtype, &dsize);
    } else {
        ompi_datatype_type_size(rdtype, &dsize);
    }

    total_dsize = 0;
    for (i = 0; i < communicator_size; i++) { total_dsize += dsize * rcounts[i]; }

    /* use the per-rank data size as basis, similar to allgather */
    per_rank_dsize = total_dsize / communicator_size;

    /** Algorithms:
     *  {1, "default"},
     *  {2, "bruck"},
     *  {3, "ring"},
     *  {4, "neighbor"},
     *  {5, "two_proc"},
     */
    if (communicator_size == 2) {
        if (per_rank_dsize < 2048) {
            alg = 3;
        } else if (per_rank_dsize < 4096) {
            alg = 5;
        } else if (per_rank_dsize < 8192) {
            alg = 3;
        } else {
            alg = 5;
        }
    } else if (communicator_size < 8) {
        if (per_rank_dsize < 256) {
            alg = 1;
        } else if (per_rank_dsize < 4096) {
            alg = 4;
        } else if (per_rank_dsize < 8192) {
            alg = 3;
        } else if (per_rank_dsize < 16384) {
            alg = 4;
        } else if (per_rank_dsize < 262144) {
            alg = 2;
        } else {
            alg = 4;
        }
    } else if (communicator_size < 16) {
        if (per_rank_dsize < 1024) {
            alg = 1;
        } else {
            alg = 2;
        }
    } else if (communicator_size < 32) {
        if (per_rank_dsize < 128) {
            alg = 1;
        } else if (per_rank_dsize < 262144) {
            alg = 2;
        } else {
            alg = 3;
        }
    } else if (communicator_size < 64) {
        if (per_rank_dsize < 256) {
            alg = 1;
        } else if (per_rank_dsize < 8192) {
            alg = 2;
        } else {
            alg = 3;
        }
    } else if (communicator_size < 128) {
        if (per_rank_dsize < 256) {
            alg = 1;
        } else if (per_rank_dsize < 4096) {
            alg = 2;
        } else {
            alg = 3;
        }
    } else if (communicator_size < 256) {
        if (per_rank_dsize < 1024) {
            alg = 2;
        } else if (per_rank_dsize < 65536) {
            alg = 4;
        } else {
            alg = 3;
        }
    } else if (communicator_size < 512) {
        if (per_rank_dsize < 1024) {
            alg = 2;
        } else {
            alg = 3;
        }
    } else if (communicator_size < 1024) {
        if (per_rank_dsize < 512) {
            alg = 2;
        } else if (per_rank_dsize < 1024) {
            alg = 1;
        } else if (per_rank_dsize < 4096) {
            alg = 2;
        } else if (per_rank_dsize < 1048576) {
            alg = 4;
        } else {
            alg = 3;
        }
    } else {
        if (per_rank_dsize < 4096) {
            alg = 2;
        } else {
            alg = 4;
        }
    }

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_allgatherv_intra_dec_fixed"
                 " rank %d com_size %d", ompi_comm_rank(comm), communicator_size));

    return ompi_coll_tuned_allgatherv_intra_do_this (sbuf, scount, sdtype,
                                                     rbuf, rcounts,
                                                     rdispls, rdtype,
                                                     comm, module,
                                                     alg, 0, 0);
}

/*
 *	gather_intra_dec
 *
 *	Function:	- selects gather algorithm to use
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code, passed from corresponding
 *                        internal allgather function.
 */

int ompi_coll_tuned_gather_intra_dec_fixed(const void *sbuf, int scount,
                                           struct ompi_datatype_t *sdtype,
                                           void* rbuf, int rcount,
                                           struct ompi_datatype_t *rdtype,
                                           int root,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module)
{
    int communicator_size, alg, rank;
    size_t dsize, total_dsize;

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_gather_intra_dec_fixed"));

    communicator_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* Determine block size */
    if ( (rank == root) || (MPI_IN_PLACE == sbuf) ) {
        ompi_datatype_type_size(rdtype, &dsize);
        total_dsize = dsize * (ptrdiff_t)rcount;
    } else {
        ompi_datatype_type_size(sdtype, &dsize);
        total_dsize = dsize * (ptrdiff_t)scount;
    }

    /** Algorithms:
     *  {1, "basic_linear"},
     *  {2, "binomial"},
     *  {3, "linear_sync"},
     *
     * We do not make any rank specific checks since the params
     * should be uniform across ranks.
     */
    if (communicator_size < 4) {
        if (total_dsize < 2) {
            alg = 3;
        } else if (total_dsize < 4) {
            alg = 1;
        } else if (total_dsize < 32768) {
            alg = 2;
        } else if (total_dsize < 65536) {
            alg = 1;
        } else if (total_dsize < 131072) {
            alg = 2;
        } else {
            alg = 3;
        }
    } else if (communicator_size < 8) {
        if (total_dsize < 1024) {
            alg = 2;
        } else if (total_dsize < 8192) {
            alg = 1;
        } else if (total_dsize < 32768) {
            alg = 2;
        } else if (total_dsize < 262144) {
            alg = 1;
        } else {
            alg = 3;
        }
    } else if (communicator_size < 256) {
        alg = 2;
    } else if (communicator_size < 512) {
        if (total_dsize < 2048) {
            alg = 2;
        } else if (total_dsize < 8192) {
            alg = 1;
        } else {
            alg = 2;
        }
    } else {
        alg = 2;
    }

    return ompi_coll_tuned_gather_intra_do_this (sbuf, scount, sdtype,
                                                 rbuf, rcount, rdtype,
                                                 root, comm, module,
                                                 alg, 0, 0);
}

/*
 *	scatter_intra_dec
 *
 *	Function:	- selects scatter algorithm to use
 *	Accepts:	- same arguments as MPI_Scatter()
 *	Returns:	- MPI_SUCCESS or error code, passed from corresponding
 *                        internal allgather function.
 */

int ompi_coll_tuned_scatter_intra_dec_fixed(const void *sbuf, int scount,
                                            struct ompi_datatype_t *sdtype,
                                            void* rbuf, int rcount,
                                            struct ompi_datatype_t *rdtype,
                                            int root, struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    int communicator_size, alg, rank;
    size_t dsize, total_dsize;

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_scatter_intra_dec_fixed"));

    communicator_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (root == rank) {
        ompi_datatype_type_size(sdtype, &dsize);
        total_dsize = dsize * (ptrdiff_t)scount;
    } else {
        ompi_datatype_type_size(rdtype, &dsize);
        total_dsize = dsize * (ptrdiff_t)rcount;
    }

    /** Algorithms:
     *  {1, "basic_linear"},
     *  {2, "binomial"},
     *  {3, "linear_nb"},
     *
     * We do not make any rank specific checks since the params
     * should be uniform across ranks.
     */
    if (communicator_size < 4) {
        if (total_dsize < 2) {
            alg = 3;
        } else if (total_dsize < 131072) {
            alg = 1;
        } else if (total_dsize < 262144) {
            alg = 3;
        } else {
            alg = 1;
        }
    } else if (communicator_size < 8) {
        if (total_dsize < 2048) {
            alg = 2;
        } else if (total_dsize < 4096) {
            alg = 1;
        } else if (total_dsize < 8192) {
            alg = 2;
        } else if (total_dsize < 32768) {
            alg = 1;
        } else if (total_dsize < 1048576) {
            alg = 3;
        } else {
            alg = 1;
        }
    } else if (communicator_size < 16) {
        if (total_dsize < 16384) {
            alg = 2;
        } else if (total_dsize < 1048576) {
            alg = 3;
        } else {
            alg = 1;
        }
    } else if (communicator_size < 32) {
        if (total_dsize < 16384) {
            alg = 2;
        } else if (total_dsize < 32768) {
            alg = 1;
        } else {
            alg = 3;
        }
    } else if (communicator_size < 64) {
        if (total_dsize < 512) {
            alg = 2;
        } else if (total_dsize < 8192) {
            alg = 3;
        } else if (total_dsize < 16384) {
            alg = 2;
        } else {
            alg = 3;
        }
    } else {
        if (total_dsize < 512) {
            alg = 2;
        } else {
            alg = 3;
        }
    }

    return ompi_coll_tuned_scatter_intra_do_this (sbuf, scount, sdtype,
                                                  rbuf, rcount, rdtype,
                                                  root, comm, module,
                                                  alg, 0, 0);
}
