/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_BASE_TAGS_H
#define MCA_COLL_BASE_TAGS_H

/*
 * Tags that can be used for MPI point-to-point functions when
 * implementing collectives via point-to-point.
 */

/* a set of special tags: */
/*  to recognize an MPI_Comm_join in the comm_connect_accept routine. */
#define OMPI_COMM_ALLGATHER_TAG -7
#define OMPI_COMM_BARRIER_TAG   -8
#define OMPI_COMM_ALLREDUCE_TAG -9

#define MCA_COLL_BASE_TAG_BLOCKING_BASE -7
#define MCA_COLL_BASE_TAG_ALLGATHER -10
#define MCA_COLL_BASE_TAG_ALLGATHERV -11
#define MCA_COLL_BASE_TAG_ALLREDUCE -12
#define MCA_COLL_BASE_TAG_ALLTOALL -13
#define MCA_COLL_BASE_TAG_ALLTOALLV -14
#define MCA_COLL_BASE_TAG_ALLTOALLW -15
#define MCA_COLL_BASE_TAG_BARRIER -16
#define MCA_COLL_BASE_TAG_BCAST -17
#define MCA_COLL_BASE_TAG_EXSCAN -18
#define MCA_COLL_BASE_TAG_GATHER -19
#define MCA_COLL_BASE_TAG_GATHERV -20
#define MCA_COLL_BASE_TAG_REDUCE -21
#define MCA_COLL_BASE_TAG_REDUCE_SCATTER -22
#define MCA_COLL_BASE_TAG_REDUCE_SCATTER_BLOCK -23
#define MCA_COLL_BASE_TAG_SCAN -24
#define MCA_COLL_BASE_TAG_SCATTER -25
#define MCA_COLL_BASE_TAG_SCATTERV -26
#define MCA_COLL_BASE_TAG_BLOCKING_END -26

/* not #if conditional on OPAL_ENABLE_FT_MPI for ABI */
#define MCA_COLL_BASE_TAG_FT_BASE                (MCA_COLL_BASE_TAG_BLOCKING_END - 1)
#define MCA_COLL_BASE_TAG_SHRINK                 (MCA_COLL_BASE_TAG_FT_BASE - 1)
#define MCA_COLL_BASE_TAG_AGREEMENT              (MCA_COLL_BASE_TAG_FT_BASE - 2)
/* one extra reserved to avoid revoke for normal reqs, see request/req_ft.c*/
#define MCA_COLL_BASE_TAG_FT_END                 (MCA_COLL_BASE_TAG_FT_BASE - 3)

#define MCA_COLL_BASE_TAG_UCC                    (MCA_COLL_BASE_TAG_FT_END - 1)

#define MCA_COLL_BASE_TAG_STATIC_END             (MCA_COLL_BASE_TAG_UCC - 1)



#define MCA_COLL_BASE_TAG_NONBLOCKING_BASE (MCA_COLL_BASE_TAG_STATIC_END - 1)
#define MCA_COLL_BASE_TAG_NONBLOCKING_END ((-1 * INT_MAX/2) + 1)
#define MCA_COLL_BASE_TAG_NEIGHBOR_BASE  (MCA_COLL_BASE_TAG_NONBLOCKING_END - 1)
#define MCA_COLL_BASE_TAG_NEIGHBOR_END   (MCA_COLL_BASE_TAG_NEIGHBOR_BASE - 1024)
#define MCA_COLL_BASE_TAG_HCOLL_BASE (-1 * INT_MAX/2)
#define MCA_COLL_BASE_TAG_HCOLL_END (-1 * INT_MAX)

#define MCA_COLL_BASE_TAG_BASE MCA_COLL_BASE_TAG_BLOCKING_BASE
#define MCA_COLL_BASE_TAG_END  MCA_COLL_BASE_TAG_HCOLL_END

#endif /* MCA_COLL_BASE_TAGS_H */
