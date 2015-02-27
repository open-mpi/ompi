/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_BASE_EXPORT_H
#define MCA_COLL_BASE_EXPORT_H

#include "ompi_config.h"

#include "ompi/mca/coll/base/base.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "coll_base_topo.h"

/* some fixed value index vars to simplify certain operations */
typedef enum COLLTYPE {
    ALLGATHER = 0,  /*  0 */
    ALLGATHERV,     /*  1 */
    ALLREDUCE,      /*  2 */
    ALLTOALL,       /*  3 */
    ALLTOALLV,      /*  4 */
    ALLTOALLW,      /*  5 */
    BARRIER,        /*  6 */
    BCAST,          /*  7 */
    EXSCAN,         /*  8 */
    GATHER,         /*  9 */
    GATHERV,        /* 10 */
    REDUCE,         /* 11 */
    REDUCESCATTER,  /* 12 */
    SCAN,           /* 13 */
    SCATTER,        /* 14 */
    SCATTERV,       /* 15 */
    COLLCOUNT       /* 16 end counter keep it as last element */
} COLLTYPE_T;

/* defined arg lists to simply auto inclusion of user overriding decision functions */
#define ALLGATHER_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define ALLGATHERV_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void * rbuf, int *rcounts, int *disps, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define ALLREDUCE_ARGS void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, struct ompi_op_t *op, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define ALLTOALL_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void* rbuf, int rcount, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define ALLTOALLV_ARGS void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t *sdtype, void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define ALLTOALLW_ARGS void *sbuf, int *scounts, int *sdisps,  struct ompi_datatype_t **sdtypes, void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t **rdtypes, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define BARRIER_ARGS struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define BCAST_ARGS void *buff, int count, struct ompi_datatype_t *datatype, int root, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define EXSCAN_ARGS void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, struct ompi_op_t *op, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define GATHER_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount, struct ompi_datatype_t *rdtype, int root, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define GATHERV_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf, int *rcounts, int *disps, struct ompi_datatype_t *rdtype, int root, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define REDUCE_ARGS void *sbuf, void* rbuf, int count, struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define REDUCESCATTER_ARGS void *sbuf, void *rbuf, int *rcounts, struct ompi_datatype_t *dtype, struct ompi_op_t *op, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define SCAN_ARGS void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,  struct ompi_op_t *op, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define SCATTER_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount, struct ompi_datatype_t *rdtype, int root, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
#define SCATTERV_ARGS void *sbuf, int *scounts, int *disps, struct ompi_datatype_t *sdtype, void* rbuf, int rcount, struct ompi_datatype_t *rdtype, int root, struct ompi_communicator_t *comm, mca_coll_base_module_t *module
/* end defined arg lists to simply auto inclusion of user overriding decision functions */

BEGIN_C_DECLS

/* All Gather */
int ompi_coll_base_allgather_intra_bruck(ALLGATHER_ARGS);
int ompi_coll_base_allgather_intra_recursivedoubling(ALLGATHER_ARGS);
int ompi_coll_base_allgather_intra_ring(ALLGATHER_ARGS);
int ompi_coll_base_allgather_intra_neighborexchange(ALLGATHER_ARGS);
int ompi_coll_base_allgather_intra_basic_linear(ALLGATHER_ARGS);
int ompi_coll_base_allgather_intra_two_procs(ALLGATHER_ARGS);

/* All GatherV */
int ompi_coll_base_allgatherv_intra_bruck(ALLGATHERV_ARGS);
int ompi_coll_base_allgatherv_intra_ring(ALLGATHERV_ARGS);
int ompi_coll_base_allgatherv_intra_neighborexchange(ALLGATHERV_ARGS);
int ompi_coll_base_allgatherv_intra_basic_default(ALLGATHERV_ARGS);
int ompi_coll_base_allgatherv_intra_two_procs(ALLGATHERV_ARGS);

/* All Reduce */
int ompi_coll_base_allreduce_intra_nonoverlapping(ALLREDUCE_ARGS);
int ompi_coll_base_allreduce_intra_recursivedoubling(ALLREDUCE_ARGS);
int ompi_coll_base_allreduce_intra_ring(ALLREDUCE_ARGS);
int ompi_coll_base_allreduce_intra_ring_segmented(ALLREDUCE_ARGS, uint32_t segsize);
int ompi_coll_base_allreduce_intra_basic_linear(ALLREDUCE_ARGS);

/* AlltoAll */
int ompi_coll_base_alltoall_intra_pairwise(ALLTOALL_ARGS);
int ompi_coll_base_alltoall_intra_bruck(ALLTOALL_ARGS);
int ompi_coll_base_alltoall_intra_basic_linear(ALLTOALL_ARGS);
int ompi_coll_base_alltoall_intra_linear_sync(ALLTOALL_ARGS, int max_requests);
int ompi_coll_base_alltoall_intra_two_procs(ALLTOALL_ARGS);
int mca_coll_base_alltoall_intra_basic_inplace(void *rbuf, int rcount,
                                               struct ompi_datatype_t *rdtype,
                                               struct ompi_communicator_t *comm,
                                               mca_coll_base_module_t *module);  /* special version for INPLACE */

/* AlltoAllV */
int ompi_coll_base_alltoallv_intra_pairwise(ALLTOALLV_ARGS);
int ompi_coll_base_alltoallv_intra_basic_linear(ALLTOALLV_ARGS);
int mca_coll_base_alltoallv_intra_basic_inplace(void *rbuf, const int *rcounts, const int *rdisps,
                                                struct ompi_datatype_t *rdtype,
                                                struct ompi_communicator_t *comm,
                                                mca_coll_base_module_t *module);  /* special version for INPLACE */

/* AlltoAllW */

/* Barrier */
int ompi_coll_base_barrier_intra_doublering(BARRIER_ARGS);
int ompi_coll_base_barrier_intra_recursivedoubling(BARRIER_ARGS);
int ompi_coll_base_barrier_intra_bruck(BARRIER_ARGS);
int ompi_coll_base_barrier_intra_two_procs(BARRIER_ARGS);
int ompi_coll_base_barrier_intra_tree(BARRIER_ARGS);
int ompi_coll_base_barrier_intra_basic_linear(BARRIER_ARGS);

/* Bcast */
int ompi_coll_base_bcast_intra_generic(BCAST_ARGS, uint32_t count_by_segment, ompi_coll_tree_t* tree);
int ompi_coll_base_bcast_intra_basic_linear(BCAST_ARGS);
int ompi_coll_base_bcast_intra_chain(BCAST_ARGS, uint32_t segsize, int32_t chains);
int ompi_coll_base_bcast_intra_pipeline(BCAST_ARGS, uint32_t segsize);
int ompi_coll_base_bcast_intra_binomial(BCAST_ARGS, uint32_t segsize);
int ompi_coll_base_bcast_intra_bintree(BCAST_ARGS, uint32_t segsize);
int ompi_coll_base_bcast_intra_split_bintree(BCAST_ARGS, uint32_t segsize);

/* Exscan */

/* Gather */
int ompi_coll_base_gather_intra_basic_linear(GATHER_ARGS);
int ompi_coll_base_gather_intra_binomial(GATHER_ARGS);
int ompi_coll_base_gather_intra_linear_sync(GATHER_ARGS, int first_segment_size);

/* GatherV */

/* Reduce */
int ompi_coll_base_reduce_generic(REDUCE_ARGS, ompi_coll_tree_t* tree, int count_by_segment, int max_outstanding_reqs);
int ompi_coll_base_reduce_intra_basic_linear(REDUCE_ARGS);
int ompi_coll_base_reduce_intra_chain(REDUCE_ARGS, uint32_t segsize, int fanout, int max_outstanding_reqs );
int ompi_coll_base_reduce_intra_pipeline(REDUCE_ARGS, uint32_t segsize, int max_outstanding_reqs );
int ompi_coll_base_reduce_intra_binary(REDUCE_ARGS, uint32_t segsize, int max_outstanding_reqs );
int ompi_coll_base_reduce_intra_binomial(REDUCE_ARGS, uint32_t segsize, int max_outstanding_reqs );
int ompi_coll_base_reduce_intra_in_order_binary(REDUCE_ARGS, uint32_t segsize, int max_outstanding_reqs );

/* Reduce_scatter */
int ompi_coll_base_reduce_scatter_intra_nonoverlapping(REDUCESCATTER_ARGS);
int ompi_coll_base_reduce_scatter_intra_basic_recursivehalving(REDUCESCATTER_ARGS);
int ompi_coll_base_reduce_scatter_intra_ring(REDUCESCATTER_ARGS);

/* Scan */

/* Scatter */
int ompi_coll_base_scatter_intra_basic_linear(SCATTER_ARGS);
int ompi_coll_base_scatter_intra_binomial(SCATTER_ARGS);

/* ScatterV */

END_C_DECLS

#define COLL_BASE_UPDATE_BINTREE( OMPI_COMM, BASE_MODULE, ROOT )	\
do {                                                                                       \
    mca_coll_base_comm_t* coll_comm = (BASE_MODULE)->base_data;                        \
    if( !( (coll_comm->cached_bintree)                                                     \
           && (coll_comm->cached_bintree_root == (ROOT)) ) ) {                             \
        if( coll_comm->cached_bintree ) { /* destroy previous binomial if defined */       \
            ompi_coll_base_topo_destroy_tree( &(coll_comm->cached_bintree) );             \
        }                                                                                  \
        coll_comm->cached_bintree = ompi_coll_base_topo_build_tree(2,(OMPI_COMM),(ROOT)); \
        coll_comm->cached_bintree_root = (ROOT);                                           \
    }                                                                                      \
} while (0)

#define COLL_BASE_UPDATE_BMTREE( OMPI_COMM, BASE_MODULE, ROOT )	\
do {                                                                                         \
    mca_coll_base_comm_t* coll_comm = (BASE_MODULE)->base_data;                           \
    if( !( (coll_comm->cached_bmtree)                                                        \
           && (coll_comm->cached_bmtree_root == (ROOT)) ) ) {                                \
        if( coll_comm->cached_bmtree ) { /* destroy previous binomial if defined */          \
            ompi_coll_base_topo_destroy_tree( &(coll_comm->cached_bmtree) );                \
        }                                                                                    \
        coll_comm->cached_bmtree = ompi_coll_base_topo_build_bmtree( (OMPI_COMM), (ROOT) ); \
        coll_comm->cached_bmtree_root = (ROOT);                                              \
    }                                                                                        \
} while (0)

#define COLL_BASE_UPDATE_IN_ORDER_BMTREE( OMPI_COMM, BASE_MODULE, ROOT ) \
do {                                                                                         \
    mca_coll_base_comm_t* coll_comm = (BASE_MODULE)->base_data;                           \
    if( !( (coll_comm->cached_in_order_bmtree)                                               \
           && (coll_comm->cached_in_order_bmtree_root == (ROOT)) ) ) {                       \
        if( coll_comm->cached_in_order_bmtree ) { /* destroy previous binomial if defined */ \
            ompi_coll_base_topo_destroy_tree( &(coll_comm->cached_in_order_bmtree) );       \
        }                                                                                    \
        coll_comm->cached_in_order_bmtree = ompi_coll_base_topo_build_in_order_bmtree( (OMPI_COMM), (ROOT) ); \
        coll_comm->cached_in_order_bmtree_root = (ROOT);                                     \
    }                                                                                        \
} while (0)

#define COLL_BASE_UPDATE_PIPELINE( OMPI_COMM, BASE_MODULE, ROOT )	\
do {                                                                                             \
    mca_coll_base_comm_t* coll_comm = (BASE_MODULE)->base_data;                               \
    if( !( (coll_comm->cached_pipeline)                                                          \
           && (coll_comm->cached_pipeline_root == (ROOT)) ) ) {                                  \
        if (coll_comm->cached_pipeline) { /* destroy previous pipeline if defined */             \
            ompi_coll_base_topo_destroy_tree( &(coll_comm->cached_pipeline) );                  \
        }                                                                                        \
        coll_comm->cached_pipeline = ompi_coll_base_topo_build_chain( 1, (OMPI_COMM), (ROOT) ); \
        coll_comm->cached_pipeline_root = (ROOT);                                                \
    }                                                                                            \
} while (0)

#define COLL_BASE_UPDATE_CHAIN( OMPI_COMM, BASE_MODULE, ROOT, FANOUT )	\
do {                                                                                             \
    mca_coll_base_comm_t* coll_comm = (BASE_MODULE)->base_data;                               \
    if( !( (coll_comm->cached_chain)                                                             \
           && (coll_comm->cached_chain_root == (ROOT))                                           \
           && (coll_comm->cached_chain_fanout == (FANOUT)) ) ) {                                 \
        if( coll_comm->cached_chain) { /* destroy previous chain if defined */                   \
            ompi_coll_base_topo_destroy_tree( &(coll_comm->cached_chain) );                     \
        }                                                                                        \
        coll_comm->cached_chain = ompi_coll_base_topo_build_chain((FANOUT), (OMPI_COMM), (ROOT)); \
        coll_comm->cached_chain_root = (ROOT);                                                   \
        coll_comm->cached_chain_fanout = (FANOUT);                                               \
    }                                                                                            \
} while (0)

#define COLL_BASE_UPDATE_IN_ORDER_BINTREE( OMPI_COMM, BASE_MODULE )	\
do {                                                                           \
    mca_coll_base_comm_t* coll_comm = (BASE_MODULE)->base_data;             \
    if( !(coll_comm->cached_in_order_bintree) ) {                              \
        /* In-order binary tree topology is defined by communicator size */    \
        /* Thus, there is no need to destroy anything */                       \
        coll_comm->cached_in_order_bintree =                                   \
        ompi_coll_base_topo_build_in_order_bintree((OMPI_COMM)); \
    }                                                                          \
} while (0)

/**
 * This macro give a generic way to compute the best count of
 * the segment (i.e. the number of complete datatypes that
 * can fit in the specified SEGSIZE). Beware, when this macro
 * is called, the SEGCOUNT should be initialized to the count as
 * expected by the collective call.
 */
#define COLL_BASE_COMPUTED_SEGCOUNT(SEGSIZE, TYPELNG, SEGCOUNT)        \
    if( ((SEGSIZE) >= (TYPELNG)) &&                                     \
        ((SEGSIZE) < ((TYPELNG) * (SEGCOUNT))) ) {                      \
        size_t residual;                                                \
        (SEGCOUNT) = (int)((SEGSIZE) / (TYPELNG));                      \
        residual = (SEGSIZE) - (SEGCOUNT) * (TYPELNG);                  \
        if( residual > ((TYPELNG) >> 1) )                               \
            (SEGCOUNT)++;                                               \
    }                                                                   \

/**
 * This macro gives a generic wait to compute the well distributed block counts
 * when the count and number of blocks are fixed.
 * Macro returns "early-block" count, "late-block" count, and "split-index"
 * which is the block at which we switch from "early-block" count to
 * the "late-block" count.
 * count = split_index * early_block_count +
 *         (block_count - split_index) * late_block_count
 * We do not perform ANY error checks - make sure that the input values
 * make sense (eg. count > num_blocks).
 */
#define COLL_BASE_COMPUTE_BLOCKCOUNT( COUNT, NUM_BLOCKS, SPLIT_INDEX,       \
                                       EARLY_BLOCK_COUNT, LATE_BLOCK_COUNT ) \
    EARLY_BLOCK_COUNT = LATE_BLOCK_COUNT = COUNT / NUM_BLOCKS;               \
    SPLIT_INDEX = COUNT % NUM_BLOCKS;                                        \
    if (0 != SPLIT_INDEX) {                                                  \
        EARLY_BLOCK_COUNT = EARLY_BLOCK_COUNT + 1;                           \
    }                                                                        \

/*
 * Data structure for hanging data off the communicator
 * i.e. per module instance
 */
struct mca_coll_base_comm_t {
    opal_object_t super;

    /* standard data for requests and PML usage */

    /* Precreate space for requests
     * Note this does not effect basic,
     * but if in wrong context can confuse a debugger
     * this is controlled by an MCA param
     */

    ompi_request_t **mcct_reqs;
    int mcct_num_reqs;

    /*
     * base topo information caching per communicator
     *
     * for each communicator we cache the topo information so we can
     * reuse without regenerating if we change the root, [or fanout]
     * then regenerate and recache this information
     */

    /* general tree with n fan out */
    ompi_coll_tree_t *cached_ntree;
    int cached_ntree_root;
    int cached_ntree_fanout;

    /* binary tree */
    ompi_coll_tree_t *cached_bintree;
    int cached_bintree_root;

    /* binomial tree */
    ompi_coll_tree_t *cached_bmtree;
    int cached_bmtree_root;

    /* binomial tree */
    ompi_coll_tree_t *cached_in_order_bmtree;
    int cached_in_order_bmtree_root;

    /* chained tree (fanout followed by pipelines) */
    ompi_coll_tree_t *cached_chain;
    int cached_chain_root;
    int cached_chain_fanout;

    /* pipeline */
    ompi_coll_tree_t *cached_pipeline;
    int cached_pipeline_root;

    /* in-order binary tree (root of the in-order binary tree is rank 0) */
    ompi_coll_tree_t *cached_in_order_bintree;
};
typedef struct mca_coll_base_comm_t mca_coll_base_comm_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_base_comm_t);

static inline void ompi_coll_base_free_reqs(ompi_request_t **reqs, int count)
{
    int i;
    for (i = 0; i < count; ++i)
        ompi_request_free(&reqs[i]);
}

/**
 * Return the array of requests on the data. If the array was not initialized
 * or if it's size was too small, allocate it to fit the requested size.
 */
ompi_request_t** coll_base_comm_get_reqs(mca_coll_base_comm_t* data, int nreqs);

#endif /* MCA_COLL_BASE_EXPORT_H */
