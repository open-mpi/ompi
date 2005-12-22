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

#ifndef MCA_COLL_TUNED_EXPORT_H
#define MCA_COLL_TUNED_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/coll/coll.h"
#include "request/request.h"
#include "mca/pml/pml.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "coll_tuned_topo.h"

/* also need the dynamic rule structures */
#include "coll_tuned_dynamic_rules.h"

/* some fixed value index vars to simplify certain operations */
#define ALLGATHER 0
#define ALLGATHERV 1
#define ALLREDUCE 2
#define ALLTOALL 3
#define ALLTOALLV 4
#define ALLTOALLW 5
#define BARRIER 6
#define BCAST 7
#define EXSCAN 8
#define GATHER 9
#define GATHERV 10
#define REDUCE 11
#define REDUCESCATTER 11
#define SCAN 12
#define SCATTER 13
#define SCATTERV 14
#define COLLCOUNT 15

/* defined arg lists to simply auto inclusion of user overriding decision functions */
#define ALLGATHER_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm
#define ALLGATHERV_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void * rbuf, int *rcounts, int *disps, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm
#define ALLREDUCE_ARGS void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, struct ompi_op_t *op, struct ompi_communicator_t *comm
#define ALLTOALL_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void* rbuf, int rcount, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm
#define ALLTOALLV_ARGS void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t *sdtype, void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm
#define ALLTOALLW_ARGS void *sbuf, int *scounts, int *sdisps,  struct ompi_datatype_t **sdtypes, void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t **rdtypes, struct ompi_communicator_t *comm
#define BARRIER_ARGS struct ompi_communicator_t *comm
#define BCAST_ARGS void *buff, int count, struct ompi_datatype_t *datatype, int root, struct ompi_communicator_t *comm
#define EXSCAN_ARGS void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, struct ompi_op_t *op, struct ompi_communicator_t *comm
#define GATHER_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount, struct ompi_datatype_t *rdtype, int root, struct ompi_communicator_t *comm
#define GATHERV_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf, int *rcounts, int *disps, struct ompi_datatype_t *rdtype, int root, struct ompi_communicator_t *comm
#define REDUCE_ARGS void *sbuf, void* rbuf, int count, struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root, struct ompi_communicator_t *comm
#define REDUCESCATTER_ARGS void *sbuf, void *rbuf, int *rcounts, struct ompi_datatype_t *dtype, struct ompi_op_t *op, struct ompi_communicator_t *comm
#define SCAN_ARGS void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,  struct ompi_op_t *op, struct ompi_communicator_t *comm
#define SCATTER_ARGS void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount, struct ompi_datatype_t *rdtype, int root, struct ompi_communicator_t *comm
#define SCATTERV_ARGS void *sbuf, int *scounts, int *disps, struct ompi_datatype_t *sdtype, void* rbuf, int rcount, struct ompi_datatype_t *rdtype, int root, struct ompi_communicator_t *comm
/* end defined arg lists to simply auto inclusion of user overriding decision functions */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Globally exported variable
 */

OMPI_COMP_EXPORT extern const mca_coll_base_component_1_0_0_t mca_coll_tuned_component;

OMPI_COMP_EXPORT extern int   ompi_coll_tuned_stream;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_priority;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_preallocate_memory_comm_size_limit;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_use_dynamic_rules;
OMPI_COMP_EXPORT extern char* ompi_coll_tuned_dynamic_rules_filename;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_init_tree_fanout;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_init_chain_fanout;

/* forced algorithm choices */
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_allreduce_forced_choice;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_allreduce_forced_segsize;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_allreduce_forced_tree_fanout;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_allreduce_forced_chain_fanout;

OMPI_COMP_EXPORT extern int   ompi_coll_tuned_alltoall_forced_choice;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_alltoall_forced_segsize;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_alltoall_forced_tree_fanout;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_alltoall_forced_chain_fanout;

OMPI_COMP_EXPORT extern int   ompi_coll_tuned_barrier_forced_choice;

OMPI_COMP_EXPORT extern int   ompi_coll_tuned_bcast_forced_choice;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_bcast_forced_segsize;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_bcast_forced_tree_fanout;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_bcast_forced_chain_fanout;

OMPI_COMP_EXPORT extern int   ompi_coll_tuned_reduce_forced_choice;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_reduce_forced_segsize;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_reduce_forced_tree_fanout;
OMPI_COMP_EXPORT extern int   ompi_coll_tuned_reduce_forced_chain_fanout;

/*
 * coll API functions
 */


  /* API functions */

  int ompi_coll_tuned_init_query(bool enable_progress_threads,
                                bool enable_mpi_threads);

  const struct mca_coll_base_module_1_0_0_t *
    ompi_coll_tuned_comm_query(struct ompi_communicator_t *comm, int *priority,
                              struct mca_coll_base_comm_t **data);

  const struct mca_coll_base_module_1_0_0_t *
    ompi_coll_tuned_module_init(struct ompi_communicator_t *comm);

  int ompi_coll_tuned_module_finalize(struct ompi_communicator_t *comm);

  /* API functions of decision functions and any implementations */

  /*
   * Note this gets long as we have to have a prototype for each 
   * MPI collective 4 times.. 2 for the comm type and 2 for each decision
   * type. 
   * we might cut down the decision prototypes by conditional compiling
   */

  /* All Gather */
  int ompi_coll_tuned_allgather_intra_dec_fixed(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_intra_dec_dynamic(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_inter_dec_fixed(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_inter_dec_dynamic(ALLGATHER_ARGS);

  /* All GatherV */
  int ompi_coll_tuned_allgatherv_intra_dec_fixed(ALLGATHERV_ARGS);
  int ompi_coll_tuned_allgatherv_intra_dec_dynamic(ALLGATHERV_ARGS);
  int ompi_coll_tuned_allgatherv_inter_dec_fixed(ALLGATHERV_ARGS);
  int ompi_coll_tuned_allgatherv_inter_dec_dynamic(ALLGATHERV_ARGS);

  /* All Reduce */
  int ompi_coll_tuned_allreduce_intra_dec_fixed(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_intra_dec_dynamic(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_intra_do_forced(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_intra_do_this(ALLREDUCE_ARGS, int choice, int faninout, int segsize);
  int ompi_coll_tuned_allreduce_intra_check_forced(void);
  int ompi_coll_tuned_allreduce_intra_query(void);
  int ompi_coll_tuned_allreduce_intra_nonoverlapping(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_intra_basic_linear(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_inter_dec_fixed(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_inter_dec_dynamic(ALLREDUCE_ARGS);

  /* AlltoAll */
  int ompi_coll_tuned_alltoall_intra_dec_fixed(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_dec_dynamic(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_do_forced(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_do_this(ALLTOALL_ARGS, int choice, int faninout, int segsize);
  int ompi_coll_tuned_alltoall_intra_check_forced(void);
  int ompi_coll_tuned_alltoall_intra_query (void);
  int ompi_coll_tuned_alltoall_intra_pairwise(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_bruck(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_basic_linear(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_two_procs(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_inter_dec_fixed(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_inter_dec_dynamic(ALLTOALL_ARGS);

  /* AlltoAllV */
  int ompi_coll_tuned_alltoallv_intra_dec_fixed(ALLTOALLV_ARGS);
  int ompi_coll_tuned_alltoallv_intra_dec_dynamic(ALLTOALLV_ARGS);
  int ompi_coll_tuned_alltoallv_inter_dec_fixed(ALLTOALLV_ARGS);
  int ompi_coll_tuned_alltoallv_inter_dec_dynamic(ALLTOALLV_ARGS);

  /* AlltoAllW */
  int ompi_coll_tuned_alltoallw_intra_dec_fixed(ALLTOALLW_ARGS);
  int ompi_coll_tuned_alltoallw_intra_dec_dynamic(ALLTOALLW_ARGS);
  int ompi_coll_tuned_alltoallw_inter_dec_fixed(ALLTOALLW_ARGS);
  int ompi_coll_tuned_alltoallw_inter_dec_dynamic(ALLTOALLW_ARGS);

  /* Barrier */
  int ompi_coll_tuned_barrier_intra_dec_fixed(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_dec_dynamic(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_do_forced(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_do_this(BARRIER_ARGS, int choice, int faninout, int segsize);

  int ompi_coll_tuned_barrier_intra_check_forced(void);
  int ompi_coll_tuned_barrier_intra_query (void);

  int ompi_coll_tuned_barrier_inter_dec_fixed(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_inter_dec_dynamic(BARRIER_ARGS);

  int ompi_coll_tuned_barrier_intra_doublering(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_recursivedoubling(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_bruck(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_two_procs(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_linear(BARRIER_ARGS);

  /* Bcast */
  int ompi_coll_tuned_bcast_intra_dec_fixed(BCAST_ARGS);
  int ompi_coll_tuned_bcast_intra_dec_dynamic(BCAST_ARGS);
  int ompi_coll_tuned_bcast_intra_do_forced(BCAST_ARGS);
  int ompi_coll_tuned_bcast_intra_do_this(BCAST_ARGS, int choice, int faninout, int segsize);
  int ompi_coll_tuned_bcast_intra_check_forced(void);
  int ompi_coll_tuned_bcast_intra_query (void);
  int ompi_coll_tuned_bcast_intra_basic_linear(BCAST_ARGS);
  int ompi_coll_tuned_bcast_intra_chain(BCAST_ARGS, uint32_t segsize, int32_t chains);
  int ompi_coll_tuned_bcast_intra_pipeline(BCAST_ARGS, uint32_t segsize);
  int ompi_coll_tuned_bcast_intra_bmtree(BCAST_ARGS, uint32_t segsize, int32_t chains);
  int ompi_coll_tuned_bcast_intra_bintree(BCAST_ARGS, uint32_t segsize);
  int ompi_coll_tuned_bcast_intra_split_bintree(BCAST_ARGS, uint32_t segsize);
  int ompi_coll_tuned_bcast_inter_dec_fixed(BCAST_ARGS);
  int ompi_coll_tuned_bcast_inter_dec_dynamic(BCAST_ARGS);

  /* Exscan */
  int ompi_coll_tuned_exscan_intra_dec_fixed(EXSCAN_ARGS);
  int ompi_coll_tuned_exscan_intra_dec_dynamic(EXSCAN_ARGS);
  int ompi_coll_tuned_exscan_inter_dec_fixed(EXSCAN_ARGS);
  int ompi_coll_tuned_exscan_inter_dec_dynamic(EXSCAN_ARGS);

  /* Gather */
  int ompi_coll_tuned_gather_intra_dec_fixed(GATHER_ARGS);
  int ompi_coll_tuned_gather_intra_dec_dynamic(GATHER_ARGS);
  int ompi_coll_tuned_gather_inter_dec_fixed(GATHER_ARGS);
  int ompi_coll_tuned_gather_inter_dec_dynamic(GATHER_ARGS);

  /* GatherV */
  int ompi_coll_tuned_gatherv_intra_dec_fixed(GATHERV_ARGS);
  int ompi_coll_tuned_gatherv_intra_dec_dynamic(GATHER_ARGS);
  int ompi_coll_tuned_gatherv_inter_dec_fixed(GATHER_ARGS);
  int ompi_coll_tuned_gatherv_inter_dec_dynamic(GATHER_ARGS);

  /* Reduce */
  int ompi_coll_tuned_reduce_intra_dec_fixed(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_intra_dec_dynamic(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_intra_do_forced(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_intra_do_this(REDUCE_ARGS, int choice, int faninout, int segsize);
  int ompi_coll_tuned_reduce_intra_check_forced(void);
  int ompi_coll_tuned_reduce_intra_query (void);
  int ompi_coll_tuned_reduce_intra_basic_linear(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_intra_chain(REDUCE_ARGS, uint32_t segsize, int fanout);
  int ompi_coll_tuned_reduce_intra_pipeline(REDUCE_ARGS, uint32_t segsize);
  int ompi_coll_tuned_reduce_inter_dec_fixed(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_inter_dec_dynamic(REDUCE_ARGS);

  /* Reduce-Scatter */
  int ompi_coll_tuned_reduce_scatter_intra_dec_fixed(REDUCESCATTER_ARGS);
  int ompi_coll_tuned_reduce_scatter_intra_dec_dynamic(REDUCESCATTER_ARGS);
  int ompi_coll_tuned_reduce_scatter_inter_dec_fixed(REDUCESCATTER_ARGS);
  int ompi_coll_tuned_reduce_scatter_inter_dec_dynamic(REDUCESCATTER_ARGS);
 
  /* Scan */
  int ompi_coll_tuned_scan_intra_dec_fixed(SCAN_ARGS);
  int ompi_coll_tuned_scan_intra_dec_dynamic(SCAN_ARGS);
  int ompi_coll_tuned_scan_inter_dec_fixed(SCAN_ARGS);
  int ompi_coll_tuned_scan_inter_dec_dynamic(SCAN_ARGS);

  /* Scatter */
  int ompi_coll_tuned_scatter_intra_dec_fixed(SCATTER_ARGS);
  int ompi_coll_tuned_scatter_intra_dec_dynamic(SCATTER_ARGS);
  int ompi_coll_tuned_scatter_inter_dec_fixed(SCATTER_ARGS);
  int ompi_coll_tuned_scatter_inter_dec_dynamic(SCATTER_ARGS);

  /* ScatterV */
  int ompi_coll_tuned_scatterv_intra_dec_fixed(SCATTERV_ARGS);
  int ompi_coll_tuned_scatterv_intra_dec_dynamic(SCATTERV_ARGS);
  int ompi_coll_tuned_scatterv_inter_dec_fixed(SCATTERV_ARGS);
  int ompi_coll_tuned_scatterv_inter_dec_dynamic(SCATTERV_ARGS);



/* Utility functions */

static inline void ompi_coll_tuned_free_reqs(ompi_request_t **reqs, int count)
{
  int i;
  for (i = 0; i < count; ++i)
     ompi_request_free(&reqs[i]);
}


/*
 * Data structure for hanging data off the communicator 
 */
struct mca_coll_base_comm_t {
  /* standard data for requests and PML usage */

  /* Precreate space for requests 
   * Note this does not effect basic, 
   * but if in wrong context can confuse a debugger
   * this is controlled by an MCA param
   */

  ompi_request_t **mcct_reqs;
  int mcct_num_reqs;

  /* 
   * tuned topo information caching per communicator 
   *
   * for each communicator we cache the topo information so we can 
   * reuse without regenerating if we change the root, [or fanout]
   * then regenerate and recache this information 
   *
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

   /* chained tree (fanout followed by pipelines) */
   ompi_coll_chain_t *cached_chain;
   int cached_chain_root;
   int cached_chain_fanout; 

   /* pipeline */
   ompi_coll_chain_t *cached_pipeline;
   int cached_pipeline_root;

   /* extra data required by the decision functions */
   ompi_coll_alg_rule_t *all_base_rules;       /* stored only on MCW, all other coms ref it */
   ompi_coll_com_rule_t *com_rules[COLLCOUNT]; /* the communicator rules for each MPI collective for ONLY my comsize */
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_COLL_TUNED_EXPORT_H */
