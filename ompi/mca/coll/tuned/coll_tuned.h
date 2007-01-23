/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "coll_tuned_topo.h"

/* also need the dynamic rule structures */
#include "coll_tuned_dynamic_rules.h"

/* need the forced user choice structures */
#include "coll_tuned_forced.h"

/* some fixed value index vars to simplify certain operations */
typedef enum COLLTYPE {ALLGATHER, ALLGATHERV, ALLREDUCE, ALLTOALL, ALLTOALLV, ALLTOALLW, BARRIER, BCAST,
EXSCAN, GATHER, GATHERV, REDUCE, REDUCESCATTER, SCAN, SCATTER, SCATTERV, COLLCOUNT} COLLTYPE_T;

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

/* these are the same across all modules and are loaded at component query time */
extern int   ompi_coll_tuned_stream;
extern int   ompi_coll_tuned_priority;
extern int   ompi_coll_tuned_preallocate_memory_comm_size_limit;
extern int   ompi_coll_tuned_use_dynamic_rules;
extern char* ompi_coll_tuned_dynamic_rules_filename;
extern int   ompi_coll_tuned_init_tree_fanout;
extern int   ompi_coll_tuned_init_chain_fanout;

/* forced algorithm choices */
/* the indices to the MCA params so that modules can look them up at open / comm create time  */
extern coll_tuned_force_algorithm_mca_param_indices_t ompi_coll_tuned_forced_params[COLLCOUNT];
/* the actual max algorithm values (readonly), loaded at component open */
extern int ompi_coll_tuned_forced_max_algorithms[COLLCOUNT];

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
  int ompi_coll_tuned_allgather_intra_do_forced(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_intra_do_this(ALLGATHER_ARGS, int algorithm, int faninout, int segsize);
  int ompi_coll_tuned_allgather_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices);
  int ompi_coll_tuned_allgather_intra_bruck(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_intra_recursivedoubling(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_intra_ring(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_intra_neighborexchange(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_intra_basic_linear(ALLGATHER_ARGS);
  int ompi_coll_tuned_allgather_intra_two_procs(ALLGATHER_ARGS);
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
  int ompi_coll_tuned_allreduce_intra_do_this(ALLREDUCE_ARGS, int algorithm, int faninout, int segsize);
  int ompi_coll_tuned_allreduce_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices);
  int ompi_coll_tuned_allreduce_intra_nonoverlapping(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_intra_recursivedoubling(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_intra_ring(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_intra_basic_linear(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_inter_dec_fixed(ALLREDUCE_ARGS);
  int ompi_coll_tuned_allreduce_inter_dec_dynamic(ALLREDUCE_ARGS);

  /* AlltoAll */
  int ompi_coll_tuned_alltoall_intra_dec_fixed(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_dec_dynamic(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_do_forced(ALLTOALL_ARGS);
  int ompi_coll_tuned_alltoall_intra_do_this(ALLTOALL_ARGS, int algorithm, int faninout, int segsize);
  int ompi_coll_tuned_alltoall_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices);
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
  int ompi_coll_tuned_barrier_intra_do_this(BARRIER_ARGS, int algorithm, int faninout, int segsize);
  int ompi_coll_tuned_barrier_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices);
  int ompi_coll_tuned_barrier_inter_dec_fixed(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_inter_dec_dynamic(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_doublering(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_recursivedoubling(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_bruck(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_two_procs(BARRIER_ARGS);
  int ompi_coll_tuned_barrier_intra_linear(BARRIER_ARGS);

  /* Bcast */
  int ompi_coll_tuned_bcast_intra_generic( BCAST_ARGS, uint32_t count_by_segment, ompi_coll_tree_t* tree );
  int ompi_coll_tuned_bcast_intra_dec_fixed(BCAST_ARGS);
  int ompi_coll_tuned_bcast_intra_dec_dynamic(BCAST_ARGS);
  int ompi_coll_tuned_bcast_intra_do_forced(BCAST_ARGS);
  int ompi_coll_tuned_bcast_intra_do_this(BCAST_ARGS, int algorithm, int faninout, int segsize);
  int ompi_coll_tuned_bcast_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices);
  int ompi_coll_tuned_bcast_intra_basic_linear(BCAST_ARGS);
  int ompi_coll_tuned_bcast_intra_chain(BCAST_ARGS, uint32_t segsize, int32_t chains);
  int ompi_coll_tuned_bcast_intra_pipeline(BCAST_ARGS, uint32_t segsize);
  int ompi_coll_tuned_bcast_intra_binomial(BCAST_ARGS, uint32_t segsize);
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
  int ompi_coll_tuned_reduce_generic( REDUCE_ARGS, ompi_coll_tree_t* tree, int count_by_segment );
  int ompi_coll_tuned_reduce_intra_dec_fixed(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_intra_dec_dynamic(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_intra_do_forced(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_intra_do_this(REDUCE_ARGS, int algorithm, int faninout, int segsize);
  int ompi_coll_tuned_reduce_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices);
  int ompi_coll_tuned_reduce_intra_basic_linear(REDUCE_ARGS);
  int ompi_coll_tuned_reduce_intra_chain(REDUCE_ARGS, uint32_t segsize, int fanout);
  int ompi_coll_tuned_reduce_intra_pipeline(REDUCE_ARGS, uint32_t segsize);
  int ompi_coll_tuned_reduce_intra_binary(REDUCE_ARGS, uint32_t segsize);
  int ompi_coll_tuned_reduce_intra_binomial(REDUCE_ARGS, uint32_t segsize);
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

struct mca_coll_tuned_component_t {
    /** Base coll component */ 
    mca_coll_base_component_1_0_0_t super;

    /** MCA parameter: Priority of this component */
    int tuned_priority;

    /** global stuff that I need the component to store */

    /* MCA parameters first */

    /* cached decision table stuff (moved from MCW module) */
    ompi_coll_alg_rule_t *all_base_rules;       

};
    /**
     * Convenience typedef
     */
    typedef struct mca_coll_tuned_component_t mca_coll_tuned_component_t;

    /**
     * Global component instance
     */
    OMPI_MODULE_DECLSPEC extern mca_coll_tuned_component_t mca_coll_tuned_component;

/*
 * Data structure for hanging data off the communicator 
 * i.e. per module instance
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
   ompi_coll_tree_t *cached_chain;
   int cached_chain_root;
   int cached_chain_fanout; 

   /* pipeline */
   ompi_coll_tree_t *cached_pipeline;
   int cached_pipeline_root;

   /* extra data required by the decision functions */
   ompi_coll_alg_rule_t *all_base_rules;       /* stored only on MCW, all other coms ref it */
                                                /* moving to the component */
   ompi_coll_com_rule_t *com_rules[COLLCOUNT]; /* the communicator rules for each MPI collective for ONLY my comsize */

   /* for forced algorithms we store the information on the module */
   /* previously we only had one shared copy, ops, it really is per comm/module */
   coll_tuned_force_algorithm_params_t user_forced[COLLCOUNT];
};

   /**
    * Convenience typedef
    */
   typedef struct mca_coll_base_comm_t mca_coll_base_comm_t;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#define COLL_TUNED_UPDATE_BINTREE( OMPI_COMM, ROOT )                                       \
do {                                                                                       \
    mca_coll_base_comm_t* coll_comm = (OMPI_COMM)->c_coll_selected_data;                   \
    if( !( (coll_comm->cached_bintree)                                                     \
           && (coll_comm->cached_bintree_root == (ROOT)) ) ) {                             \
        if( coll_comm->cached_bintree ) { /* destroy previous binomial if defined */       \
            ompi_coll_tuned_topo_destroy_tree( &(coll_comm->cached_bintree) );             \
        }                                                                                  \
        coll_comm->cached_bintree = ompi_coll_tuned_topo_build_tree(2,(OMPI_COMM),(ROOT)); \
        coll_comm->cached_bintree_root = (ROOT);                                           \
    }                                                                                      \
} while (0)

#define COLL_TUNED_UPDATE_BMTREE( OMPI_COMM, ROOT )                                          \
do {                                                                                         \
    mca_coll_base_comm_t* coll_comm = (OMPI_COMM)->c_coll_selected_data;                     \
    if( !( (coll_comm->cached_bmtree)                                                        \
           && (coll_comm->cached_bmtree_root == (ROOT)) ) ) {                                \
        if( coll_comm->cached_bmtree ) { /* destroy previous binomial if defined */          \
            ompi_coll_tuned_topo_destroy_tree( &(coll_comm->cached_bmtree) );                \
        }                                                                                    \
        coll_comm->cached_bmtree = ompi_coll_tuned_topo_build_bmtree( (OMPI_COMM), (ROOT) ); \
        coll_comm->cached_bmtree_root = (ROOT);                                              \
    }                                                                                        \
} while (0)

#define COLL_TUNED_UPDATE_PIPELINE( OMPI_COMM, ROOT )                                            \
do {                                                                                             \
    mca_coll_base_comm_t* coll_comm = (OMPI_COMM)->c_coll_selected_data;                         \
    if( !( (coll_comm->cached_pipeline)                                                          \
           && (coll_comm->cached_pipeline_root == (ROOT)) ) ) {                                  \
        if (coll_comm->cached_pipeline) { /* destroy previous pipeline if defined */             \
            ompi_coll_tuned_topo_destroy_tree( &(coll_comm->cached_pipeline) );                  \
        }                                                                                        \
        coll_comm->cached_pipeline = ompi_coll_tuned_topo_build_chain( 1, (OMPI_COMM), (ROOT) ); \
        coll_comm->cached_pipeline_root = (ROOT);                                                \
    }                                                                                            \
} while (0)

#define COLL_TUNED_UPDATE_CHAIN( OMPI_COMM, ROOT, FANOUT )                                       \
do {                                                                                             \
    mca_coll_base_comm_t* coll_comm = (OMPI_COMM)->c_coll_selected_data;                         \
    if( !( (coll_comm->cached_chain)                                                             \
           && (coll_comm->cached_chain_root == (ROOT))                                           \
           && (coll_comm->cached_chain_fanout == (FANOUT)) ) ) {                                 \
        if( coll_comm->cached_chain) { /* destroy previous chain if defined */                   \
            ompi_coll_tuned_topo_destroy_tree( &(coll_comm->cached_chain) );                     \
        }                                                                                        \
        coll_comm->cached_chain = ompi_coll_tuned_topo_build_chain((FANOUT),(OMPI_COMM),(ROOT)); \
        coll_comm->cached_chain_root = (ROOT);                                                   \
        coll_comm->cached_chain_fanout = (FANOUT);                                               \
    }                                                                                            \
} while (0)

/**
 * This macro give a generic way to compute the best count of
 * the segment (i.e. the number of complete datatypes that
 * can fit in the specified SEGSIZE). Beware, when this macro
 * is called, the SEGCOUNT should be initialized to the count as
 * expected by the collective call.
 */
#define COLL_TUNED_COMPUTED_SEGCOUNT(SEGSIZE, TYPELNG, SEGCOUNT)        \
    if( ((SEGSIZE) >= (TYPELNG)) &&                                     \
        ((SEGSIZE) < ((TYPELNG) * (SEGCOUNT))) ) {                      \
        size_t residual;                                                \
        (SEGCOUNT) = (int)((SEGSIZE) / (TYPELNG));                      \
        residual = (SEGSIZE) - (SEGCOUNT) * (TYPELNG);                  \
        if( residual > ((TYPELNG) >> 1) )                               \
            (SEGCOUNT)++;                                               \
    }                                                                   \

#endif /* MCA_COLL_TUNED_EXPORT_H */
