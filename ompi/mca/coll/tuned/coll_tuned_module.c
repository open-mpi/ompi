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
#include "coll_tuned.h"

#include <stdio.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/base/mca_base_param.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"

/* from component.. shouldn't it be cached on the component somehow */
extern int mca_coll_tuned_use_dynamic_rules_param;
extern int mca_coll_tuned_init_tree_fanout_param;
extern int mca_coll_tuned_init_chain_fanout_param;
extern int mca_coll_tuned_preallocate_memory_comm_size_limit_param;



/*
 * Which set are we using?
 */
static const mca_coll_base_module_1_0_0_t *to_use = NULL;

/*
 * Intra communicator decision functions
 * 
 * Two prototypes, one for fixed rules and one for dynamic rules
 *
 */
static const mca_coll_base_module_1_0_0_t intra_fixed = {

  /* Initialization / finalization functions */

  mca_coll_tuned_module_init,
  mca_coll_tuned_module_finalize,

  /* Collective function pointers */

/*   mca_coll_tuned_allgather_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_allgatherv_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_allreduce_intra_dec_fixed, */
    NULL,
  mca_coll_tuned_alltoall_intra_dec_fixed,
/*     NULL, */
/*   mca_coll_tuned_alltoallv_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_alltoallw_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_barrier_intra_dec_fixed, */
    NULL,
  mca_coll_tuned_bcast_intra_dec_fixed,
/*     NULL, */
/*   mca_coll_tuned_exscan_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_gather_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_gatherv_intra_dec_fixed, */
    NULL,
  mca_coll_tuned_reduce_intra_dec_fixed,
/*     NULL, */
/*   mca_coll_tuned_reduce_scatter_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_scan_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_scatter_intra_dec_fixed, */
    NULL,
/*   mca_coll_tuned_scatterv_intra_dec_fixed */
    NULL
};

static const mca_coll_base_module_1_0_0_t intra_dynamic = {

  /* Initialization / finalization functions */

  mca_coll_tuned_module_init,
  mca_coll_tuned_module_finalize,

  /* Collective function pointers */

/*   mca_coll_tuned_allgather_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_allgatherv_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_allreduce_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_alltoall_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_alltoallv_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_alltoallw_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_barrier_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_bcast_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_exscan_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_gather_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_gatherv_intra_dec_dynamic, */
    NULL,
  mca_coll_tuned_reduce_intra_dec_dynamic,
/*     NULL, */
/*   mca_coll_tuned_reduce_scatter_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_scan_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_scatter_intra_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_scatterv_intra_dec_dynamic */
    NULL
};

/*
 * collective decision functions for intercommunicators
 * 
 * Two prototypes, one for fixed rules and one for dynamic rules
 *
 */
static const mca_coll_base_module_1_0_0_t inter_fixed = {

  /* Initialization / finalization functions */

  mca_coll_tuned_module_init,
  mca_coll_tuned_module_finalize,

  /* Collective function pointers */

/*   mca_coll_tuned_allgather_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_allgatherv_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_allreduce_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_alltoall_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_alltoallv_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_alltoallw_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_barrier_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_bcast_inter_dec_fixed, */
    NULL,
  /* mca_coll_tuned_exscan_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_gather_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_gatherv_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_reduce_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_reduce_scatter_inter_dec_fixed, */
    NULL,
  /* mca_coll_tuned_scan_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_scatter_inter_dec_fixed, */
    NULL,
/*   mca_coll_tuned_scatterv_inter_dec_fixed */
    NULL
};

static const mca_coll_base_module_1_0_0_t inter_dynamic = {

  /* Initialization / finalization functions */

  mca_coll_tuned_module_init,
  mca_coll_tuned_module_finalize,

  /* Collective function pointers */

/*   mca_coll_tuned_allgather_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_allgatherv_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_allreduce_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_alltoall_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_alltoallv_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_alltoallw_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_barrier_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_bcast_inter_dec_dynamic, */
    NULL,
  /* mca_coll_tuned_exscan_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_gather_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_gatherv_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_reduce_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_reduce_scatter_inter_dec_dynamic, */
    NULL,
  /* mca_coll_tuned_scan_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_scatter_inter_dec_dynamic, */
    NULL,
/*   mca_coll_tuned_scatterv_inter_dec_dynamic */
    NULL
};

/* 
 * Note I keep the names here as place markers until all the functions
 * are implemented
 */


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int mca_coll_tuned_init_query(bool enable_progress_threads,
                              bool enable_mpi_threads)
{
    /* Nothing to do */
  
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_tuned_comm_query(struct ompi_communicator_t *comm, int *priority,
                          struct mca_coll_base_comm_t **data)
{
    int use_dynamic = -1;

    printf("Tuned query called\n");
  if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_tuned_priority_param,
                                                priority)) {
    return NULL;
  }


  /* 
   * Choose whether to use [intra|inter] decision functions 
   * and if using fixed OR dynamic rule sets.
   * Right now you cannot mix them, maybe later on it can be changed
   * but this would probably add an extra if and funct call to the path
   *
   */

  if (OMPI_SUCCESS !=
  mca_base_param_lookup_int(mca_coll_tuned_use_dynamic_rules_param,
                                                &use_dynamic)) {
    printf("No use_dynamic param found!\n");
    return NULL;
  }

  if (OMPI_COMM_IS_INTER(comm)) {
    if (use_dynamic) {
printf("using inter_dynamic\n");
      to_use = &inter_dynamic;
    } else {
printf("using inter_fixed\n");
      to_use = &inter_fixed;
    }
  } else { /* is an intra comm */
    if (use_dynamic) {
printf("using intra_dynamic\n");
      to_use = &intra_dynamic;
    } else {
printf("using intra_fixed\n");
      to_use = &intra_fixed;
    }
  }
  return to_use;
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_tuned_module_init(struct ompi_communicator_t *comm)
{
  int size;
  struct mca_coll_base_comm_t *data;
  /* fanout parameters */
  int tree_fanout_default = 0;
  int chain_fanout_default = 0;
  int pre_allocate_limit = -1;
  int pre_allocate = 1;


  printf("Tuned init module called.\n");

  /* This routine will become more complex and might have to be */
  /* broken into more sections/function calls */

  /* Order of operations:
   * alloc memory for nb reqs (in case we fall through) 
   * add decision rules if using dynamic rules
   *     compact rules using communicator size info etc
   * build first guess cached topologies (might depend on the rules from above)
   *
   * then attach all to the communicator and return base module funct ptrs 
   */

  /* Allocate the data that hangs off the communicator */

  if (OMPI_COMM_IS_INTER(comm)) {
      size = ompi_comm_remote_size(comm);
  } else {
      size = ompi_comm_size(comm);
  }

  /* 
   * we still malloc data as it is used by the TUNED modules
   * if we don't allocate it and fall back to a BASIC module routine then confuses debuggers 
   * we place any special info after the default data
   *
   * BUT on very large systems we might not be able to allocate all this memory so
   * we do check a MCA parameter to see if if we should allocate this memory
   *
   * The default is set very high  
   *
   */

  if (OMPI_SUCCESS !=
    mca_base_param_lookup_int(mca_coll_tuned_preallocate_memory_comm_size_limit_param,
                                                &pre_allocate_limit)) {
    printf("No pre_allocate param found!\n");
    return NULL;
  }

  /* if we within the memory/size limit, allow preallocated data */
  if (size<=pre_allocate_limit) {
    data = malloc(sizeof(struct mca_coll_base_comm_t) +
                 (sizeof(ompi_request_t *) * size * 2));
  
    if (NULL == data) {
         return NULL;
    }
    data->mcct_reqs = (ompi_request_t **) (data + 1);
    data->mcct_num_reqs = size * 2;
  }
  else {
     data = malloc(sizeof(struct mca_coll_base_comm_t)); 
  
     if (NULL == data) {
         data->mcct_reqs = (ompi_request_t **) NULL;
         data->mcct_num_reqs = 0;
     }
  }

  /* 
   * now for the cached topo functions 
   * guess the initial topologies to use rank 0 as root 
   */

  /* get default fanouts is made available via the MCA */
  if (OMPI_SUCCESS !=
  mca_base_param_lookup_int(mca_coll_tuned_init_tree_fanout_param,
                                                &tree_fanout_default)) {
    printf("warning: no mca_coll_tuned_init_tree_fanout_param found?\n");
  }
  if (OMPI_SUCCESS !=
  mca_base_param_lookup_int(mca_coll_tuned_init_chain_fanout_param,
                                                &chain_fanout_default)) {
    printf("warning: no mca_coll_tuned_init_chain_fanout_param found?\n");
  }

 
  /* general n fan out tree */
  data->cached_ntree = ompi_coll_tuned_topo_build_tree (tree_fanout_default, comm, 0); 
  data->cached_ntree_root = 0;
  data->cached_ntree_fanout = tree_fanout_default;

  /* binary tree */
  data->cached_bintree = ompi_coll_tuned_topo_build_tree (2, comm, 0); 
  data->cached_bintree_root = 0;

  /* binomial tree */
  data->cached_bmtree = ompi_coll_tuned_topo_build_bmtree (comm, 0);
  data->cached_bmtree_root = 0;

  /* 
   * chains (fanout followed by pipelines)
   * are more difficuilt as the fan out really really depends on message size [sometimes].. 
   * as size gets larger fan-out gets smaller [usually]
   * 
   * will probably change how we cache this later, for now a midsize
   * GEF
   */
  data->cached_chain = ompi_coll_tuned_topo_build_chain (chain_fanout_default, comm, 0);
  data->cached_chain_root = 0;
  data->cached_chain_fanout = chain_fanout_default;

  /* standard pipeline */
  data->cached_pipeline = ompi_coll_tuned_topo_build_chain (1, comm, 0);
  data->cached_pipeline_root = 0;

  /* All done */

  comm->c_coll_selected_data = data;

  printf("Tuned looks like it is in use :)\n");
  return to_use;
}


/*
 * Finalize module on the communicator
 */
int mca_coll_tuned_module_finalize(struct ompi_communicator_t *comm)
{
  if (NULL == comm->c_coll_selected_module) {
    return OMPI_SUCCESS;
  }

#if OMPI_ENABLE_DEBUG
  /* Reset the reqs to NULL/0 -- they'll be freed as part of freeing
     the generel c_coll_selected_data */

  comm->c_coll_selected_data->mcct_reqs = NULL;
  comm->c_coll_selected_data->mcct_num_reqs = 0;
#endif

  /* free any cached information that has been allocated */
  if (comm->c_coll_selected_data->cached_ntree) { /* destroy general tree if defined */
        ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_ntree);
  }
  if (comm->c_coll_selected_data->cached_bintree) { /* destroy bintree if defined */
        ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_bintree);
  }
  if (comm->c_coll_selected_data->cached_bmtree) { /* destroy bmtree if defined */
        ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_bmtree);
  }
  if (comm->c_coll_selected_data->cached_chain) { /* destroy general chain if defined */
        ompi_coll_tuned_topo_destroy_chain (&comm->c_coll_selected_data->cached_chain);
  }
  if (comm->c_coll_selected_data->cached_pipeline) { /* destroy pipeline if defined */
        ompi_coll_tuned_topo_destroy_chain (&comm->c_coll_selected_data->cached_pipeline);
  }


  /* if allocated memory free it */
  if (comm->c_coll_selected_data) {
    free(comm->c_coll_selected_data);
    comm->c_coll_selected_data = NULL;
  }
  return OMPI_SUCCESS;
}
