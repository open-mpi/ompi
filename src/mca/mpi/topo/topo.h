/*
 * $HEADER
 */

#ifndef MCA_TOPO_H
#define MCA_TOPO_H

#include "lam_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/mpi/base/base.h"
/*
 * Topo module function prototypes
 */
typedef int (*mca_topo_base_init_query_fn_t)(int *thread_min,
                                             int *thread_max);

typedef const struct mca_topo_1_0_0_t * 
  (*mca_topo_base_comm_query_1_0_0_fn_t)(lam_comm_t *comm, int *priority);
/*
 * asfhsfs
 */
typedef int (*mca_topo_base_init_1_0_0_fn_t)
  (lam_comm_t *comm, const struct mca_topo_1_0_0_t **new_topo);

/*
 * Topo module function typedefs
 */
typedef int (*mca_topo_base_cart_coords_fn_t) 
    (lam_communicator_t* comm, int rank, int maxdims, int *coords);

typedef int (*mca_topo_base_cart_create_fn_t)(lam_communicator_t* old_comm, int ndims, 
        int *dims, int *periods, int redorder, lam_communicator_t** comm_cart);

typedef int (*mca_topo_base_cart_get_fn_t)(lam_communicator_t* comm, int maxdims, int *dims,
                         int *periods, int *coords);

typedef int (*mca_topo_base_cartdim_get_fn_t)(lam_communicator_t *comm,
                                              int *ndims);

typedef int (*mca_topo_base_cart_map_fn_t)(lam_communicator_t* comm, int ndims, int *dims,
                        int *periods, int *newrank);

typedef int (*mca_topo_base_cart_rank_fn_t)(lam_communicator_t* comm, int *coords, int *rank);

typedef int (*mca_topo_base_cart_shift_fn_t)(lam_communicator_t* comm, int direction, int disp,
                           int *rank_source, int *rank_dest);

typedef int (*mca_topo_base_cart_sub_fn_t)
            (lam_communicator_t* comm, int *remain_dims, lam_communicator_t** new_comm);

typedef int (*mca_topo_base_graph_create_fn_t)(lam_communicator_t* comm_old, int nnodes, 
             int *index, int *edges, int reorder, lam_communicator_t** comm_graph);

typedef int (*mca_topo_base_graph_get_fn_t)(lam_communicator_t* comm, int maxindex, 
                                   int maxedges, int *index, int *edges);

typedef int (*mca_topo_base_graph_map_fn_t)(lam_communicator_t* comm, int nnodes, int *index, 
                                   int *edges, int *newrank);

typedef int (*mca_topo_base_graph_neighbors_fn_t)(lam_communicator_t* comm, int rank, 
                             int maxneighbors, int *neighbors);

typedef int (*mca_topo_base_graph_neighbors_count_fn_t)
            (lam_communicator_t* comm, int rank, int *nneighbors);


struct mca_topo_1_0_0_t {

    /* Per-communicator initialization and finalization functions */

    mca_topo_base_init_1_0_0_fn_t topo_init;
    mca_topo_base_finalize_fn_t topo_finalize;

    /* Graph related functions */
    mca_topo_base_cart_coords_fn_t topo_cart_coords;
    mca_topo_base_cart_create_fn_t topo_cart_create;
    mca_topo_base_cart_get_fn_t topo_cart_get;
    mca_topo_base_cartdim_get_fn_t topo_cartdim_get;
    mca_topo_base_cart_map_fn_t topo_cart_map;
    mca_topo_base_cart_rank_fn_t topo_cart_rank;
    mca_topo_base_cart_shift_fn_t topo_cart_shift;
    mca_topo_base_cart_sub_fn_t topo_cart_sub;
    mca_topo_base_graph_create_fn_t topo_graph_create;
    mca_topo_base_graph_get_fn_t topo_graph_get;
    mca_topo_base_graph_map_fn_t topo_graph_map;
    mca_topo_base_graph_neighbors_fn_t topo_graph_neighbors;
    mca_topo_base_graph_neighbors_count_fn_t topo_graph_neighbors_count;
};
typedef struct mca_topo_1_0_0_t mca_topo_1_0_0_t;
typedef mca_topo_1_0_0_t mca_topo_t;

struct mca_topo_base_module_1_0_0_t {
    mca_base_module_t topom_version;
    mca_base_module_data_1_0_0_t topom_data;

    /* Intialization functions */
    mca_topo_base_init_query_fn_t topom_init_query;
    mca_topo_base_comm_query_1_0_0_fn_t topom_comm_query;
};
typedef struct mca_topo_base_module_1_0_0_t mca_topo_base_module_1_0_0_t;       
    
/*
 * Macro for use in modules that are of type topo v1.0.0
 */
#define MCA_TOPO_BASE_VERSION_1_0_0 \
  /* topo v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* topo v1.0 */ \
  "topo", 1, 0, 0

/*
 * This function is technically part of the unity module, but since it
 * ships with LAM, and other modules may use the unity module for
 * query/init functionality, prototype this function here.
 */

#if defined(c_plusplus) || defined(__cplusplus)
  extern "C" {
#endif
        const mca_topo_1_0_0_t *
                mca_topo_unity_comm_query(lam_comm_t *comm, int *priority);
#if defined(c_plusplus) || defined(__cplusplus)
  }
#endif
    
#endif /* MCA_TOPO_H */
