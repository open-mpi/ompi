/*
 * $HEADER$
 */
#include "lam_config.h"
#include "mca/mpi/topo/topo.h"
#include "mca/mpi/topo/base/base.h"


static const mca_topo_t unity {

    /*
     * Per-communicator initialization and finalization functions 
     */
    mca_topo_unity_init,
    mca_topo_unity_finalize,
    
    NULL, /* topo_cart_coords */
    NULL, /* topo_cart_create */
    NULL, /* topo_cart_get */
    NULL, /* topo_cartdim_get */
    mca_topo_unity_cart_map,
    NULL, /* topo_cart_rank */
    NULL, /* topo_cart_shift */
    NULL, /* topo_cart_sub */
    NULL, /* topo_graph_create */
    NULL, /* topo_graph_get */
    mca_topo_unity_graph_map,
    NULL, /* topo_graph_neighbors */
    NULL, /* topo_graph_neighbors_count */
};

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides
 */
int mca_topo_unity_init_query (int *thread_min, int *thread_max) {
   
    *thread_min = MPI_THREAD_SINGLE;
    *thread_max = MPI_THREAD_MULTIPLE;
    
    return LAM_SUCCESS;
}

/*
 * Init on the communicator
 */
int mca_topo_unity_init (lam_communicator_t *comm, 
                         const mca_topo_1_0_0_t **new_topo) {
    /* 
     * Nothing to init on the communicator
     */
    return LAM_SUCCESS;
}

/*
 * Init on the communicator
 */
int mca_topo_unity_finalize (lam_communicator_t *comm) {

    /* 
     * Nothing to finalize on the communicator
     */
    return LAM_SUCCESS;
}

int mca_topo_1_0_0_t *mca_topo_unity_comm_query (lam_communicator_t *comm,
                                                 int *priority) {

    /*
     * This is the unity module and hence the priority is 0
     */
    *priority = 0;
    return &unity;
}
