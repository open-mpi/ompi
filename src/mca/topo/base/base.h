/*
 * $HEADER$
 */

#ifndef MCA_TOPO_BASE_H
#define MCA_TOPO_BASE_H

#include "lam_config.h"

#include "mpi.h"
#include "lfc/lam_list.h"
#include "mca/topo/topo.h"

/*
 * All stuff goes in here
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_topo_base_open(void);
    int mca_topo_base_close(void);
    int mca_topo_base_select(mca_topo_t *selected,
                             bool *allow_multi_user_threads,
                             bool *have_hidden_threads);


    int mca_topo_base_init_comm (MPI_Comm comm);
    int mca_topo_base_get_param (MPI_Comm comm, int keyval);

    const mca_topo_1_0_0_t *
        mca_topo_unity_query(int *priority,
                             bool *allow_multi_user_threads,
                             bool *have_hidden_threads);
    /*
     * All the glue functions which we will provide to the users
     * by default. The users need to only write back-end functions
     * for graph_map() and cart_map() for their topology modules.
     * But they can implement these glue functions if they want.
     */
    int topo_base_cart_coords (MPI_Comm comm, 
                               int rank, 
                               int maxdims,
                               int *coords);

    int topo_base_cart_create (MPI_Comm old_comm, 
                               int ndims, 
                               int *dims,
                               int *periods, 
                               int reorder, 
                               MPI_Comm *comm_cart);

    int topo_base_cartdim_get (MPI_Comm comm, 
                               int *ndims);

    int topo_base_cart_get (MPI_Comm comm, 
                            int maxdims, 
                            int *dims,
                            int *periods, 
                            int *coords);

    int topo_base_cart_rank (MPI_Comm comm, 
                             int *coords, 
                             int *rank);

    int topo_base_cart_shift (MPI_Comm comm, 
                              int direction, 
                              int disp,
                              int *rank_source, 
                              int *rank_dest);

    int topo_base_cart_sub (MPI_Comm comm, 
                            int *remain_dims,
                            MPI_Comm *new_comm);

    int topo_base_graph_create (MPI_Comm comm_old, 
                                int nnodes,
                                int *index, 
                                int *edges,
                                int reorder,
                                MPI_Comm *comm_graph);

    int topo_base_graph_dims_get (MPI_Comm comm, 
                                  int *nodes,
                                  int *nedges);

    int topo_base_graph_get (MPI_Comm comm, 
                             int maxindex, 
                             int maxedges, 
                             int *index, 
                             int *edges);

    int topo_base_graph_neighbors (MPI_Comm comm, 
                                   int rank,
                                   int maxneighbors, 
                                   int *neighbors);

    int topo_base_graph_neighbors_count (MPI_Comm comm, 
                                         int rank,
                                         int *nneighbors);


/*
 * Globals
 */
extern int mca_topo_base_output;
extern lam_list_t mca_topo_base_modules_available;
extern mca_topo_base_module_t mca_topo_base_selected_module;
extern mca_topo_t mca_topo;

#endif /* MCA_BASE_TOPO_H */
