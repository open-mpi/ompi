/*
 * $HEADER$
 */

#ifndef MCA_TOPO_BASE_H
#define MCA_TOPO_BASE_H

#include "ompi_config.h"

#include "mpi.h"
#include "class/ompi_list.h"
#include "mca/topo/topo.h"
#include "proc/proc.h"

/*
 * All stuff goes in here
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_topo_base_open(void);
    
    int mca_topo_base_close(void);
    
    int mca_topo_base_comm_select(struct ompi_communicator_t *comm,
                                  struct mca_base_component_t *preferred);

    int mca_topo_base_comm_unselect(struct ompi_communicator_t *comm);
    
    int mca_topo_base_find_available (bool *allow_multi_user_threads,
                                      bool *have_hidden_threads);


    int mca_topo_base_init_comm (struct ompi_communicator_t *comm);
    
    int mca_topo_base_get_param (struct ompi_communicator_t *comm, int keyval);

    /*
     * All the glue functions which we will provide to the users by
     * default. The component authors need to only write back-end
     * functions for graph_map() and cart_map() for their topology
     * components.  But they can implement these glue functions if
     * they want.
     */
    int mca_topo_base_cart_coords (struct ompi_communicator_t *comm, 
                                   int rank, 
                                   int maxdims,
                                   int *coords);

    int mca_topo_base_cart_create (mca_topo_base_comm_t *topo_data,
                                   int *proc_count,
                                   ompi_proc_t **proc_pointers,
                                   int *new_rank,
                                   int ndims, 
                                   int *dims,
                                   int *periods, 
                                   bool reorder);

    int mca_topo_base_cartdim_get (struct ompi_communicator_t *comm, 
                                   int *ndims);

    int mca_topo_base_cart_get (struct ompi_communicator_t *comm, 
                                int maxdims, 
                                int *dims,
                                int *periods, 
                                int *coords);

    int mca_topo_base_cart_rank (struct ompi_communicator_t *comm, 
                                 int *coords, 
                                 int *rank);

    int mca_topo_base_cart_shift (struct ompi_communicator_t *comm, 
                                  int direction, 
                                  int disp,
                                  int *rank_source, 
                                  int *rank_dest);
  
    int mca_topo_base_cart_sub (struct ompi_communicator_t *comm, 
                                int *remain_dims,
                                struct ompi_communicator_t **new_comm);
  
    int mca_topo_base_graph_create (mca_topo_base_comm_t *topo_data,
                                    int *proc_count,
                                    ompi_proc_t **proc_pointers,
                                    int *new_rank,
                                    int nnodes,
                                    int *index, 
                                    int *edges,
                                    bool reorder);

    int mca_topo_base_graphdims_get (struct ompi_communicator_t *comm, 
                                     int *nodes,
                                     int *nedges);
  
    int mca_topo_base_graph_get (struct ompi_communicator_t *comm, 
                                 int maxindex, 
                                 int maxedges, 
                                 int *index, 
                                 int *edges);

    int mca_topo_base_graph_neighbors (struct ompi_communicator_t *comm, 
                                       int rank,
                                       int maxneighbors, 
                                       int *neighbors);

    int mca_topo_base_graph_neighbors_count (struct ompi_communicator_t *comm, 
                                             int rank,
                                             int *nneighbors);


/*
 * Globals
 */
extern int mca_topo_base_output;
extern int mca_topo_base_param;

extern ompi_list_t mca_topo_base_components_available;
extern ompi_list_t mca_topo_base_components_opened;

extern bool mca_topo_base_components_opened_valid;
extern bool mca_topo_base_components_available_valid;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_BASE_TOPO_H */
