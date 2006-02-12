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

#ifndef MCA_TOPO_BASE_H
#define MCA_TOPO_BASE_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/proc/proc.h"

/*
 * All stuff goes in here
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC    int mca_topo_base_open(void);
    
OMPI_DECLSPEC    int mca_topo_base_close(void);
    
OMPI_DECLSPEC    int mca_topo_base_comm_select(struct ompi_communicator_t *comm,
                                  struct mca_base_component_t *preferred);

OMPI_DECLSPEC    int mca_topo_base_comm_unselect(struct ompi_communicator_t *comm);
    
OMPI_DECLSPEC    int mca_topo_base_find_available (bool enable_progress_threads,
                                                   bool enable_mpi_threads);


OMPI_DECLSPEC    int mca_topo_base_init_comm (struct ompi_communicator_t *comm);
    
OMPI_DECLSPEC    int mca_topo_base_get_param (struct ompi_communicator_t *comm, int keyval);

    /*
     * All the glue functions which we will provide to the users by
     * default. The component authors need to only write back-end
     * functions for graph_map() and cart_map() for their topology
     * components.  But they can implement these glue functions if
     * they want.
     */
OMPI_DECLSPEC    int mca_topo_base_cart_coords (struct ompi_communicator_t *comm, 
                                   int rank, 
                                   int maxdims,
                                   int *coords);

OMPI_DECLSPEC    int mca_topo_base_cart_create (mca_topo_base_comm_t *topo_data,
                                   int *proc_count,
                                   ompi_proc_t **proc_pointers,
                                   int *new_rank,
                                   int ndims, 
                                   int *dims,
                                   int *periods, 
                                   bool reorder);

OMPI_DECLSPEC    int mca_topo_base_cartdim_get (struct ompi_communicator_t *comm, 
                                   int *ndims);

OMPI_DECLSPEC    int mca_topo_base_cart_get (struct ompi_communicator_t *comm, 
                                int maxdims, 
                                int *dims,
                                int *periods, 
                                int *coords);

OMPI_DECLSPEC    int mca_topo_base_cart_rank (struct ompi_communicator_t *comm, 
                                 int *coords, 
                                 int *rank);

OMPI_DECLSPEC    int mca_topo_base_cart_shift (struct ompi_communicator_t *comm, 
                                  int direction, 
                                  int disp,
                                  int *rank_source, 
                                  int *rank_dest);
  
OMPI_DECLSPEC    int mca_topo_base_cart_sub (struct ompi_communicator_t *comm, 
                                int *remain_dims,
                                struct ompi_communicator_t **new_comm);
  
OMPI_DECLSPEC    int mca_topo_base_graph_create (mca_topo_base_comm_t *topo_data,
                                    int *proc_count,
                                    ompi_proc_t **proc_pointers,
                                    int *new_rank,
                                    int nnodes,
                                    int *index, 
                                    int *edges,
                                    bool reorder);

OMPI_DECLSPEC    int mca_topo_base_graphdims_get (struct ompi_communicator_t *comm, 
                                     int *nodes,
                                     int *nedges);
  
OMPI_DECLSPEC    int mca_topo_base_graph_get (struct ompi_communicator_t *comm, 
                                 int maxindex, 
                                 int maxedges, 
                                 int *index, 
                                 int *edges);

OMPI_DECLSPEC    int mca_topo_base_graph_neighbors (struct ompi_communicator_t *comm, 
                                       int rank,
                                       int maxneighbors, 
                                       int *neighbors);

OMPI_DECLSPEC    int mca_topo_base_graph_neighbors_count (struct ompi_communicator_t *comm, 
                                             int rank,
                                             int *nneighbors);


/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_topo_base_output;
OMPI_DECLSPEC extern int mca_topo_base_param;

OMPI_DECLSPEC extern opal_list_t mca_topo_base_components_available;
OMPI_DECLSPEC extern opal_list_t mca_topo_base_components_opened;

OMPI_DECLSPEC extern bool mca_topo_base_components_opened_valid;
OMPI_DECLSPEC extern bool mca_topo_base_components_available_valid;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_BASE_TOPO_H */
