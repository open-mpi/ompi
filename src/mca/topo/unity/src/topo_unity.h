/** @file
 *
 *
 */
/*
 * $HEADER$
 */

#ifndef MCA_TOPO_UNTIY_H
#define MCA_TOPO_UNTIY_H

#include "threads/condition.h"
#include "class/ompi_free_list.h"
#include "util/cmd_line.h"
#include "request/request.h"
#include "mca/topo/topo.h"

/*
 * This structure is the interface to the MCA world. It contains the
 * version information and the four functions (see below) which
 * are needed for this module to function with the MCA framework
 */
extern const struct mca_topo_base_module_1_0_0_t mca_topo_unity_module;

/*
 * ******************************************************************
 * ******** functions which provide MCA interface comppliance *******
 * ******************************************************************
 * These functions are:
 *       - mca_topo_unity_module_open
 *       - mca_topo_unity_module_close
 *       - mca_topo_unity_module_query
 *       - mca_topo_unity_module_finalize
 * These functions are always found on the mca_topo_unity_module
 * structure. They are the "meta" functions to ensure smooth op.
 * ******************************************************************
 */
#if defined(__cplusplus) || defined(c_plusplus)
    extern "C" {
#endif
        int mca_topo_unity_component_init_query(bool *allow_multi_user_threads,
                                                bool *have_hidden_threads);
        struct mca_topo_base_module_1_0_0_t *
            mca_topo_unity_component_comm_query (int *priority);
        int mca_topo_unity_component_comm_unquery (struct ompi_communicator_t *comm);

        int mca_topo_unity_module_init (struct ompi_communicator_t *comm);
        int mca_topo_unity_module_finalize (struct ompi_communicator_t *comm);
#if defined(__cplusplus) || defined(c_plusplus)
    }
#endif
/*
 * ******************************************************************
 * ********************* meta functions end *************************
 * ******************************************************************
 */ 

/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 * This module defines just 2 functions:
 *      - graph_map
 *      - cart_map
 * rest of the functions are filled in from the "base" module. Authors
 * of other such topology modules are required to define only these 2
 * functions. They are ofcourse free to implement all of them too :-)
 * ******************************************************************
 */ 
#if defined(__cplusplus) || defined(c_plusplus)
    extern "C" {
#endif
        int mca_topo_unity_cart_map (struct ompi_communicator_t *comm,
                                     int ndims,
                                     int *dims,
                                     int *periods,
                                     int *newrank);

        int mca_topo_unity_graph_map (struct ompi_communicator_t *comm,
                                      int nnodes,
                                      int *index,
                                      int *edges,
                                      int *newrank);
#if defined(__cplusplus) || defined(c_plusplus)
    }
#endif
/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */ 
                                     
#endif /* MCA_TOPO_UNITY_H */
