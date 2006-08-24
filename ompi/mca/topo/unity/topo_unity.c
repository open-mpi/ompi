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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics. Since linkers generally pull in symbols by object fules,
 * keeping these symbols as the only symbols in this file prevents
 * utility programs such as "ompi_info" from having to import entire
 * modules just to query their version and parameters
 */
#include "ompi_config.h"


#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/topo/unity/topo_unity.h"

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
static mca_topo_base_module_1_0_0_t unity =  {
    mca_topo_unity_module_init, /* initalise after being selected */
    mca_topo_unity_module_finalize, /* close a module on a communicator */
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
    NULL, /* topo_graphdims_get */
    NULL, /* topo_graph_neighbors */
    NULL /* topo_graph_neighbors_count */
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

int mca_topo_unity_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads)
{
    /* Nothing to do */
   
   return OMPI_SUCCESS;
}      

struct mca_topo_base_module_1_0_0_t *
mca_topo_unity_component_comm_query (int *priority)
{
   /* this is the lowest module on the totem pole */
   *priority = 0;

   /* the check as to whether this is an inter communicator 
    * or and intra communicator has to be done before reaching
    * here. this is my solemn opinion. Therefore I am ignoring 
    * the checks here */
   return &unity;
}

int mca_topo_unity_component_comm_unquery (struct ompi_communicator_t *comm)
{    
   /* This function might be needed for some purposes later. for now it
    * does not have anything to do since there are no steps which need 
    * to be undone if this module is not selected */

   return OMPI_SUCCESS;
}

int mca_topo_unity_module_init (struct ompi_communicator_t *comm)
{
   /* This function is used to initialize the module on the communicator. We
    * need to hang the data off of the communicator. For this we still use the
    * same data structure which was defined in topo.h, mca_topo_comm_1_0_0_t. 
    * There are no additional things which we need. If some other module needs
    * to hang additional data, then it has to have this structure as the first
    * member and then extend. This is a must rule */

   struct mca_topo_base_comm_1_0_0_t *topo_data;

   /* allocate the data */

   comm->c_topo_comm = NULL;
   topo_data = (mca_topo_base_comm_1_0_0_t*)malloc(sizeof(struct mca_topo_base_comm_1_0_0_t));
   if (NULL == topo_data) {
       return OMPI_ERROR;
   }

   comm->c_topo_comm = topo_data;
   return OMPI_SUCCESS;
}

   
int mca_topo_unity_module_finalize (struct ompi_communicator_t *comm) 
{
    /* All we need to do for now is to remove the allocated data */
 
    if (NULL != comm->c_topo_comm) {
        free (comm->c_topo_comm);
        comm->c_topo = NULL;
    }

    return OMPI_SUCCESS;
}
