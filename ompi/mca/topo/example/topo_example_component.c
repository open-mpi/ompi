/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/topo/example/topo_example.h"

/*
 * Public string showing the topo example module version number
 */
const char *mca_topo_example_component_version_string =
    "Open MPI example topology MCA component version" OMPI_VERSION;

/*
 * Local funtions
 */
static int init_query(bool enable_progress_threads, bool enable_mpi_threads);
static struct mca_topo_base_module_t *
comm_query(const ompi_communicator_t *comm, int *priority, uint32_t type);

/*
 * Public component structure
 */
mca_topo_base_component_2_2_0_t mca_topo_example_component =
{
    .topoc_version = {
        MCA_TOPO_BASE_VERSION_2_2_0,

        .mca_component_name = "example",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        /* NULLs for the rest of the function pointers */
    },

    .topoc_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    .topoc_init_query = init_query,
    .topoc_comm_query = comm_query,
};


static int init_query(bool enable_progress_threads, bool enable_mpi_threads)
{
    /* Nothing to do */

    return OMPI_SUCCESS;
}


static struct mca_topo_base_module_t *
comm_query(const ompi_communicator_t *comm, int *priority, uint32_t type)
{
    mca_topo_example_module_t *example = OBJ_NEW(mca_topo_example_module_t);
    if (NULL == example) {
        return NULL;
    }
    if( OMPI_COMM_CART == type ) {
        example->super.topo.cart.cart_map = mca_topo_example_cart_map;
    } else if( OMPI_COMM_GRAPH == type ) {
        example->super.topo.graph.graph_map = mca_topo_example_graph_map;
    }

    /* This component has very low priority -- it's an example, after all! */
    *priority = 0;
    example->super.type = type;
    return &(example->super);
}


