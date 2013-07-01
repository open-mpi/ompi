/*
 * Copyright (c) 2011-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 INRIA.  All rights reserved.
 * Copyright (c) 2011-2013 Université Bordeaux 1
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/topo/basic/topo_basic.h"

/*
 * Public string showing the topo basic module version number
 */
const char *mca_topo_basic_component_version_string = 
    "Open MPI basic topology MCA component version" OMPI_VERSION;

/*
 * Local funtions
 */
static int init_query(bool enable_progress_threads, bool enable_mpi_threads);
static struct mca_topo_base_module_t *
comm_query(const ompi_communicator_t *comm, int *priority, uint32_t type);

/*
 * Public component structure
 */
mca_topo_basic_component_t mca_topo_basic_component = 
{
    {
        MCA_TOPO_BASE_VERSION_2_1_0,

        "basic",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,

        /* NULLs for the rest of the function pointers */
    },

    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    init_query,
    comm_query
};


static int init_query(bool enable_progress_threads, bool enable_mpi_threads)
{
    /* Nothing to do */
    return OMPI_SUCCESS;
}      


static struct mca_topo_base_module_t *
comm_query(const ompi_communicator_t *comm, int *priority, uint32_t type)
{
    /* Don't use OBJ_NEW, we need to zero the memory or the functions pointers
     * will not be correctly copied over from the base.
     */
    mca_topo_base_module_t *basic = calloc(1, sizeof(mca_topo_base_module_t));

    if (NULL == basic) {
        return NULL;
    }
    OBJ_CONSTRUCT(basic, mca_topo_base_module_t);

    /* This component has very low priority -- it's a basic, after all! */
    *priority = 0;
    basic->type = type;
    return basic;
}


