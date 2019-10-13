/**
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/util/show_help.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "coll_solo.h"


/**
 * Public string showing the coll ompi_solo component version number
 */
const char *mca_coll_solo_component_version_string =
    "Open MPI solo collective MCA component version " OMPI_VERSION;

/**
 * Local functions
 */
static int solo_close(void);
static int solo_register(void);

/**
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_coll_solo_component_t mca_coll_solo_component = {

    /* First, fill in the super */

    {
     /* First, the mca_component_t struct containing meta
        information about the component itself */
     .collm_version = {
                       MCA_COLL_BASE_VERSION_2_0_0,

                       /* Component name and version */
                       .mca_component_name = "solo",
                       MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION,
                                             OMPI_MINOR_VERSION,
                                             OMPI_RELEASE_VERSION),

                       /* Component functions */
                       .mca_close_component = solo_close,
                       .mca_register_component_params = solo_register,
                       },
     .collm_data = {
                    /* The component is not checkpoint ready */
                    MCA_BASE_METADATA_PARAM_NONE},

     /* Initialization / querying functions */
     .collm_init_query = mca_coll_solo_init_query,
     .collm_comm_query = mca_coll_solo_comm_query,
     },

    /* Shared-component specifc information */

    /* (default) priority */
    0,
    /* (default) static_block_size */
    4096,
    /* (default) mpool_small_block_size */
    1048576,
    /* (default) mpool_small_block_num */
    0,
    /* (default) mpool_large_block_size */
    8388608,
    /* (default) mpool_large_block_num */
    0,
    /* (default) pointer to the shared mpool */
    NULL
};

/**
 * Shut down the component
 */
static int solo_close(void)
{
    return OMPI_SUCCESS;
}

/**
 * Register MCA params
 */
static int solo_register(void)
{
    mca_base_component_t *c = &mca_coll_solo_component.super.collm_version;
    mca_coll_solo_component_t *cs = &mca_coll_solo_component;

    /**
     * If we want to be selected (i.e., all procs on one node), then we should have a high 
     * priority.
     */
    cs->solo_priority = 0;
    (void) mca_base_component_var_register(c, "priority",
                                           "Priority of the solo coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->solo_priority);

    cs->static_block_size = 4096;
    (void) mca_base_component_var_register(c, "static_block_size",
                                           "static block size of the static window",
                                           MCA_BASE_VAR_TYPE_UINT32_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->static_block_size);

    cs->mpool_small_block_size = 1048576;
    (void) mca_base_component_var_register(c, "mpool_small_block_size",
                                           "small block size of the mpool",
                                           MCA_BASE_VAR_TYPE_UINT32_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->mpool_small_block_size);

    cs->mpool_small_block_num = 0;
    (void) mca_base_component_var_register(c, "mpool_small_block_num",
                                           "number of small blocks of the mpool",
                                           MCA_BASE_VAR_TYPE_UINT32_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->mpool_small_block_num);

    cs->mpool_large_block_size = 8388608;
    (void) mca_base_component_var_register(c, "mpool_large_block_size",
                                           "large block size of the mpool",
                                           MCA_BASE_VAR_TYPE_UINT32_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->mpool_large_block_size);

    cs->mpool_large_block_num = 0;
    (void) mca_base_component_var_register(c, "mpool_large_block_num",
                                           "number of large blocks of the mpool",
                                           MCA_BASE_VAR_TYPE_UINT32_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &cs->mpool_large_block_num);

    return OMPI_SUCCESS;
}
