/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_nbpreq.h"

/*
 * Public string showing the coll ompi_nbpreq component version number
 */
const char *mca_coll_nbpreq_component_version_string =
    "Open MPI nbpreq collective MCA component version " OMPI_VERSION;

/*
 * Global variables
 */
int mca_coll_nbpreq_priority = 10;

/*
 * Local functions
 */
static int nbpreq_open(void);
static int nbpreq_close(void);
static int nbpreq_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_nbpreq_component_t mca_coll_nbpreq_component = {

    /* First, fill in the super */
    {
    /* First, the mca_component_t struct containing meta information
     * about the component itself */

        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "nbpreq",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component            = nbpreq_open,
            .mca_close_component           = nbpreq_close,
            .mca_register_component_params = nbpreq_register,
        },
        .collm_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Initialization / querying functions */

        .collm_init_query = mca_coll_nbpreq_init_query,
        .collm_comm_query = mca_coll_nbpreq_comm_query,
    },

    /* priority of the module */
    0,
};

static int nbpreq_open(void)
{
    int rc;

    OBJ_CONSTRUCT(&mca_coll_nbpreq_component.free_requests, opal_free_list_t);

    rc = opal_free_list_init (&mca_coll_nbpreq_component.free_requests,
                               sizeof(mca_coll_nbpreq_request_t), 8,
                               OBJ_CLASS(mca_coll_nbpreq_request_t),
                               0, 0, 0, -1, 8, NULL, 0, NULL, NULL, NULL);

    return OMPI_SUCCESS;
}
static int nbpreq_close(void)
{
    OBJ_DESTRUCT(&mca_coll_nbpreq_component.free_requests);

    return OMPI_SUCCESS;
}

static int
nbpreq_register(void)
{
    /* Use a low priority, but allow other components to be lower */

    mca_coll_nbpreq_component.priority = 10;
    (void) mca_base_component_var_register(&mca_coll_nbpreq_component.super.collm_version, "priority",
                                           "Priority of the nbpreq coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_nbpreq_component.priority);

    return OMPI_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_coll_nbpreq_module_t,
                   mca_coll_base_module_t,
                   NULL,
                   NULL);
