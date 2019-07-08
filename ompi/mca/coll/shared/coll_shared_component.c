/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Most of the description of the data layout is in the
 * coll_shared_module.c file.
 */

#include "ompi_config.h"

#include "opal/util/show_help.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "coll_shared.h"


/*
 * Public string showing the coll ompi_shared component version number
 */
const char *mca_coll_shared_component_version_string =
    "Open MPI shared collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */
static int shared_close(void);
static int shared_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_shared_component_t mca_coll_shared_component = {

    /* First, fill in the super */

    {
     /* First, the mca_component_t struct containing meta
        information about the component itself */

     .collm_version = {
                       MCA_COLL_BASE_VERSION_2_0_0,

                       /* Component name and version */
                       .mca_component_name = "shared",
                       MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION,
                                             OMPI_MINOR_VERSION,
                                             OMPI_RELEASE_VERSION),

                       /* Component functions */
                       .mca_close_component = shared_close,
                       .mca_register_component_params = shared_register,
                       },
     .collm_data = {
                    /* The component is not checkpoint ready */
                    MCA_BASE_METADATA_PARAM_NONE},

     /* Initialization / querying functions */

     .collm_init_query = mca_coll_shared_init_query,
     .collm_comm_query = mca_coll_shared_comm_query,
     },

    /* shared-component specifc information */

    /* (default) priority */
    /* JMS temporarily lowered until we can get more testing */
    0,
};


/*
 * Shut down the component
 */
static int shared_close(void)
{
    return OMPI_SUCCESS;
}


/*
 * Register MCA params
 */
static int shared_register(void)
{
    mca_base_component_t *c =
        &mca_coll_shared_component.super.collm_version;
    mca_coll_shared_component_t *cs = &mca_coll_shared_component;

    /* If we want to be selected (i.e., all procs on one node), then
       we should have a high priority */

    cs->shared_priority = 0;

    (void) mca_base_component_var_register(c, "priority",
                                           "Priority of the shared coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->shared_priority);
    return OMPI_SUCCESS;
}
