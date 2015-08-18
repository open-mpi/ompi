/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include "pubsub_orte.h"

static int pubsub_orte_component_register(void);
static int pubsub_orte_component_open(void);
static int pubsub_orte_component_close(void);
static int pubsub_orte_component_query(mca_base_module_t **module, int *priority);

static int my_priority = 50;

ompi_pubsub_orte_component_t mca_pubsub_orte_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        .base_version = {
            OMPI_PUBSUB_BASE_VERSION_2_0_0,

            .mca_component_name = "orte",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_open_component = pubsub_orte_component_open,
            .mca_close_component = pubsub_orte_component_close,
            .mca_query_component = pubsub_orte_component_query,
            .mca_register_component_params = pubsub_orte_component_register,
        },
        .base_data = {
            /* This component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
    }
};

static int pubsub_orte_component_register(void)
{
    my_priority = 50;
    (void) mca_base_component_var_register(&mca_pubsub_orte_component.super.base_version,
                                           "priority", "Priority of the pubsub pmi component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &my_priority);

    mca_pubsub_orte_component.server_uri = NULL;
    (void) mca_base_component_var_register(&mca_pubsub_orte_component.super.base_version,
                                           "server", "Contact info for ompi_server for publish/subscribe operations",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_pubsub_orte_component.server_uri);

    return OMPI_SUCCESS;
}

static int pubsub_orte_component_open(void)
{
    return OMPI_SUCCESS;
}

static int pubsub_orte_component_close(void)
{
    return OMPI_SUCCESS;
}

static int pubsub_orte_component_query(mca_base_module_t **module, int *priority)
{
    mca_pubsub_orte_component.server_found = false;

    *priority = my_priority;
    *module = (mca_base_module_t *) &ompi_pubsub_orte_module;
    return OMPI_SUCCESS;
}
