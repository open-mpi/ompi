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

#include "dpm_orte.h"

static int dpm_orte_component_open(void);
static int dpm_orte_component_close(void);
static int dpm_orte_component_query(mca_base_module_t **module, int *priority);

ompi_dpm_base_component_t mca_dpm_orte_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    .base_version = {
        OMPI_DPM_BASE_VERSION_2_0_0,
    
        .mca_component_name = "orte",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_open_component = dpm_orte_component_open,
        .mca_close_component = dpm_orte_component_close,
        .mca_query_component = dpm_orte_component_query,
    },
    .base_data = {
        /* This component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};


int dpm_orte_component_open(void)
{
    return OMPI_SUCCESS;
}

int dpm_orte_component_close(void)
{
    return OMPI_SUCCESS;
}

static int dpm_orte_component_query(mca_base_module_t **module, int *priority)
{
    *priority = 50;
    *module = (mca_base_module_t *) &ompi_dpm_orte_module;
    return OMPI_SUCCESS;
}
