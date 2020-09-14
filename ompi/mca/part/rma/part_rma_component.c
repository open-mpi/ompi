/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2020 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "part_rma.h"
#include "opal/mca/event/event.h"

#include "part_rma_sendreq.h"
#include "part_rma_recvreq.h"
#include "part_rma_component.h"

static int mca_part_rma_component_register(void);
static int mca_part_rma_component_open(void);
static int mca_part_rma_component_close(void);
static mca_part_base_module_t* mca_part_rma_component_init( int* priority,
                            bool enable_progress_threads, bool enable_mpi_threads);
static int mca_part_rma_component_fini(void);

mca_part_base_component_4_0_0_t mca_part_rma_component = {
    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    .partm_version = {
        MCA_PART_BASE_VERSION_2_0_0,

        .mca_component_name = "rma",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_open_component = mca_part_rma_component_open,
        .mca_close_component = mca_part_rma_component_close,
        .mca_register_component_params = mca_part_rma_component_register,
    },
    .partm_data = {
        /* This component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },

    .partm_init = mca_part_rma_component_init,
    .partm_finalize = mca_part_rma_component_fini,
};

static int
mca_part_rma_component_register(void)
{
    ompi_part_rma.free_list_num = 4;
    (void) mca_base_component_var_register(&mca_part_rma_component.partm_version, "free_list_num",
                                           "Initial size of request free lists",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_part_rma.free_list_num);

    ompi_part_rma.free_list_max = -1;
    (void) mca_base_component_var_register(&mca_part_rma_component.partm_version, "free_list_max",
                                           "Maximum size of request free lists",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_part_rma.free_list_max);

    ompi_part_rma.free_list_inc = 64;
    (void) mca_base_component_var_register(&mca_part_rma_component.partm_version, "free_list_inc",
                                           "Number of elements to add when growing request free lists",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_part_rma.free_list_inc);


    return OPAL_SUCCESS;
}

static int
mca_part_rma_component_open(void)
{
    OBJ_CONSTRUCT(&ompi_part_rma.lock, opal_mutex_t);
 
    mca_part_rma_init_lists();

    return OMPI_SUCCESS;
}


static int
mca_part_rma_component_close(void)
{
    OBJ_DESTRUCT(&ompi_part_rma.lock);
    return OMPI_SUCCESS; 
}


static mca_part_base_module_t*
mca_part_rma_component_init(int* priority,
                            bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    *priority = 1;

    opal_output_verbose( 10, 0,
                         "in rma part priority is %d\n", *priority);
    return &ompi_part_rma.super;
}


static int
mca_part_rma_component_fini(void)
{
    return OMPI_SUCCESS;
}

