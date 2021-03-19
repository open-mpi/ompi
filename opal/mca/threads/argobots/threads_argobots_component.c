/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/threads/argobots/threads_argobots.h"
#include "opal/mca/threads/thread.h"
#include "opal/mca/threads/threads.h"

static int opal_threads_argobots_open(void);

const opal_threads_base_component_1_0_0_t mca_threads_argobots_component = {
    /* First, the mca_component_t struct containing meta information
     * about the component itself */
    .threadsc_version =
        {
            OPAL_THREADS_BASE_VERSION_1_0_0,

            /* Component name and version */
            .mca_component_name = "argobots",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            .mca_open_component = opal_threads_argobots_open,
        },
    .threadsc_data =
        {/* The component is checkpoint ready */
         MCA_BASE_METADATA_PARAM_CHECKPOINT},
};

int opal_threads_argobots_open(void)
{
    opal_threads_argobots_ensure_init();
    return OPAL_SUCCESS;
}
