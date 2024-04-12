/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennptlee and The University
 *                         of Tennptlee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2020 IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc. All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "src/include/pmix_config.h"
#include "pmix_common.h"

#include "src/include/pmix_globals.h"
#include "src/mca/ptl/ptl.h"
#include "src/mca/ptl/server/ptl_server.h"
#include "src/util/pmix_error.h"

static int component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
PMIX_EXPORT pmix_ptl_base_component_t pmix_mca_ptl_server_component = {
    .base = {
        PMIX_PTL_BASE_VERSION_2_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "server",
        PMIX_MCA_BASE_MAKE_VERSION(component, PMIX_MAJOR_VERSION, PMIX_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_query_component = component_query
    },
    .priority = 30,
    .uri = NULL,
};

static int component_query(pmix_mca_base_module_t **module, int *priority)
{
    /* if I am a server and not a tool, then take me */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer) && !PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) {
        *module = (pmix_mca_base_module_t *) &pmix_ptl_server_module;
        *priority = pmix_mca_ptl_server_component.priority;
        return PMIX_SUCCESS;
    }

    *module = NULL;
    *priority = -1;
    return PMIX_ERR_TAKE_NEXT_OPTION;
}
