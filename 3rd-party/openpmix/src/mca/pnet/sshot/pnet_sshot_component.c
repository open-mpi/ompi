/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
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
#include "mca/base/pmix_mca_base_var.h"
#include "pmix_common.h"

#include "src/hwloc/pmix_hwloc.h"
#include "pnet_sshot.h"
#include "src/mca/pnet/pnet.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_show_help.h"

static pmix_status_t component_open(void);
static pmix_status_t component_close(void);
static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority);
static pmix_status_t component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
pmix_pnet_sshot_component_t pmix_mca_pnet_sshot_component = {
    .super = {
        PMIX_PNET_BASE_VERSION_1_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "sshot",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PMIX_MAJOR_VERSION,
                                   PMIX_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_open_component = component_open,
        .pmix_mca_close_component = component_close,
        .pmix_mca_query_component = component_query,
        .pmix_mca_register_component_params = component_register
    },
    .vnid_username = "cxi",
    .credential= NULL,
    .vnid_url = NULL,
    .nodes = NULL,
    .numnodes = 0,
    .ppn = 0
};

static pmix_status_t component_register(void)
{
    pmix_mca_base_component_t *component = &pmix_mca_pnet_sshot_component.super;

    (void) pmix_mca_base_component_var_register(
        component, "vnid_url", "URL for the vnid",
        PMIX_MCA_BASE_VAR_TYPE_STRING,
        &pmix_mca_pnet_sshot_component.vnid_url);

    (void) pmix_mca_base_component_var_register(
        component, "ppn", "Procs per node.",
        PMIX_MCA_BASE_VAR_TYPE_INT,
        &pmix_mca_pnet_sshot_component.ppn);

    (void) pmix_mca_base_component_var_register(
        component, "vnid_username", "The Username for the vnid. Default = \"cxi\"",
        PMIX_MCA_BASE_VAR_TYPE_STRING,
        &pmix_mca_pnet_sshot_component.vnid_username);

    (void) pmix_mca_base_component_var_register(
        component, "credential", "The HPE provided credential, typically found in /etc/vnid/passwd.",
        PMIX_MCA_BASE_VAR_TYPE_STRING,
        &pmix_mca_pnet_sshot_component.credential);

    (void) pmix_mca_base_component_var_register(
        component, "nodes", "The nodes to get group/switch membership for.",
        PMIX_MCA_BASE_VAR_TYPE_STRING,
        &pmix_mca_pnet_sshot_component.nodes);

    (void) pmix_mca_base_component_var_register(
        component, "num_nodes",
        "Number of nodes in query.",
        PMIX_MCA_BASE_VAR_TYPE_INT,
        &pmix_mca_pnet_sshot_component.numnodes);

    return PMIX_SUCCESS;
}

static pmix_status_t component_open(void)
{

    #if 0
    pmix_status_t rc;
    rc = pmix_hwloc_check_vendor(&pmix_globals.topology, 0x17db, 0x208);  // Cray
    if (PMIX_SUCCESS != rc) {
        rc = pmix_hwloc_check_vendor(&pmix_globals.topology, 0x18c8, 0x208);  // Cray
    }
    if (PMIX_SUCCESS != rc) {
        rc = pmix_hwloc_check_vendor(&pmix_globals.topology, 0x1590, 0x208);  // HPE
    }

    return rc;
    #endif
    return PMIX_SUCCESS;
}

static pmix_status_t component_close(void)
{
    return PMIX_SUCCESS;
}

static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority)
{
    *priority = 0;
    if (!PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
        /* only servers are supported */
        *module = NULL;
        return PMIX_ERROR;
    }

    *module = (pmix_mca_base_module_t *) &pmix_sshot_module;
    return PMIX_SUCCESS;
}
