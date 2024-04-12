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
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
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

#include "prte_config.h"
#include "constants.h"
#include "src/mca/common/alps/common_alps.h"
#include "src/mca/ess/alps/ess_alps.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/ess/ess.h"
#include "src/runtime/prte_globals.h"
#include "src/util/proc_info.h"

#include <sys/syscall.h>

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
prte_ess_base_component_t prte_mca_ess_alps_component = {
    PRTE_ESS_BASE_VERSION_3_0_0,

    /* Component name and version */
    .pmix_mca_component_name = "alps",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PRTE_MAJOR_VERSION,
                               PRTE_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),

    /* Component open and close functions */
    .pmix_mca_open_component = prte_mca_ess_alps_component_open,
    .pmix_mca_close_component = prte_mca_ess_alps_component_close,
    .pmix_mca_query_component = prte_mca_ess_alps_component_query,
};

int prte_mca_ess_alps_component_open(void)
{
    return PRTE_SUCCESS;
}

int prte_mca_ess_alps_component_query(pmix_mca_base_module_t **module, int *priority)
{
    int rc = PRTE_SUCCESS;
    bool flag;

    /*
    /*
     * make sure we're in a Cray PAGG container, and that we are also on
     * a compute node (i.e. we are thought of as an application task by
     * the cray job kernel module  - the thing that creates the PAGG)
     */

    rc = prte_common_alps_proc_in_pagg(&flag);
    if ((PRTE_SUCCESS == rc) && flag) {
        *priority = 35; /* take precendence over base */
        *module = (pmix_mca_base_module_t *) &prte_ess_alps_module;
    }

    return rc;
}

int prte_mca_ess_alps_component_close(void)
{
    return PRTE_SUCCESS;
}
