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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

#include "pmix_config.h"

#include "pmix_common.h"
#include "pstat_test.h"
#include "src/mca/pstat/pstat.h"

/*
 * Public string showing the pstat ompi_test component version number
 */
const char *pmix_pstat_test_component_version_string
    = "PMIX test pstat MCA component version " PMIX_VERSION;

/*
 * Local function
 */
static int pstat_test_component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const pmix_pstat_base_component_t pmix_mca_pstat_test_component = {
    /* Indicate that we are a pstat v1.1.0 component (which also
       implies a specific MCA version) */

    PMIX_PSTAT_BASE_VERSION_1_0_0,

    /* Component name and version */

    .pmix_mca_component_name = "test",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PMIX_MAJOR_VERSION,
                               PMIX_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),

    .pmix_mca_query_component = pstat_test_component_query,
};

static int pstat_test_component_query(pmix_mca_base_module_t **module, int *priority)
{
    *priority = 20;
    *module = (pmix_mca_base_module_t *) &pmix_pstat_test_module;

    return PMIX_SUCCESS;
}
