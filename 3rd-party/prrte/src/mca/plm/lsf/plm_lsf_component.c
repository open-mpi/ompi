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
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2008      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
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

#include "prte_config.h"
#include "constants.h"

#include <lsf/lsbatch.h>

#include "src/util/pmix_output.h"

#include "plm_lsf.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/plm/base/plm_private.h"
#include "src/mca/plm/plm.h"

/*
 * Public string showing the plm lsf component version number
 */
const char *prte_mca_plm_lsf_component_version_string
    = "PRTE lsf plm MCA component version " PRTE_VERSION;

/*
 * Local function
 */
static int plm_lsf_open(void);
static int plm_lsf_close(void);
static int prte_mca_plm_lsf_component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

prte_mca_plm_lsf_component_t prte_mca_plm_lsf_component = {
    .super = {
        PRTE_PLM_BASE_VERSION_2_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "lsf",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PRTE_MAJOR_VERSION,
                                   PRTE_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_open_component = plm_lsf_open,
        .pmix_mca_close_component = plm_lsf_close,
        .pmix_mca_query_component = prte_mca_plm_lsf_component_query,
    }
};

static int plm_lsf_open(void)
{
    return PRTE_SUCCESS;
}

static int plm_lsf_close(void)
{
    return PRTE_SUCCESS;
}

static int prte_mca_plm_lsf_component_query(pmix_mca_base_module_t **module, int *priority)
{

    /* check if lsf is running here and make sure IBM CSM is NOT enabled */
    if (NULL == getenv("LSB_JOBID") || getenv("CSM_ALLOCATION_ID")
        || lsb_init("PRTE launcher") < 0) {
        /* nope, not here */
        pmix_output_verbose(10, prte_plm_base_framework.framework_output,
                            "plm:lsf: NOT available for selection");
        *module = NULL;
        return PRTE_ERROR;
    }

    *priority = 75;
    *module = (pmix_mca_base_module_t *) &prte_plm_lsf_module;
    return PRTE_SUCCESS;
}
