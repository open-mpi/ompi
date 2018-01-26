/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#include "opal/util/show_help.h"

#include "ompi/mca/cid/cid.h"

#include "cid_pmix.h"

static int ompi_cid_pmix_query(ompi_cid_base_module_t **module, int *priority, bool use_mpi_thread_multiple);

/*
 * Struct of function pointers and all that to let us be initialized
 */
ompi_cid_base_component_t mca_cid_pmix_component = {
    .base_version = {
        OMPI_CID_BASE_VERSION_1_0_0,
        .mca_component_name = "pmix",
        MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                              ORTE_RELEASE_VERSION),
    },
    .base_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .query= ompi_cid_pmix_query,
};

static int ompi_cid_pmix_query(ompi_cid_base_module_t **module, int *priority, bool use_mpi_thread_multiple)
{
    *module = &ompi_cid_pmix_module;
    *priority = 20;
    return ORTE_SUCCESS;
}
