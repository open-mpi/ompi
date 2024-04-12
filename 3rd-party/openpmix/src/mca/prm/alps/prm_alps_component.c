/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
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

#include "prm_alps.h"
#include "src/mca/prm/prm.h"

static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
pmix_prm_base_component_t pmix_mca_prm_alps_component = {
    PMIX_PRM_BASE_VERSION_1_0_0,

    /* Component name and version */
    .pmix_mca_component_name = "alps",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PMIX_MAJOR_VERSION,
                               PMIX_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),

    /* Component open and close functions */
    .pmix_mca_query_component = component_query
};

static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority)
{
    if (PMIX_PEER_IS_CLIENT(pmix_globals.mypeer)) {
        *priority = 0;
        *module = NULL;
        return PMIX_ERROR;
    }

#if CRAY_WLM_DETECT
    char slurm[] = "SLURM";
    char *wlm_detected = NULL;

    wlm_detected = wlm_detect_get_active();

    /*
     * The content of wlm_detected.h indicates wlm_detect_get_active
     * may return NULL upon failure.  Resort to the suggested plan
     * B in that event.
     */

    if (NULL == wlm_detected) {
        wlm_detected = (char *) wlm_detect_get_default();
        PMIX_OUTPUT_VERBOSE((10, pmix_prm_base_framework.framework_output,
                             "%s prm:alps: wlm_detect_get_active returned NULL, using %s",
                             PMIX_NAME_PRINT(&pmix_globals.myid), wlm_detected));
    }

    if ((NULL != wlm_detected) && !strcmp(slurm, wlm_detected)) {
        /* we are in a Cray SLURM environment, so we don't want
         * this plm component */
        *priority = 0;
        *module = NULL;
        return PMIX_ERROR;
    }
#endif
    *priority = 20;
    *module = (pmix_mca_base_module_t *) &pmix_prm_alps_module;
    return PMIX_SUCCESS;
}
