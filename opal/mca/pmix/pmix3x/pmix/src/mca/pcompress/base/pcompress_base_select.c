/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#ifdef HAVE_UNISTD_H
#include "unistd.h"
#endif

#include "include/pmix_common.h"
#include "src/util/output.h"
#include "src/mca/mca.h"
#include "src/mca/base/base.h"
#include "src/mca/pcompress/base/base.h"

int pmix_compress_base_select(void)
{
    int ret = PMIX_SUCCESS;
    pmix_compress_base_component_t *best_component = NULL;
    pmix_compress_base_module_t *best_module = NULL;

    if (pmix_compress_base.selected) {
        /* ensure we don't do this twice */
        return PMIX_SUCCESS;
    }
    pmix_compress_base.selected = true;
    /*
     * Select the best component
     */
    if( PMIX_SUCCESS != pmix_mca_base_select("pcompress", pmix_pcompress_base_framework.framework_output,
                                             &pmix_pcompress_base_framework.framework_components,
                                             (pmix_mca_base_module_t **) &best_module,
                                             (pmix_mca_base_component_t **) &best_component, NULL) ) {
        /* This will only happen if no component was selected,
         * in which case we use the default one */
        goto cleanup;
    }

    /* Initialize the winner */
    if (NULL != best_module) {
        if (PMIX_SUCCESS != (ret = best_module->init()) ) {
            goto cleanup;
        }
        pmix_compress = *best_module;
    }

 cleanup:
    return ret;
}
