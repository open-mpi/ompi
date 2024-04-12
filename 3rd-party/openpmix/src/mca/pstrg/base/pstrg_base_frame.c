/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "pmix_common.h"

#include <pthread.h>
#include <event.h>

#include "src/class/pmix_list.h"
#include "src/include/pmix_types.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/runtime/pmix_progress_threads.h"

#include "src/mca/pstrg/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/pstrg/base/static-components.h"

/*
 * Global variables
 */
pmix_pstrg_API_module_t pmix_pstrg = {
    .query = pmix_pstrg_base_query
};

pmix_pstrg_base_t pmix_pstrg_base = {
    .actives = PMIX_LIST_STATIC_INIT,
    .evbase = NULL,
    .selected = false,
    .init = false
};

static int pmix_pstrg_base_close(void)
{
    pmix_pstrg_active_module_t *active;

    if (!pmix_pstrg_base.init || !pmix_pstrg_base.selected) {
        return PMIX_SUCCESS;
    }
    pmix_pstrg_base.init = false;
    pmix_pstrg_base.selected = false;

    PMIX_LIST_FOREACH (active, &pmix_pstrg_base.actives, pmix_pstrg_active_module_t) {
        if (NULL != active->module->finalize) {
            active->module->finalize();
        }
    }
    PMIX_LIST_DESTRUCT(&pmix_pstrg_base.actives);

    /* Close all remaining available components */
    return pmix_mca_base_framework_components_close(&pmix_pstrg_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int pmix_pstrg_base_open(pmix_mca_base_open_flag_t flags)
{
    if (pmix_pstrg_base.init) {
        return PMIX_SUCCESS;
    }
    pmix_pstrg_base.init = true;

    /* construct the list of modules */
    PMIX_CONSTRUCT(&pmix_pstrg_base.actives, pmix_list_t);

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_pstrg_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pstrg, "PMIx Storage Support", NULL, pmix_pstrg_base_open,
                                pmix_pstrg_base_close, pmix_mca_pstrg_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);

PMIX_CLASS_INSTANCE(pmix_pstrg_active_module_t, pmix_list_item_t, NULL, NULL);

static void qcon(pmix_pstrg_query_results_t *p)
{
    PMIX_CONSTRUCT(&p->results, pmix_list_t);
}
static void qdes(pmix_pstrg_query_results_t *p)
{
    PMIX_LIST_DESTRUCT(&p->results);
}
PMIX_CLASS_INSTANCE(pmix_pstrg_query_results_t, pmix_list_item_t, qcon, qdes);
