/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2020 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */
#include "src/include/pmix_config.h"

#include "pmix_common.h"

#ifdef HAVE_STRING_H
#    include <string.h>
#endif

#include "src/class/pmix_list.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/pgpu/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/pgpu/base/static-components.h"

/* Instantiate the global vars */
pmix_pgpu_globals_t pmix_pgpu_globals = {
    .actives = PMIX_LIST_STATIC_INIT,
    .nspaces = PMIX_LIST_STATIC_INIT
};
pmix_pgpu_API_module_t pmix_pgpu = {
    .allocate = pmix_pgpu_base_allocate,
    .setup_local = pmix_pgpu_base_setup_local,
    .setup_fork = pmix_pgpu_base_setup_fork,
    .child_finalized = pmix_pgpu_base_child_finalized,
    .local_app_finalized = pmix_pgpu_base_local_app_finalized,
    .deregister_nspace = pmix_pgpu_base_deregister_nspace,
    .collect_inventory = pmix_pgpu_base_collect_inventory,
    .deliver_inventory = pmix_pgpu_base_deliver_inventory
};

static pmix_status_t pmix_pgpu_close(void)
{
    pmix_pgpu_base_active_module_t *active, *prev;

    pmix_pgpu_globals.selected = false;

    PMIX_LIST_FOREACH_SAFE (active, prev, &pmix_pgpu_globals.actives,
                            pmix_pgpu_base_active_module_t) {
        pmix_list_remove_item(&pmix_pgpu_globals.actives, &active->super);
        if (NULL != active->module->finalize) {
            active->module->finalize();
        }
        PMIX_RELEASE(active);
    }
    PMIX_LIST_DESTRUCT(&pmix_pgpu_globals.actives);
    PMIX_LIST_DESTRUCT(&pmix_pgpu_globals.nspaces);

    return pmix_mca_base_framework_components_close(&pmix_pgpu_base_framework, NULL);
}

static pmix_status_t pmix_pgpu_open(pmix_mca_base_open_flag_t flags)
{
    /* initialize globals */
    PMIX_CONSTRUCT(&pmix_pgpu_globals.actives, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_pgpu_globals.nspaces, pmix_list_t);

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_pgpu_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pgpu, "PMIx GPU Operations", NULL, pmix_pgpu_open,
                                pmix_pgpu_close, pmix_mca_pgpu_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);

PMIX_CLASS_INSTANCE(pmix_pgpu_base_active_module_t,
                    pmix_list_item_t,
                    NULL, NULL);
