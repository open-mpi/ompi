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
#include "src/mca/pnet/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/pnet/base/static-components.h"

/* Instantiate the global vars */
pmix_pnet_globals_t pmix_pnet_globals = {
    .actives = PMIX_LIST_STATIC_INIT,
    .fabrics = PMIX_LIST_STATIC_INIT
};
pmix_pnet_API_module_t pmix_pnet = {
    .allocate = pmix_pnet_base_allocate,
    .setup_local_network = pmix_pnet_base_setup_local_network,
    .setup_fork = pmix_pnet_base_setup_fork,
    .child_finalized = pmix_pnet_base_child_finalized,
    .local_app_finalized = pmix_pnet_base_local_app_finalized,
    .deregister_nspace = pmix_pnet_base_deregister_nspace,
    .collect_inventory = pmix_pnet_base_collect_inventory,
    .deliver_inventory = pmix_pnet_base_deliver_inventory,
    .register_fabric = pmix_pnet_base_register_fabric,
    .update_fabric = pmix_pnet_base_update_fabric,
    .deregister_fabric = pmix_pnet_base_deregister_fabric
};

static pmix_status_t pmix_pnet_close(void)
{
    pmix_pnet_base_active_module_t *active, *prev;

    pmix_pnet_globals.selected = false;

    PMIX_LIST_FOREACH_SAFE (active, prev, &pmix_pnet_globals.actives,
                            pmix_pnet_base_active_module_t) {
        pmix_list_remove_item(&pmix_pnet_globals.actives, &active->super);
        if (NULL != active->module->finalize) {
            active->module->finalize();
        }
        PMIX_RELEASE(active);
    }
    PMIX_LIST_DESTRUCT(&pmix_pnet_globals.actives);
    PMIX_LIST_DESTRUCT(&pmix_pnet_globals.fabrics);
    PMIX_LIST_DESTRUCT(&pmix_pnet_globals.nspaces);

    return pmix_mca_base_framework_components_close(&pmix_pnet_base_framework, NULL);
}

static pmix_status_t pmix_pnet_open(pmix_mca_base_open_flag_t flags)
{
    /* initialize globals */
    PMIX_CONSTRUCT(&pmix_pnet_globals.actives, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_pnet_globals.fabrics, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_pnet_globals.nspaces, pmix_list_t);

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_pnet_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pnet, "PMIx Network Operations", NULL, pmix_pnet_open,
                                pmix_pnet_close, pmix_mca_pnet_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);

PMIX_CLASS_INSTANCE(pmix_pnet_base_active_module_t, pmix_list_item_t, NULL, NULL);

static void ftcon(pmix_pnet_fabric_t *p)
{
    p->name = NULL;
    p->index = 0;
    p->module = NULL;
    p->payload = NULL;
}
static void ftdes(pmix_pnet_fabric_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
}
PMIX_CLASS_INSTANCE(pmix_pnet_fabric_t, pmix_list_item_t, ftcon, ftdes);
