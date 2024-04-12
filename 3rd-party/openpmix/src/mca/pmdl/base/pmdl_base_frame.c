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
#include "src/mca/base/pmix_mca_base_alias.h"
#include "src/mca/pmdl/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/pmdl/base/static-components.h"

/* Instantiate the global vars */
pmix_pmdl_globals_t pmix_pmdl_globals = {
    .lock = PMIX_LOCK_STATIC_INIT,
    .actives = PMIX_LIST_STATIC_INIT,
    .initialized = false,
    .selected = false
};

pmix_pmdl_API_module_t pmix_pmdl = {
    .harvest_envars = pmix_pmdl_base_harvest_envars,
    .parse_file_envars = pmix_pmdl_base_parse_file_envars,
    .setup_nspace = pmix_pmdl_base_setup_nspace,
    .setup_nspace_kv = pmix_pmdl_base_setup_nspace_kv,
    .register_nspace = pmix_pmdl_base_register_nspace,
    .setup_client = pmix_pmdl_base_setup_client,
    .setup_fork = pmix_pmdl_base_setup_fork,
    .deregister_nspace = pmix_pmdl_base_deregister_nspace};

static int pmix_pmdl_register(pmix_mca_base_register_flag_t flags)
{
    if (PMIX_MCA_BASE_REGISTER_STATIC_ONLY == flags) {
        return PMIX_SUCCESS;
    }

    /* Note that we break abstraction rules here by listing a
     specific PMDL here in the base.  This is necessary, however,
     due to extraordinary circumstances:

     1. In PMIx v4.0.1, we want to unify the "ompi5" and "ompi4"
     components to be "ompi" to more closely represent its usage.

     2. The MCA aliasing mechanism was therefore ported from
     OMPI for this purpose. Both the component itself and
     all of its MCA vars are aliased.

     3. However -- at least as currently implemented -- by the time
     individual components are registered, it's too late to make
     aliases.  Hence, if we want to preserve the prior names for
     some semblance of backwards compatibility (and we do!), we
     have to register "ompi" as an "alias for xxx" up here in
     the PMDL base, before any PMDL components are registered.

     This is why we tolerate this abstraction break up here in the
     PMDL component base. */
    (void) pmix_mca_base_alias_register("pmix", "pmdl", "ompi", "ompi5", PMIX_MCA_BASE_ALIAS_FLAG_NONE);
    (void) pmix_mca_base_alias_register("pmix", "pmdl", "ompi", "ompi4", PMIX_MCA_BASE_ALIAS_FLAG_NONE);

    return PMIX_SUCCESS;
}

static pmix_status_t pmix_pmdl_close(void)
{
    pmix_pmdl_base_active_module_t *active, *prev;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_SUCCESS;
    }
    pmix_pmdl_globals.initialized = false;
    pmix_pmdl_globals.selected = false;

    PMIX_LIST_FOREACH_SAFE (active, prev, &pmix_pmdl_globals.actives,
                            pmix_pmdl_base_active_module_t) {
        pmix_list_remove_item(&pmix_pmdl_globals.actives, &active->super);
        if (NULL != active->module->finalize) {
            active->module->finalize();
        }
        PMIX_RELEASE(active);
    }
    PMIX_DESTRUCT(&pmix_pmdl_globals.actives);

    PMIX_DESTRUCT_LOCK(&pmix_pmdl_globals.lock);
    return pmix_mca_base_framework_components_close(&pmix_pmdl_base_framework, NULL);
}

static pmix_status_t pmix_pmdl_open(pmix_mca_base_open_flag_t flags)
{
    /* initialize globals */
    pmix_pmdl_globals.initialized = true;
    PMIX_CONSTRUCT_LOCK(&pmix_pmdl_globals.lock);
    pmix_pmdl_globals.lock.active = false;
    PMIX_CONSTRUCT(&pmix_pmdl_globals.actives, pmix_list_t);

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_pmdl_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pmdl, "PMIx Network Operations", pmix_pmdl_register,
                                pmix_pmdl_open, pmix_pmdl_close, pmix_mca_pmdl_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);

PMIX_CLASS_INSTANCE(pmix_pmdl_base_active_module_t, pmix_list_item_t, NULL, NULL);
