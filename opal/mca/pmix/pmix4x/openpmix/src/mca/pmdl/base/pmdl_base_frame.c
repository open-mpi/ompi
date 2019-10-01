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
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */
#include <src/include/pmix_config.h>

#include <pmix_common.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "src/class/pmix_list.h"
#include "src/mca/base/base.h"
#include "src/mca/pmdl/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/pmdl/base/static-components.h"

/* Instantiate the global vars */
pmix_pmdl_globals_t pmix_pmdl_globals = {{0}};
pmix_pmdl_API_module_t pmix_pmdl = {
    .harvest_envars = pmix_pmdl_base_harvest_envars,
    .setup_nspace = pmix_pmdl_base_setup_nspace,
    .setup_nspace_kv = pmix_pmdl_base_setup_nspace_kv,
    .setup_client = pmix_pmdl_base_setup_client,
    .setup_fork = pmix_pmdl_base_setup_fork,
    .deregister_nspace = pmix_pmdl_base_deregister_nspace
};

static pmix_status_t pmix_pmdl_close(void)
{
  pmix_pmdl_base_active_module_t *active, *prev;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_SUCCESS;
    }
    pmix_pmdl_globals.initialized = false;

    PMIX_LIST_FOREACH_SAFE(active, prev, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
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

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pmdl, "PMIx Network Operations",
                                NULL, pmix_pmdl_open, pmix_pmdl_close,
                                mca_pmdl_base_static_components, 0);

PMIX_CLASS_INSTANCE(pmix_pmdl_base_active_module_t,
                    pmix_list_item_t,
                    NULL, NULL);
