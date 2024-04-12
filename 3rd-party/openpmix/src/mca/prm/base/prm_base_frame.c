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
#include "src/mca/prm/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/prm/base/static-components.h"

/* Instantiate the global vars */
pmix_prm_globals_t pmix_prm_base = {
    .initialized = false,
    .selected = false
};

static pmix_status_t base_allocate(pmix_alloc_directive_t directive,
                                   pmix_info_t *info, size_t ninfo,
                                   pmix_info_t **results, size_t *nresults)
{
    PMIX_HIDE_UNUSED_PARAMS(directive, info, ninfo, results, nresults);
    return PMIX_ERR_NOT_SUPPORTED;
}

static pmix_status_t base_notify(pmix_status_t status, const pmix_proc_t *source,
                                 pmix_data_range_t range, const pmix_info_t info[], size_t ninfo,
                                 pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(status, source, range, info, ninfo, cbfunc, cbdata);
    return PMIX_ERR_NOT_SUPPORTED;
}

static pmix_status_t base_grt(uint32_t *timeleft)
{
    PMIX_HIDE_UNUSED_PARAMS(timeleft);
    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_prm_module_t pmix_prm = {
    .name = "base",
    .init = NULL,
    .finalize = NULL,
    .allocate = base_allocate,
    .notify = base_notify,
    .get_remaining_time = base_grt
};

static pmix_status_t pmix_prm_close(void)
{
    if (!pmix_prm_base.initialized) {
        return PMIX_SUCCESS;
    }
    pmix_prm_base.initialized = false;
    pmix_prm_base.selected = false;

    if (NULL != pmix_prm.finalize) {
        pmix_prm.finalize();
    }
    return pmix_mca_base_framework_components_close(&pmix_prm_base_framework, NULL);
}

static pmix_status_t pmix_prm_open(pmix_mca_base_open_flag_t flags)
{
    /* initialize globals */
    pmix_prm_base.initialized = true;

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_prm_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, prm, "PMIx RM Operations", NULL, pmix_prm_open,
                                pmix_prm_close, pmix_mca_prm_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
