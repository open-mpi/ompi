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
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
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

#include "include/pmix_common.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "src/class/pmix_list.h"
#include "src/mca/base/base.h"
#include "src/mca/psquash/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/psquash/base/static-components.h"

pmix_psquash_base_module_t pmix_psquash = {0};
pmix_psquash_globals_t pmix_psquash_globals = {0};

static pmix_status_t pmix_psquash_close(void)
{
    if (!pmix_psquash_globals.initialized) {
        return PMIX_SUCCESS;
    }
    pmix_psquash_globals.initialized = false;
    pmix_psquash_globals.selected = false;

    return pmix_mca_base_framework_components_close(&pmix_psquash_base_framework, NULL);
}

static pmix_status_t pmix_psquash_open(pmix_mca_base_open_flag_t flags)
{
    if (pmix_psquash_globals.initialized) {
        return PMIX_SUCCESS;
    }
    /* initialize globals */
    pmix_psquash_globals.initialized = true;

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_psquash_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, psquash, "PMIx Squash Operations",
                                NULL, pmix_psquash_open, pmix_psquash_close,
                                mca_psquash_base_static_components, 0);
