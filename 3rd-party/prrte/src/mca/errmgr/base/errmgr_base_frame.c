/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

#include "src/class/pmix_list.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"

#include "src/mca/errmgr/base/base.h"
#include "src/mca/errmgr/base/errmgr_private.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/base/static-components.h"

/* Public module provides a wrapper around previous functions */
prte_errmgr_base_module_t prte_errmgr_default_fns = {
    .init = NULL,     /* init     */
    .finalize = NULL, /* finalize */
    .logfn = prte_errmgr_base_log
};

/* NOTE: ABSOLUTELY MUST initialize this
 * struct to include the log function as it
 * gets called even if the errmgr hasn't been
 * opened yet due to error
 */
prte_errmgr_base_module_t prte_errmgr = {
    .logfn = prte_errmgr_base_log
};

static int prte_errmgr_base_close(void)
{
    /* Close selected component */
    if (NULL != prte_errmgr.finalize) {
        prte_errmgr.finalize();
    }

    /* always leave a default set of fn pointers */
    prte_errmgr = prte_errmgr_default_fns;

    return pmix_mca_base_framework_components_close(&prte_errmgr_base_framework, NULL);
}

/**
 *  * Function for finding and opening either all MCA components, or the one
 *   * that was specifically requested via a MCA parameter.
 *    */
static int prte_errmgr_base_open(pmix_mca_base_open_flag_t flags)
{
    /* load the default fns */
    prte_errmgr = prte_errmgr_default_fns;

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_errmgr_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, errmgr, "PRTE Error Manager", NULL, prte_errmgr_base_open,
                                prte_errmgr_base_close, prte_errmgr_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
