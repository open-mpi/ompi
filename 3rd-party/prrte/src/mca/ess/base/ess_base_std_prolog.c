/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_wait.h"
#include "src/runtime/runtime_internals.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/ess/base/base.h"

int prte_ess_base_std_prolog(void)
{
    int ret;
    char *error = NULL;

    /* Initialize the PRTE data type support */
    if (PRTE_SUCCESS != (ret = prte_dt_init())) {
        error = "prte_dt_init";
        goto error;
    }
    /*
     * Setup the waitpid/sigchld system
     */
    if (PRTE_SUCCESS != (ret = prte_wait_init())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_wait_init";
        goto error;
    }

    return PRTE_SUCCESS;

error:
    pmix_show_help("help-prte-runtime",
                   "prte_init:startup:internal-failure", true,
                   error, PRTE_ERROR_NAME(ret), ret);

    return ret;
}
