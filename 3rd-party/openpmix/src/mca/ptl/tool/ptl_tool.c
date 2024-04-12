/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "src/include/pmix_config.h"
#include "src/include/pmix_globals.h"

#include "src/util/pmix_argv.h"
#include "src/util/pmix_show_help.h"
#include "ptl_tool.h"
#include "src/mca/ptl/base/base.h"

static pmix_status_t setup_listener(pmix_info_t info[], size_t ninfo);

pmix_ptl_module_t pmix_ptl_tool_module = {
    .name = "tool",
    .connect_to_peer = pmix_ptl_base_connect_to_peer,
    .setup_fork = pmix_ptl_base_setup_fork,
    .setup_listener = setup_listener
};

static pmix_status_t setup_listener(pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    char **clnup = NULL, *cptr = NULL;
    pmix_info_t dir;

    rc = pmix_ptl_base_setup_listener(info, ninfo);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* if we are connected, then register any rendezvous files for cleanup */
    if (pmix_globals.connected) {
        if (NULL != pmix_ptl_base.nspace_filename) {
            PMIx_Argv_append_nosize(&clnup, pmix_ptl_base.nspace_filename);
        }
        if (NULL != pmix_ptl_base.session_filename) {
            PMIx_Argv_append_nosize(&clnup, pmix_ptl_base.session_filename);
        }
        if (NULL != clnup) {
            cptr = PMIx_Argv_join(clnup, ',');
            PMIx_Argv_free(clnup);
            PMIX_INFO_LOAD(&dir, PMIX_REGISTER_CLEANUP, cptr, PMIX_STRING);
            free(cptr);
            PMIx_Job_control_nb(&pmix_globals.myid, 1, &dir, 1, NULL, NULL);
            PMIX_INFO_DESTRUCT(&dir);
        }
    }

    return PMIX_SUCCESS;
}
