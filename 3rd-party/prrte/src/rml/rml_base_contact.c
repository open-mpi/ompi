/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"

#include "src/rml/rml_contact.h"
#include "src/rml/rml.h"

int prte_rml_parse_uris(const char *uri, pmix_proc_t *peer, char ***uris)
{
    int rc;

    /* parse the process name */
    char *cinfo = strdup(uri);
    char *ptr = strchr(cinfo, ';');
    if (NULL == ptr) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        free(cinfo);
        return PRTE_ERR_BAD_PARAM;
    }
    *ptr = '\0';
    ptr++;
    if (PRTE_SUCCESS != (rc = prte_util_convert_string_to_process_name(peer, cinfo))) {
        PRTE_ERROR_LOG(rc);
        free(cinfo);
        return rc;
    }

    if (NULL != uris) {
        /* parse the remainder of the string into an array of uris */
        *uris = PMIX_ARGV_SPLIT_COMPAT(ptr, ';');
    }
    free(cinfo);
    return PRTE_SUCCESS;
}
