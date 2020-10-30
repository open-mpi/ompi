/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/include/pmix_globals.h"
#include "src/client/pmix_client_ops.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/preg/base/base.h"

pmix_status_t pmix_preg_base_generate_node_regex(const char *input,
                                                 char **regex)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->generate_node_regex) {
            if (PMIX_SUCCESS == active->module->generate_node_regex(input, regex)) {
                return PMIX_SUCCESS;
            }
        }
    }

    /* no regex could be generated */
    *regex = strdup(input);
    return PMIX_SUCCESS;
}

pmix_status_t pmix_preg_base_generate_ppn(const char *input,
                                          char **ppn)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->generate_ppn) {
            if (PMIX_SUCCESS == active->module->generate_ppn(input, ppn)) {
                return PMIX_SUCCESS;
            }
        }
    }

    /* no regex could be generated */
    *ppn = strdup(input);
    return PMIX_SUCCESS;
}

pmix_status_t pmix_preg_base_parse_nodes(const char *regexp,
                                         char ***names)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->parse_nodes) {
            if (PMIX_SUCCESS == active->module->parse_nodes(regexp, names)) {
                return PMIX_SUCCESS;
            }
        }
    }

    /* nobody could parse it, so just process it here */
    *names = pmix_argv_split(regexp, ',');
    return PMIX_SUCCESS;
}

pmix_status_t pmix_preg_base_parse_procs(const char *regexp,
                                         char ***procs)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->parse_procs) {
            if (PMIX_SUCCESS == active->module->parse_procs(regexp, procs)) {
                return PMIX_SUCCESS;
            }
        }
    }

    /* nobody could parse it, so just process it here */
    *procs = pmix_argv_split(regexp, ';');
    return PMIX_SUCCESS;
}

pmix_status_t pmix_preg_base_copy(char **dest, size_t *len, const char *input)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->copy) {
            if (PMIX_SUCCESS == active->module->copy(dest, len, input)) {
                return PMIX_SUCCESS;
            }
        }
    }

    /* nobody could handle it, so it must just be a string */
    *dest = strdup(input);
    *len = strlen(input)+1;
    return PMIX_SUCCESS;
}

pmix_status_t pmix_preg_base_pack(pmix_buffer_t *buffer, const char *input)
{
    pmix_preg_base_active_module_t *active;
    pmix_status_t rc;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->pack) {
            if (PMIX_SUCCESS == active->module->pack(buffer, input)) {
                return PMIX_SUCCESS;
            }
        }
    }

    /* just pack it as a string */
    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, buffer, input, 1, PMIX_STRING);
    return rc;
}

pmix_status_t pmix_preg_base_unpack(pmix_buffer_t *buffer, char **regex)
{
    pmix_preg_base_active_module_t *active;
    pmix_status_t rc;
    int32_t cnt = 1;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->unpack) {
            if (PMIX_SUCCESS == active->module->unpack(buffer, regex)) {
                return PMIX_SUCCESS;
            }
        }
    }

    /* must just be a string */
    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, buffer, regex, &cnt, PMIX_STRING);
    return rc;
}
