/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <pmix_common.h>
#include "src/include/pmix_globals.h"

#include "src/class/pmix_list.h"
#include "src/util/error.h"

#include "src/mca/pnet/base/base.h"


pmix_status_t pmix_pnet_base_setup_app(char *nspace, pmix_list_t *ilist)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pnet_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    /* protect against bozo inputs */
    if (NULL == nspace || NULL == ilist) {
        return PMIX_ERR_BAD_PARAM;
    }

    PMIX_LIST_FOREACH(active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->setup_app) {
            if (PMIX_SUCCESS != (rc = active->module->setup_app(nspace, ilist))) {
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_pnet_base_setup_local_network(char *nspace,
                                                 pmix_info_t info[],
                                                 size_t ninfo)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pnet_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    /* protect against bozo inputs */
    if (NULL == nspace) {
        return PMIX_ERR_BAD_PARAM;
    }

    PMIX_LIST_FOREACH(active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->setup_local_network) {
            if (PMIX_SUCCESS != (rc = active->module->setup_local_network(nspace, info, ninfo))) {
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_pnet_base_setup_fork(const pmix_proc_t *peer, char ***env)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pnet_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    /* protect against bozo inputs */
    if (NULL == peer || NULL == env) {
        return PMIX_ERR_BAD_PARAM;
    }

    PMIX_LIST_FOREACH(active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->setup_fork) {
            rc = active->module->setup_fork(peer,  env);
            if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_AVAILABLE != rc) {
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

void pmix_pnet_base_child_finalized(pmix_peer_t *peer)
{
    pmix_pnet_base_active_module_t *active;

    if (!pmix_pnet_globals.initialized) {
        return;
    }

    /* protect against bozo inputs */
    if (NULL == peer) {
        return;
    }

    PMIX_LIST_FOREACH(active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->child_finalized) {
            active->module->child_finalized(peer);
        }
    }

    return;
}

void pmix_pnet_base_local_app_finalized(char *nspace)
{
    pmix_pnet_base_active_module_t *active;

    if (!pmix_pnet_globals.initialized) {
        return;
    }

    /* protect against bozo inputs */
    if (NULL == nspace) {
        return;
    }

    PMIX_LIST_FOREACH(active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->local_app_finalized) {
            active->module->local_app_finalized(nspace);
        }
    }

    return;
}
