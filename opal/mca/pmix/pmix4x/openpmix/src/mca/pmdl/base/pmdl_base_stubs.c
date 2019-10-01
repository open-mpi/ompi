/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "src/mca/preg/preg.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/pmix_environ.h"
#include "src/server/pmix_server_ops.h"

#include "src/mca/pmdl/base/base.h"


pmix_status_t pmix_pmdl_base_harvest_envars(char *nspace,
                                            pmix_info_t info[], size_t ninfo,
                                            pmix_list_t *ilist)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;
    pmix_namespace_t *nptr, *ns;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:harvest envars called");

    /* protect against bozo inputs */
    if (NULL == nspace || NULL == ilist) {
        return PMIX_ERR_BAD_PARAM;
    }
    nptr = NULL;
    /* find this nspace - note that it may not have
     * been registered yet */
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strcmp(ns->nspace, nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* add it */
        nptr = PMIX_NEW(pmix_namespace_t);
        if (NULL == nptr) {
            return PMIX_ERR_NOMEM;
        }
        nptr->nspace = strdup(nspace);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }

    /* process the request */
    PMIX_LIST_FOREACH(active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->harvest_envars) {
            if (PMIX_SUCCESS == (rc = active->module->harvest_envars(nptr, info, ninfo, ilist))) {
                break;
            }
            if (PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_pmdl_base_setup_nspace(pmix_namespace_t *nptr,
                                          uint32_t appnum,
                                          pmix_info_t *info)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:setup_nspace called");

    /* process the request */
    PMIX_LIST_FOREACH(active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->setup_nspace) {
            if (PMIX_SUCCESS == (rc = active->module->setup_nspace(nptr, appnum, info))) {
                break;
            }
            if (PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_pmdl_base_setup_nspace_kv(pmix_namespace_t *nptr,
                                            uint32_t appnum,
                                            pmix_kval_t *kv)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:setup_nspace called");

    /* process the request */
    PMIX_LIST_FOREACH(active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->setup_nspace_kv) {
            rc = active->module->setup_nspace_kv(nptr, appnum, kv);
            if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

/* can only be called by a server */
pmix_status_t pmix_pmdl_base_setup_client(pmix_namespace_t *nptr,
                                          pmix_rank_t rank,
                                          uint32_t appnum)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl: setup_client called");

    PMIX_LIST_FOREACH(active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->setup_client) {
            rc = active->module->setup_client(nptr, rank, appnum);
            if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

/* can only be called by a server */
pmix_status_t pmix_pmdl_base_setup_fork(const pmix_proc_t *proc, char ***env)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }
    PMIX_LIST_FOREACH(active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->setup_fork) {
            rc = active->module->setup_fork(proc, env);
            if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

void pmix_pmdl_base_deregister_nspace(const char *ns)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_namespace_t *nptr, *n2;

    if (!pmix_pmdl_globals.initialized) {
        return;
    }

    /* search for the namespace */
    nptr = NULL;
    PMIX_LIST_FOREACH(n2, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strncmp(ns, n2->nspace, PMIX_MAX_NSLEN)) {
            nptr = n2;
            break;
        }
    }
    if (NULL == nptr) {
        return;
    }

    PMIX_LIST_FOREACH(active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->deregister_nspace) {
            active->module->deregister_nspace(nptr);
        }
    }
}
