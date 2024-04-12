/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "pmix_common.h"
#include "src/include/pmix_globals.h"

#include "src/class/pmix_list.h"
#include "src/mca/preg/preg.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_environ.h"

#include "src/mca/pnet/base/base.h"

/* NOTE: a tool (e.g., prun) may call this function to
 * harvest local envars for inclusion in a call to
 * PMIx_Spawn, or it might be called in response to
 * a call to PMIx_Allocate_resources */
pmix_status_t pmix_pnet_base_allocate(char *nspace, pmix_info_t info[], size_t ninfo,
                                      pmix_list_t *ilist)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;
    pmix_namespace_t *nptr, *ns;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output, "pnet:allocate called");

    /* protect against bozo inputs */
    if (NULL == nspace || NULL == ilist) {
        return PMIX_ERR_BAD_PARAM;
    }

    if (0 == pmix_list_get_size(&pmix_pnet_globals.actives)) {
        return PMIX_SUCCESS;
    }

    /* find this proc's nspace object */
    nptr = NULL;
    PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
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

    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
        /* process the allocation request */
        PMIX_LIST_FOREACH (active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
            if (NULL != active->module->allocate) {
                rc = active->module->allocate(nptr, info, ninfo, ilist);
                if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_AVAILABLE != rc
                    && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                    /* true error */
                    return rc;
                }
            }
        }
    }

    return PMIX_SUCCESS;
}

/* can only be called by a server from within an event! */
pmix_status_t pmix_pnet_base_setup_local_network(char *nspace, pmix_info_t info[], size_t ninfo)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;
    pmix_namespace_t *nsp, *nsp2;
    pmix_nspace_env_cache_t *ns, *ns2;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: setup_local_network called");

    /* protect against bozo inputs */
    if (NULL == nspace) {
        return PMIX_ERR_BAD_PARAM;
    }

    if (0 == pmix_list_get_size(&pmix_pnet_globals.actives)) {
        return PMIX_SUCCESS;
    }

    /* find this proc's nspace object */
    ns = NULL;
    PMIX_LIST_FOREACH (ns2, &pmix_pnet_globals.nspaces, pmix_nspace_env_cache_t) {
        if (PMIX_CHECK_NSPACE(ns2->ns->nspace, nspace)) {
            ns = ns2;
            break;
        }
    }
    if (NULL == ns) {
        /* find the namespace object for this nspace */
        nsp = NULL;
        PMIX_LIST_FOREACH (nsp2, &pmix_globals.nspaces, pmix_namespace_t) {
            if (0 == strcmp(nsp2->nspace, nspace)) {
                nsp = nsp2;
                break;
            }
        }
        if (NULL == nsp) {
            /* add it */
            nsp = PMIX_NEW(pmix_namespace_t);
            if (NULL == nsp) {
                return PMIX_ERR_NOMEM;
            }
            nsp->nspace = strdup(nspace);
            pmix_list_append(&pmix_globals.nspaces, &nsp->super);
        }
        ns = PMIX_NEW(pmix_nspace_env_cache_t);
        PMIX_RETAIN(nsp);
        ns->ns = nsp;
        pmix_list_append(&pmix_pnet_globals.nspaces, &ns->super);
    }

    PMIX_LIST_FOREACH (active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->setup_local_network) {
            rc = active->module->setup_local_network(ns, info, ninfo);
            if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_AVAILABLE != rc
                && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

/* can only be called by a server from within an event! */
pmix_status_t pmix_pnet_base_setup_fork(const pmix_proc_t *proc, char ***env)
{
    pmix_nspace_env_cache_t *ns, *ns2;
    pmix_envar_list_item_t *ev;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output, "pnet: setup_fork called");

    /* protect against bozo inputs */
    if (NULL == proc || NULL == env) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* see if we have this nspace */
    ns = NULL;
    PMIX_LIST_FOREACH (ns2, &pmix_pnet_globals.nspaces, pmix_nspace_env_cache_t) {
        if (PMIX_CHECK_NSPACE(ns2->ns->nspace, proc->nspace)) {
            ns = ns2;
            break;
        }
    }
    if (NULL != ns) {
        PMIX_LIST_FOREACH (ev, &ns->envars, pmix_envar_list_item_t) {
            PMIx_Setenv(ev->envar.envar, ev->envar.value, true, env);
        }
    }

    return PMIX_SUCCESS;
}

void pmix_pnet_base_child_finalized(pmix_proc_t *peer)
{
    pmix_pnet_base_active_module_t *active;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: child_finalized called");

    /* protect against bozo inputs */
    if (NULL == peer) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return;
    }

    if (0 == pmix_list_get_size(&pmix_pnet_globals.actives)) {
        return;
    }

    PMIX_LIST_FOREACH (active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->child_finalized) {
            active->module->child_finalized(peer);
        }
    }

    return;
}

void pmix_pnet_base_local_app_finalized(pmix_namespace_t *nptr)
{
    pmix_pnet_base_active_module_t *active;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: local_app_finalized called");

    /* protect against bozo inputs */
    if (NULL == nptr) {
        return;
    }

    if (0 == pmix_list_get_size(&pmix_pnet_globals.actives)) {
        return;
    }

    PMIX_LIST_FOREACH (active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->local_app_finalized) {
            active->module->local_app_finalized(nptr);
        }
    }

    return;
}

void pmix_pnet_base_deregister_nspace(char *nspace)
{
    pmix_pnet_base_active_module_t *active;
    pmix_nspace_env_cache_t *ns, *ns2;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: deregister_nspace called");

    /* protect against bozo inputs */
    if (NULL == nspace) {
        return;
    }

    /* find this nspace object */
    ns = NULL;
    PMIX_LIST_FOREACH (ns2, &pmix_pnet_globals.nspaces, pmix_nspace_env_cache_t) {
        if (PMIX_CHECK_NSPACE(ns2->ns->nspace, nspace)) {
            ns = ns2;
            pmix_list_remove_item(&pmix_pnet_globals.nspaces, &ns->super);
            break;
        }
    }
    if (NULL == ns) {
        return;
    }

    PMIX_LIST_FOREACH (active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->deregister_nspace) {
            active->module->deregister_nspace(ns->ns);
        }
    }
    PMIX_RELEASE(ns);
}

pmix_status_t pmix_pnet_base_collect_inventory(pmix_info_t directives[], size_t ndirs,
                                               pmix_list_t *inventory)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;

    PMIX_LIST_FOREACH (active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->collect_inventory) {
            pmix_output_verbose(5, pmix_pnet_base_framework.framework_output, "COLLECTING %s",
                                active->module->name);
            rc = active->module->collect_inventory(directives, ndirs, inventory);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_pnet_base_deliver_inventory(pmix_info_t info[], size_t ninfo,
                                               pmix_info_t directives[], size_t ndirs)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;

    PMIX_LIST_FOREACH (active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->deliver_inventory) {
            pmix_output_verbose(5, pmix_pnet_base_framework.framework_output, "DELIVERING TO %s",
                                active->module->name);
            rc = active->module->deliver_inventory(info, ninfo, directives, ndirs);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_pnet_base_register_fabric(pmix_fabric_t *fabric, const pmix_info_t directives[],
                                             size_t ndirs, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;
    pmix_pnet_fabric_t *ft;

    /* ensure our fields of the fabric object are initialized */
    fabric->info = NULL;
    fabric->ninfo = 0;
    fabric->module = NULL;

    if (0 == pmix_list_get_size(&pmix_pnet_globals.actives)) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* scan across active modules until one returns success */
    PMIX_LIST_FOREACH (active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->register_fabric) {
            rc = active->module->register_fabric(fabric, directives, ndirs, cbfunc, cbdata);
            if (PMIX_OPERATION_SUCCEEDED == rc) {
                /* track this fabric so we can respond to remote requests */
                ft = PMIX_NEW(pmix_pnet_fabric_t);
                ft->index = fabric->index;
                if (NULL != fabric->name) {
                    ft->name = strdup(fabric->name);
                }
                ft->module = active->module;
                pmix_list_append(&pmix_pnet_globals.fabrics, &ft->super);
                return rc;
            } else if (PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* just return the result */
                return rc;
            }
        }
    }

    return PMIX_ERR_NOT_FOUND;
}

pmix_status_t pmix_pnet_base_update_fabric(pmix_fabric_t *fabric)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_pnet_fabric_t *active;
    pmix_pnet_module_t *module = NULL;
    pmix_pnet_fabric_t *ft;

    /* protect against bozo input */
    if (NULL == fabric) {
        return PMIX_ERR_BAD_PARAM;
    } else if (NULL == fabric->module) {
        /* this might be a remote request, so look at the
         * list of fabrics we have registered locally and
         * see if we have one with the matching index */
        PMIX_LIST_FOREACH (ft, &pmix_pnet_globals.fabrics, pmix_pnet_fabric_t) {
            if (fabric->index == ft->index) {
                module = ft->module;
            } else if (NULL != fabric->name && NULL != ft->name
                       && 0 == strcmp(ft->name, fabric->name)) {
                module = ft->module;
            }
        }
    } else {
        active = (pmix_pnet_fabric_t *) fabric->module;
        module = (pmix_pnet_module_t *) active->module;
    }
    if (NULL == module) {
        return PMIX_ERR_BAD_PARAM;
    }

    if (NULL != module->update_fabric) {
        rc = module->update_fabric(fabric);
    }
    return rc;
}

pmix_status_t pmix_pnet_base_deregister_fabric(pmix_fabric_t *fabric)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_pnet_fabric_t *active;
    pmix_pnet_module_t *module = NULL;
    pmix_pnet_fabric_t *ft;

    /* protect against bozo input */
    if (NULL == fabric) {
        return PMIX_ERR_BAD_PARAM;
    } else if (NULL == fabric->module) {
        /* this might be a remote request, so look at the
         * list of fabrics we have registered locally and
         * see if we have one with the matching index */
        PMIX_LIST_FOREACH (ft, &pmix_pnet_globals.fabrics, pmix_pnet_fabric_t) {
            if (fabric->index == ft->index) {
                module = ft->module;
            } else if (NULL != fabric->name && NULL != ft->name
                       && 0 == strcmp(ft->name, fabric->name)) {
                module = ft->module;
            }
        }
    } else {
        active = (pmix_pnet_fabric_t *) fabric->module;
        module = (pmix_pnet_module_t *) active->module;
    }
    if (NULL == module) {
        return PMIX_ERR_BAD_PARAM;
    }

    if (NULL != module->deregister_fabric) {
        rc = module->deregister_fabric(fabric);
    }
    return rc;
}
