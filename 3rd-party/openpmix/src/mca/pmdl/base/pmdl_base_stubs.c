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

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#    include <sys/utsname.h>
#endif
#include <time.h>

#include "pmix_common.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_frameworks.h"

#include "src/class/pmix_list.h"
#include "src/mca/base/pmix_mca_base_vari.h"
#include "src/mca/preg/preg.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"

#include "src/mca/pmdl/base/base.h"

pmix_status_t pmix_pmdl_base_harvest_envars(char *nspace, const pmix_info_t info[], size_t ninfo,
                                            pmix_list_t *ilist)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;
    pmix_namespace_t *nptr = NULL, *ns;
    char *params[2] = {"PMIX_MCA_", NULL};
    char **priors = NULL, *tmp;
    pmix_kval_t *kv;
    pmix_mca_base_var_file_value_t *fv;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:harvest envars called");

    /* protect against bozo inputs */
    if (NULL == ilist) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* start by adding in the vars found in the default param
     * files, if any - this will allow the model-specific
     * components to override those values, if necessary */
    PMIX_LIST_FOREACH(fv, &pmix_mca_base_var_file_values, pmix_mca_base_var_file_value_t) {
        PMIX_KVAL_NEW(kv, PMIX_SET_ENVAR);
        if (NULL == kv) {
            return PMIX_ERR_NOMEM;
        }
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_NOMEM;
        }
        kv->value->type = PMIX_ENVAR;
        pmix_asprintf(&tmp, "PMIX_MCA_%s", fv->mbvfv_var);
        PMIX_ENVAR_LOAD(&kv->value->data.envar, tmp, fv->mbvfv_value, ':');
        free(tmp);
        pmix_list_append(ilist, &kv->super);
    }
    PMIX_LIST_FOREACH(fv, &pmix_mca_base_var_override_values, pmix_mca_base_var_file_value_t) {
        PMIX_KVAL_NEW(kv, PMIX_SET_ENVAR);
        if (NULL == kv) {
            return PMIX_ERR_NOMEM;
        }
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_NOMEM;
        }
        kv->value->type = PMIX_ENVAR;
        pmix_asprintf(&tmp, "PMIX_MCA_%s", fv->mbvfv_var);
        PMIX_ENVAR_LOAD(&kv->value->data.envar, tmp, fv->mbvfv_value, ':');
        free(tmp);
        pmix_list_append(ilist, &kv->super);
    }

    if (NULL != nspace) {
        nptr = NULL;
        /* find this nspace - note that it may not have
         * been registered yet */
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
    }

    /* process the request */
    PMIX_LIST_FOREACH (active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->harvest_envars) {
            rc = active->module->harvest_envars(nptr, info, ninfo, ilist, &priors);
            if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                PMIx_Argv_free(priors);
                return rc;
            }
        }
    }
    PMIx_Argv_free(priors);

    /* add any local PMIx MCA params */
    rc = pmix_util_harvest_envars(params, NULL, ilist);
    // mark that we are passing the PMIx envars
    PMIX_KVAL_NEW(kv, PMIX_SET_ENVAR);
    if (NULL == kv) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL == kv->value) {
        PMIX_RELEASE(kv);
        return PMIX_ERR_NOMEM;
    }
    kv->value->type = PMIX_ENVAR;
    PMIX_ENVAR_LOAD(&kv->value->data.envar, "PMIX_PARAM_FILE_PASSED", "1", ':');
    pmix_list_append(ilist, &kv->super);

    return PMIX_SUCCESS;
}

static char *prte_frameworks_static_3_0_1[] = {
    "errmgr",
    "ess",
    "filem",
    "grpcomm",
    "iof",
    "odls",
    "oob",
    "plm",
    "prtebacktrace",
    "prtedl",
    "prteinstalldirs",
    "prtereachable",
    "ras",
    "rmaps",
    "rml",
    "routed",
    "rtc",
    "schizo",
    "state",
    // inherited from OPAL
    "hwloc",
    "if",
    "reachable",
    NULL,
};

static char **prte_frameworks = prte_frameworks_static_3_0_1;
static bool prte_frameworks_setup = false;

static void setup_prte_frameworks(void)
{
    if (prte_frameworks_setup) {
        return;
    }
    prte_frameworks_setup = true;

    char *env = getenv("PRTE_MCA_PREFIXES");
    if (NULL == env) {
        return;
    }

    // If we found the env variable, it will be a comma-delimited list
    // of values.  Split it into an argv-style array.
    char **tmp = PMIx_Argv_split(env, ',');
    if (NULL != tmp) {
        prte_frameworks = tmp;
    }
}

bool pmix_pmdl_base_check_prte_param(char *param)
{
    char *p;
    size_t n;
    int len;

    setup_prte_frameworks();

    p = strchr(param, '_');
    len = (int)(p - param);

    if (0 == strncmp(param, "prte", len)) {
        return true;
    }
    for (n=0; NULL != prte_frameworks[n]; n++) {
        if (0 == strncmp(param, prte_frameworks[n], len)) {
            return true;
        }
    }
    return false;
}


bool pmix_pmdl_base_check_pmix_param(char *param)
{
    char *p;
    size_t n;
    int len;

    p = strchr(param, '_');
    len = (int)(p - param);

    if (0 == strncmp(param, "pmix", len)) {
        return true;
    }
    for (n=0; NULL != pmix_framework_names[n]; n++) {
        if (0 == strncmp(param, pmix_framework_names[n], len)) {
            return true;
        }
    }
    return false;
}

void pmix_pmdl_base_parse_file_envars(pmix_list_t *ilist)
{
    pmix_pmdl_base_active_module_t *active;

    /* process the request */
    PMIX_LIST_FOREACH (active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->parse_file_envars) {
            active->module->parse_file_envars(ilist);
        }
    }
}

pmix_status_t pmix_pmdl_base_setup_nspace(pmix_namespace_t *nptr, pmix_info_t *info)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output, "pmdl:setup_nspace called");

    /* process the request */
    PMIX_LIST_FOREACH (active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->setup_nspace) {
            rc = active->module->setup_nspace(nptr, info);
            if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_pmdl_base_setup_nspace_kv(pmix_namespace_t *nptr, pmix_kval_t *kv)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output, "pmdl:setup_nspace called");

    /* process the request */
    PMIX_LIST_FOREACH (active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->setup_nspace_kv) {
            rc = active->module->setup_nspace_kv(nptr, kv);
            if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_pmdl_base_register_nspace(pmix_namespace_t *nptr)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:register_nspace called");

    /* process the request */
    PMIX_LIST_FOREACH (active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->register_nspace) {
            rc = active->module->register_nspace(nptr);
            if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                return rc;
            }
        }
    }

    return PMIX_SUCCESS;
}

/* can only be called by a server */
pmix_status_t pmix_pmdl_base_setup_client(pmix_namespace_t *nptr, pmix_rank_t rank, uint32_t appnum)
{
    pmix_pmdl_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output, "pmdl: setup_client called");

    PMIX_LIST_FOREACH (active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
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
    char **priors = NULL;

    if (!pmix_pmdl_globals.initialized) {
        return PMIX_ERR_INIT;
    }
    PMIX_LIST_FOREACH (active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->setup_fork) {
            rc = active->module->setup_fork(proc, env, &priors);
            if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                /* true error */
                PMIx_Argv_free(priors);
                return rc;
            }
        }
    }
    PMIx_Argv_free(priors);

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
    PMIX_LIST_FOREACH (n2, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strncmp(ns, n2->nspace, PMIX_MAX_NSLEN)) {
            nptr = n2;
            break;
        }
    }
    if (NULL == nptr) {
        return;
    }

    PMIX_LIST_FOREACH (active, &pmix_pmdl_globals.actives, pmix_pmdl_base_active_module_t) {
        if (NULL != active->module->deregister_nspace) {
            active->module->deregister_nspace(nptr);
        }
    }
}
