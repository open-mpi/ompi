/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
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

#include "include/pmix.h"

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_mca_base_vari.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_alfg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"

#include "pmdl_mpich.h"
#include "src/mca/pmdl/base/base.h"
#include "src/mca/pmdl/pmdl.h"

static pmix_status_t mpich_init(void);
static void mpich_finalize(void);
static pmix_status_t harvest_envars(pmix_namespace_t *nptr, const pmix_info_t info[], size_t ninfo,
                                    pmix_list_t *ilist, char ***priors);
static pmix_status_t setup_nspace(pmix_namespace_t *nptr, pmix_info_t *info);
static pmix_status_t setup_nspace_kv(pmix_namespace_t *nptr, pmix_kval_t *kv);
static pmix_status_t register_nspace(pmix_namespace_t *nptr);
static pmix_status_t setup_fork(const pmix_proc_t *proc, char ***env, char ***priors);
static void deregister_nspace(pmix_namespace_t *nptr);
static void deregister_nspace(pmix_namespace_t *nptr);
pmix_pmdl_module_t pmix_pmdl_mpich_module = {
    .name = "mpich",
    .init = mpich_init,
    .finalize = mpich_finalize,
    .harvest_envars = harvest_envars,
    .setup_nspace = setup_nspace,
    .setup_nspace_kv = setup_nspace_kv,
    .register_nspace = register_nspace,
    .setup_fork = setup_fork,
    .deregister_nspace = deregister_nspace
};

/* internal structures */
typedef struct {
    pmix_list_item_t super;
    pmix_nspace_t nspace;
    uint32_t univ_size;
    uint32_t job_size;
    uint32_t local_size;
    uint32_t num_apps;
} pmdl_nspace_t;
static void nscon(pmdl_nspace_t *p)
{
    p->univ_size = UINT32_MAX;
    p->job_size = UINT32_MAX;
    p->local_size = UINT32_MAX;
    p->num_apps = UINT32_MAX;
}
static PMIX_CLASS_INSTANCE(pmdl_nspace_t, pmix_list_item_t, nscon, NULL);

/* internal variables */
static pmix_list_t mynspaces;

static pmix_status_t mpich_init(void)
{
    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output, "pmdl: mpich init");

    PMIX_CONSTRUCT(&mynspaces, pmix_list_t);

    return PMIX_SUCCESS;
}

static void mpich_finalize(void)
{
    PMIX_LIST_DESTRUCT(&mynspaces);
}

static bool checkus(const pmix_info_t info[], size_t ninfo)
{
    bool takeus = false;
    size_t n;

    if (NULL == info) {
        return false;
    }

    /* check the directives */
    for (n = 0; n < ninfo && !takeus; n++) {
        /* check the attribute */
        if (PMIX_CHECK_KEY(&info[n], PMIX_PROGRAMMING_MODEL)
            || PMIX_CHECK_KEY(&info[n], PMIX_PERSONALITY)) {
            if (NULL != strstr(info[n].value.data.string, "mpich")) {
                takeus = true;
                break;
            }
        }
    }

    return takeus;
}

static pmix_status_t harvest_envars(pmix_namespace_t *nptr, const pmix_info_t info[], size_t ninfo,
                                    pmix_list_t *ilist, char ***priors)
{
    pmdl_nspace_t *ns, *ns2;
    pmix_status_t rc;
    size_t n;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:mpich:harvest envars");

    if (!checkus(info, ninfo)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* don't do MPICH again if already done */
    if (NULL != *priors) {
        char **t2 = *priors;
        for (n = 0; NULL != t2[n]; n++) {
            if (0 == strncmp(t2[n], "mpich", strlen("mpich"))) {
                return PMIX_ERR_TAKE_NEXT_OPTION;
            }
        }
    }
    /* flag that we worked on this */
    PMIx_Argv_append_nosize(priors, "mpich");

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:mpich:harvest envars active");

    /* are we to harvest envars? */
    for (n=0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_SETUP_APP_ENVARS)) {
            goto harvest;
        }
    }
    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:mpich:harvest envars: NO");
    return PMIX_ERR_TAKE_NEXT_OPTION;

harvest:
    if (NULL != nptr) {
        /* see if we already have this nspace */
        ns = NULL;
        PMIX_LIST_FOREACH (ns2, &mynspaces, pmdl_nspace_t) {
            if (PMIX_CHECK_NSPACE(ns2->nspace, nptr->nspace)) {
                ns = ns2;
                break;
            }
        }
        if (NULL == ns) {
            ns = PMIX_NEW(pmdl_nspace_t);
            PMIX_LOAD_NSPACE(ns->nspace, nptr->nspace);
            pmix_list_append(&mynspaces, &ns->super);
        }
    }

    /* harvest our local envars */
    if (NULL != pmix_mca_pmdl_mpich_component.include) {
        pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                            "pmdl: mpich harvesting envars %s excluding %s",
                            (NULL == pmix_mca_pmdl_mpich_component.incparms)
                            ? "NONE"
                            : pmix_mca_pmdl_mpich_component.incparms,
                            (NULL == pmix_mca_pmdl_mpich_component.excparms)
                            ? "NONE"
                            : pmix_mca_pmdl_mpich_component.excparms);
        rc = pmix_util_harvest_envars(pmix_mca_pmdl_mpich_component.include,
                                      pmix_mca_pmdl_mpich_component.exclude, ilist);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
    }

    return PMIX_SUCCESS;
}

static pmix_status_t setup_nspace(pmix_namespace_t *nptr, pmix_info_t *info)
{
    pmdl_nspace_t *ns, *ns2;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:mpich: setup nspace for nspace %s with %s", nptr->nspace,
                        info->value.data.string);

    if (!checkus(info, 1)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* see if we already have this nspace */
    ns = NULL;
    PMIX_LIST_FOREACH (ns2, &mynspaces, pmdl_nspace_t) {
        if (PMIX_CHECK_NSPACE(ns2->nspace, nptr->nspace)) {
            ns = ns2;
            break;
        }
    }
    if (NULL == ns) {
        ns = PMIX_NEW(pmdl_nspace_t);
        PMIX_LOAD_NSPACE(ns->nspace, nptr->nspace);
        pmix_list_append(&mynspaces, &ns->super);
    }

    return PMIX_SUCCESS;
}

static pmix_status_t setup_nspace_kv(pmix_namespace_t *nptr, pmix_kval_t *kv)
{
    pmdl_nspace_t *ns, *ns2;
    char **tmp;
    size_t m;
    bool takeus = false;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:mpich: setup nspace_kv for nspace %s with %s", nptr->nspace,
                        kv->value->data.string);

    /* check the attribute */
    if (PMIX_CHECK_KEY(kv, PMIX_PROGRAMMING_MODEL) ||
        PMIX_CHECK_KEY(kv, PMIX_PERSONALITY)) {
        tmp = PMIx_Argv_split(kv->value->data.string, ',');
        for (m = 0; NULL != tmp[m]; m++) {
            if (0 == strcmp(tmp[m], "mpich")) {
                takeus = true;
                break;
            }
        }
        PMIx_Argv_free(tmp);
    }
    if (!takeus) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* see if we already have this nspace */
    ns = NULL;
    PMIX_LIST_FOREACH (ns2, &mynspaces, pmdl_nspace_t) {
        if (PMIX_CHECK_NSPACE(ns2->nspace, nptr->nspace)) {
            ns = ns2;
            break;
        }
    }
    if (NULL == ns) {
        ns = PMIX_NEW(pmdl_nspace_t);
        PMIX_LOAD_NSPACE(ns->nspace, nptr->nspace);
        pmix_list_append(&mynspaces, &ns->super);
    }

    return PMIX_SUCCESS;
}

static pmix_status_t register_nspace(pmix_namespace_t *nptr)
{
    pmdl_nspace_t *ns, *ns2;
    pmix_proc_t wildcard;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_cb_t cb;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:mpich: register_nspace for %s", nptr->nspace);

    /* see if we already have this nspace */
    ns = NULL;
    PMIX_LIST_FOREACH (ns2, &mynspaces, pmdl_nspace_t) {
        if (PMIX_CHECK_NSPACE(ns2->nspace, nptr->nspace)) {
            ns = ns2;
            break;
        }
    }
    if (NULL == ns) {
        /* we don't know anything about this one or
         * it doesn't have any mpich-based apps */
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* do we already have the data we need here? Servers are
     * allowed to call register_nspace multiple times with
     * different info, so we really need to recheck those
     * values that haven't already been filled */
    PMIX_LOAD_PROCID(&wildcard, nptr->nspace, PMIX_RANK_WILDCARD);

    /* fetch the universe size */
    if (UINT32_MAX == ns->univ_size) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &wildcard;
        cb.copy = true;
        cb.key = PMIX_UNIV_SIZE;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        cb.key = NULL;
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&cb);
            return rc;
        }
        /* the data is the first value on the cb.kvs list */
        if (1 != pmix_list_get_size(&cb.kvs)) {
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            PMIX_DESTRUCT(&cb);
            return PMIX_ERR_BAD_PARAM;
        }
        kv = (pmix_kval_t *) pmix_list_get_first(&cb.kvs);
        ns->univ_size = kv->value->data.uint32;
        PMIX_DESTRUCT(&cb);
    }

    /* fetch the job size */
    if (UINT32_MAX == ns->job_size) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &wildcard;
        cb.copy = true;
        cb.key = PMIX_JOB_SIZE;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        cb.key = NULL;
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&cb);
            return rc;
        }
        /* the data is the first value on the cb.kvs list */
        if (1 != pmix_list_get_size(&cb.kvs)) {
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            PMIX_DESTRUCT(&cb);
            return PMIX_ERR_BAD_PARAM;
        }
        kv = (pmix_kval_t *) pmix_list_get_first(&cb.kvs);
        ns->job_size = kv->value->data.uint32;
        PMIX_DESTRUCT(&cb);
    }

    /* fetch the number of apps */
    if (UINT32_MAX == ns->num_apps) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &wildcard;
        cb.copy = true;
        cb.key = PMIX_JOB_NUM_APPS;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        cb.key = NULL;
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&cb);
            return rc;
        }
        /* the data is the first value on the cb.kvs list */
        if (1 != pmix_list_get_size(&cb.kvs)) {
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            PMIX_DESTRUCT(&cb);
            return PMIX_ERR_BAD_PARAM;
        }
        kv = (pmix_kval_t *) pmix_list_get_first(&cb.kvs);
        ns->num_apps = kv->value->data.uint32;
        PMIX_DESTRUCT(&cb);
    }

    /* fetch the number of local peers */
    if (UINT32_MAX == ns->local_size) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &wildcard;
        cb.copy = true;
        cb.key = PMIX_LOCAL_SIZE;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        cb.key = NULL;
        /* it is okay if there are no local procs */
        if (PMIX_SUCCESS == rc) {
            /* the data is the first value on the cb.kvs list */
            if (1 != pmix_list_get_size(&cb.kvs)) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                PMIX_DESTRUCT(&cb);
                return PMIX_ERR_BAD_PARAM;
            }
            kv = (pmix_kval_t *) pmix_list_get_first(&cb.kvs);
            ns->local_size = kv->value->data.uint32;
            PMIX_DESTRUCT(&cb);
        }
    }

    return PMIX_SUCCESS;
}

static pmix_status_t setup_fork(const pmix_proc_t *proc, char ***env, char ***priors)
{
    pmdl_nspace_t *ns, *ns2;
    char *param;
    pmix_status_t rc;
    uint16_t u16;
    pmix_kval_t *kv;
    uint32_t n;
    pmix_cb_t cb;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:mpich: setup fork for %s", PMIX_NAME_PRINT(proc));

    /* don't do MPICH again if already done */
    if (NULL != *priors) {
        char **t2 = *priors;
        for (n = 0; NULL != t2[n]; n++) {
            if (0 == strncmp(t2[n], "mpich", 4)) {
                return PMIX_ERR_TAKE_NEXT_OPTION;
            }
        }
    }
    /* flag that we worked on this */
    PMIx_Argv_append_nosize(priors, "mpich");

    /* see if we already have this nspace */
    ns = NULL;
    PMIX_LIST_FOREACH (ns2, &mynspaces, pmdl_nspace_t) {
        if (PMIX_CHECK_NSPACE(ns2->nspace, proc->nspace)) {
            ns = ns2;
            break;
        }
    }
    if (NULL == ns) {
        /* we don't know anything about this one or
         * it doesn't have any mpich-based apps */
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* pass the proc's global rank */
    if (0 > asprintf(&param, "%u", proc->rank)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("PMI_RANK", param, true, env);
    free(param);

    /* pass the job size */
    if (0 > asprintf(&param, "%u", ns->job_size)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("PMI_SIZE", param, true, env);
    free(param);

    /* pass the local size */
    if (0 > asprintf(&param, "%u", ns->local_size)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("MPI_LOCALNRANKS", param, true, env);
    free(param);

    /* pass the local rank */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = (pmix_proc_t *) proc;
    cb.copy = true;
    cb.key = PMIX_LOCAL_RANK;
    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
    cb.key = NULL;
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DESTRUCT(&cb);
        return rc;
    }
    /* the data is the first value on the cb.kvs list */
    if (1 != pmix_list_get_size(&cb.kvs)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        PMIX_DESTRUCT(&cb);
        return PMIX_ERR_BAD_PARAM;
    }
    kv = (pmix_kval_t *) pmix_list_get_first(&cb.kvs);
    u16 = kv->value->data.uint16;
    PMIX_DESTRUCT(&cb);
    if (0 > asprintf(&param, "%lu", (unsigned long) u16)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("MPI_LOCALNRANKID", param, true, env);
    free(param);

    /* pass the hostname */
    PMIx_Setenv("MPIR_CVAR_CH3_INTERFACE_HOSTNAME", pmix_globals.hostname, true, env);

    return PMIX_SUCCESS;
}

static void deregister_nspace(pmix_namespace_t *nptr)
{
    pmdl_nspace_t *ns;

    /* find our tracker for this nspace */
    PMIX_LIST_FOREACH (ns, &mynspaces, pmdl_nspace_t) {
        if (PMIX_CHECK_NSPACE(ns->nspace, nptr->nspace)) {
            pmix_list_remove_item(&mynspaces, &ns->super);
            PMIX_RELEASE(ns);
            return;
        }
    }
}
