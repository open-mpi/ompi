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

#include "pmdl_ompi.h"
#include "src/mca/pmdl/base/base.h"
#include "src/mca/pmdl/pmdl.h"

static pmix_status_t ompi_init(void);
static void ompi_finalize(void);
static pmix_status_t harvest_envars(pmix_namespace_t *nptr,
                                    const pmix_info_t info[], size_t ninfo,
                                    pmix_list_t *ilist, char ***priors);
static void parse_file_envars(pmix_list_t *ilist);
static pmix_status_t setup_nspace(pmix_namespace_t *nptr, pmix_info_t *info);
static pmix_status_t setup_nspace_kv(pmix_namespace_t *nptr, pmix_kval_t *kv);
static pmix_status_t register_nspace(pmix_namespace_t *nptr);
static pmix_status_t setup_fork(const pmix_proc_t *proc, char ***env, char ***priors);
static void deregister_nspace(pmix_namespace_t *nptr);
static void deregister_nspace(pmix_namespace_t *nptr);
pmix_pmdl_module_t pmix_pmdl_ompi_module = {
    .name = "ompi",
    .init = ompi_init,
    .finalize = ompi_finalize,
    .harvest_envars = harvest_envars,
    .parse_file_envars = parse_file_envars,
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
static PMIX_CLASS_INSTANCE(pmdl_nspace_t,
                           pmix_list_item_t,
                           nscon, NULL);

/* internal variables */
static pmix_list_t mynspaces;
static pmix_list_t myenvars;

static pmix_status_t ompi_init(void)
{
    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output, "pmdl: ompi init");

    PMIX_CONSTRUCT(&mynspaces, pmix_list_t);
    PMIX_CONSTRUCT(&myenvars, pmix_list_t);

    return PMIX_SUCCESS;
}

static void ompi_finalize(void)
{
    PMIX_LIST_DESTRUCT(&mynspaces);
    PMIX_LIST_DESTRUCT(&myenvars);
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
        if (PMIX_CHECK_KEY(&info[n], PMIX_PROGRAMMING_MODEL) ||
            PMIX_CHECK_KEY(&info[n], PMIX_PERSONALITY)) {
            if (NULL != strstr(info[n].value.data.string, "ompi")) {
                takeus = true;
                break;
            }
        }
    }

    return takeus;
}

static pmix_status_t process_param_file(char *file, pmix_list_t *ilist)
{
    pmix_list_t params;
    pmix_mca_base_var_file_value_t *fv;
    pmix_kval_t *kv;
    char *tmp;

    PMIX_CONSTRUCT(&params, pmix_list_t);
    pmix_mca_base_parse_paramfile(file, &params);
    PMIX_LIST_FOREACH (fv, &params, pmix_mca_base_var_file_value_t) {
        /* see if this param actually refers to a PMIx value */
        if (pmix_pmdl_base_check_pmix_param(fv->mbvfv_var)) {
            kv = PMIX_NEW(pmix_kval_t);
            if (NULL == kv) {
                PMIX_LIST_DESTRUCT(&params);
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            kv->key = strdup(PMIX_SET_ENVAR);
            kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            if (NULL == kv->value) {
                PMIX_RELEASE(kv);
                PMIX_LIST_DESTRUCT(&params);
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            kv->value->type = PMIX_ENVAR;
            pmix_asprintf(&tmp, "PMIX_MCA_%s", fv->mbvfv_var);
            PMIX_ENVAR_LOAD(&kv->value->data.envar, tmp, fv->mbvfv_value, ':');
            free(tmp);
            pmix_list_append(ilist, &kv->super);
            continue;
        }
        /* see of this param actually refers to an old ORTE
         * or PRRTE value */
        if (pmix_pmdl_base_check_prte_param(fv->mbvfv_var)) {
            kv = PMIX_NEW(pmix_kval_t);
            if (NULL == kv) {
                PMIX_LIST_DESTRUCT(&params);
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            kv->key = strdup(PMIX_SET_ENVAR);
            kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            if (NULL == kv->value) {
                PMIX_RELEASE(kv);
                PMIX_LIST_DESTRUCT(&params);
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            kv->value->type = PMIX_ENVAR;
            pmix_asprintf(&tmp, "PRTE_MCA_%s", fv->mbvfv_var);
            PMIX_ENVAR_LOAD(&kv->value->data.envar, tmp, fv->mbvfv_value, ':');
            free(tmp);
            pmix_list_append(ilist, &kv->super);
            continue;
        }
        /* assume this is an OMPI param - need to prefix the param name */
        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            PMIX_LIST_DESTRUCT(&params);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->key = strdup(PMIX_SET_ENVAR);
        kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            PMIX_LIST_DESTRUCT(&params);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->value->type = PMIX_ENVAR;
        pmix_asprintf(&tmp, "OMPI_MCA_%s", fv->mbvfv_var);
        PMIX_ENVAR_LOAD(&kv->value->data.envar, tmp, fv->mbvfv_value, ':');
        free(tmp);
        pmix_list_append(ilist, &kv->super);
    }
    PMIX_LIST_DESTRUCT(&params);
    return PMIX_SUCCESS;
}

static pmix_status_t harvest_envars(pmix_namespace_t *nptr,
                                    const pmix_info_t info[], size_t ninfo,
                                    pmix_list_t *ilist, char ***priors)
{
    pmdl_nspace_t *ns, *ns2;
    pmix_status_t rc;
    uint32_t uid = UINT32_MAX;
    const char *home;
    pmix_mca_base_var_file_value_t *fv;
    pmix_kval_t *kv;
    size_t n;
    char *file, *evar;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi:harvest envars");

    if (!checkus(info, ninfo)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* don't do OMPI again if already done */
    if (NULL != *priors) {
        char **t2 = *priors;
        for (n = 0; NULL != t2[n]; n++) {
            if (0 == strncmp(t2[n], "ompi", strlen("ompi"))) {
                return PMIX_ERR_TAKE_NEXT_OPTION;
            }
        }
    }
    /* flag that we worked on this */
    PMIx_Argv_append_nosize(priors, "ompi");

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi:harvest envars active");

    /* are we to harvest envars? */
    for (n=0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_SETUP_APP_ENVARS)) {
            goto harvest;
        }
    }
    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi:harvest envars: NO");
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

    /* check if the user has set OMPIHOME in their environment */
    if (NULL != (evar = getenv("OMPIHOME"))) {
        /* look for the default MCA param file */
        file = pmix_os_path(false, evar, "etc", "openmpi-mca-params.conf", NULL);
        rc = process_param_file(file, ilist);
        free(file);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        /* add an envar indicating that we did this so the OMPI
         * processes won't duplicate it */
        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->key = strdup(PMIX_SET_ENVAR);
        kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->value->type = PMIX_ENVAR;
        PMIX_ENVAR_LOAD(&kv->value->data.envar, "OPAL_SYS_PARAMS_GIVEN", "1", ':');
        pmix_list_append(ilist, &kv->super);
    }

    /* see if the user has a default MCA param file */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_USERID)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, uid, uint32_t);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
            break;
        }
    }
    if (UINT32_MAX == uid) {
        uid = geteuid();
    }
    /* try to get their home directory */
    home = pmix_home_directory(uid);
    if (NULL != home) {
        file = pmix_os_path(false, home, ".openmpi", "mca-params.conf", NULL);
        rc = process_param_file(file, ilist);
        free(file);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        /* add an envar indicating that we did this so the OMPI
         * processes won't duplicate it */
        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->key = strdup(PMIX_SET_ENVAR);
        kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->value->type = PMIX_ENVAR;
        PMIX_ENVAR_LOAD(&kv->value->data.envar, "OPAL_USER_PARAMS_GIVEN", "1", ':');
        pmix_list_append(ilist, &kv->super);
    }

    /* harvest our local envars */
    if (NULL != pmix_mca_pmdl_ompi_component.include) {
        pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                            "pmdl: ompi harvesting envars %s excluding %s",
                            (NULL == pmix_mca_pmdl_ompi_component.incparms)
                            ? "NONE"
                            : pmix_mca_pmdl_ompi_component.incparms,
                            (NULL == pmix_mca_pmdl_ompi_component.excparms)
                            ? "NONE"
                            : pmix_mca_pmdl_ompi_component.excparms);
        rc = pmix_util_harvest_envars(pmix_mca_pmdl_ompi_component.include,
                                      pmix_mca_pmdl_ompi_component.exclude, ilist);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
    }

    /* add in any OMPI-specific envars that were in an MCA
     * base paramfile since PMIx overlaps in that area */
    PMIX_LIST_FOREACH(fv, &myenvars, pmix_mca_base_var_file_value_t) {
        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->key = strdup(PMIX_SET_ENVAR);
        kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->value->type = PMIX_ENVAR;
        // the OMPI_MCA_ has already been prefixed
        PMIX_ENVAR_LOAD(&kv->value->data.envar, fv->mbvfv_var, fv->mbvfv_value, ':');
        pmix_list_append(ilist, &kv->super);
    }
    return PMIX_SUCCESS;
}

// These frameworks are current as of 16 Sep, 2022, and are the list
// of frameworks that are planned to be in Open MPI v5.0.0.
static char *ompi_frameworks_static_5_0_0[] = {
    // Generic prefixes used by OMPI
    "mca",
    "opal",
    "ompi",

    /* OPAL frameworks */
    "allocator",
    "backtrace",
    "btl",
    "dl",
    "hwloc",
    "if",
    "installdirs",
    "memchecker",
    "memcpy",
    "memory",
    "mpool",
    "patcher",
    "pmix",
    "rcache",
    "reachable",
    "shmem",
    "smsc",
    "threads",
    "timer",
    /* OMPI frameworks */
    "mpi", /* global options set in runtime/ompi_mpi_params.c */
    "bml",
    "coll",
    "fbtl",
    "fcoll",
    "fs",
    "hook",
    "io",
    "mtl",
    "op",
    "osc",
    "part",
    "pml",
    "sharedfp",
    "topo",
    "vprotocol",
    /* OSHMEM frameworks */
    "memheap",
    "scoll",
    "spml",
    "sshmem",
    NULL,
};
static char **ompi_frameworks = ompi_frameworks_static_5_0_0;
static bool ompi_frameworks_setup = false;

static void setup_ompi_frameworks(void)
{
    if (ompi_frameworks_setup) {
        return;
    }
    ompi_frameworks_setup = true;

    char *env = getenv("OMPI_MCA_PREFIXES");
    if (NULL == env) {
        return;
    }

    // If we found the env variable, it will be a comma-delimited list
    // of values.  Split it into an argv-style array.
    char **tmp = PMIx_Argv_split(env, ',');
    if (NULL != tmp) {
        ompi_frameworks = tmp;
    }
}

static void parse_file_envars(pmix_list_t *ilist)
{
    pmix_mca_base_var_file_value_t *fv, *fvsave;
    char *tmp;

    // ensure we have the current list of frameworks
    setup_ompi_frameworks();

    // scan the list for values directed at OMPI
    // frameworks/components
    PMIX_LIST_FOREACH_SAFE (fv, fvsave, ilist, pmix_mca_base_var_file_value_t) {
        for (int j = 0; NULL != ompi_frameworks[j]; j++) {
            if (0 == strncmp(fv->mbvfv_var, ompi_frameworks[j], strlen(ompi_frameworks[j]))) {
                pmix_list_remove_item(ilist, &fv->super);
                pmix_asprintf(&tmp, "OMPI_MCA_%s", fv->mbvfv_var);
                free(fv->mbvfv_var);
                fv->mbvfv_var = tmp;
                pmix_list_append(&myenvars, &fv->super);
                break;
            }
        }
    }
}

static pmix_status_t setup_nspace(pmix_namespace_t *nptr, pmix_info_t *info)
{
    pmdl_nspace_t *ns, *ns2;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi: setup nspace for nspace %s with %s", nptr->nspace,
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
    char **tmp, *ptr;
    size_t m;
    uint vers;
    bool takeus = false;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi: setup nspace_kv for nspace %s with %s", nptr->nspace,
                        kv->value->data.string);

    /* check the attribute */
    if (PMIX_CHECK_KEY(kv, PMIX_PROGRAMMING_MODEL) || PMIX_CHECK_KEY(kv, PMIX_PERSONALITY)) {
        tmp = PMIx_Argv_split(kv->value->data.string, ',');
        for (m = 0; NULL != tmp[m]; m++) {
            if (0 == strcmp(tmp[m], "ompi")) {
                /* they didn't specify a level, so we will service
                 * them just in case */
                takeus = true;
                break;
            }
            if (0 == strncmp(tmp[m], "ompi", 4)) {
                /* if they specifically requested an ompi level greater
                 * than or equal to us, then we service it */
                ptr = &tmp[m][4];
                vers = strtoul(ptr, NULL, 10);
                if (vers >= 5) {
                    takeus = true;
                }
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
    char *ev1, **tmp;
    pmix_proc_t wildcard, undef;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_info_t info[2];
    uint32_t n;
    pmix_cb_t cb;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi: register_nspace for %s", nptr->nspace);

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
         * it doesn't have any ompi-based apps */
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

    if (1 == ns->num_apps) {
        return PMIX_SUCCESS;
    }

    /* construct the list of app sizes */
    PMIX_LOAD_PROCID(&undef, nptr->nspace, PMIX_RANK_UNDEF);
    PMIX_INFO_LOAD(&info[0], PMIX_APP_INFO, NULL, PMIX_BOOL);
    tmp = NULL;
    for (n = 0; n < ns->num_apps; n++) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &undef;
        cb.copy = true;
        cb.info = info;
        cb.ninfo = 2;
        cb.key = PMIX_APP_SIZE;
        PMIX_INFO_LOAD(&info[1], PMIX_APPNUM, &n, PMIX_UINT32);
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        PMIX_INFO_DESTRUCT(&info[1]);
        cb.key = NULL;
        cb.info = NULL;
        cb.ninfo = 0;
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
        pmix_asprintf(&ev1, "%u", kv->value->data.uint32);
        PMIx_Argv_append_nosize(&tmp, ev1);
        free(ev1);
        PMIX_DESTRUCT(&cb);
    }
    PMIX_INFO_DESTRUCT(&info[0]);

    if (NULL != tmp) {
        ev1 = PMIx_Argv_join(tmp, ' ');
        PMIx_Argv_free(tmp);
        PMIX_INFO_LOAD(&info[0], "OMPI_APP_SIZES", ev1, PMIX_STRING);
        free(ev1);
        PMIX_GDS_CACHE_JOB_INFO(rc, pmix_globals.mypeer, nptr, info, 1);
        PMIX_INFO_DESTRUCT(&info[0]);
    }

    /* construct the list of app leaders */
    PMIX_INFO_LOAD(&info[0], PMIX_APP_INFO, NULL, PMIX_BOOL);
    tmp = NULL;
    for (n = 0; n < ns->num_apps; n++) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &undef;
        cb.copy = true;
        cb.info = info;
        cb.ninfo = 2;
        cb.key = PMIX_APPLDR;
        PMIX_INFO_LOAD(&info[1], PMIX_APPNUM, &n, PMIX_UINT32);
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        PMIX_INFO_DESTRUCT(&info[1]);
        cb.key = NULL;
        cb.info = NULL;
        cb.ninfo = 0;
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
        pmix_asprintf(&ev1, "%u", kv->value->data.uint32);
        PMIx_Argv_append_nosize(&tmp, ev1);
        free(ev1);
        PMIX_DESTRUCT(&cb);
    }
    PMIX_INFO_DESTRUCT(&info[0]);

    if (NULL != tmp) {
        ev1 = PMIx_Argv_join(tmp, ' ');
        PMIx_Argv_free(tmp);
        tmp = NULL;
        PMIX_INFO_LOAD(&info[0], "OMPI_FIRST_RANKS", ev1, PMIX_STRING);
        free(ev1);
        PMIX_GDS_CACHE_JOB_INFO(rc, pmix_globals.mypeer, nptr, info, 1);
        PMIX_INFO_DESTRUCT(&info[0]);
    }

    return PMIX_SUCCESS;
}

static pmix_status_t setup_fork(const pmix_proc_t *proc, char ***env, char ***priors)
{
    pmdl_nspace_t *ns, *ns2;
    char *param;
    char *ev1, **tmp;
    pmix_proc_t wildcard, undef;
    pmix_status_t rc;
    uint16_t u16;
    pmix_kval_t *kv;
    pmix_info_t info[2];
    uint32_t n;
    pmix_cb_t cb;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi: setup fork for %s", PMIX_NAME_PRINT(proc));

    /* don't do OMPI again if already done */
    if (NULL != *priors) {
        char **t2 = *priors;
        for (n = 0; NULL != t2[n]; n++) {
            if (0 == strncmp(t2[n], "ompi", 4)) {
                return PMIX_ERR_TAKE_NEXT_OPTION;
            }
        }
    }
    /* flag that we worked on this */
    PMIx_Argv_append_nosize(priors, "ompi");

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
         * it doesn't have any ompi-based apps */
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    PMIX_LOAD_PROCID(&wildcard, proc->nspace, PMIX_RANK_WILDCARD);
    PMIX_LOAD_PROCID(&undef, proc->nspace, PMIX_RANK_UNDEF);

    /* pass universe size */
    if (0 > asprintf(&param, "%u", ns->univ_size)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("OMPI_UNIVERSE_SIZE", param, true, env);
    free(param);

    /* pass the comm_world size in various formats */
    if (0 > asprintf(&param, "%u", ns->job_size)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("OMPI_COMM_WORLD_SIZE", param, true, env);
    PMIx_Setenv("OMPI_WORLD_SIZE", param, true, env);
    PMIx_Setenv("OMPI_MCA_num_procs", param, true, env);
    free(param);

    /* pass the local size in various formats */
    if (0 > asprintf(&param, "%u", ns->local_size)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("OMPI_COMM_WORLD_LOCAL_SIZE", param, true, env);
    PMIx_Setenv("OMPI_WORLD_LOCAL_SIZE", param, true, env);
    free(param);

    /* pass the number of apps in the job */
    if (0 > asprintf(&param, "%u", ns->num_apps)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("OMPI_NUM_APP_CTX", param, true, env);
    free(param);

    /* pass an envar so the proc can find any files it had prepositioned */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = (pmix_proc_t *) proc;
    cb.copy = true;
    cb.key = PMIX_PROCDIR;
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
    PMIx_Setenv("OMPI_FILE_LOCATION", kv->value->data.string, true, env);
    PMIX_DESTRUCT(&cb);

    /* pass the cwd */
    PMIX_INFO_LOAD(&info[0], PMIX_APP_INFO, NULL, PMIX_BOOL);
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = &undef;
    cb.copy = true;
    cb.info = info;
    cb.ninfo = 2;
    cb.key = PMIX_WDIR;
    PMIX_INFO_LOAD(&info[1], PMIX_APPNUM, &pmix_globals.appnum, PMIX_UINT32);
    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
    PMIX_INFO_DESTRUCT(&info[1]);
    cb.key = NULL;
    cb.info = NULL;
    cb.ninfo = 0;
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
    PMIx_Setenv("OMPI_MCA_initial_wdir", kv->value->data.string, true, env);
    PMIX_DESTRUCT(&cb);
    PMIX_INFO_DESTRUCT(&info[0]);

    /* pass its command. */
    PMIX_INFO_LOAD(&info[0], PMIX_APP_INFO, NULL, PMIX_BOOL);
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = &undef;
    cb.copy = true;
    cb.info = info;
    cb.ninfo = 2;
    cb.key = PMIX_APP_ARGV;
    PMIX_INFO_LOAD(&info[1], PMIX_APPNUM, &pmix_globals.appnum, PMIX_UINT32);
    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
    PMIX_INFO_DESTRUCT(&info[1]);
    cb.key = NULL;
    cb.info = NULL;
    cb.ninfo = 0;
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
    tmp = PMIx_Argv_split(kv->value->data.string, ' ');
    PMIX_DESTRUCT(&cb);
    PMIX_INFO_DESTRUCT(&info[0]);
    PMIx_Setenv("OMPI_COMMAND", tmp[0], true, env);
    ev1 = PMIx_Argv_join(&tmp[1], ' ');
    PMIx_Setenv("OMPI_ARGV", ev1, true, env);
    free(ev1);
    PMIx_Argv_free(tmp);

    /* pass the arch - if available */
#ifdef HAVE_SYS_UTSNAME_H
    struct utsname sysname;
    memset(&sysname, 0, sizeof(sysname));
    if (-1 < uname(&sysname)) {
        if (sysname.machine[0] != '\0') {
            PMIx_Setenv("OMPI_MCA_cpu_type", (const char *) &sysname.machine, true, env);
        }
    }
#endif

    /* pass the rank */
    if (0 > asprintf(&param, "%lu", (unsigned long) proc->rank)) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Setenv("OMPI_COMM_WORLD_RANK", param, true, env);
    free(param); /* done with this now */

    /* get the proc's local rank */
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
    PMIx_Setenv("OMPI_COMM_WORLD_LOCAL_RANK", param, true, env);
    free(param);

    /* get the proc's node rank */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = (pmix_proc_t *) proc;
    cb.copy = true;
    cb.key = PMIX_NODE_RANK;
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
    PMIx_Setenv("OMPI_COMM_WORLD_NODE_RANK", param, true, env);
    free(param);

    if (1 == ns->num_apps) {
        return PMIX_SUCCESS;
    }

    PMIX_LOAD_PROCID(&undef, proc->nspace, PMIX_RANK_UNDEF);
    PMIX_INFO_LOAD(&info[0], PMIX_APP_INFO, NULL, PMIX_BOOL);
    tmp = NULL;
    for (n = 0; n < ns->num_apps; n++) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &undef;
        cb.copy = true;
        cb.info = info;
        cb.ninfo = 2;
        cb.key = PMIX_APP_SIZE;
        PMIX_INFO_LOAD(&info[1], PMIX_APPNUM, &n, PMIX_UINT32);
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        PMIX_INFO_DESTRUCT(&info[1]);
        cb.key = NULL;
        cb.info = NULL;
        cb.ninfo = 0;
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
        pmix_asprintf(&ev1, "%u", kv->value->data.uint32);
        PMIx_Argv_append_nosize(&tmp, ev1);
        free(ev1);
        PMIX_DESTRUCT(&cb);
    }
    PMIX_INFO_DESTRUCT(&info[0]);

    if (NULL != tmp) {
        ev1 = PMIx_Argv_join(tmp, ' ');
        PMIx_Argv_free(tmp);
        PMIx_Setenv("OMPI_APP_CTX_NUM_PROCS", ev1, true, env);
        free(ev1);
    }

    PMIX_INFO_LOAD(&info[0], PMIX_APP_INFO, NULL, PMIX_BOOL);
    tmp = NULL;
    for (n = 0; n < ns->num_apps; n++) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &undef;
        cb.copy = true;
        cb.info = info;
        cb.ninfo = 2;
        cb.key = PMIX_APPLDR;
        PMIX_INFO_LOAD(&info[1], PMIX_APPNUM, &n, PMIX_UINT32);
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        PMIX_INFO_DESTRUCT(&info[1]);
        cb.key = NULL;
        cb.info = NULL;
        cb.ninfo = 0;
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
        pmix_asprintf(&ev1, "%u", kv->value->data.uint32);
        PMIx_Argv_append_nosize(&tmp, ev1);
        free(ev1);
        PMIX_DESTRUCT(&cb);
    }
    PMIX_INFO_DESTRUCT(&info[0]);

    if (NULL != tmp) {
        ev1 = PMIx_Argv_join(tmp, ' ');
        PMIx_Argv_free(tmp);
        tmp = NULL;
        PMIx_Setenv("OMPI_FIRST_RANKS", ev1, true, env);
        free(ev1);
    }

    /* provide the reincarnation number */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = (pmix_proc_t *) proc;
    cb.copy = true;
    cb.key = PMIX_REINCARNATION;
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
    pmix_asprintf(&ev1, "%u", kv->value->data.uint32);
    PMIx_Setenv("OMPI_MCA_num_restarts", ev1, true, env);
    free(ev1);
    PMIX_DESTRUCT(&cb);

    /* add any envars we collected from param files */
    PMIX_LIST_FOREACH(kv, &myenvars, pmix_kval_t) {
        PMIx_Setenv(kv->value->data.envar.envar, kv->value->data.envar.value, true, env);
    }

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
