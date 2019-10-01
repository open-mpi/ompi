/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <time.h>

#include <pmix.h>

#include "src/mca/base/pmix_mca_base_var.h"
#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/util/alfg.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/name_fns.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/mca/preg/preg.h"

#include "src/mca/pmdl/pmdl.h"
#include "src/mca/pmdl/base/base.h"
#include "pmdl_ompi.h"

static pmix_status_t ompi_init(void);
static void ompi_finalize(void);
static pmix_status_t harvest_envars(pmix_namespace_t *nptr,
                                    pmix_info_t info[], size_t ninfo,
                                    pmix_list_t *ilist);
static pmix_status_t setup_nspace(pmix_namespace_t *nptr,
                                  uint32_t appnum,
                                  pmix_info_t *info);
static pmix_status_t setup_nspace_kv(pmix_namespace_t *nptr,
                                     uint32_t appnum,
                                     pmix_kval_t *kv);
static pmix_status_t setup_client(pmix_namespace_t *nptr,
                                  pmix_rank_t rank,
                                  uint32_t appnum);
static pmix_status_t setup_fork(const pmix_proc_t *proc,
                                char ***env);
static void deregister_nspace(pmix_namespace_t *nptr);
pmix_pmdl_module_t pmix_pmdl_ompi_module = {
    .name = "ompi",
    .init = ompi_init,
    .finalize = ompi_finalize,
    .harvest_envars = harvest_envars,
    .setup_nspace = setup_nspace,
    .setup_nspace_kv = setup_nspace_kv,
    .setup_client = setup_client,
    .setup_fork = setup_fork,
    .deregister_nspace = deregister_nspace
};

/* internal structures */
typedef struct {
	pmix_list_item_t super;
	bool ompi;
	uint32_t appnum;
	pmix_rank_t start;
	pmix_rank_t end;
	uint32_t num_procs;
} pmdl_app_t;
static void apcon(pmdl_app_t *p)
{
	p->ompi = false;
	p->appnum = 0;
	p->start = UINT32_MAX;
	p->end = 0;
	p->num_procs = 0;
}
static PMIX_CLASS_INSTANCE(pmdl_app_t,
						   pmix_list_item_t,
						   apcon, NULL);

typedef struct {
	pmix_list_item_t super;
	pmix_nspace_t nspace;
    bool ompi;
	bool datacollected;
	uint32_t univ_size;
	uint32_t job_size;
	uint32_t local_size;
	uint32_t num_apps;
	pmix_list_t apps;
} pmdl_nspace_t;
static void nscon(pmdl_nspace_t *p)
{
    p->ompi = false;
	p->datacollected = false;
	p->univ_size = 0;
	p->job_size = 0;
	p->local_size = 0;
	p->num_apps = 0;
	PMIX_CONSTRUCT(&p->apps, pmix_list_t);
}
static void nsdes(pmdl_nspace_t *p)
{
	PMIX_LIST_DESTRUCT(&p->apps);
}
static PMIX_CLASS_INSTANCE(pmdl_nspace_t,
						   pmix_list_item_t,
						   nscon, nsdes);

/* internal variables */
static pmix_list_t mynspaces;

static pmix_status_t ompi_init(void)
{
    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl: ompi init");

    PMIX_CONSTRUCT(&mynspaces, pmix_list_t);

    return PMIX_SUCCESS;
}

static void ompi_finalize(void)
{
    PMIX_LIST_DESTRUCT(&mynspaces);
}

static pmix_status_t harvest_envars(pmix_namespace_t *nptr,
                                    pmix_info_t info[], size_t ninfo,
                                    pmix_list_t *ilist)
{
    char *cs_env, *string_key;
    pmix_kval_t *kv;
    size_t n;
    pmdl_nspace_t *ns, *ns2;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi:harvest envars");

    /* see if we already have this nspace */
    ns = NULL;
    PMIX_LIST_FOREACH(ns2, &mynspaces, pmdl_nspace_t) {
        if (PMIX_CHECK_NSPACE(ns2->nspace, nptr->nspace)) {
            ns = ns2;
            break;
        }
    }
    if (NULL == ns) {
        ns = PMIX_NEW(pmdl_nspace_t);
        PMIX_LOAD_NSPACE(ns->nspace, nptr->nspace);
        pmix_list_append(&mynspaces, &ns->super);
        /* check the directives */
        for (n=0; n < ninfo; n++) {
            /* check the attribute */
            if (PMIX_CHECK_KEY(&info[n], PMIX_PROGRAMMING_MODEL)) {
                if (0 == strcasecmp(info->value.data.string, "ompi")) {
                    ns->ompi = true;
                    break;
                }
            }
        }
    }
    if (!ns->ompi) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* OMPI forwards everything that starts with OMPI_ */
    for (n=0; NULL != environ[n]; n++) {
        if (0 == strncmp(environ[n], "OMPI_", 5)) {
            cs_env = strdup(environ[n]);
            kv = PMIX_NEW(pmix_kval_t);
            if (NULL == kv) {
                free(cs_env);
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            kv->key = strdup(PMIX_SET_ENVAR);
            kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
            if (NULL == kv->value) {
                PMIX_RELEASE(kv);
                free(cs_env);
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            kv->value->type = PMIX_ENVAR;
            string_key = strchr(cs_env, '=');
            if (NULL == string_key) {
                free(cs_env);
                PMIX_RELEASE(kv);
                return PMIX_ERR_BAD_PARAM;
            }
            *string_key = '\0';
            ++string_key;
            pmix_output_verbose(5, pmix_pmdl_base_framework.framework_output,
                                "pmdl:ompi: adding envar %s", cs_env);
            PMIX_ENVAR_LOAD(&kv->value->data.envar, cs_env, string_key, ':');
            pmix_list_append(ilist, &kv->super);
            free(cs_env);
        }
    }

    return PMIX_SUCCESS;
}


static pmix_status_t setup_nspace(pmix_namespace_t *nptr,
                                  uint32_t appnum,
                                  pmix_info_t *info)
{
	pmdl_nspace_t *ns, *ns2;
	pmdl_app_t *ap, *ap2;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi: setup nspace for app %u with %s",
                        appnum, info->value.data.string);

	/* see if we already have this nspace */
	ns = NULL;
	PMIX_LIST_FOREACH(ns2, &mynspaces, pmdl_nspace_t) {
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
	/* see if we have this appnum yet */
	ap = NULL;
	PMIX_LIST_FOREACH(ap2, &ns->apps, pmdl_app_t) {
		if (ap2->appnum == appnum) {
			ap = ap2;
			break;
		}
	}
	if (NULL == ap) {
		ap = PMIX_NEW(pmdl_app_t);
		ap->appnum = appnum;
		pmix_list_append(&ns->apps, &ap->super);
	}
	/* check the attribute */
	if (PMIX_CHECK_KEY(info, PMIX_PROGRAMMING_MODEL)) {
		if (0 == strcasecmp(info->value.data.string, "ompi")) {
			ap->ompi = true;
            ns->ompi = true;  // flag that at least one app is ompi
		}
	}
	/* we don't care about the rest of the possible values */
	return PMIX_SUCCESS;
}

static pmix_status_t setup_nspace_kv(pmix_namespace_t *nptr,
                                     uint32_t appnum,
                                     pmix_kval_t *kv)
{
	pmdl_nspace_t *ns, *ns2;
	pmdl_app_t *ap, *ap2;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi: setup nspace_kv with %s", kv->value->data.string);

	/* see if we already have this nspace */
	ns = NULL;
	PMIX_LIST_FOREACH(ns2, &mynspaces, pmdl_nspace_t) {
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
	/* see if we have this appnum yet */
	ap = NULL;
	PMIX_LIST_FOREACH(ap2, &ns->apps, pmdl_app_t) {
		if (ap2->appnum == appnum) {
			ap = ap2;
			break;
		}
	}
	if (NULL == ap) {
		ap = PMIX_NEW(pmdl_app_t);
		ap->appnum = appnum;
		pmix_list_append(&ns->apps, &ap->super);
	}
	/* check the attribute */
	if (PMIX_CHECK_KEY(kv, PMIX_PROGRAMMING_MODEL)) {
		if (0 == strcasecmp(kv->value->data.string, "ompi")) {
			ap->ompi = true;
            ns->ompi = true;  // flag that at least one app is ompi
		}
	}
	/* we don't care about the rest of the possible values */
	return PMIX_SUCCESS;
}

static pmix_status_t setup_client(pmix_namespace_t *nptr,
                                  pmix_rank_t rank,
                                  uint32_t appnum)
{
	pmdl_nspace_t *ns, *ns2;
	pmdl_app_t *ap, *ap2;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi: setup client with %s", PMIX_RANK_PRINT(rank));

	/* see if we already have this nspace */
	ns = NULL;
	PMIX_LIST_FOREACH(ns2, &mynspaces, pmdl_nspace_t) {
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
	/* see if we have this appnum yet */
	ap = NULL;
	PMIX_LIST_FOREACH(ap2, &ns->apps, pmdl_app_t) {
		if (ap2->appnum == appnum) {
			ap = ap2;
			break;
		}
	}
	if (NULL == ap) {
		ap = PMIX_NEW(pmdl_app_t);
		ap->appnum = appnum;
		pmix_list_append(&ns->apps, &ap->super);
	}
	/* adjust the ranks as necessary */
	if (rank < ap->start) {
		ap->start = rank;
	}
	if (rank > ap->end) {
		ap->end = rank;
	}
	return PMIX_SUCCESS;
}

static pmix_status_t setup_fork(const pmix_proc_t *proc,
                                char ***env)
{
	pmdl_nspace_t *ns, *ns2;
	pmdl_app_t *ap, *ap2;
	char *param;
	pmix_proc_t wildcard, undef;
	pmix_status_t rc;
	pmix_value_t *val;
	pmix_info_t info[2];
	uint16_t u16;

    pmix_output_verbose(2, pmix_pmdl_base_framework.framework_output,
                        "pmdl:ompi: setup fork for %s", PMIX_NAME_PRINT(proc));

	/* see if we already have this nspace */
	ns = NULL;
	PMIX_LIST_FOREACH(ns2, &mynspaces, pmdl_nspace_t) {
		if (PMIX_CHECK_NSPACE(ns2->nspace, proc->nspace)) {
			ns = ns2;
			break;
		}
	}
	if (NULL == ns || !ns->ompi) {
		/* we don't know anything about this one or
         * it doesn't have any ompi-based apps */
		return PMIX_ERR_TAKE_NEXT_OPTION;
	}
	/* see if we have this rank */
	ap = NULL;
	PMIX_LIST_FOREACH(ap2, &ns->apps, pmdl_app_t) {
		if (proc->rank >= ap2->start &&
			proc->rank <= ap2->end) {
			ap = ap2;
			break;
		}
	}
    if (NULL != ap && !ap->ompi) {
        /* not an OMPI app */
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

	/* do we already have the data we need here? */
	if (!ns->datacollected) {
	    (void)strncpy(wildcard.nspace, proc->nspace, PMIX_MAX_NSLEN);
	    wildcard.rank = PMIX_RANK_WILDCARD;
	    (void)strncpy(undef.nspace, proc->nspace, PMIX_MAX_NSLEN);
	    undef.rank = PMIX_RANK_UNDEF;

		/* fetch the universe size */
	    if (PMIX_SUCCESS == (rc = PMIx_Get(&wildcard, PMIX_UNIV_SIZE, NULL, 0, &val))) {
            PMIX_VALUE_GET_NUMBER(rc, val, ns->univ_size, uint32_t);
            PMIX_VALUE_RELEASE(val);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
	    }
		/* fetch the job size */
	    if (PMIX_SUCCESS == (rc = PMIx_Get(&wildcard, PMIX_JOB_SIZE, NULL, 0, &val))) {
            PMIX_VALUE_GET_NUMBER(rc, val, ns->job_size, uint32_t);
            PMIX_VALUE_RELEASE(val);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
	    }
		/* fetch the number of local procs */
	    if (PMIX_SUCCESS == (rc = PMIx_Get(&wildcard, PMIX_LOCAL_SIZE, NULL, 0, &val))) {
            PMIX_VALUE_GET_NUMBER(rc, val, ns->local_size, uint32_t);
            PMIX_VALUE_RELEASE(val);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
	    }
		/* fetch the number of apps */
	    if (PMIX_SUCCESS == (rc = PMIx_Get(&wildcard, PMIX_JOB_NUM_APPS, NULL, 0, &val))) {
            PMIX_VALUE_GET_NUMBER(rc, val, ns->num_apps, uint32_t);
            PMIX_VALUE_RELEASE(val);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
	    }
        if (NULL != ap) {
    		/* fetch the number of procs in this app */
    		PMIX_INFO_LOAD(&info[0], PMIX_APP_INFO, NULL, PMIX_BOOL);
    		PMIX_INFO_LOAD(&info[1], PMIX_APPNUM, &ap->appnum, PMIX_UINT32);
    	    if (PMIX_SUCCESS == (rc = PMIx_Get(&undef, PMIX_APP_SIZE, info, 2, &val))) {
                PMIX_VALUE_GET_NUMBER(rc, val, ap->num_procs, uint32_t);
                PMIX_VALUE_RELEASE(val);
                if (PMIX_SUCCESS != rc) {
                    PMIX_INFO_DESTRUCT(&info[0]);
                    PMIX_INFO_DESTRUCT(&info[1]);
                    return rc;
                }
    	    }
    	    PMIX_INFO_DESTRUCT(&info[0]);
    	    PMIX_INFO_DESTRUCT(&info[1]);
        }
	}

    if (UINT32_MAX != ns->univ_size) {
        if (0 > asprintf(&param, "%u", ns->univ_size)) {
        	return PMIX_ERR_NOMEM;
        }
        pmix_setenv("OMPI_UNIVERSE_SIZE", param, true, env);
        free(param);
    }

    if (UINT32_MAX != ns->job_size) {
        if (0 > asprintf(&param, "%u", ns->job_size)) {
        	return PMIX_ERR_NOMEM;
        }
        pmix_setenv("OMPI_COMM_WORLD_SIZE", param, true, env);
        free(param);
    }

    if (UINT32_MAX != ns->local_size) {
        if (0 > asprintf(&param, "%u", ns->local_size)) {
        	return PMIX_ERR_NOMEM;
        }
        pmix_setenv("OMPI_COMM_WORLD_LOCAL_SIZE", param, true, env);
        free(param);
    }

    /* add the MPI-3 envars */
    if (UINT32_MAX != ns->num_apps) {
        if (0 > asprintf(&param, "%u", ns->num_apps)) {
        	return PMIX_ERR_NOMEM;
        }
        pmix_setenv("OMPI_NUM_APP_CTX", param, true, env);
        free(param);
    }

    if (NULL != ap && UINT32_MAX != ap->num_procs) {
        if (0 > asprintf(&param, "%u", ap->num_procs)) {
        	return PMIX_ERR_NOMEM;
        }
        pmix_setenv("OMPI_APP_CTX_NUM_PROCS", param, true, env);
        free(param);
    }

    if (0 > asprintf(&param, "%lu", (unsigned long)proc->rank)) {
    	return PMIX_ERR_NOMEM;
    }
    pmix_setenv("OMPI_COMM_WORLD_RANK", param, true, env);
    free(param);  /* done with this now */

    /* get the proc's local rank */
    if (PMIX_SUCCESS == (rc = PMIx_Get(proc, PMIX_LOCAL_RANK, NULL, 0, &val))) {
        PMIX_VALUE_GET_NUMBER(rc, val, u16, uint16_t);
        PMIX_VALUE_RELEASE(val);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        if (0 > asprintf(&param, "%lu", (unsigned long)u16)) {
            return PMIX_ERR_NOMEM;
        }
        pmix_setenv("OMPI_COMM_WORLD_LOCAL_RANK", param, true, env);
        free(param);
    }

    /* get the proc's node rank */
    if (PMIX_SUCCESS == (rc = PMIx_Get(proc, PMIX_NODE_RANK, NULL, 0, &val))) {
        PMIX_VALUE_GET_NUMBER(rc, val, u16, uint16_t);
        PMIX_VALUE_RELEASE(val);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        if (0 > asprintf(&param, "%lu", (unsigned long)u16)) {
            return PMIX_ERR_NOMEM;
        }
        pmix_setenv("OMPI_COMM_WORLD_NODE_RANK", param, true, env);
        free(param);
    }

    /* pass an envar so the proc can find any files it had prepositioned */
    if (PMIX_SUCCESS == (rc = PMIx_Get(proc, PMIX_PROCDIR, NULL, 0, &val))) {
        pmix_setenv("OMPI_FILE_LOCATION", val->data.string, true, env);
        PMIX_VALUE_RELEASE(val);
    }

    return PMIX_SUCCESS;
}

static void deregister_nspace(pmix_namespace_t *nptr)
{
	pmdl_nspace_t *ns;

	/* find our tracker for this nspace */
	PMIX_LIST_FOREACH(ns, &mynspaces, pmdl_nspace_t) {
		if (PMIX_CHECK_NSPACE(ns->nspace, nptr->nspace)) {
			pmix_list_remove_item(&mynspaces, &ns->super);
			PMIX_RELEASE(ns);
			return;
		}
	}
}
