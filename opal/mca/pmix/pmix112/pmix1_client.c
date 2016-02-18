/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/hash_string.h"
#include "opal/util/argv.h"
#include "opal/util/proc.h"

#include "opal/mca/pmix/base/base.h"
#include "pmix1.h"
#include "opal/mca/pmix/pmix112/pmix/include/pmix.h"
#include "opal/mca/pmix/pmix112/pmix/src/buffer_ops/buffer_ops.h"

static pmix_proc_t my_proc;
static char *dbgvalue=NULL;
static int errhdler_ref = 0;

static void release_cbfunc(void *cbdata)
{
    pmix1_opalcaddy_t *cd = (pmix1_opalcaddy_t*)cbdata;
    OBJ_RELEASE(cd);
}
static void myerr(pmix_status_t status,
                  pmix_proc_t procs[], size_t nprocs,
                  pmix_info_t info[], size_t ninfo)
{
    int rc;
    opal_namelist_t *nm;
    opal_value_t *iptr;
    size_t n;
    pmix1_opalcaddy_t *cd;

    /* convert the incoming status */
    rc = pmix1_convert_rc(status);

    /* setup the caddy */
    cd = OBJ_NEW(pmix1_opalcaddy_t);

    /* convert the array of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(cd);
            return;
        }
        nm->name.vpid = procs[n].rank;
        opal_list_append(&cd->procs, &nm->super);
    }

    /* convert the array of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        iptr->key = strdup(info[n].key);
        pmix1_value_unload(iptr, &info[n].value);
        opal_list_append(&cd->info, &iptr->super);
    }

    /* call the base errhandler */
    opal_pmix_base_errhandler(rc, &cd->procs, &cd->info, release_cbfunc, cd);
}

static void errreg_cbfunc (pmix_status_t status,
                          int errhandler_ref,
                          void *cbdata)
{
    errhdler_ref = errhandler_ref;
    opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                        "PMIX client errreg_cbfunc - error handler registered status=%d, reference=%d",
                         status, errhandler_ref);
}

int pmix1_client_init(void)
{
    opal_process_name_t pname;
    pmix_status_t rc;
    int dbg;
    opal_pmix1_jobid_trkr_t *job;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client init");

    if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
        asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
        putenv(dbgvalue);
    }
    rc = PMIx_Init(&my_proc);
    if (PMIX_SUCCESS != rc) {
        return pmix1_convert_rc(rc);
    }

    /* store our jobid and rank */
   if (NULL != getenv(OPAL_MCA_PREFIX"orte_launch")) {
        /* if we were launched by the OMPI RTE, then
         * the jobid is in a special format - so get it */
        mca_pmix_pmix112_component.native_launch = true;
        opal_convert_string_to_jobid(&pname.jobid, my_proc.nspace);
    } else {
        /* we were launched by someone else, so make the
         * jobid just be the hash of the nspace */
        OPAL_HASH_STR(my_proc.nspace, pname.jobid);
    }
    /* insert this into our list of jobids - it will be the
     * first, and so we'll check it first */
    job = OBJ_NEW(opal_pmix1_jobid_trkr_t);
    (void)strncpy(job->nspace, my_proc.nspace, PMIX_MAX_NSLEN);
    job->jobid = pname.jobid;
    opal_list_append(&mca_pmix_pmix112_component.jobids, &job->super);

    pname.vpid = my_proc.rank;
    opal_proc_set_name(&pname);

    /* register the errhandler */
    PMIx_Register_errhandler(NULL, 0, myerr, errreg_cbfunc, NULL );
    return OPAL_SUCCESS;

}

int pmix1_client_finalize(void)
{
    pmix_status_t rc;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client finalize");

    /* deregister the errhandler */
    PMIx_Deregister_errhandler(errhdler_ref, NULL, NULL);

    rc = PMIx_Finalize();

    return pmix1_convert_rc(rc);
}

int pmix1_initialized(void)
{
    pmix_status_t rc;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client initialized");

    rc = PMIx_Initialized();
    return pmix1_convert_rc(rc);
}

int pmix1_abort(int flag, const char *msg,
                  opal_list_t *procs)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client abort");

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            /* look thru our list of jobids and find the
             * corresponding nspace */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
                if (jptr->jobid == ptr->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                return OPAL_ERR_NOT_FOUND;
            }
            (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
            parray[n].rank = ptr->name.vpid;
            ++n;
        }
    }

    /* call the library abort */
    rc = PMIx_Abort(flag, msg, parray, cnt);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);

    return pmix1_convert_rc(rc);
}

int pmix1_store_local(const opal_process_name_t *proc, opal_value_t *val)
{
    pmix_value_t kv;
    pmix_status_t rc;
    pmix_proc_t p;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    if (NULL != proc) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (jptr->jobid == proc->jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
            return OPAL_ERR_NOT_FOUND;
        }
        (void)strncpy(p.nspace, job->nspace, PMIX_MAX_NSLEN);
        p.rank = proc->vpid;
    } else {
        /* use our name */
        (void)strncpy(p.nspace, my_proc.nspace, PMIX_MAX_NSLEN);
        p.rank = OPAL_PROC_MY_NAME.vpid;
    }

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix1_value_load(&kv, val);

    rc = PMIx_Store_internal(&p, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);

    return pmix1_convert_rc(rc);
}

int pmix1_commit(void)
{
    pmix_status_t rc;

    rc = PMIx_Commit();
    return pmix1_convert_rc(rc);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;

    if (NULL != op->opcbfunc) {
        op->opcbfunc(pmix1_convert_rc(status), op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix1_fence(opal_list_t *procs, int collect_data)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix_info_t info, *iptr;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client fence");

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            /* look thru our list of jobids and find the
             * corresponding nspace */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
                if (jptr->jobid == ptr->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                return OPAL_ERR_NOT_FOUND;
            }
            (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
            parray[n].rank = ptr->name.vpid;
            ++n;
        }
    }
    if (collect_data) {
        PMIX_INFO_CONSTRUCT(&info);
        (void)strncpy(info.key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);
        info.value.type = PMIX_BOOL;
        info.value.data.flag = true;
        iptr = &info;
        n = 1;
    } else {
        iptr = NULL;
        n = 0;
    }

    /* call the library function */
    rc = PMIx_Fence(parray, cnt, iptr, n);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);
    if (NULL != iptr) {
        PMIX_INFO_DESTRUCT(&info);
    }

    return pmix1_convert_rc(rc);

}

int pmix1_fencenb(opal_list_t *procs, int collect_data,
                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix1_opcaddy_t *op;
    pmix_info_t info, *iptr;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client fence_nb");

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            /* look thru our list of jobids and find the
             * corresponding nspace */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
                if (jptr->jobid == ptr->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                return OPAL_ERR_NOT_FOUND;
            }
            (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
            parray[n].rank = ptr->name.vpid;
            ++n;
        }
    }

    if (collect_data) {
        PMIX_INFO_CONSTRUCT(&info);
        (void)strncpy(info.key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);
        iptr = &info;
        n = 1;
    } else {
        iptr = NULL;
        n = 0;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->procs = parray;
    op->nprocs = cnt;

    /* call the library function */
    rc = PMIx_Fence_nb(parray, cnt, iptr, n, opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }

    return pmix1_convert_rc(rc);

}

int pmix1_put(opal_pmix_scope_t scope,
              opal_value_t *val)
{
    pmix_value_t kv;
    pmix_status_t rc;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client put");

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix1_value_load(&kv, val);

    rc = PMIx_Put(scope, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);
    return pmix1_convert_rc(rc);
}

int pmix1_get(const opal_process_name_t *proc, const char *key,
              opal_list_t *info, opal_value_t **val)
{
    int ret;
    pmix_value_t *kv;
    pmix_status_t rc;
    pmix_proc_t p, *pptr;
    size_t ninfo, n;
    pmix_info_t *pinfo;
    opal_value_t *ival;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "%s PMIx_client get on proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == proc) ? "NULL" : OPAL_NAME_PRINT(*proc), key);

    /* prep default response */
    *val = NULL;
    if (NULL != proc) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (jptr->jobid == proc->jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        (void)strncpy(p.nspace, job->nspace, PMIX_MAX_NSLEN);
        p.rank = proc->vpid;
        pptr = &p;
    } else {
        /* if they are asking for our jobid, then return it */
        if (0 == strcmp(key, OPAL_PMIX_JOBID)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_UINT32;
            (*val)->data.uint32 = OPAL_PROC_MY_NAME.jobid;
            return OPAL_SUCCESS;
        } else if (0 == strcmp(key, OPAL_PMIX_RANK)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_INT;
            (*val)->data.integer = my_proc.rank;
            return OPAL_SUCCESS;
        }
        pptr = NULL;
    }

    if (NULL != info) {
        ninfo = opal_list_get_size(info);
        if (0 < ninfo) {
            PMIX_INFO_CREATE(pinfo, ninfo);
            n=0;
            OPAL_LIST_FOREACH(ival, info, opal_value_t) {
                (void)strncpy(pinfo[n].key, ival->key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&pinfo[n].value, ival);
            }
        } else {
            pinfo = NULL;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    /* pass the request down */
    rc = PMIx_Get(pptr, key, pinfo, ninfo, &kv);
    if (PMIX_SUCCESS == rc) {
        if (NULL == kv) {
            ret = OPAL_SUCCESS;
        } else {
            *val = OBJ_NEW(opal_value_t);
            ret = pmix1_value_unload(*val, kv);
            PMIX_VALUE_FREE(kv, 1);
        }
    } else {
        ret = pmix1_convert_rc(rc);
    }
    PMIX_INFO_FREE(pinfo, ninfo);
    return ret;
}

static void val_cbfunc(pmix_status_t status,
                       pmix_value_t *kv, void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;
    int rc;
    opal_value_t val, *v=NULL;

    rc = pmix1_convert_opalrc(status);
    if (PMIX_SUCCESS == status && NULL != kv) {
        rc = pmix1_value_unload(&val, kv);
        v = &val;
    }

    if (NULL != op->valcbfunc) {
        op->valcbfunc(rc, v, op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix1_getnb(const opal_process_name_t *proc, const char *key,
                opal_list_t *info,
                opal_pmix_value_cbfunc_t cbfunc, void *cbdata)
{
    pmix1_opcaddy_t *op;
    pmix_status_t rc;
    size_t n;
    opal_value_t *ival;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "%s PMIx_client get_nb on proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == proc) ? "NULL" : OPAL_NAME_PRINT(*proc), key);

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->valcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != proc) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (jptr->jobid == proc->jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        (void)strncpy(op->p.nspace, job->nspace, PMIX_MAX_NSLEN);
        op->p.rank = proc->vpid;
    } else {
        (void)strncpy(op->p.nspace, my_proc.nspace, PMIX_MAX_NSLEN);
        op->p.rank = PMIX_RANK_WILDCARD;
    }

    if (NULL != info) {
        op->sz = opal_list_get_size(info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(ival, info, opal_value_t) {
                (void)strncpy(op->info[n].key, ival->key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&op->info[n].value, ival);
            }
        }
    }

    /* call the library function */
    rc = PMIx_Get_nb(&op->p, key, op->info, op->sz, val_cbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }

    return pmix1_convert_rc(rc);
}

int pmix1_publish(opal_list_t *info)
{
    pmix_info_t *pinfo;
    pmix_status_t ret;
    opal_value_t *iptr;
    size_t sz, n;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client publish");

    if (NULL == info) {
        return OPAL_ERR_BAD_PARAM;
    }

    sz = opal_list_get_size(info);
    if (0 < sz) {
        PMIX_INFO_CREATE(pinfo, sz);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&pinfo[n].value, iptr);
            ++n;
        }
    } else {
        pinfo = NULL;
    }

    ret = PMIx_Publish(pinfo, sz);

    return pmix1_convert_rc(ret);
}

int pmix1_publishnb(opal_list_t *info,
                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    opal_value_t *iptr;
    size_t n;
    pmix1_opcaddy_t *op;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client publish_nb");

    if (NULL == info) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    op->sz = opal_list_get_size(info);
    if (0 < op->sz) {
        PMIX_INFO_CREATE(op->info, op->sz);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&op->info[n].value, iptr);
            ++n;
        }
    }

    ret = PMIx_Publish_nb(op->info, op->sz, opcbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_lookup(opal_list_t *data, opal_list_t *info)
{
    pmix_pdata_t *pdata;
    pmix_info_t *pinfo;
    size_t sz, ninfo, n;
    int rc;
    pmix_status_t ret;
    opal_pmix_pdata_t *d;
    opal_value_t *iptr;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client lookup");

    if (NULL == data) {
        return OPAL_ERR_BAD_PARAM;
    }

    sz = opal_list_get_size(data);
    PMIX_PDATA_CREATE(pdata, sz);
    n=0;
    OPAL_LIST_FOREACH(d, data, opal_pmix_pdata_t) {
        (void)strncpy(pdata[n++].key, d->value.key, PMIX_MAX_KEYLEN);
    }

    if (NULL != info) {
        ninfo = opal_list_get_size(info);
        PMIX_INFO_CREATE(pinfo, ninfo);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(pinfo[n++].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&pinfo[n].value, iptr);
            ++n;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    ret = PMIx_Lookup(pdata, sz, pinfo, ninfo);
    PMIX_INFO_FREE(pinfo, ninfo);

    if (PMIX_SUCCESS == ret) {
        /* transfer the data back */
        n=0;
        OPAL_LIST_FOREACH(d, data, opal_pmix_pdata_t) {
            if (mca_pmix_pmix112_component.native_launch) {
                /* if we were launched by the OMPI RTE, then
                 * the jobid is in a special format - so get it */
                opal_convert_string_to_jobid(&d->proc.jobid, pdata[n].proc.nspace);
            } else {
                /* we were launched by someone else, so make the
                 * jobid just be the hash of the nspace */
                OPAL_HASH_STR(pdata[n].proc.nspace, d->proc.jobid);
            }
            /* if we don't already have it, add this to our jobid tracker */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
                if (jptr->jobid == d->proc.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
               job = OBJ_NEW(opal_pmix1_jobid_trkr_t);
                (void)strncpy(job->nspace, pdata[n].proc.nspace, PMIX_MAX_NSLEN);
                job->jobid = d->proc.jobid;
                opal_list_append(&mca_pmix_pmix112_component.jobids, &job->super);
            }
            if (PMIX_RANK_WILDCARD == pdata[n].proc.rank) {
                d->proc.vpid = OPAL_VPID_WILDCARD;
            } else {
                d->proc.vpid = pdata[n].proc.rank;
            }
            rc = pmix1_value_unload(&d->value, &pdata[n].value);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                PMIX_PDATA_FREE(pdata, sz);
                return OPAL_ERR_BAD_PARAM;
            }
            ++n;
        }
    }

    return pmix1_convert_rc(ret);
}

static void lk_cbfunc(pmix_status_t status,
                      pmix_pdata_t data[], size_t ndata,
                      void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;
    opal_pmix_pdata_t *d;
    opal_list_t results, *r = NULL;
    int rc;
    size_t n;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    if (NULL == op->lkcbfunc) {
        OBJ_RELEASE(op);
        return;
    }

    rc = pmix1_convert_rc(status);
    if (OPAL_SUCCESS == rc) {
        OBJ_CONSTRUCT(&results, opal_list_t);
        for (n=0; n < ndata; n++) {
            d = OBJ_NEW(opal_pmix_pdata_t);
            opal_list_append(&results, &d->super);
            if (mca_pmix_pmix112_component.native_launch) {
                /* if we were launched by the OMPI RTE, then
                 * the jobid is in a special format - so get it */
                opal_convert_string_to_jobid(&d->proc.jobid, data[n].proc.nspace);
            } else {
                /* we were launched by someone else, so make the
                 * jobid just be the hash of the nspace */
                OPAL_HASH_STR(data[n].proc.nspace, d->proc.jobid);
            }
            /* if we don't already have it, add this to our jobid tracker */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
                if (jptr->jobid == d->proc.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                job = OBJ_NEW(opal_pmix1_jobid_trkr_t);
                (void)strncpy(job->nspace, data[n].proc.nspace, PMIX_MAX_NSLEN);
                job->jobid = d->proc.jobid;
                opal_list_append(&mca_pmix_pmix112_component.jobids, &job->super);
            }
            if (PMIX_RANK_WILDCARD == data[n].proc.rank) {
                d->proc.vpid = OPAL_VPID_WILDCARD;
            } else {
                d->proc.vpid = data[n].proc.rank;
            }
            d->value.key = strdup(data[n].key);
            rc = pmix1_value_unload(&d->value, &data[n].value);
            if (OPAL_SUCCESS != rc) {
                rc = OPAL_ERR_BAD_PARAM;
                OPAL_ERROR_LOG(rc);
                goto release;
            }
        }
        r = &results;
    }
  release:
    /* execute the callback */
    op->lkcbfunc(rc, r, op->cbdata);

    if (NULL != r) {
        OPAL_LIST_DESTRUCT(&results);
    }
    OBJ_RELEASE(op);
}

int pmix1_lookupnb(char **keys, opal_list_t *info,
                   opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix1_opcaddy_t *op;
    opal_value_t *iptr;
    size_t n;


    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client lookup_nb");

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->lkcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != info) {
        op->sz = opal_list_get_size(info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
                (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&op->info[n].value, iptr);
                ++n;
            }
        }
    }

    ret = PMIx_Lookup_nb(keys, op->info, op->sz, lk_cbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_unpublish(char **keys, opal_list_t *info)
{
    pmix_status_t ret;
    size_t ninfo, n;
    pmix_info_t *pinfo;
    opal_value_t *iptr;

    if (NULL != info) {
        ninfo = opal_list_get_size(info);
        PMIX_INFO_CREATE(pinfo, ninfo);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(pinfo[n++].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&pinfo[n].value, iptr);
            ++n;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    ret = PMIx_Unpublish(keys, pinfo, ninfo);
    PMIX_INFO_FREE(pinfo, ninfo);

    return pmix1_convert_rc(ret);
}

int pmix1_unpublishnb(char **keys, opal_list_t *info,
                      opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix1_opcaddy_t *op;
    opal_value_t *iptr;
    size_t n;

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != info) {
        op->sz = opal_list_get_size(info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
                (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&op->info[n].value, iptr);
                ++n;
            }
        }
    }

    ret = PMIx_Unpublish_nb(keys, op->info, op->sz, opcbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_spawn(opal_list_t *job_info, opal_list_t *apps, opal_jobid_t *jobid)
{
    pmix_status_t ret;
    pmix_info_t *pinfo = NULL;
    pmix_app_t *papps;
    size_t napps, n, m, ninfo = 0;
    char nspace[PMIX_MAX_NSLEN+1];
    opal_value_t *info;
    opal_pmix_app_t *app;
    opal_pmix1_jobid_trkr_t *job;

    if (NULL != job_info && 0 < (ninfo = opal_list_get_size(job_info))) {
        PMIX_INFO_CREATE(pinfo, ninfo);
        n=0;
        OPAL_LIST_FOREACH(info, job_info, opal_value_t) {
            (void)strncpy(pinfo[n].key, info->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&pinfo[n].value, info);
            ++n;
        }
    }

    napps = opal_list_get_size(apps);
    PMIX_APP_CREATE(papps, napps);
    n=0;
    OPAL_LIST_FOREACH(app, apps, opal_pmix_app_t) {
        papps[n].cmd = strdup(app->cmd);
        papps[n].argc = app->argc;
        papps[n].argv = opal_argv_copy(app->argv);
        papps[n].env = opal_argv_copy(app->env);
        papps[n].maxprocs = app->maxprocs;
        if (0 < (papps[n].ninfo = opal_list_get_size(&app->info))) {
            PMIX_INFO_CREATE(papps[n].info, papps[n].ninfo);
            m=0;
            OPAL_LIST_FOREACH(info, &app->info, opal_value_t) {
                (void)strncpy(papps[n].info[m].key, info->key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&papps[n].info[m].value, info);
                ++m;
            }
        }
        ++n;
    }

    ret = PMIx_Spawn(pinfo, ninfo, papps, napps, nspace);
    if (PMIX_SUCCESS == ret) {
        if (mca_pmix_pmix112_component.native_launch) {
            /* if we were launched by the OMPI RTE, then
             * the jobid is in a special format - so get it */
            opal_convert_string_to_jobid(jobid, nspace);
        } else {
            /* we were launched by someone else, so make the
             * jobid just be the hash of the nspace */
            OPAL_HASH_STR(nspace, *jobid);
        }
        /* add this to our jobid tracker */
        job = OBJ_NEW(opal_pmix1_jobid_trkr_t);
        (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
        job->jobid = *jobid;
        opal_list_append(&mca_pmix_pmix112_component.jobids, &job->super);
    }
    PMIX_APP_FREE(papps, napps);

    return pmix1_convert_rc(ret);
}

static void spcbfunc(pmix_status_t status,
                     char *nspace, void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;
    int rc;
    opal_jobid_t jobid=OPAL_JOBID_INVALID;
    opal_pmix1_jobid_trkr_t *job;

    rc = pmix1_convert_rc(status);
    if (PMIX_SUCCESS == status) {
        if (mca_pmix_pmix112_component.native_launch) {
            /* if we were launched by the OMPI RTE, then
             * the jobid is in a special format - so get it */
            opal_convert_string_to_jobid(&jobid, nspace);
        } else {
            /* we were launched by someone else, so make the
             * jobid just be the hash of the nspace */
            OPAL_HASH_STR(nspace, jobid);
        }
        /* add this to our jobid tracker */
        job = OBJ_NEW(opal_pmix1_jobid_trkr_t);
        (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
        job->jobid = jobid;
        opal_list_append(&mca_pmix_pmix112_component.jobids, &job->super);
    }

    op->spcbfunc(rc, jobid, op->cbdata);
    OBJ_RELEASE(op);
}

int pmix1_spawnnb(opal_list_t *job_info, opal_list_t *apps,
                    opal_pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix1_opcaddy_t *op;
    size_t n, m;
    opal_value_t *info;
    opal_pmix_app_t *app;

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->spcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != job_info && 0 < (op->ninfo = opal_list_get_size(job_info))) {
        PMIX_INFO_CREATE(op->info, op->ninfo);
        n=0;
        OPAL_LIST_FOREACH(info, job_info, opal_value_t) {
            (void)strncpy(op->info[n].key, info->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&op->info[n].value, info);
            ++n;
        }
    }

    op->sz = opal_list_get_size(apps);
    PMIX_APP_CREATE(op->apps, op->sz);
    n=0;
    OPAL_LIST_FOREACH(app, apps, opal_pmix_app_t) {
        op->apps[n].cmd = strdup(app->cmd);
        op->apps[n].argc = app->argc;
        op->apps[n].argv = opal_argv_copy(app->argv);
        op->apps[n].env = opal_argv_copy(app->env);
        op->apps[n].maxprocs = app->maxprocs;
        if (0 < (op->apps[n].ninfo = opal_list_get_size(&app->info))) {
            PMIX_INFO_CREATE(op->apps[n].info, op->apps[n].ninfo);
            m=0;
            OPAL_LIST_FOREACH(info, &app->info, opal_value_t) {
                (void)strncpy(op->apps[n].info[m].key, info->key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&op->apps[n].info[m].value, info);
                ++m;
            }
        }
        ++n;
    }

    ret = PMIx_Spawn_nb(op->info, op->ninfo, op->apps, op->sz, spcbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_connect(opal_list_t *procs)
{
    pmix_status_t ret;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(parray, cnt);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (jptr->jobid == ptr->name.jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
            return OPAL_ERR_NOT_FOUND;
        }
        (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
        if (OPAL_VPID_WILDCARD == ptr->name.vpid) {
            parray[n].rank = PMIX_RANK_WILDCARD;
        } else {
            parray[n].rank = ptr->name.vpid;
        }
        ++n;
    }

    ret = PMIx_Connect(parray, cnt, NULL, 0);
    PMIX_PROC_FREE(parray, cnt);

    return pmix1_convert_rc(ret);
}

int pmix1_connectnb(opal_list_t *procs,
                    opal_pmix_op_cbfunc_t cbfunc,
                    void *cbdata)
{
    pmix_status_t ret;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix1_opcaddy_t *op;
    opal_pmix1_jobid_trkr_t *job;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->nprocs = cnt;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(op->procs, op->nprocs);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        OPAL_LIST_FOREACH(job, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (job->jobid == ptr->name.jobid) {
                (void)strncpy(op->procs[n].nspace, job->nspace, PMIX_MAX_NSLEN);
                break;
            }
        }
        if (OPAL_VPID_WILDCARD == ptr->name.vpid) {
            op->procs[n].rank = PMIX_RANK_WILDCARD;
        } else {
            op->procs[n].rank = ptr->name.vpid;
        }
        ++n;
    }

    ret = PMIx_Connect_nb(op->procs, op->nprocs, NULL, 0, opcbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_disconnect(opal_list_t *procs)
{
    pmix_status_t ret;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    opal_pmix1_jobid_trkr_t *job;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(parray, cnt);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        OPAL_LIST_FOREACH(job, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (job->jobid == ptr->name.jobid) {
                (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
                break;
            }
        }
        if (OPAL_VPID_WILDCARD == ptr->name.vpid) {
            parray[n].rank = PMIX_RANK_WILDCARD;
        } else {
            parray[n].rank = ptr->name.vpid;
        }
        ++n;
    }

    ret = PMIx_Disconnect(parray, cnt, NULL, 0);
    PMIX_PROC_FREE(parray, cnt);

    return pmix1_convert_rc(ret);
}

int pmix1_disconnectnb(opal_list_t *procs,
                         opal_pmix_op_cbfunc_t cbfunc,
                         void *cbdata)
{
    pmix_status_t ret;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix1_opcaddy_t *op;
    opal_pmix1_jobid_trkr_t *job;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->nprocs = cnt;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(op->procs, op->nprocs);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        OPAL_LIST_FOREACH(job, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (job->jobid == ptr->name.jobid) {
                (void)strncpy(op->procs[n].nspace, job->nspace, PMIX_MAX_NSLEN);
                break;
            }
        }
        if (OPAL_VPID_WILDCARD == ptr->name.vpid) {
            op->procs[n].rank = PMIX_RANK_WILDCARD;
        } else {
            op->procs[n].rank = ptr->name.vpid;
        }
        ++n;
    }

    ret = PMIx_Disconnect_nb(op->procs, op->nprocs, NULL, 0, opcbfunc, op);

    return pmix1_convert_rc(ret);
}


int pmix1_resolve_peers(const char *nodename, opal_jobid_t jobid,
                          opal_list_t *procs)
{
    char *nspace;
    pmix_proc_t *array=NULL;
    size_t nprocs, n;
    opal_namelist_t *nm;
    int rc;
    pmix_status_t ret;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    if (OPAL_JOBID_WILDCARD == jobid) {
        nspace = NULL;
    } else {
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (jptr->jobid == jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        nspace = job->nspace;
    }

    ret = PMIx_Resolve_peers(nodename, nspace, &array, &nprocs);
    rc = pmix1_convert_rc(ret);

    if (NULL != array && 0 < nprocs) {
        for (n=0; n < nprocs; n++) {
            nm = OBJ_NEW(opal_namelist_t);
            opal_list_append(procs, &nm->super);
            if (mca_pmix_pmix112_component.native_launch) {
                /* if we were launched by the OMPI RTE, then
                 * the jobid is in a special format - so get it */
                opal_convert_string_to_jobid(&nm->name.jobid, array[n].nspace);
            } else {
                /* we were launched by someone else, so make the
                 * jobid just be the hash of the nspace */
                OPAL_HASH_STR(array[n].nspace, nm->name.jobid);
            }
            /* if we don't already have it, add this to our jobid tracker */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
                if (jptr->jobid == nm->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                job = OBJ_NEW(opal_pmix1_jobid_trkr_t);
                (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
                job->jobid = jobid;
                opal_list_append(&mca_pmix_pmix112_component.jobids, &job->super);
            }
            nm->name.vpid = array[n].rank;
        }
    }
    PMIX_PROC_FREE(array, nprocs);

    return rc;
}

int pmix1_resolve_nodes(opal_jobid_t jobid, char **nodelist)
{
    pmix_status_t ret;
    char *nspace=NULL;
    opal_pmix1_jobid_trkr_t *job, *jptr;

    if (OPAL_JOBID_WILDCARD != jobid) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix112_component.jobids, opal_pmix1_jobid_trkr_t) {
            if (jptr->jobid == jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        nspace = job->nspace;
    }

    ret = PMIx_Resolve_nodes(nspace, nodelist);

    return pmix1_convert_rc(ret);;
}
