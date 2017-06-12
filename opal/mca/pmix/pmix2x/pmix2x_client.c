/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
#include "opal/threads/threads.h"
#include "opal/util/argv.h"
#include "opal/util/proc.h"

#include "opal/mca/pmix/base/base.h"
#include "pmix2x.h"
#include "pmix.h"

static pmix_proc_t my_proc;
static char *dbgvalue=NULL;
static bool initialized = false;

static void errreg_cbfunc (pmix_status_t status,
                           size_t errhandler_ref,
                           void *cbdata)
{
    opal_pmix2x_event_t *event = (opal_pmix2x_event_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(event);

    event->index = errhandler_ref;
    opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                        "PMIX client errreg_cbfunc - error handler registered status=%d, reference=%lu",
                        status, (unsigned long)errhandler_ref);
    OPAL_POST_OBJECT(regactive);
    OPAL_PMIX_WAKEUP_THREAD(&event->lock);
}

int pmix2x_client_init(opal_list_t *ilist)
{
    opal_process_name_t pname;
    pmix_status_t rc;
    int dbg;
    opal_pmix2x_jobid_trkr_t *job;
    opal_pmix2x_event_t *event;
    pmix_info_t *pinfo;
    size_t ninfo, n;
    opal_value_t *ival;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client init");

    if (!initialized) {
        if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
            asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
            putenv(dbgvalue);
        }
    }

    /* convert the incoming list to info structs */
    if (NULL != ilist) {
        ninfo = opal_list_get_size(ilist);
        if (0 < ninfo) {
            PMIX_INFO_CREATE(pinfo, ninfo);
            n=0;
            OPAL_LIST_FOREACH(ival, ilist, opal_value_t) {
                (void)strncpy(pinfo[n].key, ival->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&pinfo[n].value, ival);
                ++n;
            }
        } else {
            pinfo = NULL;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    rc = PMIx_Init(&my_proc, pinfo, ninfo);
    if (PMIX_SUCCESS != rc) {
        return pmix2x_convert_rc(rc);
    }
    if (0 < ninfo) {
        PMIX_INFO_FREE(pinfo, ninfo);

    }
    if (initialized) {
        return OPAL_SUCCESS;
    }
    initialized = true;

    /* store our jobid and rank */
    if (NULL != getenv(OPAL_MCA_PREFIX"orte_launch")) {
        /* if we were launched by the OMPI RTE, then
         * the jobid is in a special format - so get it */
        mca_pmix_pmix2x_component.native_launch = true;
        opal_convert_string_to_jobid(&pname.jobid, my_proc.nspace);
    } else {
        /* we were launched by someone else, so make the
         * jobid just be the hash of the nspace */
        OPAL_HASH_JOBID(my_proc.nspace, pname.jobid);
    }
    /* insert this into our list of jobids - it will be the
     * first, and so we'll check it first */
    job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
    (void)strncpy(job->nspace, my_proc.nspace, PMIX_MAX_NSLEN);
    job->jobid = pname.jobid;
    opal_list_append(&mca_pmix_pmix2x_component.jobids, &job->super);

    pname.vpid = pmix2x_convert_rank(my_proc.rank);
    opal_proc_set_name(&pname);

    /* register the default event handler */
    event = OBJ_NEW(opal_pmix2x_event_t);
    opal_list_append(&mca_pmix_pmix2x_component.events, &event->super);
    PMIX_INFO_CREATE(pinfo, 1);
    PMIX_INFO_LOAD(&pinfo[0], PMIX_EVENT_HDLR_NAME, "OPAL-PMIX-2X-DEFAULT", PMIX_STRING);
    PMIx_Register_event_handler(NULL, 0, pinfo, 1, pmix2x_event_hdlr, errreg_cbfunc, event);
    OPAL_PMIX_WAIT_THREAD(&event->lock);
    PMIX_INFO_FREE(pinfo, 1);

    return OPAL_SUCCESS;

}

static void dereg_cbfunc(pmix_status_t st, void *cbdata)
{
    opal_pmix2x_event_t *ev = (opal_pmix2x_event_t*)cbdata;
    OPAL_PMIX_WAKEUP_THREAD(&ev->lock);
}

int pmix2x_client_finalize(void)
{
    pmix_status_t rc;
    opal_pmix2x_event_t *event;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client finalize");

    /* deregister all event handlers */
    OPAL_LIST_FOREACH(event, &mca_pmix_pmix2x_component.events, opal_pmix2x_event_t) {
        PMIx_Deregister_event_handler(event->index, dereg_cbfunc, (void*)event);
        OPAL_PMIX_WAIT_THREAD(&event->lock);
    }
    /* the list will be destructed when the component is finalized */

    rc = PMIx_Finalize(NULL, 0);
    return pmix2x_convert_rc(rc);
}

int pmix2x_initialized(void)
{
    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client initialized");

    return initialized;
}

static void _abort(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != pt->info && 0 < (cnt = opal_list_get_size(pt->info))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, pt->info, opal_namelist_t) {
            /* look thru our list of jobids and find the
             * corresponding nspace */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == ptr->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                PMIX_PROC_FREE(parray, cnt);
                pt->status = OPAL_ERR_NOT_FOUND;
                OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
                return;
            }
            (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
            parray[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
            ++n;
        }
    }

    /* call the library abort - this is a blocking call */
    rc = PMIx_Abort(pt->status, pt->msg, parray, cnt);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);

    pt->status = pmix2x_convert_rc(rc);
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}

int pmix2x_abort(int flag, const char *msg,
                 opal_list_t *procs)
{
    pmix2x_threadshift_t pt;
    int rc;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client abort");

    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.status = flag;
    pt.info = procs;
    pt.msg = msg;
    OPAL_PMIX2X_THREADSHIFT(&pt, _abort);
    OPAL_PMIX_WAIT_THREAD(&pt.lock);
    rc = pt.status;
    OBJ_DESTRUCT(&pt);

    return rc;
}

static void _store(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    pmix_value_t kv;
    pmix_status_t rc;
    pmix_proc_t p;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    if (NULL != pt->source) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == pt->source->jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            pt->status = OPAL_ERR_NOT_FOUND;
            OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
            return;
        }
        (void)strncpy(p.nspace, job->nspace, PMIX_MAX_NSLEN);
        p.rank = pmix2x_convert_opalrank(pt->source->vpid);
    } else {
        /* use our name */
        (void)strncpy(p.nspace, my_proc.nspace, PMIX_MAX_NSLEN);
        p.rank = pmix2x_convert_opalrank(OPAL_PROC_MY_NAME.vpid);
    }

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix2x_value_load(&kv, pt->val);

    /* call the library - this is a blocking call */
    rc = PMIx_Store_internal(&p, pt->val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);

    pt->status = pmix2x_convert_rc(rc);
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);

}
int pmix2x_store_local(const opal_process_name_t *proc, opal_value_t *val)
{
    pmix2x_threadshift_t pt;
    int rc;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */
    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.source = proc;
    pt.val = val;
    OPAL_PMIX2X_THREADSHIFT(&pt, _store);
    OPAL_PMIX_WAIT_THREAD(&pt.lock);
    rc = pt.status;
    OBJ_DESTRUCT(&pt);
    return rc;
}

int pmix2x_commit(void)
{
    pmix_status_t rc;

    rc = PMIx_Commit();
    return pmix2x_convert_rc(rc);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(op);
    if (NULL != op->opcbfunc) {
        op->opcbfunc(pmix2x_convert_rc(status), op->cbdata);
    }
    OBJ_RELEASE(op);
}

static void lkcbfunc(int status, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    pt->status = status;
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}

static void _fence(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix2x_opcaddy_t *op;
    pmix_info_t info, *iptr;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client executing fence");

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != pt->info && 0 < (cnt = opal_list_get_size(pt->info))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, pt->info, opal_namelist_t) {
            /* look thru our list of jobids and find the
             * corresponding nspace */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == ptr->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                PMIX_PROC_FREE(parray, cnt);
                if (NULL != pt->opcbfunc) {
                    pt->opcbfunc(pmix2x_convert_rc(OPAL_ERR_NOT_FOUND), pt->cbdata);
                }
                if (pt->lock.active) {
                    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
                    return;
                }
                OBJ_RELEASE(pt);
                return;
            }
            (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
            parray[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
            ++n;
        }
    }

    if (pt->nondefault) {
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

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = pt->opcbfunc;
    op->cbdata = pt->cbdata;
    op->procs = parray;
    op->nprocs = cnt;

    /* call the library function */
    rc = PMIx_Fence_nb(parray, cnt, iptr, n, opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        if (NULL != pt->opcbfunc) {
            pt->opcbfunc(pmix2x_convert_rc(rc), pt->cbdata);
        }
        OBJ_RELEASE(op);
        if (pt->lock.active) {
            pt->status = pmix2x_convert_rc(rc);
            OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
            return;
        }
    }
    if (!pt->lock.active) {
        OBJ_RELEASE(pt);
    }
}

int pmix2x_fence(opal_list_t *procs, int collect_data)
{
    int rc;
    pmix2x_threadshift_t pt;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client fence");

    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.info = procs;
    pt.nondefault = collect_data;
    pt.opcbfunc = lkcbfunc;
    pt.cbdata = &pt;
    OPAL_PMIX2X_THREADSHIFT(&pt, _fence);
    OPAL_PMIX_WAIT_THREAD(&pt.lock);
    rc = pt.status;
    OBJ_DESTRUCT(&pt);
    return rc;
}

int pmix2x_fencenb(opal_list_t *procs, int collect_data,
                   opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix2x_threadshift_t *pt;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client fencenb");

    pt = OBJ_NEW(pmix2x_threadshift_t);
    pt->info = procs;
    pt->nondefault = collect_data;
    pt->opcbfunc = cbfunc;
    pt->cbdata = cbdata;
    pt->lock.active = false;
    OPAL_PMIX2X_THREADSHIFT(pt, _fence);
    return OPAL_SUCCESS;
}

int pmix2x_put(opal_pmix_scope_t opal_scope,
               opal_value_t *val)
{
    pmix_value_t kv;
    pmix_scope_t pmix_scope = pmix2x_convert_opalscope(opal_scope);
    pmix_status_t rc;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client put");

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix2x_value_load(&kv, val);

    rc = PMIx_Put(pmix_scope, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);
    return pmix2x_convert_rc(rc);
}

static void getcbfunc(int status,
                      opal_value_t *kv, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(pt);
    pt->status = status;
    opal_dss.copy((void**)&pt->val, kv, OPAL_VALUE);
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}


static void val_cbfunc(pmix_status_t status,
                       pmix_value_t *kv, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;
    int rc;
    opal_value_t val, *v=NULL;

    OPAL_ACQUIRE_OBJECT(op);
    rc = pmix2x_convert_opalrc(status);
    if (PMIX_SUCCESS == status && NULL != kv) {
        rc = pmix2x_value_unload(&val, kv);
        v = &val;
    }

    if (NULL != op->valcbfunc) {
        op->valcbfunc(rc, v, op->cbdata);
    }
    OBJ_RELEASE(op);
}

static void _get(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    pmix2x_opcaddy_t *op;
    pmix_status_t rc;
    size_t n;
    opal_value_t *ival;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "%s pmix2x:client executing get on proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == pt->source) ? "NULL" : OPAL_NAME_PRINT(*(pt->source)), pt->msg);

    /* set default response */
    pt->val = NULL;

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->valcbfunc = pt->valcbfunc;
    op->cbdata = pt->cbdata;

    if (NULL != pt->source) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == pt->source->jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            OBJ_RELEASE(op);
            if (NULL != pt->valcbfunc) {
                pt->valcbfunc(OPAL_ERR_NOT_FOUND, NULL, pt->cbdata);
            }
            if (pt->lock.active) {
                pt->status = OPAL_ERR_NOT_FOUND;
                OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
                return;
            }
            OBJ_RELEASE(pt);
            return;
        }
        (void)strncpy(op->p.nspace, job->nspace, PMIX_MAX_NSLEN);
        op->p.rank = pmix2x_convert_opalrank(pt->source->vpid);
    } else {
        (void)strncpy(op->p.nspace, my_proc.nspace, PMIX_MAX_NSLEN);
        op->p.rank = pmix2x_convert_rank(PMIX_RANK_WILDCARD);
    }

    if (NULL != pt->info) {
        op->sz = opal_list_get_size(pt->info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(ival, pt->info, opal_value_t) {
                (void)strncpy(op->info[n].key, ival->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&op->info[n].value, ival);
                ++n;
            }
        }
    }

    /* call the library function */
    rc = PMIx_Get_nb(&op->p, pt->msg, op->info, op->sz, val_cbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
        if (NULL != pt->valcbfunc) {
            pt->valcbfunc(pmix2x_convert_rc(rc), NULL, pt->cbdata);
        }
        if (pt->lock.active) {
            pt->status = pmix2x_convert_rc(rc);
            OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
            return;
        }
    }

    if (!pt->lock.active) {
        OBJ_RELEASE(pt);
    }
}

int pmix2x_get(const opal_process_name_t *proc, const char *key,
               opal_list_t *info, opal_value_t **val)
{
    int rc;
    pmix2x_threadshift_t pt;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "%s pmix2x:client get on proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == proc) ? "NULL" : OPAL_NAME_PRINT(*proc), key);

    if (NULL == proc) {
        /* if they are asking for our jobid, then return it */
        if (0 == strcmp(key, OPAL_PMIX_JOBID)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_UINT32;
            (*val)->data.uint32 = OPAL_PROC_MY_NAME.jobid;
            return OPAL_SUCCESS;
        }
        /* if they are asking for our rank, return it */
        if (0 == strcmp(key, OPAL_PMIX_RANK)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_INT;
            (*val)->data.integer = pmix2x_convert_rank(my_proc.rank);
            return OPAL_SUCCESS;
        }
    }

    /* go ahead and threadshift anything else */
    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.source = proc;
    pt.info = info;
    pt.msg = key;
    pt.valcbfunc = getcbfunc;
    pt.cbdata = &pt;
    OPAL_PMIX2X_THREADSHIFT(&pt, _get);
    OPAL_PMIX_WAIT_THREAD(&pt.lock);
    rc = pt.status;
    *val = pt.val;
    OBJ_DESTRUCT(&pt);
    return rc;
}

int pmix2x_getnb(const opal_process_name_t *proc, const char *key,
                 opal_list_t *info,
                 opal_pmix_value_cbfunc_t cbfunc, void *cbdata)
{
    pmix2x_threadshift_t *pt;
    opal_value_t *val;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "%s PMIx_client get_nb on proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == proc) ? "NULL" : OPAL_NAME_PRINT(*proc), key);

    if (NULL == proc) {
        /* if they are asking for our jobid, then return it */
        if (0 == strcmp(key, OPAL_PMIX_JOBID)) {
            if (NULL != cbfunc) {
                val = OBJ_NEW(opal_value_t);
                val->type = OPAL_UINT32;
                val->data.uint32 = OPAL_PROC_MY_NAME.jobid;
                cbfunc(OPAL_SUCCESS, val, cbdata);
            }
            return OPAL_SUCCESS;
        }
        /* if they are asking for our rank, return it */
        if (0 == strcmp(key, OPAL_PMIX_RANK)) {
            if (NULL != cbfunc) {
                val = OBJ_NEW(opal_value_t);
                val->type = OPAL_INT;
                val->data.integer = pmix2x_convert_rank(my_proc.rank);
                cbfunc(OPAL_SUCCESS, val, cbdata);
            }
            return OPAL_SUCCESS;
        }
    }

    /* go ahead and threadshift anything else */
    pt = OBJ_NEW(pmix2x_threadshift_t);
    pt->source = proc;
    pt->msg = key;
    pt->info = info;
    pt->valcbfunc = cbfunc;
    pt->cbdata = cbdata;
    pt->lock.active = false;
    OPAL_PMIX2X_THREADSHIFT(pt, _fence);
    return OPAL_SUCCESS;
}

int pmix2x_publish(opal_list_t *info)
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
            pmix2x_value_load(&pinfo[n].value, iptr);
            ++n;
        }
    } else {
        pinfo = NULL;
    }

    ret = PMIx_Publish(pinfo, sz);
    if (0 < sz) {
        PMIX_INFO_FREE(pinfo, sz);
    }

    return pmix2x_convert_rc(ret);
}

int pmix2x_publishnb(opal_list_t *info,
                     opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    opal_value_t *iptr;
    size_t n;
    pmix2x_opcaddy_t *op;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client publish_nb");

    if (NULL == info) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    op->sz = opal_list_get_size(info);
    if (0 < op->sz) {
        PMIX_INFO_CREATE(op->info, op->sz);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&op->info[n].value, iptr);
            ++n;
        }
    }

    ret = PMIx_Publish_nb(op->info, op->sz, opcbfunc, op);
    if (0 < op->sz) {
        PMIX_INFO_FREE(op->info, op->sz);
    }

    return pmix2x_convert_rc(ret);
}

static void _lkcb(int status, opal_list_t *results, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    opal_pmix_pdata_t *d, *tgt;

    pt->status = status;
    if (OPAL_SUCCESS == status) {
        /* transfer the data back */
        tgt = (opal_pmix_pdata_t*)opal_list_get_first(pt->info);
        OPAL_LIST_FOREACH(d, results, opal_pmix_pdata_t) {
            opal_value_xfer(&tgt->value, &d->value);
            tgt = (opal_pmix_pdata_t*)opal_list_get_next(&tgt->super);
        }
    }
    /* release the thread */
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}

int pmix2x_lookup(opal_list_t *data, opal_list_t *info)
{
    char **keys = NULL;
    opal_pmix_pdata_t *d;
    pmix2x_threadshift_t pt;
    int rc;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "pmix2x:client lookup");

    if (NULL == data) {
        return OPAL_ERR_BAD_PARAM;
    }

    OPAL_LIST_FOREACH(d, data, opal_pmix_pdata_t) {
        opal_argv_append_nosize(&keys, d->value.key);
    }

    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.info = data;

    rc = pmix2x_lookupnb(keys, info, _lkcb, &pt);
    if (OPAL_SUCCESS == rc) {
        OPAL_PMIX_WAIT_THREAD(&pt.lock);
        rc = pt.status;
    }
    OBJ_DESTRUCT(&pt);

    return rc;
}

static void _lkprocess(int sd, short args, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;
    opal_pmix_pdata_t *d;
    opal_list_t results, *r = NULL;
    int rc;
    size_t n;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    OPAL_ACQUIRE_OBJECT(op);

    rc = pmix2x_convert_rc(op->status);
    if (OPAL_SUCCESS == rc) {
        OBJ_CONSTRUCT(&results, opal_list_t);
        for (n=0; n < op->npdata; n++) {
            d = OBJ_NEW(opal_pmix_pdata_t);
            opal_list_append(&results, &d->super);
            if (mca_pmix_pmix2x_component.native_launch) {
                /* if we were launched by the OMPI RTE, then
                 * the jobid is in a special format - so get it */
                opal_convert_string_to_jobid(&d->proc.jobid, op->pdata[n].proc.nspace);
            } else {
                /* we were launched by someone else, so make the
                 * jobid just be the hash of the nspace */
                OPAL_HASH_JOBID(op->pdata[n].proc.nspace, d->proc.jobid);
            }
            /* if we don't already have it, add this to our jobid tracker */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == d->proc.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
                (void)strncpy(job->nspace, op->pdata[n].proc.nspace, PMIX_MAX_NSLEN);
                job->jobid = d->proc.jobid;
                opal_list_append(&mca_pmix_pmix2x_component.jobids, &job->super);
            }
            d->proc.vpid = pmix2x_convert_rank(op->pdata[n].proc.rank);
            d->value.key = strdup(op->pdata[n].key);
            rc = pmix2x_value_unload(&d->value, &op->pdata[n].value);
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

static void lk_cbfunc(pmix_status_t status,
                      pmix_pdata_t data[], size_t ndata,
                      void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;
    size_t n;

    OPAL_ACQUIRE_OBJECT(op);

    if (NULL == op->lkcbfunc) {
        OBJ_RELEASE(op);
        return;
    }

    /* this is in the PMIx local thread - need to threadshift to
     * our own thread as we will be accessing framework-global
     * lists and objects - sadly, we must copy the returned
     * data to preserve it */
    op->status = status;
    if (0 < ndata) {
        PMIX_PDATA_CREATE(op->pdata, ndata);
        op->npdata = ndata;
        for (n=0; n < ndata; n++) {
            (void)strncpy(op->pdata[n].proc.nspace, data[n].proc.nspace, PMIX_MAX_NSLEN);
            op->pdata[n].proc.rank = data[n].proc.rank;
            (void)strncpy(op->pdata[n].key, data[n].key, PMIX_MAX_KEYLEN);
            pmix_value_xfer(&op->pdata[n].value, &data[n].value);
        }
    }
    opal_event_assign(&(op->ev), opal_pmix_base.evbase,
                      -1, EV_WRITE, _lkprocess, op);
    OPAL_POST_OBJECT(op);
    opal_event_active(&(op->ev), EV_WRITE, 1);
}

int pmix2x_lookupnb(char **keys, opal_list_t *info,
                    opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix2x_opcaddy_t *op;
    opal_value_t *iptr;
    size_t n;


    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "pmix2x:client lookup_nb");

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->lkcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != info) {
        op->sz = opal_list_get_size(info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
                (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&op->info[n].value, iptr);
                ++n;
            }
        }
    }

    ret = PMIx_Lookup_nb(keys, op->info, op->sz, lk_cbfunc, op);

    return pmix2x_convert_rc(ret);
}

int pmix2x_unpublish(char **keys, opal_list_t *info)
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
            (void)strncpy(pinfo[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&pinfo[n].value, iptr);
            ++n;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    ret = PMIx_Unpublish(keys, pinfo, ninfo);
    PMIX_INFO_FREE(pinfo, ninfo);

    return pmix2x_convert_rc(ret);
}

int pmix2x_unpublishnb(char **keys, opal_list_t *info,
                       opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix2x_opcaddy_t *op;
    opal_value_t *iptr;
    size_t n;

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != info) {
        op->sz = opal_list_get_size(info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
                (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&op->info[n].value, iptr);
                ++n;
            }
        }
    }

    ret = PMIx_Unpublish_nb(keys, op->info, op->sz, opcbfunc, op);

    return pmix2x_convert_rc(ret);
}

static void _spcb(int status, opal_jobid_t jobid, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(pt);
    pt->status = status;
    pt->pname.jobid = jobid;
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}

int pmix2x_spawn(opal_list_t *job_info, opal_list_t *apps, opal_jobid_t *jobid)
{
    int rc;
    pmix2x_threadshift_t pt;

    *jobid = OPAL_JOBID_INVALID;

    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    rc = pmix2x_spawnnb(job_info, apps, _spcb, &pt);
    if (OPAL_SUCCESS == rc) {
        OPAL_PMIX_WAIT_THREAD(&pt.lock);
        rc = pt.status;
        if (OPAL_SUCCESS == pt.status) {
            *jobid = pt.pname.jobid;
        }
    }
    OBJ_DESTRUCT(&pt);

    return rc;
}

static void _spcbfunc(int sd, short args, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;
    int rc;
    opal_jobid_t jobid=OPAL_JOBID_INVALID;
    opal_pmix2x_jobid_trkr_t *job;

    rc = pmix2x_convert_rc(op->status);
    if (PMIX_SUCCESS == op->status) {
        if (mca_pmix_pmix2x_component.native_launch) {
            /* if we were launched by the OMPI RTE, then
             * the jobid is in a special format - so get it */
            opal_convert_string_to_jobid(&jobid, op->nspace);
        } else {
            /* we were launched by someone else, so make the
             * jobid just be the hash of the nspace */
            OPAL_HASH_JOBID(op->nspace, jobid);
        }
        /* add this to our jobid tracker */
        job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
        (void)strncpy(job->nspace, op->nspace, PMIX_MAX_NSLEN);
        job->jobid = jobid;
        opal_list_append(&mca_pmix_pmix2x_component.jobids, &job->super);
    }

    op->spcbfunc(rc, jobid, op->cbdata);
    OBJ_RELEASE(op);
}

static void spcbfunc(pmix_status_t status,
                     char *nspace, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(op);

    /* this is in the PMIx local thread - need to threadshift to
     * our own thread as we will be accessing framework-global
     * lists and objects */
    op->status = status;
    if (NULL != nspace) {
        op->nspace = strdup(nspace);
    }
    opal_event_assign(&(op->ev), opal_pmix_base.evbase,
                      -1, EV_WRITE, _spcbfunc, op);
    OPAL_POST_OBJECT(op);
    opal_event_active(&(op->ev), EV_WRITE, 1);
}

int pmix2x_spawnnb(opal_list_t *job_info, opal_list_t *apps,
                   opal_pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix2x_opcaddy_t *op;
    size_t n, m;
    opal_value_t *info;
    opal_pmix_app_t *app;

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->spcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != job_info && 0 < (op->ninfo = opal_list_get_size(job_info))) {
        PMIX_INFO_CREATE(op->info, op->ninfo);
        n=0;
        OPAL_LIST_FOREACH(info, job_info, opal_value_t) {
            (void)strncpy(op->info[n].key, info->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&op->info[n].value, info);
            ++n;
        }
    }

    op->sz = opal_list_get_size(apps);
    PMIX_APP_CREATE(op->apps, op->sz);
    n=0;
    OPAL_LIST_FOREACH(app, apps, opal_pmix_app_t) {
        op->apps[n].cmd = strdup(app->cmd);
        op->apps[n].argv = opal_argv_copy(app->argv);
        op->apps[n].env = opal_argv_copy(app->env);
        op->apps[n].maxprocs = app->maxprocs;
        if (0 < (op->apps[n].ninfo = opal_list_get_size(&app->info))) {
            PMIX_INFO_CREATE(op->apps[n].info, op->apps[n].ninfo);
            m=0;
            OPAL_LIST_FOREACH(info, &app->info, opal_value_t) {
                (void)strncpy(op->apps[n].info[m].key, info->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&op->apps[n].info[m].value, info);
                ++m;
            }
        }
        ++n;
    }

    ret = PMIx_Spawn_nb(op->info, op->ninfo, op->apps, op->sz, spcbfunc, op);

    return pmix2x_convert_rc(ret);
}

static void _concb(int status, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(pt);
    pt->status = status;
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}

static void _con(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    pmix_status_t ret;
    size_t n;
    opal_namelist_t *ptr;
    pmix2x_opcaddy_t *op;
    opal_pmix2x_jobid_trkr_t *job;

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = pt->opcbfunc;
    op->cbdata = pt->cbdata;
    op->nprocs = opal_list_get_size(pt->info);

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(op->procs, op->nprocs);
    n=0;
    OPAL_LIST_FOREACH(ptr, pt->info, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        OPAL_LIST_FOREACH(job, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (job->jobid == ptr->name.jobid) {
                (void)strncpy(op->procs[n].nspace, job->nspace, PMIX_MAX_NSLEN);
                break;
            }
        }
        op->procs[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
        ++n;
    }

    ret = PMIx_Connect_nb(op->procs, op->nprocs, NULL, 0, op->opcbfunc, op->cbdata);
    if (PMIX_SUCCESS != ret) {
        if (NULL != op->opcbfunc) {
            op->opcbfunc(pmix2x_convert_rc(ret), op->cbdata);
        }
        OBJ_RELEASE(op);
        if (!pt->lock.active) {
            OBJ_RELEASE(pt);
        }
    }
}

int pmix2x_connect(opal_list_t *procs)
{
    int rc;
    pmix2x_threadshift_t pt;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "pmix2x:client connect");

    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.info = procs;
    pt.opcbfunc = _concb;
    pt.cbdata = &pt;
    OPAL_PMIX2X_THREADSHIFT(&pt, _con);
    OPAL_PMIX_WAIT_THREAD(&pt.lock);
    rc = pt.status;
    OBJ_DESTRUCT(&pt);
    return rc;
}

int pmix2x_connectnb(opal_list_t *procs,
                     opal_pmix_op_cbfunc_t cbfunc,
                     void *cbdata)
{
    pmix2x_threadshift_t *pt;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "pmix2x:client connect NB");

    /* protect against bozo error */
    if (NULL == procs || 0 == opal_list_get_size(procs)) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    /* create the caddy */
    pt = OBJ_NEW(pmix2x_threadshift_t);
    pt->opcbfunc = cbfunc;
    pt->cbdata = cbdata;
    pt->info = procs;
    pt->lock.active = false;
    OPAL_PMIX2X_THREADSHIFT(pt, _con);
    return OPAL_SUCCESS;
}

static void _dscb(int status, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(pt);
    pt->status = status;
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}

static void _dscon(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    pmix_status_t ret;
    size_t n;
    opal_namelist_t *ptr;
    pmix2x_opcaddy_t *op;
    opal_pmix2x_jobid_trkr_t *job;

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = pt->opcbfunc;
    op->cbdata = pt->cbdata;
    op->nprocs = opal_list_get_size(pt->info);

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(op->procs, op->nprocs);
    n=0;
    OPAL_LIST_FOREACH(ptr, pt->info, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        OPAL_LIST_FOREACH(job, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (job->jobid == ptr->name.jobid) {
                (void)strncpy(op->procs[n].nspace, job->nspace, PMIX_MAX_NSLEN);
                break;
            }
        }
        op->procs[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
        ++n;
    }

    ret = PMIx_Disconnect_nb(op->procs, op->nprocs, NULL, 0, op->opcbfunc, op->cbdata);
    if (PMIX_SUCCESS != ret) {
        if (NULL != op->opcbfunc) {
            op->opcbfunc(pmix2x_convert_rc(ret), op->cbdata);
        }
        OBJ_RELEASE(op);
        if (!pt->lock.active) {
            OBJ_RELEASE(pt);
        }
    }
}

int pmix2x_disconnect(opal_list_t *procs)
{
    int rc;
    pmix2x_threadshift_t pt;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "pmix2x:client disconnect");

    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.info = procs;
    pt.opcbfunc = _dscb;
    pt.cbdata = &pt;
    OPAL_PMIX2X_THREADSHIFT(&pt, _dscon);
    OPAL_PMIX_WAIT_THREAD(&pt.lock);
    rc = pt.status;
    OBJ_DESTRUCT(&pt);
    return rc;
}

int pmix2x_disconnectnb(opal_list_t *procs,
                        opal_pmix_op_cbfunc_t cbfunc,
                        void *cbdata)
{
    pmix2x_threadshift_t *pt;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "pmix2x:client disconnect NB");

    /* protect against bozo error */
    if (NULL == procs || 0 == opal_list_get_size(procs)) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    /* create the caddy */
    pt = OBJ_NEW(pmix2x_threadshift_t);
    pt->opcbfunc = cbfunc;
    pt->cbdata = cbdata;
    pt->info = procs;
    pt->lock.active = false;
    OPAL_PMIX2X_THREADSHIFT(pt, _dscon);
    return OPAL_SUCCESS;
}

static void _repr(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    char *nspace;
    pmix_proc_t *array=NULL;
    size_t nprocs, n;
    opal_namelist_t *nm;
    pmix_status_t ret;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    if (OPAL_JOBID_WILDCARD == pt->pname.jobid) {
        nspace = NULL;
    } else {
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == pt->pname.jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            pt->status = OPAL_ERR_NOT_FOUND;
            OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
            return;
        }
        nspace = job->nspace;
    }

    ret = PMIx_Resolve_peers(pt->msg, nspace, &array, &nprocs);
    pt->status = pmix2x_convert_rc(ret);

    if (NULL != array && 0 < nprocs) {
        for (n=0; n < nprocs; n++) {
            nm = OBJ_NEW(opal_namelist_t);
            opal_list_append(pt->info, &nm->super);
            if (mca_pmix_pmix2x_component.native_launch) {
                /* if we were launched by the OMPI RTE, then
                 * the jobid is in a special format - so get it */
                opal_convert_string_to_jobid(&nm->name.jobid, array[n].nspace);
            } else {
                /* we were launched by someone else, so make the
                 * jobid just be the hash of the nspace */
                OPAL_HASH_JOBID(array[n].nspace, nm->name.jobid);
            }
            /* if we don't already have it, add this to our jobid tracker */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == nm->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
                (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
                job->jobid = nm->name.jobid;
                opal_list_append(&mca_pmix_pmix2x_component.jobids, &job->super);
            }
            nm->name.vpid = pmix2x_convert_rank(array[n].rank);
        }
    }
    PMIX_PROC_FREE(array, nprocs);
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}

int pmix2x_resolve_peers(const char *nodename, opal_jobid_t jobid,
                         opal_list_t *procs)
{
    pmix2x_threadshift_t pt;
    int rc;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */
    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.msg = nodename;
    pt.pname.jobid = jobid;
    pt.info = procs;
    OPAL_PMIX2X_THREADSHIFT(&pt, _repr);
    OPAL_PMIX_WAIT_THREAD(&pt.lock);
    /* results were added to the list */
    rc = pt.status;
    OBJ_DESTRUCT(&pt);

    return rc;
}

static void _rend(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *pt = (pmix2x_threadshift_t*)cbdata;
    pmix_status_t ret;
    char *nspace=NULL;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    if (OPAL_JOBID_WILDCARD != pt->pname.jobid) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == pt->pname.jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            pt->status = OPAL_ERR_NOT_FOUND;
            OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
            return;
        }
        nspace = job->nspace;
    }

    ret = PMIx_Resolve_nodes(nspace, &pt->strings);
    pt->status = pmix2x_convert_rc(ret);
    OPAL_PMIX_WAKEUP_THREAD(&pt->lock);
}

int pmix2x_resolve_nodes(opal_jobid_t jobid, char **nodelist)
{
    pmix2x_threadshift_t pt;
    int rc;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */
    OBJ_CONSTRUCT(&pt, pmix2x_threadshift_t);
    pt.pname.jobid = jobid;
    OPAL_PMIX2X_THREADSHIFT(&pt, _rend);
    OPAL_PMIX_WAIT_THREAD(&pt.lock);
    if (NULL != pt.strings) {
        *nodelist = strdup(pt.strings);
    }
    rc = pt.status;
    OBJ_DESTRUCT(&pt);

    return rc;
}
