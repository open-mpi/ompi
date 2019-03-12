/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
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

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress_threads.h"
#include "opal/threads/threads.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/mca/pmix/base/base.h"
#include "ext2x.h"

#include "pmix.h"
#include "pmix_server.h"

/****    S.O.U.T.H.B.O.U.N.D   I.N.T.E.R.F.A.C.E.S     ****/

/* These are the interfaces used by the OMPI/ORTE/OPAL layer to call
 * down into the embedded PMIx server. */

extern pmix_server_module_t mymodule;
extern opal_pmix_server_module_t *host_module;
static char *dbgvalue=NULL;

static void errreg_cbfunc (pmix_status_t status,
                          size_t errhandler_ref,
                          void *cbdata)
{
    opal_ext2x_event_t *ev = (opal_ext2x_event_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(ev);
    ev->index = errhandler_ref;
    opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                        "PMIX server errreg_cbfunc - error handler registered status=%d, reference=%lu",
                         status, (unsigned long)errhandler_ref);
    OPAL_POST_OBJECT(ev);
    OPAL_PMIX_WAKEUP_THREAD(&ev->lock);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    ext2x_opcaddy_t *op = (ext2x_opcaddy_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(op);

    if (NULL != op->opcbfunc) {
        op->opcbfunc(ext2x_convert_rc(status), op->cbdata);
    }
    OBJ_RELEASE(op);
}

static void lkcbfunc(pmix_status_t status, void *cbdata)
{
    opal_pmix_lock_t *lk = (opal_pmix_lock_t*)cbdata;

    OPAL_POST_OBJECT(lk);
    OPAL_PMIX_WAKEUP_THREAD(lk);
}

int ext2x_server_init(opal_pmix_server_module_t *module,
                      opal_list_t *info)
{
    pmix_status_t rc;
    int dbg;
    opal_value_t *kv;
    pmix_info_t *pinfo;
    size_t sz, n;
    opal_ext2x_event_t *event;
    opal_ext2x_jobid_trkr_t *job;
    opal_pmix_lock_t lk;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);

    if (0 == opal_pmix_base.initialized) {
        if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
            asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
            putenv(dbgvalue);
        }
        /* check the evars for a mismatch */
        if (OPAL_SUCCESS != (dbg = opal_pmix_ext2x_check_evars())) {
            OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
            return dbg;
        }
    }
    ++opal_pmix_base.initialized;

    /* convert the list to an array of pmix_info_t */
    sz = 2 + ((NULL==info)?0:opal_list_get_size(info));
    PMIX_INFO_CREATE(pinfo, sz);
    n = 0;
    if (NULL != info) {
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            ext2x_value_load(&pinfo[n].value, kv);
            ++n;
        }
    }

    /* insert ourselves into our list of jobids - it will be the
     * first, and so we'll check it first */
    job = OBJ_NEW(opal_ext2x_jobid_trkr_t);
    (void)opal_snprintf_jobid(job->nspace, PMIX_MAX_NSLEN, OPAL_PROC_MY_NAME.jobid);
    job->jobid = OPAL_PROC_MY_NAME.jobid;
    opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);

    /* add our nspace and rank to the array going down to the PMIx server */
    PMIX_INFO_LOAD(&pinfo[sz-2], PMIX_SERVER_NSPACE, job->nspace, PMIX_STRING);
    PMIX_INFO_LOAD(&pinfo[sz-1], PMIX_SERVER_RANK, &OPAL_PROC_MY_NAME.vpid, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != (rc = PMIx_server_init(&mymodule, pinfo, sz))) {
        PMIX_INFO_FREE(pinfo, sz);
        return ext2x_convert_rc(rc);
    }
    PMIX_INFO_FREE(pinfo, sz);

    /* record the host module */
    host_module = module;

    /* register the default event handler */
    event = OBJ_NEW(opal_ext2x_event_t);
    opal_list_append(&mca_pmix_ext2x_component.events, &event->super);
    PMIX_INFO_CREATE(pinfo, 1);
    PMIX_INFO_LOAD(&pinfo[0], PMIX_EVENT_HDLR_NAME, "OPAL-PMIX-2X-SERVER-DEFAULT", PMIX_STRING);
    PMIx_Register_event_handler(NULL, 0, pinfo, 1, ext2x_event_hdlr, errreg_cbfunc, (void*)event);
    OPAL_PMIX_WAIT_THREAD(&event->lock);
    PMIX_INFO_FREE(pinfo, 1);

    /* as we might want to use some client-side functions, be sure
     * to register our own nspace */
    OPAL_PMIX_CONSTRUCT_LOCK(&lk);
    PMIX_INFO_CREATE(pinfo, 1);
    PMIX_INFO_LOAD(&pinfo[0], PMIX_REGISTER_NODATA, NULL, PMIX_BOOL);
    PMIx_server_register_nspace(job->nspace, 1, pinfo, 1, lkcbfunc, (void*)&lk);
    OPAL_PMIX_WAIT_THREAD(&lk);
    OPAL_PMIX_DESTRUCT_LOCK(&lk);
    PMIX_INFO_FREE(pinfo, 1);

    return OPAL_SUCCESS;
}

static void dereg_cbfunc(pmix_status_t st, void *cbdata)
{
    opal_ext2x_event_t *ev = (opal_ext2x_event_t*)cbdata;
    OPAL_PMIX_WAKEUP_THREAD(&ev->lock);
}

int ext2x_server_finalize(void)
{
    pmix_status_t rc;
    opal_ext2x_event_t *event, *ev2;
    opal_list_t evlist;
    OBJ_CONSTRUCT(&evlist, opal_list_t);

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    --opal_pmix_base.initialized;

    if (0 < opal_pmix_base.initialized) {
        /* deregister all event handlers */
        OPAL_LIST_FOREACH_SAFE(event, ev2, &mca_pmix_ext2x_component.events, opal_ext2x_event_t) {
            OPAL_PMIX_DESTRUCT_LOCK(&event->lock);
            OPAL_PMIX_CONSTRUCT_LOCK(&event->lock);
            PMIx_Deregister_event_handler(event->index, dereg_cbfunc, (void*)event);
            opal_list_remove_item(&mca_pmix_ext2x_component.events, &event->super);
            /* wait and release outside the loop to avoid double mutex
             * interlock */
            opal_list_append(&evlist, &event->super);
        }
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
    OPAL_LIST_FOREACH_SAFE(event, ev2, &evlist, opal_ext2x_event_t) {
        OPAL_PMIX_WAIT_THREAD(&event->lock);
        opal_list_remove_item(&evlist, &event->super);
        OBJ_RELEASE(event);
    }
    OBJ_DESTRUCT(&evlist);
    rc = PMIx_server_finalize();
    return ext2x_convert_rc(rc);
}

int ext2x_server_gen_regex(const char *input, char **regex)
{
    pmix_status_t rc;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);

    rc = PMIx_generate_regex(input, regex);
    return ext2x_convert_rc(rc);
}


int ext2x_server_gen_ppn(const char *input, char **ppn)
{
    pmix_status_t rc;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);

    rc = PMIx_generate_ppn(input, ppn);
    return ext2x_convert_rc(rc);
}

int ext2x_server_register_nspace(opal_jobid_t jobid,
                                 int nlocalprocs,
                                 opal_list_t *info,
                                 opal_pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    opal_value_t *kv, *k2;
    pmix_info_t *pinfo = NULL, *pmap;
    size_t sz, szmap, m, n;
    char nspace[PMIX_MAX_NSLEN];
    pmix_status_t rc;
    opal_list_t *pmapinfo;
    opal_ext2x_jobid_trkr_t *job;
    opal_pmix_lock_t lock;
    int ret;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }

    /* convert the jobid */
    (void)opal_snprintf_jobid(nspace, PMIX_MAX_NSLEN, jobid);

    /* store this job in our list of known nspaces */
    job = OBJ_NEW(opal_ext2x_jobid_trkr_t);
    (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
    job->jobid = jobid;
    opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);

    /* convert the list to an array of pmix_info_t */
    if (NULL != info && 0 < (sz = opal_list_get_size(info))) {
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            if (0 == strcmp(kv->key, OPAL_PMIX_PROC_DATA)) {
                pinfo[n].value.type = PMIX_DATA_ARRAY;
                /* the value contains a list of values - convert
                 * that list to another array */
                pmapinfo = (opal_list_t*)kv->data.ptr;
                szmap = opal_list_get_size(pmapinfo);
                if (0 < szmap) {
                    PMIX_INFO_CREATE(pmap, szmap);
                    pinfo[n].value.data.darray = (pmix_data_array_t*)calloc(1, sizeof(pmix_data_array_t));
                    pinfo[n].value.data.darray->type = PMIX_INFO;
                    pinfo[n].value.data.darray->array = (struct pmix_info_t*)pmap;
                    pinfo[n].value.data.darray->size = szmap;
                    m = 0;
                    OPAL_LIST_FOREACH(k2, pmapinfo, opal_value_t) {
                        (void)strncpy(pmap[m].key, k2->key, PMIX_MAX_KEYLEN);
                        ext2x_value_load(&pmap[m].value, k2);
                        ++m;
                    }
                }
                OPAL_LIST_RELEASE(pmapinfo);
            } else {
                ext2x_value_load(&pinfo[n].value, kv);
            }
            ++n;
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }

    OPAL_PMIX_CONSTRUCT_LOCK(&lock);
    rc = PMIx_server_register_nspace(nspace, nlocalprocs, pinfo, sz,
                                     lkcbfunc, (void*)&lock);
    if (PMIX_SUCCESS == rc) {
        OPAL_PMIX_WAIT_THREAD(&lock);
    }
    OPAL_PMIX_DESTRUCT_LOCK(&lock);

    if (NULL != pinfo) {
        PMIX_INFO_FREE(pinfo, sz);
    }

    ret = ext2x_convert_rc(rc);

    /* release the caller */
    if (NULL != cbfunc) {
        cbfunc(ret, cbdata);
    }
    return ret;
}

void ext2x_server_deregister_nspace(opal_jobid_t jobid,
                                     opal_pmix_op_cbfunc_t cbfunc,
                                     void *cbdata)
{
    opal_ext2x_jobid_trkr_t *jptr;
    opal_pmix_lock_t lock;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        /* release the caller */
        if (NULL != cbfunc) {
            cbfunc(OPAL_ERR_NOT_INITIALIZED, cbdata);
        }
        return;
    }

    /* if we don't already have it, we can ignore this */
    OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_ext2x_jobid_trkr_t) {
        if (jptr->jobid == jobid) {
            /* found it - tell the server to deregister */
            OPAL_PMIX_CONSTRUCT_LOCK(&lock);
            OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
            PMIx_server_deregister_nspace(jptr->nspace, lkcbfunc, (void*)&lock);
            OPAL_PMIX_WAIT_THREAD(&lock);
            OPAL_PMIX_DESTRUCT_LOCK(&lock);
            /* now get rid of it from our list */
            OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
            opal_list_remove_item(&mca_pmix_ext2x_component.jobids, &jptr->super);
            OBJ_RELEASE(jptr);
            break;
        }
    }

    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
    /* release the caller */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }
}

int ext2x_server_register_client(const opal_process_name_t *proc,
                                 uid_t uid, gid_t gid,
                                 void *server_object,
                                 opal_pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    pmix_status_t rc;
    pmix_proc_t p;
    opal_pmix_lock_t lock;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);

    /* convert the jobid */
    (void)opal_snprintf_jobid(p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    p.rank = ext2x_convert_opalrank(proc->vpid);

    OPAL_PMIX_CONSTRUCT_LOCK(&lock);
    rc = PMIx_server_register_client(&p, uid, gid, server_object,
                                     lkcbfunc, (void*)&lock);
    if (PMIX_SUCCESS == rc) {
        OPAL_PMIX_WAIT_THREAD(&lock);
    }
    OPAL_PMIX_DESTRUCT_LOCK(&lock);
    return ext2x_convert_rc(rc);
}

/* tell the local PMIx server to cleanup this client as it is
 * done executing */
void ext2x_server_deregister_client(const opal_process_name_t *proc,
                                     opal_pmix_op_cbfunc_t cbfunc,
                                     void *cbdata)
{
    opal_ext2x_jobid_trkr_t *jptr;
    pmix_proc_t p;
    opal_pmix_lock_t lock;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        if (NULL != cbfunc) {
            cbfunc(OPAL_ERR_NOT_INITIALIZED, cbdata);
        }
        return;
    }

    /* if we don't already have it, we can ignore this */
    OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_ext2x_jobid_trkr_t) {
        if (jptr->jobid == proc->jobid) {
            /* found it - tell the server to deregister */
            (void)strncpy(p.nspace, jptr->nspace, PMIX_MAX_NSLEN);
            p.rank = ext2x_convert_opalrank(proc->vpid);
            OPAL_PMIX_CONSTRUCT_LOCK(&lock);
            OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
            PMIx_server_deregister_client(&p, lkcbfunc, (void*)&lock);
            OPAL_PMIX_WAIT_THREAD(&lock);
            OPAL_PMIX_DESTRUCT_LOCK(&lock);
            OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
            break;
        }
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }
}

/* have the local PMIx server setup the environment for this client */
int ext2x_server_setup_fork(const opal_process_name_t *proc, char ***env)
{
    pmix_status_t rc;
    pmix_proc_t p;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);

    /* convert the jobid */
    (void)opal_snprintf_jobid(p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    p.rank = ext2x_convert_opalrank(proc->vpid);

    rc = PMIx_server_setup_fork(&p, env);
    return ext2x_convert_rc(rc);
}

/* this is the call back up from the embedded PMIx server that
 * will contain the returned data. Note that the embedded server
 * "owns" the data and will free it upon return from this function */
static void dmdx_response(pmix_status_t status, char *data, size_t sz, void *cbdata)
{
    int rc;
    ext2x_opcaddy_t *op = (ext2x_opcaddy_t*)cbdata;

    rc = ext2x_convert_rc(status);
    if (NULL != op->mdxcbfunc) {
        op->mdxcbfunc(rc, data, sz, op->cbdata, NULL, NULL);
    }
    OBJ_RELEASE(op);
}

/* request modex data for a local proc from the PMIx server */
int ext2x_server_dmodex(const opal_process_name_t *proc,
                        opal_pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    ext2x_opcaddy_t *op;
    pmix_status_t rc;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);

    /* setup the caddy */
    op = OBJ_NEW(ext2x_opcaddy_t);
    op->mdxcbfunc = cbfunc;
    op->cbdata = cbdata;

    /* convert the jobid */
    (void)opal_snprintf_jobid(op->p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    op->p.rank = ext2x_convert_opalrank(proc->vpid);

    /* find the internally-cached data for this proc */
    rc = PMIx_server_dmodex_request(&op->p, dmdx_response, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return ext2x_convert_rc(rc);
}

/* tell the PMIx server to notify its local clients of an event */
int ext2x_server_notify_event(int status,
                               const opal_process_name_t *source,
                               opal_list_t *info,
                               opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    opal_value_t *kv;
    pmix_info_t *pinfo;
    size_t sz, n;
    pmix_status_t rc;
    ext2x_opcaddy_t *op;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
    if (0 >= opal_pmix_base.initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);

    /* convert the list to an array of pmix_info_t */
    if (NULL != info && 0 < (sz = opal_list_get_size(info))) {
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            ext2x_value_load(&pinfo[n].value, kv);
            ++n;
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }
    /* setup the caddy */
    op = OBJ_NEW(ext2x_opcaddy_t);
    op->info = pinfo;
    op->sz = sz;
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    /* convert the jobid */
    if (NULL == source) {
        (void)opal_snprintf_jobid(op->p.nspace, PMIX_MAX_NSLEN, OPAL_JOBID_INVALID);
        op->p.rank = ext2x_convert_opalrank(OPAL_VPID_INVALID);
    } else {
        (void)opal_snprintf_jobid(op->p.nspace, PMIX_MAX_NSLEN, source->jobid);
        op->p.rank = ext2x_convert_opalrank(source->vpid);
    }


    rc = ext2x_convert_opalrc(status);
    /* the range must be nonlocal so the server will pass
     * the event down to its local clients */
    rc = PMIx_Notify_event(rc, &op->p, PMIX_RANGE_SESSION,
                           pinfo, sz, opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return ext2x_convert_rc(rc);
}
