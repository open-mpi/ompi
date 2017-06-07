/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC. All rights
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
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/mca/pmix/base/base.h"
#include "pmix2x.h"

#include "pmix.h"
#include "pmix_server.h"

/****    S.O.U.T.H.B.O.U.N.D   I.N.T.E.R.F.A.C.E.S     ****/

/* These are the interfaces used by the OMPI/ORTE/OPAL layer to call
 * down into the embedded PMIx server. */

extern pmix_server_module_t mymodule;
extern opal_pmix_server_module_t *host_module;
static char *dbgvalue=NULL;
static size_t errhdler_ref = 0;

#define PMIX_WAIT_FOR_COMPLETION(a)             \
    do {                                        \
        while ((a)) {                           \
            usleep(10);                         \
        }                                       \
        OPAL_ACQUIRE_OBJECT(a);                 \
    } while (0)

static void errreg_cbfunc (pmix_status_t status,
                          size_t errhandler_ref,
                          void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;

    OPAL_ACQUIRE_OBJECT(active);
    errhdler_ref = errhandler_ref;
    opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                        "PMIX server errreg_cbfunc - error handler registered status=%d, reference=%lu",
                         status, (unsigned long)errhandler_ref);
    OPAL_POST_OBJECT(active);
    *active = false;
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(op);

    if (NULL != op->opcbfunc) {
        op->opcbfunc(pmix2x_convert_rc(status), op->cbdata);
    }
    if (op->active) {
        op->status = status;
        OPAL_POST_OBJECT(op);
        op->active = false;
    } else {
        OBJ_RELEASE(op);
    }
}

static void op2cbfunc(pmix_status_t status, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;

    OPAL_POST_OBJECT(active);
    *active = false;
}

int pmix2x_server_init(opal_pmix_server_module_t *module,
                      opal_list_t *info)
{
    pmix_status_t rc;
    int dbg;
    opal_value_t *kv;
    pmix_info_t *pinfo;
    size_t sz, n;
    volatile bool active;
    opal_pmix2x_jobid_trkr_t *job;

    if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
        asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
        putenv(dbgvalue);
    }

    /* convert the list to an array of pmix_info_t */
    if (NULL != info) {
        sz = opal_list_get_size(info);
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&pinfo[n].value, kv);
            ++n;
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }

    /* insert ourselves into our list of jobids - it will be the
     * first, and so we'll check it first */
    job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
    (void)opal_snprintf_jobid(job->nspace, PMIX_MAX_NSLEN, OPAL_PROC_MY_NAME.jobid);
    job->jobid = OPAL_PROC_MY_NAME.jobid;
    opal_list_append(&mca_pmix_pmix2x_component.jobids, &job->super);

    if (PMIX_SUCCESS != (rc = PMIx_server_init(&mymodule, pinfo, sz))) {
        PMIX_INFO_FREE(pinfo, sz);
        return pmix2x_convert_rc(rc);
    }
    PMIX_INFO_FREE(pinfo, sz);

    /* record the host module */
    host_module = module;

    /* register the default event handler */
    active = true;
    PMIX_INFO_CREATE(pinfo, 1);
    PMIX_INFO_LOAD(&pinfo[0], PMIX_EVENT_HDLR_NAME, "OPAL-PMIX-2X-SERVER-DEFAULT", PMIX_STRING);
    PMIx_Register_event_handler(NULL, 0, pinfo, 1, pmix2x_event_hdlr, errreg_cbfunc, (void*)&active);
    PMIX_WAIT_FOR_COMPLETION(active);
    PMIX_INFO_FREE(pinfo, 1);

    /* as we might want to use some client-side functions, be sure
     * to register our own nspace */
    PMIX_INFO_CREATE(pinfo, 1);
    PMIX_INFO_LOAD(&pinfo[0], PMIX_REGISTER_NODATA, NULL, PMIX_BOOL);
    active = true;
    PMIx_server_register_nspace(job->nspace, 1, pinfo, 1, op2cbfunc, (void*)&active);
    PMIX_WAIT_FOR_COMPLETION(active);
    PMIX_INFO_FREE(pinfo, 1);

    return OPAL_SUCCESS;
}

static void fincb(pmix_status_t status, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;
    OPAL_POST_OBJECT(active);
    *active = false;
}

int pmix2x_server_finalize(void)
{
    pmix_status_t rc;
    volatile bool active;

    /* deregister the default event handler */
    active = true;
    PMIx_Deregister_event_handler(errhdler_ref, fincb, (void*)&active);
    PMIX_WAIT_FOR_COMPLETION(active);

    rc = PMIx_server_finalize();
    return pmix2x_convert_rc(rc);
}

int pmix2x_server_gen_regex(const char *input, char **regex)
{
    pmix_status_t rc;

    rc = PMIx_generate_regex(input, regex);
    return pmix2x_convert_rc(rc);
}


int pmix2x_server_gen_ppn(const char *input, char **ppn)
{
    pmix_status_t rc;

    rc = PMIx_generate_ppn(input, ppn);
    return pmix2x_convert_rc(rc);
}

static void _reg_nspace(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *cd = (pmix2x_threadshift_t*)cbdata;
    opal_value_t *kv, *k2;
    pmix_info_t *pinfo = NULL, *pmap;
    size_t sz, szmap, m, n;
    char nspace[PMIX_MAX_NSLEN];
    pmix_status_t rc;
    opal_list_t *pmapinfo;
    opal_pmix2x_jobid_trkr_t *job;
    pmix2x_opcaddy_t op;

    OPAL_ACQUIRE_OBJECT(cd);

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    /* convert the jobid */
    (void)opal_snprintf_jobid(nspace, PMIX_MAX_NSLEN, cd->jobid);

    /* store this job in our list of known nspaces */
    job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
    (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
    job->jobid = cd->jobid;
    opal_list_append(&mca_pmix_pmix2x_component.jobids, &job->super);

    /* convert the list to an array of pmix_info_t */
    if (NULL != cd->info) {
        sz = opal_list_get_size(cd->info);
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, cd->info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            if (0 == strcmp(kv->key, OPAL_PMIX_PROC_DATA)) {
                pinfo[n].value.type = PMIX_DATA_ARRAY;
                /* the value contains a list of values - convert
                 * that list to another array */
                pmapinfo = (opal_list_t*)kv->data.ptr;
                szmap = opal_list_get_size(pmapinfo);
                PMIX_INFO_CREATE(pmap, szmap);
                pinfo[n].value.data.darray = (pmix_data_array_t*)calloc(1, sizeof(pmix_data_array_t));
                pinfo[n].value.data.darray->type = PMIX_INFO;
                pinfo[n].value.data.darray->array = (struct pmix_info_t*)pmap;
                pinfo[n].value.data.darray->size = szmap;
                m = 0;
                OPAL_LIST_FOREACH(k2, pmapinfo, opal_value_t) {
                    (void)strncpy(pmap[m].key, k2->key, PMIX_MAX_KEYLEN);
                    pmix2x_value_load(&pmap[m].value, k2);
                    ++m;
                }
                OPAL_LIST_RELEASE(pmapinfo);
            } else {
                pmix2x_value_load(&pinfo[n].value, kv);
            }
            ++n;
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }

    OBJ_CONSTRUCT(&op, pmix2x_opcaddy_t);
    op.active = true;
    rc = PMIx_server_register_nspace(nspace, cd->status, pinfo, sz,
                                     opcbfunc, (void*)&op);
    if (PMIX_SUCCESS == rc) {
        PMIX_WAIT_FOR_COMPLETION(op.active);
    } else {
        op.status = rc;
    }
    /* ensure we execute the cbfunc so the caller doesn't hang */
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(pmix2x_convert_rc(op.status), cd->cbdata);
    }
    if (NULL != pinfo) {
        PMIX_INFO_FREE(pinfo, sz);
    }
    OBJ_DESTRUCT(&op);
    OBJ_RELEASE(cd);
}

int pmix2x_server_register_nspace(opal_jobid_t jobid,
                                 int nlocalprocs,
                                 opal_list_t *info,
                                 opal_pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    pmix2x_threadshift_t *cd;

    /* we must threadshift this request as it touches
     * shared lists of objects */
    cd = OBJ_NEW(pmix2x_threadshift_t);
    cd->jobid = jobid;
    cd->status = nlocalprocs;
    cd->info = info;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* if the cbfunc is NULL, then the caller is in an event
     * and we can directly call the processing function */
    if (NULL == cbfunc) {
        _reg_nspace(0, 0, cd);
    } else {
        opal_event_assign(&cd->ev, opal_pmix_base.evbase,
                          -1, EV_WRITE, _reg_nspace, cd);
        OPAL_POST_OBJECT(cd);
        opal_event_active(&cd->ev, EV_WRITE, 1);
    }

    return OPAL_SUCCESS;
}

static void tdcbfunc(pmix_status_t status, void *cbdata)
{
    pmix2x_threadshift_t *cd = (pmix2x_threadshift_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(cd);
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(pmix2x_convert_rc(status), cd->cbdata);
    }
    if (cd->active) {
        OPAL_POST_OBJECT(cd);
        cd->active = false;
    } else {
        OBJ_RELEASE(cd);
    }
}

static void _dereg_nspace(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *cd = (pmix2x_threadshift_t*)cbdata;
    opal_pmix2x_jobid_trkr_t *jptr;

    OPAL_ACQUIRE_OBJECT(cd);
    /* if we don't already have it, we can ignore this */
    OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
        if (jptr->jobid == cd->jobid) {
            /* found it - tell the server to deregister */
            cd->active = true;
            PMIx_server_deregister_nspace(jptr->nspace, tdcbfunc, cd);
            PMIX_WAIT_FOR_COMPLETION(cd->active);
            OBJ_RELEASE(cd);
            /* now get rid of it from our list */
            opal_list_remove_item(&mca_pmix_pmix2x_component.jobids, &jptr->super);
            OBJ_RELEASE(jptr);
            return;
        }
    }
    /* must release the caller */
    tdcbfunc(PMIX_ERR_NOT_FOUND, cd);
}

void pmix2x_server_deregister_nspace(opal_jobid_t jobid,
                                     opal_pmix_op_cbfunc_t cbfunc,
                                     void *cbdata)
{
    pmix2x_threadshift_t *cd;

    /* we must threadshift this request as it touches
     * shared lists of objects */
    cd = OBJ_NEW(pmix2x_threadshift_t);
    cd->jobid = jobid;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    if (NULL == cbfunc) {
        _dereg_nspace(0, 0, cd);
    } else {
        opal_event_assign(&cd->ev, opal_pmix_base.evbase,
                     -1, EV_WRITE, _dereg_nspace, cd);
        OPAL_POST_OBJECT(cd);
        opal_event_active(&cd->ev, EV_WRITE, 1);
    }
}

int pmix2x_server_register_client(const opal_process_name_t *proc,
                                 uid_t uid, gid_t gid,
                                 void *server_object,
                                 opal_pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    pmix_status_t rc;
    pmix_proc_t p;
    pmix2x_opcaddy_t op;

    /* convert the jobid */
    (void)opal_snprintf_jobid(p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    p.rank = pmix2x_convert_opalrank(proc->vpid);

    OBJ_CONSTRUCT(&op, pmix2x_opcaddy_t);
    op.active = true;
    rc = PMIx_server_register_client(&p, uid, gid, server_object,
                                     opcbfunc, (void*)&op);
    if (PMIX_SUCCESS == rc) {
        PMIX_WAIT_FOR_COMPLETION(op.active);
        rc = op.status;
    }
    OBJ_DESTRUCT(&op);
    return pmix2x_convert_rc(rc);
}

static void _dereg_client(int sd, short args, void *cbdata)
{
    pmix2x_threadshift_t *cd = (pmix2x_threadshift_t*)cbdata;
    opal_pmix2x_jobid_trkr_t *jptr;
    pmix_proc_t p;

    OPAL_ACQUIRE_OBJECT(cd);
    /* if we don't already have it, we can ignore this */
    OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
        if (jptr->jobid == cd->source->jobid) {
            /* found it - tell the server to deregister */
            (void)strncpy(p.nspace, jptr->nspace, PMIX_MAX_NSLEN);
            p.rank = pmix2x_convert_opalrank(cd->source->vpid);
            cd->active = true;
            PMIx_server_deregister_client(&p, tdcbfunc, (void*)cd);
            PMIX_WAIT_FOR_COMPLETION(cd->active);
            break;
        }
    }
    OBJ_RELEASE(cd);
}

/* tell the local PMIx server to cleanup this client as it is
 * done executing */
void pmix2x_server_deregister_client(const opal_process_name_t *proc,
                                     opal_pmix_op_cbfunc_t cbfunc,
                                     void *cbdata)
{
    pmix2x_threadshift_t *cd;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */
    cd = OBJ_NEW(pmix2x_threadshift_t);
    cd->source = proc;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    if (NULL == cbfunc) {
        _dereg_client(0, 0, cd);
    } else {
        opal_event_assign(&cd->ev, opal_pmix_base.evbase,
                     -1, EV_WRITE, _dereg_client, cd);
        OPAL_POST_OBJECT(cd);
        opal_event_active(&cd->ev, EV_WRITE, 1);
    }
}

/* have the local PMIx server setup the environment for this client */
int pmix2x_server_setup_fork(const opal_process_name_t *proc, char ***env)
{
    pmix_status_t rc;
    pmix_proc_t p;

    /* convert the jobid */
    (void)opal_snprintf_jobid(p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    p.rank = pmix2x_convert_opalrank(proc->vpid);

    rc = PMIx_server_setup_fork(&p, env);
    return pmix2x_convert_rc(rc);
}

/* this is the call back up from the embedded PMIx server that
 * will contain the returned data. Note that the embedded server
 * "owns" the data and will free it upon return from this function */
static void dmdx_response(pmix_status_t status, char *data, size_t sz, void *cbdata)
{
    int rc;
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;

    rc = pmix2x_convert_rc(status);
    if (NULL != op->mdxcbfunc) {
        op->mdxcbfunc(rc, data, sz, op->cbdata, NULL, NULL);
    }
    OBJ_RELEASE(op);
}

/* request modex data for a local proc from the PMIx server */
int pmix2x_server_dmodex(const opal_process_name_t *proc,
                        opal_pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix2x_opcaddy_t *op;
    pmix_status_t rc;

    /* setup the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->mdxcbfunc = cbfunc;
    op->cbdata = cbdata;

    /* convert the jobid */
    (void)opal_snprintf_jobid(op->p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    op->p.rank = pmix2x_convert_opalrank(proc->vpid);

    /* find the internally-cached data for this proc */
    rc = PMIx_server_dmodex_request(&op->p, dmdx_response, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix2x_convert_rc(rc);
}

/* tell the PMIx server to notify its local clients of an event */
int pmix2x_server_notify_event(int status,
                               const opal_process_name_t *source,
                               opal_list_t *info,
                               opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    opal_value_t *kv;
    pmix_info_t *pinfo;
    size_t sz, n;
    pmix_status_t rc;
    pmix2x_opcaddy_t *op;

    /* convert the list to an array of pmix_info_t */
    if (NULL != info) {
        sz = opal_list_get_size(info);
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&pinfo[n].value, kv);
            ++n;
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }
    /* setup the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->info = pinfo;
    op->sz = sz;
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    /* convert the jobid */
    if (NULL == source) {
        (void)opal_snprintf_jobid(op->p.nspace, PMIX_MAX_NSLEN, OPAL_JOBID_INVALID);
        op->p.rank = pmix2x_convert_opalrank(OPAL_VPID_INVALID);
    } else {
        (void)opal_snprintf_jobid(op->p.nspace, PMIX_MAX_NSLEN, source->jobid);
        op->p.rank = pmix2x_convert_opalrank(source->vpid);
    }


    rc = pmix2x_convert_opalrc(status);
    /* the range is irrelevant here as the server is passing
     * the event down to its local clients */
    rc = PMIx_Notify_event(rc, &op->p, PMIX_RANGE_LOCAL,
                           pinfo, sz, opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix2x_convert_rc(rc);
}
