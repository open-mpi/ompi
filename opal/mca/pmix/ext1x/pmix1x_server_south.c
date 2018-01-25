/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
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
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/mca/pmix/base/base.h"
#include "pmix1x.h"

#include "pmix.h"
#include "pmix_server.h"

/****    S.O.U.T.H.B.O.U.N.D   I.N.T.E.R.F.A.C.E.S     ****/

/* These are the interfaces used by the OMPI/ORTE/OPAL layer to call
 * down into the embedded PMIx server. */

extern pmix_server_module_t mymodule;
extern opal_pmix_server_module_t *host_module;
static char *dbgvalue=NULL;
static int errhdler_ref = 0;

static void completion_handler(int status, opal_list_t *results,
                               opal_pmix_op_cbfunc_t cbfunc, void *thiscbdata,
                               void *notification_cbdata) {
    int * cond = (int *)notification_cbdata;
    *cond = 0;
}

#define PMIX_WAIT_FOR_COMPLETION(a)             \
    do {                                        \
        while ((a)) {                           \
            usleep(10);                         \
        }                                       \
    } while (0);

static void myerr(pmix_status_t status,
                  pmix_proc_t procs[], size_t nprocs,
                  pmix_info_t info[], size_t ninfo)
{
    int rc;
    opal_list_t plist, ilist;
    opal_namelist_t *nm;
    opal_value_t *iptr;
    volatile int cond = 1;
    size_t n;

    /* convert the incoming status */
    rc = pmix1_convert_rc(status);

    /* convert the array of procs */
    OBJ_CONSTRUCT(&plist, opal_list_t);
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        nm->name.jobid = strtoul(procs[n].nspace, NULL, 10);
        nm->name.vpid = procs[n].rank;
        opal_list_append(&plist, &nm->super);
    }

    /* convert the array of info */
    OBJ_CONSTRUCT(&ilist, opal_list_t);
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        iptr->key = strdup(info[n].key);
        pmix1_value_unload(iptr, &info[n].value);
        opal_list_append(&plist, &iptr->super);
    }

    /* call the base errhandler */
    opal_pmix_base_evhandler(rc, &OPAL_PROC_MY_NAME, &plist, &ilist, completion_handler, (void *)&cond);
    PMIX_WAIT_FOR_COMPLETION(cond);

    OPAL_LIST_DESTRUCT(&plist);
    OPAL_LIST_DESTRUCT(&ilist);
}

static void errreg_cbfunc(pmix_status_t status,
                          int errhandler_ref,
                          void *cbdata)
{
    errhdler_ref = errhandler_ref;
    opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                        "PMIX server errreg_cbfunc - error handler registered status=%d, reference=%d",
                        status, errhandler_ref);
}

static void op2cbfunc(pmix_status_t status, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;
    if (active)
        *active = false;
}

int pmix1_server_init(opal_pmix_server_module_t *module,
                      opal_list_t *info)
{
    pmix_status_t rc;
    int dbg;
    opal_value_t *kv;
    pmix_info_t *pinfo;
    size_t sz, n;
    opal_pmix1_jobid_trkr_t *job;
    volatile bool active;

    if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
        asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
        putenv(dbgvalue);
    }

    /* convert the list to an array of pmix_info_t */
    if (NULL != info && 0 < (sz = opal_list_get_size(info))) {
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&pinfo[n].value, kv);
            ++n;
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }

    /* insert this into our list of jobids - it will be the
     * first, and so we'll check it first */
    job = OBJ_NEW(opal_pmix1_jobid_trkr_t);
    (void)opal_snprintf_jobid(job->nspace, PMIX_MAX_NSLEN, OPAL_PROC_MY_NAME.jobid);
    job->jobid = OPAL_PROC_MY_NAME.jobid;
    opal_list_append(&mca_pmix_ext1x_component.jobids, &job->super);

    if (PMIX_SUCCESS != (rc = PMIx_server_init(&mymodule, pinfo, sz))) {
        PMIX_INFO_FREE(pinfo, sz);
        return pmix1_convert_rc(rc);
    }
    PMIX_INFO_FREE(pinfo, sz);

    /* record the host module */
    host_module = module;

    /* register the errhandler */
    PMIx_Register_errhandler(NULL, 0, myerr, errreg_cbfunc, NULL);

    /* as we might want to use some client-side functions, be sure
     * to register our own nspace */
    active = true;
    PMIx_server_register_nspace(job->nspace, 1, NULL, 0, op2cbfunc, (void*)&active);
    PMIX_WAIT_FOR_COMPLETION(active);

    return OPAL_SUCCESS;
}

int pmix1_server_finalize(void)
{
    pmix_status_t rc;

    /* deregister the errhandler */
    PMIx_Deregister_errhandler(errhdler_ref, NULL, NULL);

    rc = PMIx_server_finalize();
    return pmix1_convert_rc(rc);
}

int pmix1_server_gen_regex(const char *input, char **regex)
{
    pmix_status_t rc;

    rc = PMIx_generate_regex(input, regex);
    return pmix1_convert_rc(rc);
}


int pmix1_server_gen_ppn(const char *input, char **ppn)
{
    pmix_status_t rc;

    rc = PMIx_generate_ppn(input, ppn);
    return pmix1_convert_rc(rc);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;

    if (NULL != op->opcbfunc) {
        op->opcbfunc(pmix1_convert_rc(status), op->cbdata);
    }
    if (op->active) {
        op->active = false;
    } else {
        OBJ_RELEASE(op);
    }
}

int pmix1_server_register_nspace(opal_jobid_t jobid,
                                 int nlocalprocs,
                                 opal_list_t *info,
                                 opal_pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    opal_value_t *kv, *k2;
    pmix_info_t *pinfo, *pmap;
    size_t sz, szmap, m, n;
    char nspace[PMIX_MAX_NSLEN];
    pmix_status_t rc;
    pmix1_opcaddy_t op;
    opal_list_t *pmapinfo;
    opal_pmix1_jobid_trkr_t *job;

    /* convert the jobid */
    (void)opal_snprintf_jobid(nspace, PMIX_MAX_NSLEN, jobid);

    /* store this job in our list of known nspaces */
    job = OBJ_NEW(opal_pmix1_jobid_trkr_t);
    (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
    job->jobid = jobid;
    opal_list_append(&mca_pmix_ext1x_component.jobids, &job->super);

    /* convert the list to an array of pmix_info_t */
    if (NULL != info && 0 < (sz = opal_list_get_size(info))) {
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            if (0 == strcmp(kv->key, OPAL_PMIX_PROC_DATA)) {
                pinfo[n].value.type = PMIX_INFO_ARRAY;
                /* the value contains a list of values - convert
                 * that list to another array */
                pmapinfo = (opal_list_t*)kv->data.ptr;
                szmap = opal_list_get_size(pmapinfo);
                if (0 < szmap) {
                    PMIX_INFO_CREATE(pmap, szmap);
                    pinfo[n].value.data.array.array = (struct pmix_info_t*)pmap;
                    pinfo[n].value.data.array.size = szmap;
                    m = 0;
                    OPAL_LIST_FOREACH(k2, pmapinfo, opal_value_t) {
                        (void)strncpy(pmap[m].key, k2->key, PMIX_MAX_KEYLEN);
                        pmix1_value_load(&pmap[m].value, k2);
                        ++m;
                    }
                }
                OPAL_LIST_RELEASE(pmapinfo);
            } else {
                pmix1_value_load(&pinfo[n].value, kv);
            }
            ++n;
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }

    /* setup the caddy */
    OBJ_CONSTRUCT(&op, pmix1_opcaddy_t);
    op.info = pinfo;
    op.sz = sz;
    op.opcbfunc = cbfunc;
    op.cbdata = cbdata;
    op.active = true;
    rc = PMIx_server_register_nspace(nspace, nlocalprocs, pinfo, sz,
                                     opcbfunc, &op);
    if (PMIX_SUCCESS == rc) {
        PMIX_WAIT_FOR_COMPLETION(op.active);
    }
    PMIX_INFO_FREE(pinfo, sz);
    return pmix1_convert_rc(rc);
}

void pmix1_server_deregister_nspace(opal_jobid_t jobid,
                                    opal_pmix_op_cbfunc_t cbfunc,
                                    void *cbdata)
{
    opal_pmix1_jobid_trkr_t *jptr;

    /* if we don't already have it, we can ignore this */
    OPAL_LIST_FOREACH(jptr, &mca_pmix_ext1x_component.jobids, opal_pmix1_jobid_trkr_t) {
        if (jptr->jobid == jobid) {
            /* found it - tell the server to deregister */
            PMIx_server_deregister_nspace(jptr->nspace);
            /* now get rid of it from our list */
            opal_list_remove_item(&mca_pmix_ext1x_component.jobids, &jptr->super);
            OBJ_RELEASE(jptr);
            return;
        }
    }
}

int pmix1_server_register_client(const opal_process_name_t *proc,
                                 uid_t uid, gid_t gid,
                                 void *server_object,
                                 opal_pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    pmix_status_t rc;
    pmix1_opcaddy_t *op;

    /* setup the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    /* convert the jobid */
    (void)opal_snprintf_jobid(op->p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    op->p.rank = proc->vpid;

    rc = PMIx_server_register_client(&op->p, uid, gid, server_object,
                                     opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix1_convert_rc(rc);
}

void pmix1_server_deregister_client(const opal_process_name_t *proc,
                                    opal_pmix_op_cbfunc_t cbfunc,
                                    void *cbdata)
{
    opal_pmix1_jobid_trkr_t *jptr;
    pmix_proc_t p;

    /* if we don't already have it, we can ignore this */
    OPAL_LIST_FOREACH(jptr, &mca_pmix_ext1x_component.jobids, opal_pmix1_jobid_trkr_t) {
        if (jptr->jobid == proc->jobid) {
            /* found it - tell the server to deregister */
            (void)strncpy(p.nspace, jptr->nspace, PMIX_MAX_NSLEN);
            p.rank = proc->vpid;
            PMIx_server_deregister_client(&p);
            return;
        }
    }
}


int pmix1_server_setup_fork(const opal_process_name_t *proc, char ***env)
{
    pmix_status_t rc;
    pmix_proc_t p;

    /* convert the jobid */
    (void)opal_snprintf_jobid(p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    p.rank = proc->vpid;

    rc = PMIx_server_setup_fork(&p, env);
    return pmix1_convert_rc(rc);
}

/* this is the call back up from the embedded PMIx server that
 * will contain the returned data. Note that the embedded server
 * "owns" the data and will free it upon return from this function */
static void dmdx_response(pmix_status_t status, char *data, size_t sz, void *cbdata)
{
    int rc;
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;

    rc = pmix1_convert_rc(status);
    if (NULL != op->mdxcbfunc) {
        op->mdxcbfunc(rc, data, sz, op->cbdata, NULL, NULL);
    }
    OBJ_RELEASE(op);
}

int pmix1_server_dmodex(const opal_process_name_t *proc,
                        opal_pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix1_opcaddy_t *op;
    pmix_status_t rc;

    /* setup the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->mdxcbfunc = cbfunc;
    op->cbdata = cbdata;

    /* convert the jobid */
    (void)opal_snprintf_jobid(op->p.nspace, PMIX_MAX_NSLEN, proc->jobid);
    op->p.rank = proc->vpid;

    /* find the internally-cached data for this proc */
    rc = PMIx_server_dmodex_request(&op->p, dmdx_response, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix1_convert_rc(rc);
}

int pmix1_server_notify_error(int status,
                              const opal_process_name_t *source,
                              opal_list_t *info,
                              opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    opal_value_t *kv;
    pmix_info_t *pinfo;
    size_t sz, n;
    pmix_status_t rc;
    pmix1_opcaddy_t *op;

    /* setup the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    /* convert the list to an array of pmix_info_t */
    if (NULL != info && 0 < (sz = opal_list_get_size(info))) {
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&pinfo[n].value, kv);
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }
    op->info = pinfo;
    op->sz = sz;

    rc = pmix1_convert_opalrc(status);
    rc = PMIx_Notify_error(rc, NULL, 0, NULL, 0,
                           pinfo, sz, opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix1_convert_rc(rc);
}
