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

#include "pmix120.h"
#include "opal/mca/pmix/base/base.h"

#include "opal/mca/pmix/pmix120/pmix/include/pmix/pmix_common.h"

/****    C.O.M.M.O.N   I.N.T.E.R.F.A.C.E.S     ****/

/* These are functions used by both client and server to
 * access common functions in the embedded PMIx library */

static const char *pmix120_get_nspace(opal_jobid_t jobid);
static void pmix120_register_jobid(opal_jobid_t jobid, const char *nspace);
static void pmix120_register_errhandler(opal_list_t *info,
                                        opal_pmix_notification_fn_t errhandler,
                                        opal_pmix_errhandler_reg_cbfunc_t cbfunc,
                                        void *cbdata);
static void pmix120_deregister_errhandler(int errhandler,
                                        opal_pmix_op_cbfunc_t cbfunc,
                                        void *cbdata);

const opal_pmix_base_module_t opal_pmix_pmix120_module = {
    /* client APIs */
    .init = pmix120_client_init,
    .finalize = pmix120_client_finalize,
    .initialized = pmix120_initialized,
    .abort = pmix120_abort,
    .commit = pmix120_commit,
    .fence = pmix120_fence,
    .fence_nb = pmix120_fencenb,
    .put = pmix120_put,
    .get = pmix120_get,
    .get_nb = pmix120_getnb,
    .publish = pmix120_publish,
    .publish_nb = pmix120_publishnb,
    .lookup = pmix120_lookup,
    .lookup_nb = pmix120_lookupnb,
    .unpublish = pmix120_unpublish,
    .unpublish_nb = pmix120_unpublishnb,
    .spawn = pmix120_spawn,
    .spawn_nb = pmix120_spawnnb,
    .connect = pmix120_connect,
    .connect_nb = pmix120_connectnb,
    .disconnect = pmix120_disconnect,
    .disconnect_nb = pmix120_disconnectnb,
    .resolve_peers = pmix120_resolve_peers,
    .resolve_nodes = pmix120_resolve_nodes,
    /* server APIs */
    .server_init = pmix120_server_init,
    .server_finalize = pmix120_server_finalize,
    .generate_regex = pmix120_server_gen_regex,
    .generate_ppn = pmix120_server_gen_ppn,
    .server_register_nspace = pmix120_server_register_nspace,
    .server_deregister_nspace = pmix120_server_deregister_nspace,
    .server_register_client = pmix120_server_register_client,
    .server_deregister_client = pmix120_server_deregister_client,
    .server_setup_fork = pmix120_server_setup_fork,
    .server_dmodex_request = pmix120_server_dmodex,
    .server_notify_error = pmix120_server_notify_error,
    /* utility APIs */
    .get_version = PMIx_Get_version,
    .register_errhandler = pmix120_register_errhandler,
    .deregister_errhandler = pmix120_deregister_errhandler,
    .store_local = pmix120_store_local,
    .get_nspace = pmix120_get_nspace,
    .register_jobid = pmix120_register_jobid
};

static const char *pmix120_get_nspace(opal_jobid_t jobid)
{
    opal_pmix120_jobid_trkr_t *jptr;

    OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix120_component.jobids, opal_pmix120_jobid_trkr_t) {
        if (jptr->jobid == jobid) {
            return jptr->nspace;
        }
    }
    return NULL;
}

static void pmix120_register_jobid(opal_jobid_t jobid, const char *nspace)
{
    opal_pmix120_jobid_trkr_t *jptr;

    /* if we don't already have it, add this to our jobid tracker */
    OPAL_LIST_FOREACH(jptr, &mca_pmix_pmix120_component.jobids, opal_pmix120_jobid_trkr_t) {
        if (jptr->jobid == jobid) {
            return;
        }
    }
    jptr = OBJ_NEW(opal_pmix120_jobid_trkr_t);
    (void)strncpy(jptr->nspace, nspace, PMIX_MAX_NSLEN);
    jptr->jobid = jobid;
    opal_list_append(&mca_pmix_pmix120_component.jobids, &jptr->super);
}

static void cleanup_cbfunc(void *cbdata)
{
    pmix120_opalcaddy_t *cd = (pmix120_opalcaddy_t*)cbdata;
    OBJ_RELEASE(cd);
}

/* NOTE: this callback is occurring in the PMIx progress
 * thread - those receiving it must thread-shift as needed */
static void notify(pmix_status_t status,
                   pmix_proc_t procs[], size_t nprocs,
                   pmix_info_t info[], size_t ninfo)
{
    opal_pmix_notification_fn_t errcbfunc;
    int eid, rc;
    pmix120_opalcaddy_t *cd;
    opal_namelist_t *nm;
    opal_value_t *iptr;
    size_t n;
    opal_pmix120_etracker_t *trk;

    /* the errhandler reference number is always the first element
     * in the info array */
    if (NULL == info ||
        0 != strncmp(PMIX_ERROR_HANDLER_ID, info[0].key, PMIX_MAX_KEYLEN) ||
        PMIX_INT != info[0].value.type) {
        /* that's an error - should never happen */
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return;
    }
    eid = info[0].value.data.integer;

    /* lookup the errhandler */
    errcbfunc = NULL;
    OPAL_LIST_FOREACH(trk, &mca_pmix_pmix120_component.errhandlers, opal_pmix120_etracker_t) {
        if (eid == trk->eid) {
            errcbfunc = trk->cbfunc;
            break;
        }
    }
    if (NULL == errcbfunc) {
        /* that's an error - should never happen */
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return;
    }

    /* create the caddy */
    cd = OBJ_NEW(pmix120_opalcaddy_t);

    /* convert the status */
    rc = pmix120_convert_rc(status);

    /* convert the proc array */
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

    /* convert the rest of array of info */
    for (n=1; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        iptr->key = strdup(info[n].key);
        (void)pmix120_value_unload(iptr, &info[n].value);
        opal_list_append(&cd->info, &iptr->super);
    }

    /* execute the callback */
    errcbfunc(rc, &cd->procs, &cd->info, cleanup_cbfunc, cd);
}

/* registration handler */
static void reg_thread(int sd, short args, void *cbdata)
{
    pmix120_opcaddy_t *cd = (pmix120_opcaddy_t*)cbdata;
    int rc;
    opal_pmix120_etracker_t *trk;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s register complete with status %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        cd->status);

    /* convert the status */
    rc = pmix120_convert_rc(cd->status);

    /* if we successfully registered, then track it */
    if (OPAL_SUCCESS == rc) {
        trk = OBJ_NEW(opal_pmix120_etracker_t);
        trk->eid = cd->errhandler_ref;
        trk->cbfunc = cd->errhandler;
        opal_list_append(&mca_pmix_pmix120_component.errhandlers, &trk->super);
    }

    /* complete the callback */
    if (NULL != cd->reg_cbfunc) {
        cd->reg_cbfunc(rc, cd->errhandler_ref, cd->cbdata);
    }
    OBJ_RELEASE(cd);
}
/* NOTE: this callback occurs in the PMIx progress thread,
 * so we need to thread shift it before handling */
static void reg_cbfunc(pmix_status_t status,
                       int errhandler_ref,
                       void *cbdata)
{
    pmix120_opcaddy_t *cd = (pmix120_opcaddy_t*)cbdata;
    cd->status = status;
    cd->errhandler_ref = errhandler_ref;
    opal_event_set(opal_sync_event_base, &cd->ev,
                   -1, OPAL_EV_WRITE, reg_thread, cd);
    opal_event_set_priority(&cd->ev, OPAL_EV_MSG_HI_PRI);
    opal_event_active(&cd->ev, OPAL_EV_WRITE, 1);
}
static void pmix120_register_errhandler(opal_list_t *info,
                                        opal_pmix_notification_fn_t errhandler,
                                        opal_pmix_errhandler_reg_cbfunc_t cbfunc,
                                        void *cbdata)
{
    pmix120_opcaddy_t *cd;
    size_t n;
    opal_value_t *ival;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s REGISTER ERRHDNLR INFO %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == info) ? "NULL" : "NOT-NULL");

    /* setup a caddy for the operation so we can free
     * the array when done */
    cd = OBJ_NEW(pmix120_opcaddy_t);
    cd->errhandler = errhandler;
    cd->reg_cbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* convert the list into an array of pmix_info_t */
    if (NULL != info) {
        cd->ninfo = opal_list_get_size(info);
        if (0 < cd->ninfo) {
            PMIX_INFO_CREATE(cd->info, cd->ninfo);
            n=0;
            OPAL_LIST_FOREACH(ival, info, opal_value_t) {
                (void)strncpy(cd->info[n].key, ival->key, PMIX_MAX_KEYLEN);
                cd->info[n].value.type = PMIX_INT;
                cd->info[n].value.data.status = pmix120_convert_opalrc(ival->data.integer);
            }
        }
    }

    /* pass the call down to the library */
    PMIx_Register_errhandler(cd->info, cd->ninfo, notify,
                             reg_cbfunc, cd);
}

static void dereg_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix120_opcaddy_t *cd = (pmix120_opcaddy_t*)cbdata;
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(OPAL_SUCCESS, cd->cbdata);
    }
    OBJ_RELEASE(cd);
}
static void pmix120_deregister_errhandler(int errhandler,
                                          opal_pmix_op_cbfunc_t cbfunc,
                                          void *cbdata)
{
    opal_pmix120_etracker_t *trk;
    pmix120_opcaddy_t *cd;

    OPAL_LIST_FOREACH(trk, &mca_pmix_pmix120_component.errhandlers, opal_pmix120_etracker_t) {
        if (errhandler == trk->eid) {
            opal_list_remove_item(&mca_pmix_pmix120_component.errhandlers, &trk->super);
            OBJ_RELEASE(trk);
            break;
        }
    }

    /* pass it down to the library */
    cd = OBJ_NEW(pmix120_opcaddy_t);
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIx_Deregister_errhandler(errhandler, dereg_cbfunc, cd);
}


pmix_status_t pmix120_convert_opalrc(int rc)
{
    switch (rc) {
    case OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    case OPAL_ERR_COMM_FAILURE:
        return PMIX_ERR_COMM_FAILURE;
    case OPAL_ERR_NOT_IMPLEMENTED:
        return PMIX_ERR_NOT_IMPLEMENTED;
    case OPAL_ERR_NOT_SUPPORTED:
        return PMIX_ERR_NOT_SUPPORTED;
    case OPAL_ERR_NOT_FOUND:
        return PMIX_ERR_NOT_FOUND;
    case OPAL_ERR_SERVER_NOT_AVAIL:
        return PMIX_ERR_SERVER_NOT_AVAIL;

    case OPAL_ERR_BAD_PARAM:
        return PMIX_ERR_BAD_PARAM;
    case OPAL_ERR_OUT_OF_RESOURCE:
        return PMIX_ERR_NOMEM;

    case OPAL_ERR_DATA_VALUE_NOT_FOUND:
        return PMIX_ERR_DATA_VALUE_NOT_FOUND;
    case OPAL_ERR_IN_ERRNO:
        return PMIX_ERR_IN_ERRNO;
    case OPAL_ERR_UNREACH:
        return PMIX_ERR_UNREACH;
    case OPAL_ERR_TIMEOUT:
        return PMIX_ERR_TIMEOUT;
    case OPAL_ERR_PERM:
        return PMIX_ERR_NO_PERMISSIONS;
    case OPAL_ERR_PACK_MISMATCH:
        return PMIX_ERR_PACK_MISMATCH;
    case OPAL_ERR_PACK_FAILURE:
        return PMIX_ERR_PACK_FAILURE;

    case OPAL_ERR_UNPACK_FAILURE:
        return PMIX_ERR_UNPACK_FAILURE;
    case OPAL_ERR_UNPACK_INADEQUATE_SPACE:
        return PMIX_ERR_UNPACK_INADEQUATE_SPACE;
    case OPAL_ERR_TYPE_MISMATCH:
        return PMIX_ERR_TYPE_MISMATCH;
    case OPAL_ERR_PROC_ENTRY_NOT_FOUND:
        return PMIX_ERR_PROC_ENTRY_NOT_FOUND;
    case OPAL_ERR_UNKNOWN_DATA_TYPE:
        return PMIX_ERR_UNKNOWN_DATA_TYPE;
    case OPAL_ERR_WOULD_BLOCK:
        return PMIX_ERR_WOULD_BLOCK;
    case OPAL_EXISTS:
        return PMIX_EXISTS;

    case OPAL_ERR_SILENT:
        return PMIX_ERR_SILENT;
    case OPAL_ERROR:
        return PMIX_ERROR;
    case OPAL_SUCCESS:
        return PMIX_SUCCESS;
    default:
        return PMIX_ERROR;
    }
}

int pmix120_convert_rc(pmix_status_t rc)
{
    switch (rc) {
    case PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    case PMIX_ERR_COMM_FAILURE:
        return OPAL_ERR_COMM_FAILURE;
    case PMIX_ERR_NOT_IMPLEMENTED:
        return OPAL_ERR_NOT_IMPLEMENTED;
    case PMIX_ERR_NOT_SUPPORTED:
        return OPAL_ERR_NOT_SUPPORTED;
    case PMIX_ERR_NOT_FOUND:
        return OPAL_ERR_NOT_FOUND;
    case PMIX_ERR_SERVER_NOT_AVAIL:
        return OPAL_ERR_SERVER_NOT_AVAIL;

    case PMIX_ERR_INVALID_NAMESPACE:
    case PMIX_ERR_INVALID_SIZE:
    case PMIX_ERR_INVALID_KEYVALP:
    case PMIX_ERR_INVALID_NUM_PARSED:
    case PMIX_ERR_INVALID_ARGS:
    case PMIX_ERR_INVALID_NUM_ARGS:
    case PMIX_ERR_INVALID_LENGTH:
    case PMIX_ERR_INVALID_VAL_LENGTH:
    case PMIX_ERR_INVALID_VAL:
    case PMIX_ERR_INVALID_KEY_LENGTH:
    case PMIX_ERR_INVALID_KEY:
    case PMIX_ERR_INVALID_ARG:
        return OPAL_ERR_BAD_PARAM;
    case PMIX_ERR_NOMEM:
        return OPAL_ERR_OUT_OF_RESOURCE;
    case PMIX_ERR_INIT:
        return OPAL_ERROR;

    case PMIX_ERR_DATA_VALUE_NOT_FOUND:
        return OPAL_ERR_DATA_VALUE_NOT_FOUND;
    case PMIX_ERR_OUT_OF_RESOURCE:
        return OPAL_ERR_OUT_OF_RESOURCE;
    case PMIX_ERR_RESOURCE_BUSY:
        return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
    case PMIX_ERR_BAD_PARAM:
        return OPAL_ERR_BAD_PARAM;
    case PMIX_ERR_IN_ERRNO:
        return OPAL_ERR_IN_ERRNO;
    case PMIX_ERR_UNREACH:
        return OPAL_ERR_UNREACH;
    case PMIX_ERR_TIMEOUT:
        return OPAL_ERR_TIMEOUT;
    case PMIX_ERR_NO_PERMISSIONS:
        return OPAL_ERR_PERM;
    case PMIX_ERR_PACK_MISMATCH:
        return OPAL_ERR_PACK_MISMATCH;
    case PMIX_ERR_PACK_FAILURE:
        return OPAL_ERR_PACK_FAILURE;

    case PMIX_ERR_UNPACK_FAILURE:
        return OPAL_ERR_UNPACK_FAILURE;
    case PMIX_ERR_UNPACK_INADEQUATE_SPACE:
        return OPAL_ERR_UNPACK_INADEQUATE_SPACE;
    case PMIX_ERR_TYPE_MISMATCH:
        return OPAL_ERR_TYPE_MISMATCH;
    case PMIX_ERR_PROC_ENTRY_NOT_FOUND:
        return OPAL_ERR_PROC_ENTRY_NOT_FOUND;
    case PMIX_ERR_UNKNOWN_DATA_TYPE:
        return OPAL_ERR_UNKNOWN_DATA_TYPE;
    case PMIX_ERR_WOULD_BLOCK:
        return OPAL_ERR_WOULD_BLOCK;
    case PMIX_ERR_READY_FOR_HANDSHAKE:
    case PMIX_ERR_HANDSHAKE_FAILED:
    case PMIX_ERR_INVALID_CRED:
        return OPAL_ERR_COMM_FAILURE;
    case PMIX_EXISTS:
        return OPAL_EXISTS;

    case PMIX_ERR_SILENT:
        return OPAL_ERR_SILENT;
    case PMIX_ERROR:
        return OPAL_ERROR;
    case PMIX_SUCCESS:
        return OPAL_SUCCESS;
    default:
        return OPAL_ERROR;
    }
}

void pmix120_value_load(pmix_value_t *v,
                      opal_value_t *kv)
{
    switch(kv->type) {
        case OPAL_UNDEF:
            v->type = PMIX_UNDEF;
            opal_output(0, "TYPE WAS UNDEF");
            break;
        case OPAL_BOOL:
            v->type = PMIX_BOOL;
            memcpy(&(v->data.flag), &kv->data.flag, 1);
            break;
        case OPAL_BYTE:
            v->type = PMIX_BYTE;
            memcpy(&(v->data.byte), &kv->data.byte, 1);
            break;
        case OPAL_STRING:
            v->type = PMIX_STRING;
            if (NULL != kv->data.string) {
                v->data.string = strdup(kv->data.string);
            } else {
                v->data.string = NULL;
            }
            break;
        case OPAL_SIZE:
            v->type = PMIX_SIZE;
            v->data.size = (size_t)kv->data.size;
            break;
        case OPAL_PID:
            v->type = PMIX_PID;
            memcpy(&(v->data.pid), &kv->data.pid, sizeof(pid_t));
            break;
        case OPAL_INT:
            v->type = PMIX_INT;
            memcpy(&(v->data.integer), &kv->data.integer, sizeof(int));
            break;
        case OPAL_INT8:
            v->type = PMIX_INT8;
            memcpy(&(v->data.int8), &kv->data.int8, 1);
            break;
        case OPAL_INT16:
            v->type = PMIX_INT16;
            memcpy(&(v->data.int16), &kv->data.int16, 2);
            break;
        case OPAL_INT32:
            v->type = PMIX_INT32;
            memcpy(&(v->data.int32), &kv->data.int32, 4);
            break;
        case OPAL_INT64:
            v->type = PMIX_INT64;
            memcpy(&(v->data.int64), &kv->data.int64, 8);
            break;
        case OPAL_UINT:
            v->type = PMIX_UINT;
            memcpy(&(v->data.uint), &kv->data.uint, sizeof(int));
            break;
        case OPAL_UINT8:
            v->type = PMIX_UINT8;
            memcpy(&(v->data.uint8), &kv->data.uint8, 1);
            break;
        case OPAL_UINT16:
            v->type = PMIX_UINT16;
            memcpy(&(v->data.uint16), &kv->data.uint16, 2);
            break;
        case OPAL_UINT32:
            v->type = PMIX_UINT32;
            memcpy(&(v->data.uint32), &kv->data.uint32, 4);
            break;
        case OPAL_UINT64:
            v->type = PMIX_UINT64;
            memcpy(&(v->data.uint64), &kv->data.uint64, 8);
            break;
        case OPAL_FLOAT:
            v->type = PMIX_FLOAT;
            memcpy(&(v->data.fval), &kv->data.fval, sizeof(float));
            break;
        case OPAL_DOUBLE:
            v->type = PMIX_DOUBLE;
            memcpy(&(v->data.dval), &kv->data.dval, sizeof(double));
            break;
        case OPAL_TIMEVAL:
            v->type = PMIX_TIMEVAL;
            memcpy(&(v->data.tv), &kv->data.tv, sizeof(struct timeval));
            break;
        case OPAL_BYTE_OBJECT:
            v->type = PMIX_BYTE_OBJECT;
            if (NULL != kv->data.bo.bytes) {
                v->data.bo.bytes = (char*)malloc(kv->data.bo.size);
                memcpy(v->data.bo.bytes, kv->data.bo.bytes, kv->data.bo.size);
                v->data.bo.size = (size_t)kv->data.bo.size;
            } else {
                v->data.bo.bytes = NULL;
                v->data.bo.size = 0;
            }
            break;
        default:
            /* silence warnings */
            break;
    }
}

int pmix120_value_unload(opal_value_t *kv,
                       const pmix_value_t *v)
{
    int rc=OPAL_SUCCESS;


    switch(v->type) {
    case PMIX_UNDEF:
        rc = OPAL_ERR_UNKNOWN_DATA_TYPE;
        break;
    case PMIX_BOOL:
        kv->type = OPAL_BOOL;
        memcpy(&kv->data.flag, &(v->data.flag), 1);
        break;
    case PMIX_BYTE:
        kv->type = OPAL_BYTE;
        memcpy(&kv->data.byte, &(v->data.byte), 1);
        break;
    case PMIX_STRING:
        kv->type = OPAL_STRING;
        if (NULL != v->data.string) {
            kv->data.string = strdup(v->data.string);
        }
        break;
    case PMIX_SIZE:
        kv->type = OPAL_SIZE;
        kv->data.size = (int)v->data.size;
        break;
    case PMIX_PID:
        kv->type = OPAL_PID;
        memcpy(&kv->data.pid, &(v->data.pid), sizeof(pid_t));
        break;
    case PMIX_INT:
        kv->type = OPAL_INT;
        memcpy(&kv->data.integer, &(v->data.integer), sizeof(int));
        break;
    case PMIX_INT8:
        kv->type = OPAL_INT8;
        memcpy(&kv->data.int8, &(v->data.int8), 1);
        break;
    case PMIX_INT16:
        kv->type = OPAL_INT16;
        memcpy(&kv->data.int16, &(v->data.int16), 2);
        break;
    case PMIX_INT32:
        kv->type = OPAL_INT32;
        memcpy(&kv->data.int32, &(v->data.int32), 4);
        break;
    case PMIX_INT64:
        kv->type = OPAL_INT64;
        memcpy(&kv->data, &(v->data.int64), 8);
        break;
    case PMIX_UINT:
        kv->type = OPAL_UINT;
        memcpy(&kv->data, &(v->data.uint), sizeof(int));
        break;
    case PMIX_UINT8:
        kv->type = OPAL_UINT8;
        memcpy(&kv->data, &(v->data.uint8), 1);
        break;
    case PMIX_UINT16:
        kv->type = OPAL_UINT16;
        memcpy(&kv->data, &(v->data.uint16), 2);
        break;
    case PMIX_UINT32:
        kv->type = OPAL_UINT32;
        memcpy(&kv->data, &(v->data.uint32), 4);
        break;
    case PMIX_UINT64:
        kv->type = OPAL_UINT64;
        memcpy(&kv->data, &(v->data.uint64), 8);
        break;
    case PMIX_FLOAT:
        kv->type = OPAL_FLOAT;
        memcpy(&kv->data, &(v->data.fval), sizeof(float));
        break;
    case PMIX_DOUBLE:
        kv->type = OPAL_DOUBLE;
        memcpy(&kv->data, &(v->data.dval), sizeof(double));
        break;
    case PMIX_TIMEVAL:
        kv->type = OPAL_TIMEVAL;
        memcpy(&kv->data, &(v->data.tv), sizeof(struct timeval));
        break;
    case PMIX_BYTE_OBJECT:
        kv->type = OPAL_BYTE_OBJECT;
        if (NULL != v->data.bo.bytes && 0 < v->data.bo.size) {
            kv->data.bo.bytes = (uint8_t*)malloc(v->data.bo.size);
            memcpy(kv->data.bo.bytes, v->data.bo.bytes, v->data.bo.size);
            kv->data.bo.size = (int)v->data.bo.size;
        } else {
            kv->data.bo.bytes = NULL;
            kv->data.bo.size = 0;
        }
        break;
    default:
        /* silence warnings */
        rc = OPAL_ERROR;
        break;
    }
    return rc;
}


/****  INSTANTIATE INTERNAL CLASSES  ****/
OBJ_CLASS_INSTANCE(opal_pmix120_jobid_trkr_t,
                   opal_list_item_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(opal_pmix120_etracker_t,
                   opal_list_item_t,
                   NULL, NULL);

static void opcon(pmix120_opcaddy_t *p)
{
    memset(&p->p, 0, sizeof(pmix_proc_t));
    p->procs = NULL;
    p->nprocs = 0;
    p->error_procs = NULL;
    p->nerror_procs = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->apps = NULL;
    p->sz = 0;
    p->opcbfunc = NULL;
    p->mdxcbfunc = NULL;
    p->valcbfunc = NULL;
    p->lkcbfunc = NULL;
    p->spcbfunc = NULL;
    p->errhandler = NULL;
    p->reg_cbfunc = NULL;
    p->cbdata = NULL;
}
static void opdes(pmix120_opcaddy_t *p)
{
    if (NULL != p->procs) {
        PMIX_PROC_FREE(p->procs, p->nprocs);
    }
    if (NULL != p->error_procs) {
        PMIX_PROC_FREE(p->error_procs, p->nerror_procs);
    }
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->sz);
    }
    if (NULL != p->apps) {
        PMIX_APP_FREE(p->apps, p->sz);
    }
}
OBJ_CLASS_INSTANCE(pmix120_opcaddy_t,
                   opal_object_t,
                   opcon, opdes);

static void ocadcon(pmix120_opalcaddy_t *p)
{
    OBJ_CONSTRUCT(&p->procs, opal_list_t);
    OBJ_CONSTRUCT(&p->info, opal_list_t);
    OBJ_CONSTRUCT(&p->apps, opal_list_t);
    p->opcbfunc = NULL;
    p->dmdxfunc = NULL;
    p->mdxcbfunc = NULL;
    p->lkupcbfunc = NULL;
    p->spwncbfunc = NULL;
    p->cbdata = NULL;
    p->odmdxfunc = NULL;
    p->ocbdata = NULL;
}
static void ocaddes(pmix120_opalcaddy_t *p)
{
    OPAL_LIST_DESTRUCT(&p->procs);
    OPAL_LIST_DESTRUCT(&p->info);
    OPAL_LIST_DESTRUCT(&p->apps);
}
OBJ_CLASS_INSTANCE(pmix120_opalcaddy_t,
                   opal_object_t,
                   ocadcon, ocaddes);
