/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/constants.h"


#include <regex.h>

#include <time.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal_stdint.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include "opal/mca/pmix/base/base.h"


int opal_pmix_base_exchange(pmix_info_t *indat,
                            pmix_pdata_t *outdat,
                            int timeout)
{
    pmix_status_t rc;
    pmix_info_t info[2];
    pmix_persistence_t firstread = PMIX_PERSIST_FIRST_READ;

    /* publish the provided value - it defaults to
     * "session" range, but we will add a persistence
     * to delete it upon first read */
    PMIX_INFO_XFER(&info[0], indat);
    PMIX_INFO_LOAD(&info[1], PMIX_PERSISTENCE, &firstread, PMIX_PERSIST);
    /* publish it with "session" scope */
    rc = PMIx_Publish(info, 2);
    PMIX_INFO_DESTRUCT(&info[0]);
    PMIX_INFO_DESTRUCT(&info[1]);
    if (PMIX_SUCCESS != rc) {
        return opal_pmix_convert_status(rc);
    }

    /* lookup the other side's info */
    /* tell it to wait for the data to arrive */
    PMIX_INFO_LOAD(&info[0], PMIX_WAIT, NULL, PMIX_BOOL);
    /* pass along the given timeout as we don't know when
     * the other side will publish - it doesn't
     * have to be simultaneous */
    if (0 < timeout) {
        PMIX_INFO_LOAD(&info[1], PMIX_TIMEOUT, &timeout, PMIX_INT);
    } else {
        PMIX_INFO_LOAD(&info[1], PMIX_TIMEOUT, &opal_pmix_base.timeout, PMIX_INT);
    }
    rc = PMIx_Lookup(outdat, 1, info, 2);
    PMIX_INFO_DESTRUCT(&info[0]);
    PMIX_INFO_DESTRUCT(&info[1]);

    return opal_pmix_convert_status(rc);
}

pmix_status_t opal_pmix_convert_rc(int rc)
{
    switch (rc) {
    case OPAL_ERR_DEBUGGER_RELEASE:
        return PMIX_ERR_DEBUGGER_RELEASE;

    case OPAL_ERR_HANDLERS_COMPLETE:
        return PMIX_EVENT_ACTION_COMPLETE;

    case OPAL_ERR_PROC_ABORTED:
        return PMIX_ERR_PROC_ABORTED;

    case OPAL_ERR_PROC_REQUESTED_ABORT:
        return PMIX_ERR_PROC_REQUESTED_ABORT;

    case OPAL_ERR_PROC_ABORTING:
        return PMIX_ERR_PROC_ABORTING;

    case OPAL_ERR_NODE_DOWN:
        return PMIX_ERR_NODE_DOWN;

    case OPAL_ERR_NODE_OFFLINE:
        return PMIX_ERR_NODE_OFFLINE;

    case OPAL_ERR_JOB_TERMINATED:
        return PMIX_ERR_JOB_TERMINATED;

    case OPAL_ERR_PROC_RESTART:
        return PMIX_ERR_PROC_RESTART;

    case OPAL_ERR_PROC_CHECKPOINT:
        return PMIX_ERR_PROC_CHECKPOINT;

    case OPAL_ERR_PROC_MIGRATE:
        return PMIX_ERR_PROC_MIGRATE;

    case OPAL_ERR_EVENT_REGISTRATION:
        return PMIX_ERR_EVENT_REGISTRATION;

    case OPAL_ERR_NOT_IMPLEMENTED:
    case OPAL_ERR_NOT_SUPPORTED:
        return PMIX_ERR_NOT_SUPPORTED;

    case OPAL_ERR_NOT_FOUND:
        return PMIX_ERR_NOT_FOUND;

    case OPAL_ERR_PERM:
    case OPAL_ERR_UNREACH:
    case OPAL_ERR_SERVER_NOT_AVAIL:
        return PMIX_ERR_UNREACH;

    case OPAL_ERR_BAD_PARAM:
        return PMIX_ERR_BAD_PARAM;

    case OPAL_ERR_OUT_OF_RESOURCE:
        return PMIX_ERR_OUT_OF_RESOURCE;

    case OPAL_ERR_DATA_VALUE_NOT_FOUND:
        return PMIX_ERR_DATA_VALUE_NOT_FOUND;

    case OPAL_ERR_TIMEOUT:
        return PMIX_ERR_TIMEOUT;

    case OPAL_ERR_WOULD_BLOCK:
        return PMIX_ERR_WOULD_BLOCK;

    case OPAL_EXISTS:
        return PMIX_EXISTS;

    case OPAL_ERR_PARTIAL_SUCCESS:
        return PMIX_QUERY_PARTIAL_SUCCESS;

    case OPAL_ERR_MODEL_DECLARED:
        return PMIX_MODEL_DECLARED;

    case OPAL_ERROR:
        return PMIX_ERROR;
    case OPAL_SUCCESS:
        return PMIX_SUCCESS;
    default:
        return rc;
    }
}

int opal_pmix_convert_status(pmix_status_t status)
{
    switch (status) {
    case PMIX_ERR_DEBUGGER_RELEASE:
        return OPAL_ERR_DEBUGGER_RELEASE;

    case PMIX_EVENT_ACTION_COMPLETE:
        return OPAL_ERR_HANDLERS_COMPLETE;

    case PMIX_ERR_PROC_ABORTED:
        return OPAL_ERR_PROC_ABORTED;

    case PMIX_ERR_PROC_REQUESTED_ABORT:
        return OPAL_ERR_PROC_REQUESTED_ABORT;

    case PMIX_ERR_PROC_ABORTING:
        return OPAL_ERR_PROC_ABORTING;

    case PMIX_ERR_NODE_DOWN:
        return OPAL_ERR_NODE_DOWN;

    case PMIX_ERR_NODE_OFFLINE:
        return OPAL_ERR_NODE_OFFLINE;

    case PMIX_ERR_JOB_TERMINATED:
        return OPAL_ERR_JOB_TERMINATED;

    case PMIX_ERR_PROC_RESTART:
        return OPAL_ERR_PROC_RESTART;

    case PMIX_ERR_PROC_CHECKPOINT:
        return OPAL_ERR_PROC_CHECKPOINT;

    case PMIX_ERR_PROC_MIGRATE:
        return OPAL_ERR_PROC_MIGRATE;

    case PMIX_ERR_EVENT_REGISTRATION:
        return OPAL_ERR_EVENT_REGISTRATION;

    case PMIX_ERR_NOT_SUPPORTED:
        return OPAL_ERR_NOT_SUPPORTED;

    case PMIX_ERR_NOT_FOUND:
        return OPAL_ERR_NOT_FOUND;

    case PMIX_ERR_OUT_OF_RESOURCE:
        return OPAL_ERR_OUT_OF_RESOURCE;

    case PMIX_ERR_INIT:
        return OPAL_ERROR;

    case PMIX_ERR_BAD_PARAM:
        return OPAL_ERR_BAD_PARAM;

    case PMIX_ERR_UNREACH:
    case PMIX_ERR_NO_PERMISSIONS:
        return OPAL_ERR_UNREACH;

    case PMIX_ERR_TIMEOUT:
        return OPAL_ERR_TIMEOUT;

    case PMIX_ERR_WOULD_BLOCK:
        return OPAL_ERR_WOULD_BLOCK;

    case PMIX_ERR_LOST_CONNECTION_TO_SERVER:
    case PMIX_ERR_LOST_PEER_CONNECTION:
    case PMIX_ERR_LOST_CONNECTION_TO_CLIENT:
        return OPAL_ERR_COMM_FAILURE;

    case PMIX_EXISTS:
        return OPAL_EXISTS;

    case PMIX_QUERY_PARTIAL_SUCCESS:
        return OPAL_ERR_PARTIAL_SUCCESS;

    case PMIX_MONITOR_HEARTBEAT_ALERT:
        return OPAL_ERR_HEARTBEAT_ALERT;

    case PMIX_MONITOR_FILE_ALERT:
        return OPAL_ERR_FILE_ALERT;

    case PMIX_MODEL_DECLARED:
        return OPAL_ERR_MODEL_DECLARED;

    case PMIX_ERROR:
        return OPAL_ERROR;
    case PMIX_SUCCESS:
        return OPAL_SUCCESS;
    default:
        return status;
    }
}

pmix_proc_state_t opal_pmix_convert_state(int state)
{
    switch(state) {
        case 0:
            return PMIX_PROC_STATE_UNDEF;
        case 1:
            return PMIX_PROC_STATE_LAUNCH_UNDERWAY;
        case 2:
            return PMIX_PROC_STATE_RESTART;
        case 3:
            return PMIX_PROC_STATE_TERMINATE;
        case 4:
            return PMIX_PROC_STATE_RUNNING;
        case 5:
            return PMIX_PROC_STATE_CONNECTED;
        case 51:
            return PMIX_PROC_STATE_KILLED_BY_CMD;
        case 52:
            return PMIX_PROC_STATE_ABORTED;
        case 53:
            return PMIX_PROC_STATE_FAILED_TO_START;
        case 54:
            return PMIX_PROC_STATE_ABORTED_BY_SIG;
        case 55:
            return PMIX_PROC_STATE_TERM_WO_SYNC;
        case 56:
            return PMIX_PROC_STATE_COMM_FAILED;
        case 58:
            return PMIX_PROC_STATE_CALLED_ABORT;
        case 59:
            return PMIX_PROC_STATE_MIGRATING;
        case 61:
            return PMIX_PROC_STATE_CANNOT_RESTART;
        case 62:
            return PMIX_PROC_STATE_TERM_NON_ZERO;
        case 63:
            return PMIX_PROC_STATE_FAILED_TO_LAUNCH;
        default:
            return PMIX_PROC_STATE_UNDEF;
    }
}

int opal_pmix_convert_pstate(pmix_proc_state_t state)
{
    switch(state) {
        case PMIX_PROC_STATE_UNDEF:
            return 0;
        case PMIX_PROC_STATE_PREPPED:
        case PMIX_PROC_STATE_LAUNCH_UNDERWAY:
            return 1;
        case PMIX_PROC_STATE_RESTART:
            return 2;
        case PMIX_PROC_STATE_TERMINATE:
            return 3;
        case PMIX_PROC_STATE_RUNNING:
            return 4;
        case PMIX_PROC_STATE_CONNECTED:
            return 5;
        case PMIX_PROC_STATE_UNTERMINATED:
            return 15;
        case PMIX_PROC_STATE_TERMINATED:
            return 20;
        case PMIX_PROC_STATE_KILLED_BY_CMD:
            return 51;
        case PMIX_PROC_STATE_ABORTED:
            return 52;
        case PMIX_PROC_STATE_FAILED_TO_START:
            return 53;
        case PMIX_PROC_STATE_ABORTED_BY_SIG:
            return 54;
        case PMIX_PROC_STATE_TERM_WO_SYNC:
            return 55;
        case PMIX_PROC_STATE_COMM_FAILED:
            return 56;
        case PMIX_PROC_STATE_CALLED_ABORT:
            return 58;
        case PMIX_PROC_STATE_MIGRATING:
            return 60;
        case PMIX_PROC_STATE_CANNOT_RESTART:
            return 61;
        case PMIX_PROC_STATE_TERM_NON_ZERO:
            return 62;
        case PMIX_PROC_STATE_FAILED_TO_LAUNCH:
            return 63;
        default:
            return 0;  // undef
    }
}

void opal_pmix_value_load(pmix_value_t *v,
                          opal_value_t *kv)
{
    opal_list_t *list;
    opal_value_t *val;
    pmix_info_t *info;
    size_t n;

    switch(kv->type) {
        case OPAL_UNDEF:
            v->type = PMIX_UNDEF;
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
            memcpy(&(v->data.size), &kv->data.size, sizeof(size_t));
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
        case OPAL_TIME:
            v->type = PMIX_TIME;
            memcpy(&(v->data.time), &kv->data.time, sizeof(time_t));
            break;
        case OPAL_STATUS:
            v->type = PMIX_STATUS;
            v->data.status = opal_pmix_convert_rc(kv->data.status);
            break;
        case OPAL_JOBID:
            v->type = PMIX_PROC;
            /* have to stringify the jobid */
            PMIX_PROC_CREATE(v->data.proc, 1);
            OPAL_PMIX_CONVERT_JOBID(v->data.proc->nspace, kv->data.name.jobid);
            /* leave the rank as invalid */
            break;
        case OPAL_VPID:
            v->type = PMIX_PROC_RANK;
            OPAL_PMIX_CONVERT_VPID(v->data.rank, kv->data.name.vpid);
            break;
        case OPAL_NAME:
            v->type = PMIX_PROC;
            /* have to stringify the jobid */
            PMIX_PROC_CREATE(v->data.proc, 1);
            OPAL_PMIX_CONVERT_JOBID(v->data.proc->nspace, kv->data.name.jobid);
            OPAL_PMIX_CONVERT_VPID(v->data.proc->rank, kv->data.name.vpid);
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
        case OPAL_PERSIST:
            v->type = PMIX_PERSIST;
            v->data.persist = (pmix_persistence_t)kv->data.uint8;
            break;
        case OPAL_SCOPE:
            v->type = PMIX_SCOPE;
            v->data.scope = (pmix_scope_t)kv->data.uint8;
            break;
        case OPAL_DATA_RANGE:
            v->type = PMIX_DATA_RANGE;
            v->data.range = (pmix_data_range_t)kv->data.uint8;
            break;
        case OPAL_PROC_STATE:
            v->type = PMIX_PROC_STATE;
            /* the OPAL layer doesn't have any concept of proc state,
             * so the ORTE layer is responsible for converting it */
            memcpy(&v->data.state, &kv->data.uint8, sizeof(uint8_t));
            break;
        case OPAL_PTR:
            v->type = PMIX_POINTER;
            v->data.ptr = kv->data.ptr;
            break;
         case OPAL_LIST:
            list = (opal_list_t*)kv->data.ptr;
            v->type = PMIX_DATA_ARRAY;
            v->data.darray = (pmix_data_array_t*)malloc(sizeof(pmix_data_array_t));
            v->data.darray->type = PMIX_INFO;
            v->data.darray->size = (NULL == list)?0:opal_list_get_size(list);
            if (0 < v->data.darray->size) {
                PMIX_INFO_CREATE(info, v->data.darray->size);
                v->data.darray->array = info;
                n=0;
                OPAL_LIST_FOREACH(val, list, opal_value_t) {
                    if (NULL != val->key) {
                        (void)strncpy(info[n].key, val->key, PMIX_MAX_KEYLEN);
                    }
                    opal_pmix_value_load(&info[n].value, val);
                    ++n;
                }
            } else {
                v->data.darray->array = NULL;
            }
            break;
        case OPAL_PROC_INFO:
            v->type = PMIX_PROC_INFO;
            PMIX_PROC_INFO_CREATE(v->data.pinfo, 1);
            OPAL_PMIX_CONVERT_JOBID(v->data.pinfo->proc.nspace, kv->data.pinfo.name.jobid);
            OPAL_PMIX_CONVERT_VPID(v->data.pinfo->proc.rank, kv->data.pinfo.name.vpid);
            if (NULL != kv->data.pinfo.hostname) {
                v->data.pinfo->hostname = strdup(kv->data.pinfo.hostname);
            }
            if (NULL != kv->data.pinfo.executable_name) {
                v->data.pinfo->executable_name = strdup(kv->data.pinfo.executable_name);
            }
            v->data.pinfo->pid = kv->data.pinfo.pid;
            v->data.pinfo->exit_code = kv->data.pinfo.exit_code;
            v->data.pinfo->state = opal_pmix_convert_state(kv->data.pinfo.state);
            break;
        case OPAL_ENVAR:
            v->type = PMIX_ENVAR;
            PMIX_ENVAR_CONSTRUCT(&v->data.envar);
            if (NULL != kv->data.envar.envar) {
                v->data.envar.envar = strdup(kv->data.envar.envar);
            }
            if (NULL != kv->data.envar.value) {
                v->data.envar.value = strdup(kv->data.envar.value);
            }
            v->data.envar.separator = kv->data.envar.separator;
            break;
        default:
            /* silence warnings */
            break;
    }
}

int opal_pmix_value_unload(opal_value_t *kv,
                           const pmix_value_t *v)
{
    int rc=OPAL_SUCCESS;
    opal_list_t *lt;
    opal_value_t *ival;
    size_t n;

    switch(v->type) {
    case PMIX_UNDEF:
        kv->type = OPAL_UNDEF;
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
        memcpy(&kv->data.size, &(v->data.size), sizeof(size_t));
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
        memcpy(&kv->data.int64, &(v->data.int64), 8);
        break;
    case PMIX_UINT:
        kv->type = OPAL_UINT;
        memcpy(&kv->data.uint, &(v->data.uint), sizeof(int));
        break;
    case PMIX_UINT8:
        kv->type = OPAL_UINT8;
        memcpy(&kv->data.uint8, &(v->data.uint8), 1);
        break;
    case PMIX_UINT16:
        kv->type = OPAL_UINT16;
        memcpy(&kv->data.uint16, &(v->data.uint16), 2);
        break;
    case PMIX_UINT32:
        kv->type = OPAL_UINT32;
        memcpy(&kv->data.uint32, &(v->data.uint32), 4);
        break;
    case PMIX_UINT64:
        kv->type = OPAL_UINT64;
        memcpy(&kv->data.uint64, &(v->data.uint64), 8);
        break;
    case PMIX_FLOAT:
        kv->type = OPAL_FLOAT;
        memcpy(&kv->data.fval, &(v->data.fval), sizeof(float));
        break;
    case PMIX_DOUBLE:
        kv->type = OPAL_DOUBLE;
        memcpy(&kv->data.dval, &(v->data.dval), sizeof(double));
        break;
    case PMIX_TIMEVAL:
        kv->type = OPAL_TIMEVAL;
        memcpy(&kv->data.tv, &(v->data.tv), sizeof(struct timeval));
        break;
    case PMIX_TIME:
        kv->type = OPAL_TIME;
        memcpy(&kv->data.time, &(v->data.time), sizeof(time_t));
        break;
    case PMIX_STATUS:
        kv->type = OPAL_STATUS;
        kv->data.status = opal_pmix_convert_status(v->data.status);
        break;
    case PMIX_PROC_RANK:
        kv->type = OPAL_VPID;
        OPAL_PMIX_CONVERT_RANK(kv->data.name.vpid, v->data.rank);
        break;
    case PMIX_PROC:
        kv->type = OPAL_NAME;
        OPAL_PMIX_CONVERT_NSPACE(rc, &kv->data.name.jobid, v->data.proc->nspace);
        OPAL_PMIX_CONVERT_RANK(kv->data.name.vpid, v->data.proc->rank);
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
    case PMIX_PERSIST:
        kv->type = OPAL_PERSIST;
        kv->data.uint8 = v->data.persist;
        break;
    case PMIX_SCOPE:
        kv->type = OPAL_SCOPE;
        kv->data.uint8 = v->data.scope;
        break;
    case PMIX_DATA_RANGE:
        kv->type = OPAL_DATA_RANGE;
        kv->data.uint8 = v->data.range;
        break;
    case PMIX_PROC_STATE:
        kv->type = OPAL_PROC_STATE;
        kv->data.integer = opal_pmix_convert_pstate(v->data.state);
        break;
    case PMIX_POINTER:
        kv->type = OPAL_PTR;
        kv->data.ptr = v->data.ptr;
        break;
    case PMIX_DATA_ARRAY:
        if (NULL == v->data.darray || NULL == v->data.darray->array) {
            kv->data.ptr = NULL;
            break;
        }
        lt = OBJ_NEW(opal_list_t);
        kv->type = OPAL_PTR;
        kv->data.ptr = (void*)lt;
        for (n=0; n < v->data.darray->size; n++) {
            ival = OBJ_NEW(opal_value_t);
            opal_list_append(lt, &ival->super);
            /* handle the various types */
            if (PMIX_INFO == v->data.darray->type) {
                pmix_info_t *iptr = (pmix_info_t*)v->data.darray->array;
                if (0 < strlen(iptr[n].key)) {
                    ival->key = strdup(iptr[n].key);
                }
                rc = opal_pmix_value_unload(ival, &iptr[n].value);
                if (OPAL_SUCCESS != rc) {
                    OPAL_LIST_RELEASE(lt);
                    kv->type = OPAL_UNDEF;
                    kv->data.ptr = NULL;
                    break;
                }
            }
        }
        break;
    case PMIX_PROC_INFO:
        kv->type = OPAL_PROC_INFO;
        if (NULL == v->data.pinfo) {
            rc = OPAL_ERR_BAD_PARAM;
            break;
        }
        OPAL_PMIX_CONVERT_NSPACE(rc, &kv->data.pinfo.name.jobid, v->data.pinfo->proc.nspace);
        OPAL_PMIX_CONVERT_RANK(kv->data.pinfo.name.vpid, v->data.pinfo->proc.rank);
        if (NULL != v->data.pinfo->hostname) {
            kv->data.pinfo.hostname = strdup(v->data.pinfo->hostname);
        }
        if (NULL != v->data.pinfo->executable_name) {
            kv->data.pinfo.executable_name = strdup(v->data.pinfo->executable_name);
        }
        kv->data.pinfo.pid = v->data.pinfo->pid;
        kv->data.pinfo.exit_code = v->data.pinfo->exit_code;
        kv->data.pinfo.state = opal_pmix_convert_pstate(v->data.pinfo->state);
        break;
    case PMIX_ENVAR:
        kv->type = OPAL_ENVAR;
        OBJ_CONSTRUCT(&kv->data.envar, opal_envar_t);
        if (NULL != v->data.envar.envar) {
            kv->data.envar.envar = strdup(v->data.envar.envar);
        }
        if (NULL != v->data.envar.value) {
            kv->data.envar.value = strdup(v->data.envar.value);
        }
        kv->data.envar.separator = v->data.envar.separator;
        break;
    default:
        /* silence warnings */
        rc = OPAL_ERROR;
        break;
    }
    return rc;
}

static void cleanup_cbfunc(pmix_status_t status,
                           pmix_info_t *info, size_t ninfo,
                           void *cbdata,
                           pmix_release_cbfunc_t release_fn,
                           void *release_cbdata)
{
    opal_pmix_lock_t *lk = (opal_pmix_lock_t*)cbdata;

    OPAL_POST_OBJECT(lk);

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    lk->status = status;
    OPAL_PMIX_WAKEUP_THREAD(lk);
}

int opal_pmix_register_cleanup(char *path, bool directory, bool ignore, bool jobscope)
{
    opal_pmix_lock_t lk;
    pmix_info_t pinfo[3];
    size_t n, ninfo=0;
    pmix_status_t rc, ret;
    pmix_proc_t proc;

    OPAL_PMIX_CONSTRUCT_LOCK(&lk);

    if (ignore) {
        /* they want this path ignored */
        PMIX_INFO_LOAD(&pinfo[ninfo], PMIX_CLEANUP_IGNORE, path, PMIX_STRING);
        ++ninfo;
    } else {
        if (directory) {
            PMIX_INFO_LOAD(&pinfo[ninfo], PMIX_REGISTER_CLEANUP_DIR, path, PMIX_STRING);
            ++ninfo;
            /* recursively cleanup directories */
            PMIX_INFO_LOAD(&pinfo[ninfo], PMIX_CLEANUP_RECURSIVE, NULL, PMIX_BOOL);
            ++ninfo;
        } else {
            /* order cleanup of the provided path */
            PMIX_INFO_LOAD(&pinfo[ninfo], PMIX_REGISTER_CLEANUP, path, PMIX_STRING);
            ++ninfo;
        }
    }

    /* if they want this applied to the job, then indicate so */
    if (jobscope) {
        rc = PMIx_Job_control_nb(NULL, 0, pinfo, ninfo, cleanup_cbfunc, (void*)&lk);
    } else {
        /* only applies to us */
        (void)snprintf(proc.nspace, PMIX_MAX_NSLEN, "%s",
                       OPAL_JOBID_PRINT(OPAL_PROC_MY_NAME.jobid));
        proc.rank = OPAL_PROC_MY_NAME.vpid;
        rc = PMIx_Job_control_nb(&proc, 1, pinfo, ninfo, cleanup_cbfunc, (void*)&lk);
    }
    if (PMIX_SUCCESS != rc) {
        ret = rc;
    } else {
#if PMIX_VERSION_MAJOR == 3 && PMIX_VERSION_MINOR == 0 && PMIX_VERSION_RELEASE < 3
        /* There is a bug in PMIx 3.0.0 up to 3.0.2 that causes the callback never
         * being called, so assumes the everything went well and avoid a deadlock. */
        cleanup_cbfunc(PMIX_SUCCESS, NULL, 0, (void *)&lk, NULL, NULL);
#endif
        OPAL_PMIX_WAIT_THREAD(&lk);
        ret = lk.status;
    }
    OPAL_PMIX_DESTRUCT_LOCK(&lk);
    for (n=0; n < ninfo; n++) {
        PMIX_INFO_DESTRUCT(&pinfo[n]);
    }
    return ret;
}


/* CLASS INSTANTIATIONS */
static void dsicon(opal_ds_info_t *p)
{
    PMIX_PROC_CONSTRUCT(&p->source);
    p->info = NULL;
    p->persistence = PMIX_PERSIST_INVALID;
}
OBJ_CLASS_INSTANCE(opal_ds_info_t,
                   opal_list_item_t,
                   dsicon, NULL);

static void infoitmcon(opal_info_item_t *p)
{
    PMIX_INFO_CONSTRUCT(&p->info);
}
static void infoitdecon(opal_info_item_t *p)
{
    PMIX_INFO_DESTRUCT(&p->info);
}
OBJ_CLASS_INSTANCE(opal_info_item_t,
                   opal_list_item_t,
                   infoitmcon, infoitdecon);
