/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

#include "opal/threads/threads.h"
#include "opal/util/proc.h"

#include "opal/pmix/pmix-internal.h"

#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"

static opal_pmix_lock_t opal_pmix_lock = {0};

static int opal_pmix_base_initialized = 0;
 
bool opal_pmix_collect_all_data = true;

bool opal_pmix_base_async_modex = false;

int opal_pmix_base_timeout = -1;

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

static pmix_rank_t opal_pmix_convert_opalrank(opal_vpid_t vpid)
{
    switch(vpid) {
    case OPAL_VPID_WILDCARD:
        return PMIX_RANK_WILDCARD;
    case OPAL_VPID_INVALID:
        return PMIX_RANK_UNDEF;
    default:
        return (pmix_rank_t)vpid;
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
#if PMIX_NUMERIC_VERSION >= 0x00030000
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
#endif
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
#if PMIX_NUMERIC_VERSION >= 0x00030000
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
#endif
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

#if PMIX_NUMERIC_VERSION < 0x00030000
    return OPAL_ERR_NOT_SUPPORTED;
#else
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
#endif

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
#if PMIX_NUMERIC_VERSION < 0x00030000
    p->persistence = PMIX_PERSIST_INDEF;
#else
    p->persistence = PMIX_PERSIST_INVALID;
#endif
}
OBJ_CLASS_INSTANCE(opal_ds_info_t,
                   opal_list_item_t,
                   dsicon, NULL);

static void lkcon(opal_pmix_pdata_t *p)
{
    p->proc.jobid = OPAL_JOBID_INVALID;
    p->proc.vpid = OPAL_VPID_INVALID;
    OBJ_CONSTRUCT(&p->value, opal_value_t);
}
static void lkdes(opal_pmix_pdata_t *p)
{
    OBJ_DESTRUCT(&p->value);
}
OBJ_CLASS_INSTANCE(opal_pmix_pdata_t,
                   opal_list_item_t,
                   lkcon, lkdes);

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

static void apcon(opal_pmix_app_t *p)
{
    p->cmd = NULL;
    p->argv = NULL;
    p->env = NULL;
    p->cwd = NULL;
    p->maxprocs = 0;
    OBJ_CONSTRUCT(&p->info, opal_list_t);
}
static void apdes(opal_pmix_app_t *p)
{
    if (NULL != p->cmd) {
        free(p->cmd);
    }
    if (NULL != p->argv) {
        opal_argv_free(p->argv);
    }
    if (NULL != p->env) {
        opal_argv_free(p->env);
    }
    if (NULL != p->cwd) {
        free(p->cwd);
    }
    OPAL_LIST_DESTRUCT(&p->info);
}
OBJ_CLASS_INSTANCE(opal_pmix_app_t,
                   opal_list_item_t,
                   apcon, apdes);

int opal_pmix_fence(opal_list_t *procs, int collect_data) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
    return rc;
}

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    pmix_status_t status;
    char *nspace;
    pmix_proc_t p;
    pmix_proc_t *procs;
    size_t nprocs;
    pmix_pdata_t *pdata;
    size_t npdata;
    pmix_proc_t *error_procs;
    size_t nerror_procs;
    pmix_info_t *info;
    size_t ninfo;
    pmix_app_t *apps;
    size_t sz;
    opal_pmix_lock_t lock;
    opal_list_t *codes;
    pmix_status_t *pcodes;
    size_t ncodes;
    pmix_query_t *queries;
    size_t nqueries;
#if 0
    opal_pmix4x_event_t *event;
#endif
    opal_pmix_op_cbfunc_t opcbfunc;
#if 0
    opal_pmix_modex_cbfunc_t mdxcbfunc;
    opal_pmix_value_cbfunc_t valcbfunc;
    opal_pmix_lookup_cbfunc_t lkcbfunc;
    opal_pmix_spawn_cbfunc_t spcbfunc;
    opal_pmix_evhandler_reg_cbfunc_t evregcbfunc;
    opal_pmix_info_cbfunc_t qcbfunc;
    opal_pmix_setup_application_cbfunc_t setupcbfunc;
#endif
    void *cbdata;
} opal_pmix_opcaddy_t;

// static OBJ_CLASS_DECLARATION(opal_pmix_opcaddy_t);

static void opcon(opal_pmix_opcaddy_t *p)
{
    memset(&p->p, 0, sizeof(pmix_proc_t));
    p->nspace = NULL;
    p->procs = NULL;
    p->nprocs = 0;
    p->pdata = NULL;
    p->npdata = 0;
    p->error_procs = NULL;
    p->nerror_procs = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->apps = NULL;
    p->sz = 0;
    OPAL_PMIX_CONSTRUCT_LOCK(&p->lock);
    p->codes = NULL;
    p->pcodes = NULL;
    p->ncodes = 0;
    p->queries = NULL;
    p->nqueries = 0;
#if 0
    p->event = NULL;
#endif
    p->opcbfunc = NULL;
#if 0
    p->mdxcbfunc = NULL;
    p->valcbfunc = NULL;
    p->lkcbfunc = NULL;
    p->spcbfunc = NULL;
    p->evregcbfunc = NULL;
    p->qcbfunc = NULL;
    p->cbdata = NULL;
#endif
}
static void opdes(opal_pmix_opcaddy_t *p)
{
    OPAL_PMIX_DESTRUCT_LOCK(&p->lock);
    if (NULL != p->nspace) {
        free(p->nspace);
    }
    if (NULL != p->procs) {
        PMIX_PROC_FREE(p->procs, p->nprocs);
    }
    if (NULL != p->pdata) {
        PMIX_PDATA_FREE(p->pdata, p->npdata);
    }
    if (NULL != p->error_procs) {
        PMIX_PROC_FREE(p->error_procs, p->nerror_procs);
    }
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
    if (NULL != p->apps) {
        PMIX_APP_FREE(p->apps, p->sz);
    }
    if (NULL != p->pcodes) {
        free(p->pcodes);
    }
    if (NULL != p->queries) {
        PMIX_QUERY_FREE(p->queries, p->nqueries);
    }
}
static OBJ_CLASS_INSTANCE(opal_pmix_opcaddy_t,
                          opal_object_t,
                          opcon, opdes);

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    opal_pmix_opcaddy_t *op = (opal_pmix_opcaddy_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(op);

    if (NULL != op->opcbfunc) {
        op->opcbfunc(opal_pmix_convert_status(status), op->cbdata);
    }
    OBJ_RELEASE(op);
}

int opal_pmix_fence_nb(opal_list_t *procs, int collect_data,
                       opal_pmix_op_cbfunc_t cbfunc, void *cbdata) {
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    opal_pmix_opcaddy_t *op;
#if 0
    char *nsptr;
#endif

    opal_output_verbose(1, opal_pmix_verbose_output,
                        "PMIx_client fencenb");

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_lock);
    if (0 >= opal_pmix_base_initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
#if 0
            if (NULL == (nsptr = pmix4x_convert_jobid(ptr->name.jobid))) {
                PMIX_PROC_FREE(parray, cnt);
                OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
                return OPAL_ERR_NOT_FOUND;
            }
            (void)strncpy(parray[n].nspace, nsptr, PMIX_MAX_NSLEN);
#else
            OPAL_PMIX_CONVERT_JOBID(parray[n].nspace, ptr->name.jobid);
#endif
            parray[n].rank = opal_pmix_convert_opalrank(ptr->name.vpid);
            ++n;
        }
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);

    /* create the caddy */
    op = OBJ_NEW(opal_pmix_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->procs = parray;
    op->nprocs = cnt;

    if (collect_data) {
        op->ninfo = 1;
        PMIX_INFO_CREATE(op->info, op->ninfo);
        PMIX_INFO_LOAD(&op->info[0], PMIX_COLLECT_DATA, NULL, PMIX_BOOL);
    }

    /* call the library function */
    rc = PMIx_Fence_nb(op->procs, op->nprocs, op->info, op->ninfo, opcbfunc, op);
    return opal_pmix_convert_status(rc);
}

int opal_pmix_lookup(opal_list_t *data, opal_list_t *info) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
    return rc;
}

int opal_pmix_publish(opal_list_t *info) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
    return rc;
}

int opal_pmix_unpublish(char **keys, opal_list_t *info) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
    return rc;
}

int opal_pmix_initialized(void) {
    int init;

    opal_output_verbose(1, opal_pmix_verbose_output,
                        "PMIx_client initialized");

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_lock);
    init = opal_pmix_base_initialized;
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);

    return init;
}

static pmix_proc_t myproc;

static opal_vpid_t pmix_convert_rank(pmix_rank_t rank)
{
    switch(rank) {
    case PMIX_RANK_UNDEF:
        return OPAL_VPID_INVALID;
    case PMIX_RANK_WILDCARD:
        return OPAL_VPID_WILDCARD;
    default:
        return (opal_vpid_t)rank;
    }
}

static char *dbgvalue=NULL;

static opal_jobid_t myjobid = 0;

static void evhandler(size_t evhdlr_registration_id,
                      pmix_status_t status,
                      const pmix_proc_t *source,
                      pmix_info_t info[], size_t ninfo,
                      pmix_info_t *results, size_t nresults,
                      pmix_event_notification_cbfunc_fn_t cbfunc,
                      void *cbdata)
{
    opal_pmix_lock_t *lock = NULL;
    int jobstatus=0, rc;
    opal_jobid_t jobid = OPAL_JOBID_INVALID;
    size_t n;
    char *msg = NULL;

    if (true) {
        opal_output(0, "MPI: EVHANDLER WITH STATUS %s(%d)", PMIx_Error_string(status), status);
        fprintf(stderr, "MPI: EVHANDLER WITH STATUS %s(%d)\n", PMIx_Error_string(status), status);
    }

    /* we should always have info returned to us - if not, there is
     * nothing we can do */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_JOB_TERM_STATUS, PMIX_MAX_KEYLEN)) {
                jobstatus = opal_pmix_convert_status(info[n].value.data.status);
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_AFFECTED_PROC, PMIX_MAX_KEYLEN)) {
                OPAL_PMIX_CONVERT_NSPACE(rc, &jobid, info[n].value.data.proc->nspace);
                if (OPAL_SUCCESS != rc) {
                    OPAL_ERROR_LOG(rc);
                }
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                lock = (opal_pmix_lock_t*)info[n].value.data.ptr;
        #ifdef PMIX_EVENT_TEXT_MESSAGE
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_TEXT_MESSAGE, PMIX_MAX_KEYLEN)) {
                msg = info[n].value.data.string;
        #endif
            }
        }
        if (true && (myjobid != OPAL_JOBID_INVALID && jobid == myjobid)) {
            opal_output(0, "JOB %s COMPLETED WITH STATUS %d",
                        OPAL_JOBID_PRINT(jobid), jobstatus);
            fprintf(stderr, "JOB %s COMPLETED WITH STATUS %d\n",
                            OPAL_JOBID_PRINT(jobid), jobstatus);
        }
    }
    /* save the status */
    lock->status = jobstatus;
    if (NULL != msg) {
        lock->msg = strdup(msg);
    }
    /* release the lock */
    OPAL_PMIX_WAKEUP_THREAD(lock);

    /* we _always_ have to execute the evhandler callback or
     * else the event progress engine will hang */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

static void errreg_cbfunc (pmix_status_t status,
                           size_t errhandler_ref,
                           void *cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t *)cbdata;
    opal_output_verbose(5, opal_pmix_verbose_output,
                        "PMIX client errreg_cbfunc - error handler registered status=%d, reference=%lu",
                        status, (unsigned long)errhandler_ref);
    fprintf(stderr, "PMIX client errreg_cbfunc - error handler registered status=%d, reference=%lu\n",
                    status, (unsigned long)errhandler_ref);
    OPAL_PMIX_WAKEUP_THREAD(lock);
}

int opal_pmix_init(opal_list_t *ilist) {
    opal_process_name_t pname;
    pmix_status_t rc;
    int dbg;
#if 0
    opal_pmix4x_jobid_trkr_t *job;
    opal_pmix4x_event_t *event;
#endif
    pmix_info_t *pinfo;
    size_t ninfo, n;
    opal_value_t *ival;

    opal_output_verbose(1, opal_pmix_verbose_output,
                        "PMIx_client init");

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_lock);

    if (0 == opal_pmix_base_initialized) {
        if (0 < (dbg = opal_output_get_verbosity(opal_pmix_verbose_output))) {
            asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
            putenv(dbgvalue);
        }
    }

    /* convert the incoming list to info structs */
    if (NULL != ilist && 0 < (ninfo = opal_list_get_size(ilist))) {
        PMIX_INFO_CREATE(pinfo, ninfo);
        n=0;
        OPAL_LIST_FOREACH(ival, ilist, opal_value_t) {
            (void)strncpy(pinfo[n].key, ival->key, PMIX_MAX_KEYLEN);
            opal_pmix_value_load(&pinfo[n].value, ival);
            ++n;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    /* check for direct modex use-case */
    if (opal_pmix_base_async_modex && !opal_pmix_collect_all_data) {
        opal_setenv("PMIX_MCA_gds", "hash", true, &environ);
    }

    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);
    rc = PMIx_Init(&myproc, pinfo, ninfo);
printf ("PMIx_Init returned %d\n", rc);
printf ("nspace = %s\n", myproc.nspace);
    if (NULL != pinfo) {
        PMIX_INFO_FREE(pinfo, ninfo);
    }
    if (PMIX_SUCCESS != rc) {
        dbg = opal_pmix_convert_status(rc);
        OPAL_ERROR_LOG(dbg);
        return dbg;
    }
    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_lock);

    ++opal_pmix_base_initialized;
    if (1 < opal_pmix_base_initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);
        return OPAL_SUCCESS;
    }

    /* store our jobid and rank */
    opal_convert_string_to_jobid(&pname.jobid, myproc.nspace);

#if 0
    /* insert this into our list of jobids - it will be the
     * first, and so we'll check it first */
    job = OBJ_NEW(opal_pmix4x_jobid_trkr_t);
    (void)strncpy(job->nspace, mca_pmix_pmix4x_component.myproc.nspace, PMIX_MAX_NSLEN);
    job->jobid = pname.jobid;
    opal_list_append(&mca_pmix_pmix4x_component.jobids, &job->super);
#endif

    pname.vpid = pmix_convert_rank(myproc.rank);
    fprintf(stderr, "opal_proc_set_name(%x:%d)\n", pname.jobid, pname.vpid);
    opal_proc_set_name(&pname);

    /* release the thread in case the event handler fires when
     * registered */
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);

#if 0
    /* register the default event handler */
    event = OBJ_NEW(opal_pmix4x_event_t);
    opal_list_append(&mca_pmix_pmix4x_component.events, &event->super);
#endif
    opal_pmix_lock_t lock;
    OPAL_PMIX_CONSTRUCT_LOCK(&lock);
    PMIX_INFO_CREATE(pinfo, 1);
    PMIX_INFO_LOAD(&pinfo[0], PMIX_EVENT_HDLR_NAME, "OPAL-PMIX-2X-DEFAULT", PMIX_STRING);
    PMIx_Register_event_handler(NULL, 0, NULL, 0, evhandler, errreg_cbfunc, &lock);
    OPAL_PMIX_WAIT_THREAD(&lock);
    PMIX_INFO_FREE(pinfo, 1);

    return OPAL_SUCCESS;

}

int opal_pmix_store_local(const opal_process_name_t *proc,
                          opal_value_t *val) {
    pmix_value_t kv;
    pmix_status_t rc;
    pmix_proc_t p;
#if 0
    char *nsptr;
    opal_pmix4x_jobid_trkr_t *job;
#endif

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_lock);

    if (0 >= opal_pmix_base_initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);

    if (NULL != proc) {
#if 0
        if (NULL == (nsptr = pmix4x_convert_jobid(proc->jobid))) {
            job = OBJ_NEW(opal_pmix4x_jobid_trkr_t);
            (void)opal_snprintf_jobid(job->nspace, PMIX_MAX_NSLEN, proc->jobid);
            job->jobid = proc->jobid;
            OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_base.lock);
            opal_list_append(&mca_pmix_pmix4x_component.jobids, &job->super);
            OPAL_PMIX_RELEASE_THREAD(&opal_pmix_base.lock);
            nsptr = job->nspace;
        }
        (void)strncpy(p.nspace, nsptr, PMIX_MAX_NSLEN);
#else
        OPAL_PMIX_CONVERT_JOBID(p.nspace, proc->jobid);
#endif
        p.rank = opal_pmix_convert_opalrank(proc->vpid);
    } else {
        /* use our name */
        (void)strncpy(p.nspace, myproc.nspace, PMIX_MAX_NSLEN);
        p.rank = opal_pmix_convert_opalrank(OPAL_PROC_MY_NAME.vpid);
    }

    PMIX_VALUE_CONSTRUCT(&kv);
    opal_pmix_value_load(&kv, val);

    /* call the library - this is a blocking call */
    rc = PMIx_Store_internal(&p, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);

    return opal_pmix_convert_status(rc);
}

int opal_pmix_finalize(void) {
    pmix_status_t rc;
#if 0
    opal_pmix4x_event_t *event, *ev2;
    opal_list_t evlist;
    OBJ_CONSTRUCT(&evlist, opal_list_t);
#endif

    opal_output_verbose(1, opal_pmix_verbose_output,
                        "PMIx_client finalize");

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_lock);
    --opal_pmix_base_initialized;

#if 0
    if (0 == opal_pmix_base.initialized) {
        /* deregister all event handlers */
        OPAL_LIST_FOREACH_SAFE(event, ev2, &mca_pmix_pmix4x_component.events, opal_pmix4x_event_t) {
            OPAL_PMIX_DESTRUCT_LOCK(&event->lock);
            OPAL_PMIX_CONSTRUCT_LOCK(&event->lock);
            PMIx_Deregister_event_handler(event->index, dereg_cbfunc, (void*)event);
            opal_list_remove_item(&mca_pmix_pmix4x_component.events, &event->super);
            /* wait and release outside the loop to avoid double mutex
             * interlock */
            opal_list_append(&evlist, &event->super);
        }
    }
#endif
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);
#if 0
    OPAL_LIST_FOREACH_SAFE(event, ev2, &evlist, opal_pmix4x_event_t) {
        OPAL_PMIX_WAIT_THREAD(&event->lock);
        opal_list_remove_item(&evlist, &event->super);
        OBJ_RELEASE(event);
    }
    OBJ_DESTRUCT(&evlist);
#endif
    rc = PMIx_Finalize(NULL, 0);

    return opal_pmix_convert_status(rc);
}

int opal_pmix_abort(int status, const char *msg,
                    opal_list_t *procs) {
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
#if 0
    char *nsptr;
#endif

    opal_output_verbose(1, opal_pmix_verbose_output,
                        "PMIx_client abort");

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_lock);
    if (0 >= opal_pmix_base_initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
#if 0
            if (NULL == (nsptr = opal_pmix_convert_jobid(ptr->name.jobid))) {
                PMIX_PROC_FREE(parray, cnt);
                return OPAL_ERR_NOT_FOUND;
            }
            (void)strncpy(parray[n].nspace, nsptr, PMIX_MAX_NSLEN);
#else
            OPAL_PMIX_CONVERT_JOBID(parray[n].nspace, ptr->name.jobid);
#endif
            parray[n].rank = opal_pmix_convert_opalrank(ptr->name.vpid);
            ++n;
        }
    }

    /* call the library abort - this is a blocking call */
    rc = PMIx_Abort(status, msg, parray, cnt);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);

    return opal_pmix_convert_status(rc);
}

void opal_pmix_register_evhandler(opal_list_t *event_codes, opal_list_t *info,
                                  opal_pmix_notification_fn_t evhandler,
                                  opal_pmix_evhandler_reg_cbfunc_t cbfunc,
                                  void *cbdata) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    cbfunc(OPAL_SUCCESS, 0, cbdata);
    OPAL_ERROR_LOG(rc);
}

void opal_pmix_deregister_evhandler(size_t evhandler, opal_pmix_op_cbfunc_t cbfunc,
                                    void *cbdata) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
}

void opal_pmix_base_set_evbase(opal_event_base_t *evbase) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
}

char* opal_pmix_get_nspace(opal_jobid_t jobid) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
    assert(rc != OPAL_ERR_NOT_IMPLEMENTED);
    return NULL;
}

void opal_pmix_register_jobid(opal_jobid_t jobid, const char *nspace) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
}

int opal_pmix_connect(opal_list_t *procs) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
    assert(rc != OPAL_ERR_NOT_IMPLEMENTED);
    return rc;
}

int opal_pmix_spawn(opal_list_t *job_info, opal_list_t *apps, opal_jobid_t *jobid) {
    int rc = OPAL_ERR_NOT_IMPLEMENTED;
    OPAL_ERROR_LOG(rc);
    assert(rc != OPAL_ERR_NOT_IMPLEMENTED);
    return rc;
}

int opal_pmix_commit() {
    pmix_status_t rc;

    OPAL_PMIX_ACQUIRE_THREAD(&opal_pmix_lock);
    if (0 >= opal_pmix_base_initialized) {
        OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);
        return OPAL_ERR_NOT_INITIALIZED;
    }
    OPAL_PMIX_RELEASE_THREAD(&opal_pmix_lock);

    rc = PMIx_Commit();
    return opal_pmix_convert_status(rc);
}

int opal_pmix_base_exchange(opal_value_t *indat,
                            opal_pmix_pdata_t *outdat,
                            int timeout)
{
    int rc;
    opal_list_t ilist, mlist;
    opal_value_t *info;
    opal_pmix_pdata_t *pdat;

    /* protect the incoming value */
    opal_dss.copy((void**)&info, indat, OPAL_VALUE);
    OBJ_CONSTRUCT(&ilist, opal_list_t);
    opal_list_append(&ilist, &info->super);
    /* tell the server to delete upon read */
    info = OBJ_NEW(opal_value_t);
    info->key = strdup(PMIX_PERSISTENCE);
    info->type = OPAL_PERSIST;
    info->data.integer = PMIX_PERSIST_FIRST_READ;
    opal_list_append(&ilist, &info->super);

    /* publish it with "session" scope */
    rc = opal_pmix_publish(&ilist);
    OPAL_LIST_DESTRUCT(&ilist);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* lookup the other side's info - if a non-blocking form
     * of lookup isn't available, then we use the blocking
     * form and trust that the underlying system will WAIT
     * until the other side publishes its data */
    pdat = OBJ_NEW(opal_pmix_pdata_t);
    pdat->value.key = strdup(outdat->value.key);
    pdat->value.type = outdat->value.type;
    /* setup the constraints */
    OBJ_CONSTRUCT(&mlist, opal_list_t);
    /* tell it to wait for the data to arrive */
    info = OBJ_NEW(opal_value_t);
    info->key = strdup(PMIX_WAIT);
    info->type = OPAL_BOOL;
    info->data.flag = true;
    opal_list_append(&mlist, &info->super);
    /* pass along the given timeout as we don't know when
     * the other side will publish - it doesn't
     * have to be simultaneous */
    info = OBJ_NEW(opal_value_t);
    info->key = strdup(PMIX_TIMEOUT);
    info->type = OPAL_INT;
    if (0 < opal_pmix_base_timeout) {
        /* the user has overridden the default */
        info->data.integer = opal_pmix_base_timeout;
    } else {
        info->data.integer = timeout;
    }
    opal_list_append(&mlist, &info->super);

    /* if a non-blocking version of lookup isn't
     * available, then use the blocking version */
    OBJ_CONSTRUCT(&ilist, opal_list_t);
    opal_list_append(&ilist, &pdat->super);
    rc = opal_pmix_lookup(&ilist, &mlist);
    OPAL_LIST_DESTRUCT(&mlist);
    if (OPAL_SUCCESS != rc) {
        OPAL_LIST_DESTRUCT(&ilist);
        return rc;
    }

    /* pass back the result */
    outdat->proc = pdat->proc;
    free(outdat->value.key);
    rc = opal_value_xfer(&outdat->value, &pdat->value);
    OPAL_LIST_DESTRUCT(&ilist);
    return rc;
}

