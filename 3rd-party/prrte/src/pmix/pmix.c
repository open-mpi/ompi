/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "src/include/constants.h"

#include <regex.h>

#include <string.h>
#include <time.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/class/pmix_hash_table.h"
#include "src/mca/plm/base/plm_private.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/proc_info.h"

pmix_status_t prte_pmix_convert_rc(int rc)
{
    switch (rc) {

    case PRTE_ERR_HEARTBEAT_ALERT:
    case PRTE_ERR_FILE_ALERT:
    case PRTE_ERR_HEARTBEAT_LOST:
    case PRTE_ERR_SENSOR_LIMIT_EXCEEDED:
        return PMIX_ERR_JOB_SENSOR_BOUND_EXCEEDED;

    case PRTE_ERR_NO_EXE_SPECIFIED:
    case PRTE_ERR_NO_APP_SPECIFIED:
        return PMIX_ERR_JOB_NO_EXE_SPECIFIED;

    case PRTE_ERR_SLOT_LIST_RANGE:
    case PRTE_ERR_TOPO_SOCKET_NOT_SUPPORTED:
    case PRTE_ERR_INVALID_PHYS_CPU:
    case PRTE_ERR_TOPO_CORE_NOT_SUPPORTED:
    case PRTE_ERR_TOPO_SLOT_LIST_NOT_SUPPORTED:
    case PRTE_ERR_MULTIPLE_AFFINITIES:
    case PRTE_ERR_FAILED_TO_MAP:
        return PMIX_ERR_JOB_FAILED_TO_MAP;

    case PRTE_ERR_JOB_CANCELLED:
        return PMIX_ERR_JOB_CANCELED;

    case PRTE_ERR_DEBUGGER_RELEASE:
        return PMIX_ERR_DEBUGGER_RELEASE;

    case PRTE_ERR_HANDLERS_COMPLETE:
        return PMIX_EVENT_ACTION_COMPLETE;

    case PRTE_ERR_PROC_ABORTED:
        return PMIX_ERR_PROC_ABORTED;

    case PRTE_ERR_PROC_REQUESTED_ABORT:
        return PMIX_ERR_PROC_REQUESTED_ABORT;

    case PRTE_ERR_PROC_ABORTING:
        return PMIX_ERR_PROC_ABORTING;

    case PRTE_ERR_NODE_DOWN:
        return PMIX_ERR_NODE_DOWN;

    case PRTE_ERR_NODE_OFFLINE:
        return PMIX_ERR_NODE_OFFLINE;

    case PRTE_ERR_JOB_TERMINATED:
        return PMIX_EVENT_JOB_END;

    case PRTE_ERR_PROC_RESTART:
        return PMIX_ERR_PROC_RESTART;

    case PRTE_ERR_PROC_CHECKPOINT:
        return PMIX_ERR_PROC_CHECKPOINT;

    case PRTE_ERR_PROC_MIGRATE:
        return PMIX_ERR_PROC_MIGRATE;

    case PRTE_ERR_EVENT_REGISTRATION:
        return PMIX_ERR_EVENT_REGISTRATION;

    case PRTE_ERR_NOT_IMPLEMENTED:
    case PRTE_ERR_NOT_SUPPORTED:
        return PMIX_ERR_NOT_SUPPORTED;

    case PRTE_ERR_NOT_FOUND:
        return PMIX_ERR_NOT_FOUND;

    case PRTE_ERR_PERM:
    case PRTE_ERR_UNREACH:
    case PRTE_ERR_SERVER_NOT_AVAIL:
        return PMIX_ERR_UNREACH;

    case PRTE_ERR_BAD_PARAM:
        return PMIX_ERR_BAD_PARAM;

    case PRTE_ERR_SYS_LIMITS_PIPES:
    case PRTE_ERR_SYS_LIMITS_CHILDREN:
    case PRTE_ERR_SOCKET_NOT_AVAILABLE:
    case PRTE_ERR_NOT_ENOUGH_CORES:
    case PRTE_ERR_NOT_ENOUGH_SOCKETS:
        return PMIX_ERR_JOB_INSUFFICIENT_RESOURCES;

    case PRTE_ERR_PIPE_READ_FAILURE:
        return PMIX_ERR_JOB_SYS_OP_FAILED;

    case PRTE_ERR_OUT_OF_RESOURCE:
        return PMIX_ERR_OUT_OF_RESOURCE;

    case PRTE_ERR_DATA_VALUE_NOT_FOUND:
        return PMIX_ERR_DATA_VALUE_NOT_FOUND;

    case PRTE_ERR_WDIR_NOT_FOUND:
        return PMIX_ERR_JOB_WDIR_NOT_FOUND;

    case PRTE_ERR_EXE_NOT_FOUND:
    case PRTE_ERR_EXE_NOT_ACCESSIBLE:
        return PMIX_ERR_JOB_EXE_NOT_FOUND;

    case PRTE_ERR_TIMEOUT:
        return PMIX_ERR_TIMEOUT;

    case PRTE_ERR_WOULD_BLOCK:
        return PMIX_ERR_WOULD_BLOCK;

    case PRTE_EXISTS:
        return PMIX_EXISTS;

    case PRTE_ERR_PARTIAL_SUCCESS:
        return PMIX_QUERY_PARTIAL_SUCCESS;

    case PRTE_ERR_MODEL_DECLARED:
        return PMIX_MODEL_DECLARED;

    case PRTE_ERROR:
        return PMIX_ERROR;
    case PRTE_SUCCESS:
        return PMIX_SUCCESS;

    default:
        return PMIX_ERROR;
    }
}

int prte_pmix_convert_status(pmix_status_t status)
{
    switch (status) {
    case PMIX_ERR_DEBUGGER_RELEASE:
        return PRTE_ERR_DEBUGGER_RELEASE;

    case PMIX_EVENT_ACTION_COMPLETE:
        return PRTE_ERR_HANDLERS_COMPLETE;

    case PMIX_ERR_PROC_ABORTED:
        return PRTE_ERR_PROC_ABORTED;

    case PMIX_ERR_PROC_REQUESTED_ABORT:
        return PRTE_ERR_PROC_REQUESTED_ABORT;

    case PMIX_ERR_PROC_ABORTING:
        return PRTE_ERR_PROC_ABORTING;

    case PMIX_ERR_NODE_DOWN:
        return PRTE_ERR_NODE_DOWN;

    case PMIX_ERR_NODE_OFFLINE:
        return PRTE_ERR_NODE_OFFLINE;

    case PMIX_EVENT_JOB_END:
        return PRTE_ERR_JOB_TERMINATED;

    case PMIX_ERR_PROC_RESTART:
        return PRTE_ERR_PROC_RESTART;

    case PMIX_ERR_PROC_CHECKPOINT:
        return PRTE_ERR_PROC_CHECKPOINT;

    case PMIX_ERR_PROC_MIGRATE:
        return PRTE_ERR_PROC_MIGRATE;

    case PMIX_ERR_EVENT_REGISTRATION:
        return PRTE_ERR_EVENT_REGISTRATION;

    case PMIX_ERR_NOT_SUPPORTED:
        return PRTE_ERR_NOT_SUPPORTED;

    case PMIX_ERR_NOT_FOUND:
        return PRTE_ERR_NOT_FOUND;

    case PMIX_ERR_OUT_OF_RESOURCE:
        return PRTE_ERR_OUT_OF_RESOURCE;

    case PMIX_ERR_INIT:
        return PRTE_ERROR;

    case PMIX_ERR_BAD_PARAM:
        return PRTE_ERR_BAD_PARAM;

    case PMIX_ERR_UNREACH:
    case PMIX_ERR_NO_PERMISSIONS:
        return PRTE_ERR_UNREACH;

    case PMIX_ERR_TIMEOUT:
        return PRTE_ERR_TIMEOUT;

    case PMIX_ERR_WOULD_BLOCK:
        return PRTE_ERR_WOULD_BLOCK;

    case PMIX_ERR_LOST_CONNECTION:
        return PRTE_ERR_COMM_FAILURE;

    case PMIX_EXISTS:
        return PRTE_EXISTS;

    case PMIX_QUERY_PARTIAL_SUCCESS:
        return PRTE_ERR_PARTIAL_SUCCESS;

    case PMIX_MONITOR_HEARTBEAT_ALERT:
        return PRTE_ERR_HEARTBEAT_ALERT;

    case PMIX_MONITOR_FILE_ALERT:
        return PRTE_ERR_FILE_ALERT;

    case PMIX_MODEL_DECLARED:
        return PRTE_ERR_MODEL_DECLARED;

    case PMIX_ERROR:
        return PRTE_ERROR;
    case PMIX_ERR_SILENT:
        return PRTE_ERR_SILENT;
    case PMIX_SUCCESS:
    case PMIX_OPERATION_SUCCEEDED:
        return PRTE_SUCCESS;
    case PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
        return PRTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;

    default:
        return status;
    }
}

pmix_proc_state_t prte_pmix_convert_state(int state)
{
    switch (state) {
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

int prte_pmix_convert_pstate(pmix_proc_state_t state)
{
    switch (state) {
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
        return 0; // undef
    }
}

pmix_status_t prte_pmix_convert_job_state_to_error(int state)
{
    switch (state) {
        case PRTE_JOB_STATE_ALLOC_FAILED:
            return PMIX_ERR_JOB_ALLOC_FAILED;

        case PRTE_JOB_STATE_MAP_FAILED:
            return PMIX_ERR_JOB_FAILED_TO_MAP;

        case PRTE_JOB_STATE_NEVER_LAUNCHED:
        case PRTE_JOB_STATE_FAILED_TO_LAUNCH:
        case PRTE_JOB_STATE_FAILED_TO_START:
        case PRTE_JOB_STATE_CANNOT_LAUNCH:
            return PMIX_ERR_JOB_FAILED_TO_LAUNCH;

        case PRTE_JOB_STATE_KILLED_BY_CMD:
            return PMIX_ERR_JOB_CANCELED;

        case PRTE_JOB_STATE_ABORTED:
        case PRTE_JOB_STATE_CALLED_ABORT:
        case PRTE_JOB_STATE_SILENT_ABORT:
            return PMIX_ERR_JOB_ABORTED;

        case PRTE_JOB_STATE_ABORTED_BY_SIG:
            return PMIX_ERR_JOB_ABORTED_BY_SIG;

        case PRTE_JOB_STATE_ABORTED_WO_SYNC:
            return PMIX_ERR_JOB_TERM_WO_SYNC;

        case PRTE_JOB_STATE_TERMINATED:
            return PMIX_EVENT_JOB_END;

        default:
            return PMIX_ERROR;
    }
}

pmix_status_t prte_pmix_convert_proc_state_to_error(int state)
{
    switch (state) {
        case PRTE_PROC_STATE_KILLED_BY_CMD:
            return PMIX_ERR_JOB_CANCELED;

        case PRTE_PROC_STATE_ABORTED:
        case PRTE_PROC_STATE_CALLED_ABORT:
            return PMIX_ERR_JOB_ABORTED;

        case PRTE_PROC_STATE_ABORTED_BY_SIG:
            return PMIX_ERR_JOB_ABORTED_BY_SIG;

        case PRTE_PROC_STATE_FAILED_TO_LAUNCH:
        case PRTE_PROC_STATE_FAILED_TO_START:
            return PMIX_ERR_JOB_FAILED_TO_LAUNCH;

        case PRTE_PROC_STATE_TERM_WO_SYNC:
            return PMIX_ERR_JOB_TERM_WO_SYNC;

        case PRTE_PROC_STATE_COMM_FAILED:
        case PRTE_PROC_STATE_UNABLE_TO_SEND_MSG:
        case PRTE_PROC_STATE_LIFELINE_LOST:
        case PRTE_PROC_STATE_NO_PATH_TO_TARGET:
        case PRTE_PROC_STATE_FAILED_TO_CONNECT:
        case PRTE_PROC_STATE_PEER_UNKNOWN:
            return PMIX_ERR_COMM_FAILURE;

        case PRTE_PROC_STATE_CANNOT_RESTART:
            return PMIX_ERR_PROC_RESTART;

        case PRTE_PROC_STATE_TERM_NON_ZERO:
            return PMIX_ERR_JOB_NON_ZERO_TERM;

        case PRTE_PROC_STATE_SENSOR_BOUND_EXCEEDED:
            return PMIX_ERR_JOB_SENSOR_BOUND_EXCEEDED;

        default:
            return PMIX_ERROR;
    }
}

static void cleanup_cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                           pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    prte_pmix_lock_t *lk = (prte_pmix_lock_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(info, ninfo);

    PMIX_POST_OBJECT(lk);

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    lk->status = status;
    PRTE_PMIX_WAKEUP_THREAD(lk);
}

int prte_pmix_register_cleanup(char *path, bool directory, bool ignore, bool jobscope)
{
    prte_pmix_lock_t lk;
    pmix_info_t pinfo[3];
    size_t n, ninfo = 0;
    pmix_status_t rc, ret;

    PRTE_PMIX_CONSTRUCT_LOCK(&lk);

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
        rc = PMIx_Job_control_nb(NULL, 0, pinfo, ninfo, cleanup_cbfunc, (void *) &lk);
    } else {
        rc = PMIx_Job_control_nb(PRTE_PROC_MY_NAME, 1, pinfo, ninfo, cleanup_cbfunc, (void *) &lk);
    }
    if (PMIX_SUCCESS != rc) {
        ret = rc;
    } else {
        PRTE_PMIX_WAIT_THREAD(&lk);
        ret = lk.status;
    }
    PRTE_PMIX_DESTRUCT_LOCK(&lk);
    for (n = 0; n < ninfo; n++) {
        PMIX_INFO_DESTRUCT(&pinfo[n]);
    }
    return ret;
}

/* CLASS INSTANTIATIONS */
static void acon(prte_pmix_app_t *p)
{
    PMIX_APP_CONSTRUCT(&p->app);
    PMIX_INFO_LIST_START(p->info);
}
static void ades(prte_pmix_app_t *p)
{
    PMIX_APP_DESTRUCT(&p->app);
    PMIX_INFO_LIST_RELEASE(p->info);
}
PMIX_CLASS_INSTANCE(prte_pmix_app_t, pmix_list_item_t, acon, ades);

static void dsicon(prte_ds_info_t *p)
{
    PMIX_PROC_CONSTRUCT(&p->source);
    p->info = NULL;
    p->persistence = PMIX_PERSIST_INVALID;
}
PRTE_EXPORT PMIX_CLASS_INSTANCE(prte_ds_info_t, pmix_list_item_t, dsicon, NULL);

static void infoitmcon(prte_info_item_t *p)
{
    PMIX_INFO_CONSTRUCT(&p->info);
}
static void infoitdecon(prte_info_item_t *p)
{
    PMIX_INFO_DESTRUCT(&p->info);
}
PRTE_EXPORT PMIX_CLASS_INSTANCE(prte_info_item_t, pmix_list_item_t, infoitmcon, infoitdecon);

static void arritmcon(prte_info_array_item_t *p)
{
    PMIX_CONSTRUCT(&p->infolist, pmix_list_t);
}
static void arritdecon(prte_info_array_item_t *p)
{
    PMIX_LIST_DESTRUCT(&p->infolist);
}
PRTE_EXPORT PMIX_CLASS_INSTANCE(prte_info_array_item_t, pmix_list_item_t, arritmcon, arritdecon);

static void pvcon(prte_value_t *p)
{
    PMIX_VALUE_CONSTRUCT(&p->value);
}
static void pvdes(prte_value_t *p)
{
    PMIX_VALUE_DESTRUCT(&p->value);
}
PRTE_EXPORT PMIX_CLASS_INSTANCE(prte_value_t, pmix_list_item_t, pvcon, pvdes);
