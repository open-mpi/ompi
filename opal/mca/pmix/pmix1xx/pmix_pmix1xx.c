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

#include "pmix1xx.h"

#include "opal/mca/pmix/pmix1xx/pmix/include/pmix/pmix_common.h"

const opal_pmix_base_module_t opal_pmix_pmix1xx_module = {
    /* client APIs */
    pmix1xx_client_init,
    pmix1xx_client_finalize,
    pmix1xx_initialized,
    pmix1xx_abort,
    pmix1xx_commit,
    pmix1xx_fence,
    pmix1xx_fencenb,
    pmix1xx_put,
    pmix1xx_get,
    pmix1xx_getnb,
    pmix1xx_publish,
    pmix1xx_publishnb,
    pmix1xx_lookup,
    pmix1xx_lookupnb,
    pmix1xx_unpublish,
    pmix1xx_unpublishnb,
    pmix1xx_spawn,
    pmix1xx_spawnnb,
    pmix1xx_connect,
    pmix1xx_connectnb,
    pmix1xx_disconnect,
    pmix1xx_disconnectnb,
    pmix1xx_resolve_peers,
    pmix1xx_resolve_nodes,
    /* server APIs */
    pmix1xx_server_init,
    pmix1xx_server_finalize,
    pmix1xx_server_get_addr,
    pmix1xx_server_gen_regex,
    pmix1xx_server_gen_ppn,
    pmix1xx_server_register_nspace,
    pmix1xx_server_register_client,
    pmix1xx_server_setup_fork,
    pmix1xx_server_dmodex,
    pmix1xx_server_notify_error,
    /* utility APIs */
    PMIx_Get_version,
    pmix1xx_register_errhandler,
    pmix1xx_deregister_errhandler,
    pmix1xx_store_local
};

static pmix_notification_fn_t errhandler = NULL;
    
static void notification_fn(int status,
                            opal_list_t *procs,
                            opal_list_t *info)
{
    /* convert the status */

    /* convert the list of procs to an array of pmix_proc_t */

    /* convert the list of info to an array of pmix_info_t */
    
    /* pass this down to the notification function
     * we were given */
}

void pmix1xx_register_errhandler(opal_pmix_errhandler_fn_t errhandler)
{
    return;
}

void pmix1xx_deregister_errhandler(void)
{
    return;
}

int pmix1xx_store_local(const opal_process_name_t *proc,
                        opal_value_t *val)
{
    pmix_value_t kv;
    pmix_status_t rc;
    char nspace[PMIX_MAX_NSLEN];
    int rank;

    if (NULL != proc) {
        /* convert the process jobid */
        (void)strncpy(nspace, opal_convert_jobid_to_string(proc->jobid), PMIX_MAX_NSLEN);
        rank = proc->vpid;
    } else {
        /* use our name */
        (void)strncpy(nspace, opal_convert_jobid_to_string(OPAL_PROC_MY_NAME.jobid), PMIX_MAX_NSLEN);
        rank = OPAL_PROC_MY_NAME.vpid;
    }

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix1xx_value_load(&kv, val);

    rc = PMIx_Store_internal(nspace, rank, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);

    return pmix1xx_convert_rc(rc);
}

int pmix1xx_convert_data_range(opal_pmix_data_range_t *sc,
                               pmix_data_range_t scope)
{
    int rc = OPAL_SUCCESS;

    switch(scope) {
    case PMIX_DATA_RANGE_UNDEF:
        *sc = OPAL_PMIX_DATA_RANGE_UNDEF;
        break;
    case PMIX_NAMESPACE:
        *sc = OPAL_PMIX_NAMESPACE;
        break;
    case PMIX_UNIVERSAL:
        *sc = OPAL_PMIX_UNIVERSAL;
        break;
    case PMIX_USER:
        *sc = OPAL_PMIX_USER;
        break;
    default:
        *sc = OPAL_PMIX_DATA_RANGE_UNDEF;
        rc = OPAL_ERR_BAD_PARAM;
        break;
    }
}

pmix_status_t pmix1xx_convert_opalrc(int rc)
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

    case OPAL_ERROR:
        return PMIX_ERROR;
    case OPAL_SUCCESS:
        return PMIX_SUCCESS;
    default:
        return PMIX_ERROR;
    }
}

int pmix1xx_convert_rc(pmix_status_t rc)
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

    case PMIX_ERROR:
        return OPAL_ERROR;
    case PMIX_SUCCESS:
        return OPAL_SUCCESS;
    default:
        return OPAL_ERROR;
    }
}

int pmix1xx_convert_scope(opal_pmix_scope_t *sc,
                          pmix_scope_t scope)
{
    int rc = OPAL_SUCCESS;

    switch (scope) {
    case PMIX_SCOPE_UNDEF:
        *sc = OPAL_PMIX_SCOPE_UNDEF;
    case PMIX_LOCAL:
        *sc = OPAL_PMIX_LOCAL;
    case PMIX_REMOTE:
        *sc = OPAL_PMIX_REMOTE;
    case PMIX_GLOBAL:
        *sc = OPAL_PMIX_GLOBAL;
    default:
        *sc = OPAL_PMIX_SCOPE_UNDEF;
        rc = OPAL_ERR_BAD_PARAM;
    }
    return rc;
}

pmix_status_t pmix1xx_convert_opalscope(pmix_scope_t *sc,
                                        opal_pmix_scope_t scope)
{
    pmix_status_t rc = PMIX_SUCCESS;

    switch (scope) {
    case OPAL_PMIX_SCOPE_UNDEF:
        *sc = PMIX_SCOPE_UNDEF;
        break;
    case OPAL_PMIX_LOCAL:
        *sc = PMIX_LOCAL;
        break;
    case OPAL_PMIX_REMOTE:
        *sc = PMIX_REMOTE;
        break;
    case OPAL_PMIX_GLOBAL:
        *sc = PMIX_GLOBAL;
        break;
    default:
        *sc = PMIX_SCOPE_UNDEF;
        rc = PMIX_ERR_BAD_PARAM;
        break;
    }
    return rc;
}

int pmix1xx_convert_persistence(opal_pmix_persistence_t *p,
                                pmix_persistence_t persist)
{
    int rc = OPAL_SUCCESS;

    switch (persist) {
    case PMIX_PERSIST_INDEF:
        *p = OPAL_PMIX_PERSIST_INDEF;
    case PMIX_PERSIST_PROC:
        *p = OPAL_PMIX_PERSIST_PROC;
    case PMIX_PERSIST_APP:
        *p = OPAL_PMIX_PERSIST_APP;
    case PMIX_PERSIST_SESSION:
        *p = OPAL_PMIX_PERSIST_SESSION;
    default:
        *p = OPAL_PMIX_PERSIST_PROC;
        rc = OPAL_ERR_BAD_PARAM;
    }
    return rc;
}


void pmix1xx_value_load(pmix_value_t *v,
                        opal_value_t *kv)
{
    switch(kv->type) {
        case OPAL_UNDEF:
            v->type = PMIX_UNDEF;
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
        case OPAL_BYTE_OBJECT:
            v->type = PMIX_BYTE_OBJECT;
            if (NULL != kv->data.bo.bytes) {
                v->data.bo.bytes = (char*)malloc(kv->data.bo.size);
                memcpy(v->data.bo.bytes, kv->data.bo.bytes, kv->data.bo.size);
                memcpy(&(v->data.bo.size), &kv->data.bo.size, sizeof(size_t));
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

int pmix1xx_value_unload(opal_value_t *kv,
                         const pmix_value_t *v)
{
    int rc=OPAL_SUCCESS;


    switch(v->type) {
    case PMIX_UNDEF:
        rc = OPAL_ERR_UNKNOWN_DATA_TYPE;
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
            kv->data.bo.bytes = v->data.bo.bytes;
            kv->data.bo.size = v->data.bo.size;
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
static void opcon(pmix1xx_opcaddy_t *p)
{
    p->nspace = NULL;
    p->rank = PMIX_RANK_WILDCARD;
    p->procs = NULL;
    p->nprocs = 0;
    p->error_procs = NULL;
    p->nerror_procs = 0;
    p->info = NULL;
    p->sz = 0;
    p->opcbfunc = NULL;
    p->mdxcbfunc = NULL;
    p->valcbfunc = NULL;
    p->cbdata = NULL;
}
static void opdes(pmix1xx_opcaddy_t *p)
{
    if (NULL != p->nspace) {
        free(p->nspace);
    }
    if (NULL != p->procs) {
        PMIX_PROC_FREE(p->procs, p->nprocs);
    }
    if (NULL != p->error_procs) {
        PMIX_PROC_FREE(p->error_procs, p->nerror_procs);
    }
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->sz);
    }
}
OBJ_CLASS_INSTANCE(pmix1xx_opcaddy_t,
                   opal_object_t,
                   opcon, opdes);

static void ocadcon(pmix1xx_opalcaddy_t *p)
{
    OBJ_CONSTRUCT(&p->procs, opal_list_t);
    OBJ_CONSTRUCT(&p->info, opal_list_t);
    OBJ_CONSTRUCT(&p->apps, opal_list_t);
    p->opcbfunc = NULL;
    p->mdxcbfunc = NULL;
    p->lkupcbfunc = NULL;
    p->spwncbfunc = NULL;
    p->cbdata = NULL;
}
static void ocaddes(pmix1xx_opalcaddy_t *p)
{
    OPAL_LIST_DESTRUCT(&p->procs);
    OPAL_LIST_DESTRUCT(&p->info);
    OPAL_LIST_DESTRUCT(&p->apps);
}
OBJ_CLASS_INSTANCE(pmix1xx_opalcaddy_t,
                   opal_object_t,
                   ocadcon, ocaddes);
