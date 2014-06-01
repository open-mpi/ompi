/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/util/attr.h"

static int orte_attr_unload(orte_attribute_t *kv,
                            void **data, opal_data_type_t type);

static int orte_attr_load(orte_attribute_t *kv,
                          void *data, opal_data_type_t type);


bool orte_get_attribute(opal_list_t *attributes,
                        orte_attribute_key_t key,
                        void **data, opal_data_type_t type)
{
    orte_attribute_t *kv;
    int rc;

    OPAL_LIST_FOREACH(kv, attributes, orte_attribute_t) {
        if (key == kv->key) {
            if (kv->type != type) {
                ORTE_ERROR_LOG(ORTE_ERR_TYPE_MISMATCH);
                return false;
            }
            if (NULL != data) {
                if (ORTE_SUCCESS != (rc = orte_attr_unload(kv, data, type))) {
                    ORTE_ERROR_LOG(rc);
                }
            }
            return true;
        }
    }
    /* not found */
    return false;
}

int orte_set_attribute(opal_list_t *attributes,
                       orte_attribute_key_t key, bool local,
                       void *data, opal_data_type_t type)
{
    orte_attribute_t *kv;
    int rc;

    OPAL_LIST_FOREACH(kv, attributes, orte_attribute_t) {
        if (key == kv->key) {
            if (kv->type != type) {
                return ORTE_ERR_TYPE_MISMATCH;
            }
            if (ORTE_SUCCESS != (rc = orte_attr_load(kv, data, type))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
    }
    /* not found - add it */
    kv = OBJ_NEW(orte_attribute_t);
    kv->key = key;
    kv->local = local;
    if (OPAL_SUCCESS != (rc = orte_attr_load(kv, data, type))) {
        OBJ_RELEASE(kv);
        return rc;
    }
    opal_list_append(attributes, &kv->super);
    return ORTE_SUCCESS;
}

void orte_remove_attribute(opal_list_t *attributes, orte_attribute_key_t key)
{
    orte_attribute_t *kv;

    OPAL_LIST_FOREACH(kv, attributes, orte_attribute_t) {
        if (key == kv->key) {
            opal_list_remove_item(attributes, &kv->super);
            OBJ_RELEASE(kv);
            return;
        }
    }
}

const char *orte_attr_key_to_str(orte_attribute_key_t key)
{
    switch(key) {
    case ORTE_APP_HOSTFILE:
        return "APP-HOSTFILE";
    case ORTE_APP_ADD_HOSTFILE:
        return "APP-ADD-HOSTFILE";
    case ORTE_APP_DASH_HOST:
        return "APP-DASH-HOST";
    case ORTE_APP_ADD_HOST:
        return "APP-ADD-HOST";
    case ORTE_APP_USER_CWD:
        return "APP-USER-CWD";
    case ORTE_APP_SSNDIR_CWD:
        return "APP-USE-SESSION-DIR-AS-CWD";
    case ORTE_APP_PRELOAD_BIN:
        return "APP-PRELOAD-BIN";
    case ORTE_APP_PRELOAD_FILES:
        return "APP-PRELOAD-FILES";
    case ORTE_APP_SSTORE_LOAD:
        return "APP-SSTORE-LOAD";
    case ORTE_APP_RECOV_DEF:
        return "APP-RECOVERY-DEFINED";
    case ORTE_APP_MAX_RESTARTS:
        return "APP-MAX-RESTARTS";
    case ORTE_APP_MIN_NODES:
        return "APP-MIN-NODES";
    case ORTE_APP_MANDATORY:
        return "APP-NODES-MANDATORY";
    case ORTE_APP_MAX_PPN:
        return "APP-MAX-PPN";
    case ORTE_APP_PREFIX_DIR:
        return "APP-PREFIX-DIR";

    case ORTE_NODE_USERNAME:
        return "NODE-USERNAME";
    case ORTE_NODE_LAUNCH_ID:
        return "NODE-LAUNCHID";
    case ORTE_NODE_HOSTID:
        return "NODE-HOSTID";
    case ORTE_NODE_ALIAS:
        return "NODE-ALIAS";
    case ORTE_NODE_SERIAL_NUMBER:
        return "NODE-SERIAL-NUM";

    case ORTE_JOB_LAUNCH_MSG_SENT:
        return "JOB-LAUNCH-MSG-SENT";
    case ORTE_JOB_LAUNCH_MSG_RECVD:
        return "JOB-LAUNCH-MSG-RECVD";
    case ORTE_JOB_MAX_LAUNCH_MSG_RECVD:
        return "JOB-MAX-LAUNCH-MSG-RECVD";
    case ORTE_JOB_FILE_MAPS:
        return "JOB-FILE-MAPS";
    case ORTE_JOB_CKPT_STATE:
        return "JOB-CKPT-STATE";
    case ORTE_JOB_CKPT_SNAPSHOT_REF:
        return "JOB-CKPT-SNAPSHOT-REF";
    case ORTE_JOB_SNAP_LOC:
        return "JOB-SNAP-LOCATION";
    case ORTE_JOB_SNAPC_INIT_BAR:
        return "JOB-SNAPC-INIT-BARRIER-ID";
    case ORTE_JOB_SNAPC_FINI_BAR:
        return "JOB-SNAPC-FINI-BARRIER-ID";
    case ORTE_JOB_NUM_NONZERO_EXIT:
        return "JOB-NUM-NONZERO-EXIT";
    case ORTE_JOB_FAILURE_TIMER_EVENT:
        return "JOB-FAILURE-TIMER-EVENT";
    case ORTE_JOB_ABORTED_PROC:
        return "JOB-ABORTED-PROC";
    case ORTE_JOB_MAPPER:
        return "JOB-MAPPER";
    case ORTE_JOB_REDUCER:
        return "JOB-REDUCER";
    case ORTE_JOB_COMBINER:
        return "JOB-COMBINER";
    case ORTE_JOB_INDEX_ARGV:
        return "JOB-INDEX-ARGV";
    case ORTE_JOB_NO_VM:
        return "JOB-NO-VM";
    case ORTE_JOB_SPIN_FOR_DEBUG:
        return "JOB-SPIN-FOR-DEBUG";
    case ORTE_JOB_CONTINUOUS_OP:
        return "JOB-CONTINUOUS-OP";
    case ORTE_JOB_RECOVER_DEFINED:
        return "JOB-RECOVERY-DEFINED";
    case ORTE_JOB_ENABLE_RECOVERY:
        return "JOB-ENABLE-RECOVERY";
    case ORTE_JOB_NON_ORTE_JOB:
        return "JOB-NON-ORTE-JOB";
    case ORTE_JOB_STDOUT_TARGET:
        return "JOB-STDOUT-TARGET";
    case ORTE_JOB_POWER:
        return "JOB-POWER";
    case ORTE_JOB_MAX_FREQ:
        return "JOB-MAX_FREQ";
    case ORTE_JOB_MIN_FREQ:
        return "JOB-MIN_FREQ";
    case ORTE_JOB_GOVERNOR:
        return "JOB-FREQ-GOVERNOR";
    case ORTE_PROC_NOBARRIER:
        return "PROC-NOBARRIER";
    case ORTE_PROC_CPU_BITMAP:
        return "PROC-CPU-BITMAP";
    case ORTE_PROC_HWLOC_LOCALE:
        return "PROC-HWLOC-LOCALE";
    case ORTE_PROC_HWLOC_BOUND:
        return "PROC-HWLOC-BOUND";
    case ORTE_PROC_PRIOR_NODE:
        return "PROC-PRIOR-NODE";
    case ORTE_PROC_NRESTARTS:
        return "PROC-NUM-RESTARTS";
    case ORTE_PROC_RESTART_TIME:
        return "PROC-RESTART-TIME";
    case ORTE_PROC_FAST_FAILS:
        return "PROC-FAST-FAILS";
    case ORTE_PROC_CKPT_STATE:
        return "PROC-CKPT-STATE";
    case ORTE_PROC_CKPT_SNAP_REF:
        return "PROC-CKPT-SNAP-REF";
    case ORTE_PROC_SNAP_LOC:
        return "PROC-SNAP-LOC";
    case ORTE_PROC_NODENAME:
        return "PROC-NODENAME";
    case ORTE_PROC_CGROUP:
        return "PROC-CGROUP";

    default:
        return "UNKNOWN-KEY";
    }
}


static int orte_attr_load(orte_attribute_t *kv,
                          void *data, opal_data_type_t type)
{
    opal_byte_object_t *boptr;
    struct timeval *tv;

    kv->type = type;
    if (NULL == data && OPAL_STRING != type && OPAL_BYTE_OBJECT != type) {
        /* just set the fields to zero */
        memset(&kv->data, 0, sizeof(kv->data));
        return OPAL_SUCCESS;
    }

    switch (type) {
    case OPAL_BOOL:
        kv->data.flag = *(bool*)(data);
        break;
    case OPAL_BYTE:
        kv->data.byte = *(uint8_t*)(data);
        break;
    case OPAL_STRING:
        if (NULL != kv->data.string) {
            free(kv->data.string);
        }
        if (NULL != data) {
            kv->data.string = strdup( (const char *) data);
        } else {
            kv->data.string = NULL;
        }
        break;
    case OPAL_SIZE:
        kv->data.size = *(size_t*)(data);
        break;
    case OPAL_PID:
        kv->data.pid = *(pid_t*)(data);
        break;

    case OPAL_INT:
        kv->data.integer = *(int*)(data);
        break;
    case OPAL_INT8:
        kv->data.int8 = *(int8_t*)(data);
        break;
    case OPAL_INT16:
        kv->data.int16 = *(int16_t*)(data);
        break;
    case OPAL_INT32:
        kv->data.int32 = *(int32_t*)(data);
        break;
    case OPAL_INT64:
        kv->data.int64 = *(int64_t*)(data);
        break;

    case OPAL_UINT:
        kv->data.uint = *(unsigned int*)(data);
        break;
    case OPAL_UINT8:
        kv->data.uint8 = *(uint8_t*)(data);
        break;
    case OPAL_UINT16:
        kv->data.uint16 = *(uint16_t*)(data);
        break;
    case OPAL_UINT32:
        kv->data.uint32 = *(uint32_t*)data;
        break;
    case OPAL_UINT64:
        kv->data.uint64 = *(uint64_t*)(data);
        break;

    case OPAL_BYTE_OBJECT:
        if (NULL != kv->data.bo.bytes) {
            free(kv->data.bo.bytes);
        }
        boptr = (opal_byte_object_t*)data;
        if (NULL != boptr && NULL != boptr->bytes && 0 < boptr->size) {
            kv->data.bo.bytes = (uint8_t *) malloc(boptr->size);
            memcpy(kv->data.bo.bytes, boptr->bytes, boptr->size);
            kv->data.bo.size = boptr->size;
        } else {
            kv->data.bo.bytes = NULL;
            kv->data.bo.size = 0;
        }
        break;

    case OPAL_FLOAT:
        kv->data.fval = *(float*)(data);
        break;

    case OPAL_TIMEVAL:
        tv = (struct timeval*)data;
        kv->data.tv.tv_sec = tv->tv_sec;
        kv->data.tv.tv_usec = tv->tv_usec;
        break;

    case OPAL_PTR:
        kv->data.ptr = data;
        break;

    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }
    return OPAL_SUCCESS;
}

static int orte_attr_unload(orte_attribute_t *kv,
                            void **data, opal_data_type_t type)
{
    opal_byte_object_t *boptr;

    if (type != kv->type) {
        return OPAL_ERR_TYPE_MISMATCH;
    }
    if (NULL == data && OPAL_STRING != type && OPAL_BYTE_OBJECT != type) {
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        return OPAL_ERR_BAD_PARAM;
    }

    switch (type) {
    case OPAL_BOOL:
        memcpy(*data, &kv->data.flag, sizeof(bool));
        break;
    case OPAL_BYTE:
        memcpy(*data, &kv->data.byte, sizeof(uint8_t));
        break;
    case OPAL_STRING:
        if (NULL != kv->data.string) {
            *data = strdup(kv->data.string);
        } else {
            *data = NULL;
        }
        break;
    case OPAL_SIZE:
        memcpy(*data, &kv->data.size, sizeof(size_t));
        break;
    case OPAL_PID:
        memcpy(*data, &kv->data.pid, sizeof(pid_t));
        break;

    case OPAL_INT:
        memcpy(*data, &kv->data.integer, sizeof(int));
        break;
    case OPAL_INT8:
        memcpy(*data, &kv->data.int8, sizeof(int8_t));
        break;
    case OPAL_INT16:
        memcpy(*data, &kv->data.int16, sizeof(int16_t));
        break;
    case OPAL_INT32:
        memcpy(*data, &kv->data.int32, sizeof(int32_t));
        break;
    case OPAL_INT64:
        memcpy(*data, &kv->data.int64, sizeof(int64_t));
        break;

    case OPAL_UINT:
        memcpy(*data, &kv->data.uint, sizeof(unsigned int));
        break;
    case OPAL_UINT8:
        memcpy(*data, &kv->data.uint8, 1);
        break;
    case OPAL_UINT16:
        memcpy(*data, &kv->data.uint16, 2);
        break;
    case OPAL_UINT32:
        memcpy(*data, &kv->data.uint32, 4);
        break;
    case OPAL_UINT64:
        memcpy(*data, &kv->data.uint64, 8);
        break;

    case OPAL_BYTE_OBJECT:
        boptr = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        if (NULL != kv->data.bo.bytes && 0 < kv->data.bo.size) {
            boptr->bytes = (uint8_t *) malloc(kv->data.bo.size);
            memcpy(boptr->bytes, kv->data.bo.bytes, kv->data.bo.size);
            boptr->size = kv->data.bo.size;
        } else {
            boptr->bytes = NULL;
            boptr->size = 0;
        }
        *data = boptr;
        break;

    case OPAL_BUFFER:
        *data = OBJ_NEW(opal_buffer_t);
        opal_dss.copy_payload(*data, &kv->data.buf);
        break;

    case OPAL_FLOAT:
        memcpy(*data, &kv->data.fval, sizeof(float));
        break;

    case OPAL_TIMEVAL:
        memcpy(*data, &kv->data.tv, sizeof(struct timeval));
        break;

    case OPAL_PTR:
        *data = kv->data.ptr;
        break;

    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }
    return OPAL_SUCCESS;
}
