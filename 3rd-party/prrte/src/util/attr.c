/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_string_copy.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/util/attr.h"

#define MAX_CONVERTERS            5
#define MAX_CONVERTER_PROJECT_LEN 10

typedef struct {
    int init;
    char project[MAX_CONVERTER_PROJECT_LEN];
    prte_attribute_key_t key_base;
    prte_attribute_key_t key_max;
    prte_attr2str_fn_t converter;
} prte_attr_converter_t;

/* all default to NULL */
static prte_attr_converter_t converters[MAX_CONVERTERS];

bool prte_get_attribute(pmix_list_t *attributes, prte_attribute_key_t key, void **data,
                        pmix_data_type_t type)
{
    prte_attribute_t *kv;
    int rc;

    PMIX_LIST_FOREACH(kv, attributes, prte_attribute_t)
    {
        if (key == kv->key) {
            if (kv->data.type != type) {
                PRTE_ERROR_LOG(PRTE_ERR_TYPE_MISMATCH);
                pmix_output(0, "KV %s TYPE %s", PMIx_Data_type_string(kv->data.type), PMIx_Data_type_string(type));
                return false;
            }
            if (NULL != data) {
                if (PRTE_SUCCESS != (rc = prte_attr_unload(kv, data, type))) {
                    PRTE_ERROR_LOG(rc);
                }
            }
            return true;
        }
    }
    /* not found */
    return false;
}

int prte_set_attribute(pmix_list_t *attributes, prte_attribute_key_t key,
                       bool local, void *data,
                       pmix_data_type_t type)
{
    prte_attribute_t *kv;
    bool *bl, bltrue = true;
    int rc;

    PMIX_LIST_FOREACH(kv, attributes, prte_attribute_t)
    {
        if (key == kv->key) {
            if (kv->data.type != type) {
                return PRTE_ERR_TYPE_MISMATCH;
            }
            if (PMIX_BOOL == type) {
                if (NULL == data) {
                    bl = &bltrue;
                } else {
                    bl = (bool*)data;
                }
                if (false == *bl) {
                    pmix_list_remove_item(attributes, &kv->super);
                    PMIX_RELEASE(kv);
                    return PRTE_SUCCESS;
                }
            }
            if (PRTE_SUCCESS != (rc = prte_attr_load(kv, data, type))) {
                PRTE_ERROR_LOG(rc);
            }
            return rc;
        }
    }
    /* not found - add it */
    kv = PMIX_NEW(prte_attribute_t);
    kv->key = key;
    kv->local = local;
    if (PRTE_SUCCESS != (rc = prte_attr_load(kv, data, type))) {
        PMIX_RELEASE(kv);
        return rc;
    }
    pmix_list_append(attributes, &kv->super);
    return PRTE_SUCCESS;
}

prte_attribute_t *prte_fetch_attribute(pmix_list_t *attributes, prte_attribute_t *prev,
                                       prte_attribute_key_t key)
{
    prte_attribute_t *kv, *end, *next;

    /* if prev is NULL, then find the first attr on the list
     * that matches the key */
    if (NULL == prev) {
        PMIX_LIST_FOREACH(kv, attributes, prte_attribute_t)
        {
            if (key == kv->key) {
                return kv;
            }
        }
        /* if we get, then the key isn't on the list */
        return NULL;
    }

    /* if we are at the end of the list, then nothing to do */
    end = (prte_attribute_t *) pmix_list_get_end(attributes);
    if (prev == end || end == (prte_attribute_t *) pmix_list_get_next(&prev->super)
        || NULL == pmix_list_get_next(&prev->super)) {
        return NULL;
    }

    /* starting with the next item on the list, search
     * for the next attr with the matching key */
    next = (prte_attribute_t *) pmix_list_get_next(&prev->super);
    while (NULL != next) {
        if (next->key == key) {
            return next;
        }
        next = (prte_attribute_t *) pmix_list_get_next(&next->super);
    }

    /* if we get here, then no matching key was found */
    return NULL;
}

int prte_prepend_attribute(pmix_list_t *attributes, prte_attribute_key_t key, bool local,
                           void *data, pmix_data_type_t type)
{
    prte_attribute_t *kv;
    int rc;

    kv = PMIX_NEW(prte_attribute_t);
    kv->key = key;
    kv->local = local;
    if (PRTE_SUCCESS != (rc = prte_attr_load(kv, data, type))) {
        PMIX_RELEASE(kv);
        return rc;
    }
    pmix_list_prepend(attributes, &kv->super);
    return PRTE_SUCCESS;
}

void prte_remove_attribute(pmix_list_t *attributes, prte_attribute_key_t key)
{
    prte_attribute_t *kv;

    PMIX_LIST_FOREACH(kv, attributes, prte_attribute_t)
    {
        if (key == kv->key) {
            pmix_list_remove_item(attributes, &kv->super);
            PMIX_RELEASE(kv);
            return;
        }
    }
}

int prte_attr_register(const char *project, prte_attribute_key_t key_base,
                       prte_attribute_key_t key_max, prte_attr2str_fn_t converter)
{
    int i;

    for (i = 0; i < MAX_CONVERTERS; ++i) {
        if (0 == converters[i].init) {
            converters[i].init = 1;
            pmix_string_copy(converters[i].project, project, MAX_CONVERTER_PROJECT_LEN);
            converters[i].project[MAX_CONVERTER_PROJECT_LEN - 1] = '\0';
            converters[i].key_base = key_base;
            converters[i].key_max = key_max;
            converters[i].converter = converter;
            return PRTE_SUCCESS;
        }
    }

    return PRTE_ERR_OUT_OF_RESOURCE;
}

char *prte_attr_print_list(pmix_list_t *attributes)
{
    char *out1, **cache = NULL;
    prte_attribute_t *attr;

    PMIX_LIST_FOREACH(attr, attributes, prte_attribute_t)
    {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cache, prte_attr_key_to_str(attr->key));
    }
    if (NULL != cache) {
        out1 = PMIX_ARGV_JOIN_COMPAT(cache, '\n');
        PMIX_ARGV_FREE_COMPAT(cache);
    } else {
        out1 = NULL;
    }
    return out1;
}

static char unknownkey[180] = {0};

const char *prte_attr_key_to_str(prte_attribute_key_t key)
{
    int i;

    if (PRTE_ATTR_KEY_BASE < key && key < PRTE_ATTR_KEY_MAX) {
        /* belongs to PRTE, so we handle it */
        switch (key) {
        case PRTE_APP_HOSTFILE:
            return "APP-HOSTFILE";
        case PRTE_APP_ADD_HOSTFILE:
            return "APP-ADD-HOSTFILE";
        case PRTE_APP_DASH_HOST:
            return "APP-DASH-HOST";
        case PRTE_APP_ADD_HOST:
            return "APP-ADD-HOST";
        case PRTE_APP_USER_CWD:
            return "APP-USER-CWD";
        case PRTE_APP_SSNDIR_CWD:
            return "APP-USE-SESSION-DIR-AS-CWD";
        case PRTE_APP_PRELOAD_BIN:
            return "APP-PRELOAD-BIN";
        case PRTE_APP_PRELOAD_FILES:
            return "APP-PRELOAD-FILES";
        case PRTE_APP_SSTORE_LOAD:
            return "APP-SSTORE-LOAD";
        case PRTE_APP_RECOV_DEF:
            return "APP-RECOVERY-DEFINED";
        case PRTE_APP_MAX_RESTARTS:
            return "APP-MAX-RESTARTS";
        case PRTE_APP_MIN_NODES:
            return "APP-MIN-NODES";
        case PRTE_APP_MANDATORY:
            return "APP-NODES-MANDATORY";
        case PRTE_APP_MAX_PPN:
            return "APP-MAX-PPN";
        case PRTE_APP_PREFIX_DIR:
            return "APP-PREFIX-DIR";
        case PRTE_APP_NO_CACHEDIR:
            return "PRTE_APP_NO_CACHEDIR";
        case PRTE_APP_SET_ENVAR:
            return "PRTE_APP_SET_ENVAR";
        case PRTE_APP_UNSET_ENVAR:
            return "PRTE_APP_UNSET_ENVAR";
        case PRTE_APP_PREPEND_ENVAR:
            return "PRTE_APP_PREPEND_ENVAR";
        case PRTE_APP_APPEND_ENVAR:
            return "PRTE_APP_APPEND_ENVAR";
        case PRTE_APP_ADD_ENVAR:
            return "PRTE_APP_ADD_ENVAR";
        case PRTE_APP_PSET_NAME:
            return "PRTE_APP_PSET_NAME";

        case PRTE_NODE_USERNAME:
            return "NODE-USERNAME";
        case PRTE_NODE_PORT:
            return "NODE-PORT";
        case PRTE_NODE_LAUNCH_ID:
            return "NODE-LAUNCHID";
        case PRTE_NODE_HOSTID:
            return "NODE-HOSTID";
        case PRTE_NODE_SERIAL_NUMBER:
            return "NODE-SERIAL-NUM";
        case PRTE_NODE_ADD_SLOTS:
            return "NODE-ADD-SLOTS";

        case PRTE_JOB_LAUNCH_MSG_SENT:
            return "JOB-LAUNCH-MSG-SENT";
        case PRTE_JOB_LAUNCH_MSG_RECVD:
            return "JOB-LAUNCH-MSG-RECVD";
        case PRTE_JOB_MAX_LAUNCH_MSG_RECVD:
            return "JOB-MAX-LAUNCH-MSG-RECVD";
        case PRTE_JOB_CKPT_STATE:
            return "JOB-CKPT-STATE";
        case PRTE_JOB_SNAPSHOT_REF:
            return "JOB-SNAPSHOT-REF";
        case PRTE_JOB_SNAPSHOT_LOC:
            return "JOB-SNAPSHOT-LOC";
        case PRTE_JOB_SNAPC_INIT_BAR:
            return "JOB-SNAPC-INIT-BARRIER-ID";
        case PRTE_JOB_SNAPC_FINI_BAR:
            return "JOB-SNAPC-FINI-BARRIER-ID";
        case PRTE_JOB_NUM_NONZERO_EXIT:
            return "JOB-NUM-NONZERO-EXIT";
        case PRTE_SPAWN_TIMEOUT_EVENT:
            return "SPAWN-TIMEOUT-EVENT";
        case PRTE_JOB_ABORTED_PROC:
            return "JOB-ABORTED-PROC";
        case PRTE_JOB_MAPPER:
            return "JOB-MAPPER";
        case PRTE_JOB_REDUCER:
            return "JOB-REDUCER";
        case PRTE_JOB_COMBINER:
            return "JOB-COMBINER";
        case PRTE_JOB_INDEX_ARGV:
            return "JOB-INDEX-ARGV";
        case PRTE_JOB_NO_VM:
            return "JOB-NO-VM";
        case PRTE_JOB_SPIN_FOR_DEBUG:
            return "JOB-SPIN-FOR-DEBUG";
        case PRTE_JOB_CONTINUOUS:
            return "JOB-CONTINUOUS";
        case PRTE_JOB_RECOVER_DEFINED:
            return "JOB-RECOVERY-DEFINED";
        case PRTE_JOB_NON_PRTE_JOB:
            return "JOB-NON-PRTE-JOB";
        case PRTE_JOB_STDOUT_TARGET:
            return "JOB-STDOUT-TARGET";
        case PRTE_JOB_POWER:
            return "JOB-POWER";
        case PRTE_JOB_MAX_FREQ:
            return "JOB-MAX_FREQ";
        case PRTE_JOB_MIN_FREQ:
            return "JOB-MIN_FREQ";
        case PRTE_JOB_GOVERNOR:
            return "JOB-FREQ-GOVERNOR";
        case PRTE_JOB_FAIL_NOTIFIED:
            return "JOB-FAIL-NOTIFIED";
        case PRTE_JOB_TERM_NOTIFIED:
            return "JOB-TERM-NOTIFIED";
        case PRTE_JOB_PEER_MODX_ID:
            return "JOB-PEER-MODX-ID";
        case PRTE_JOB_INIT_BAR_ID:
            return "JOB-INIT-BAR-ID";
        case PRTE_JOB_FINI_BAR_ID:
            return "JOB-FINI-BAR-ID";
        case PRTE_JOB_FWDIO_TO_TOOL:
            return "JOB-FWD-IO-TO-TOOL";
        case PRTE_JOB_LAUNCHED_DAEMONS:
            return "JOB-LAUNCHED-DAEMONS";
        case PRTE_JOB_REPORT_BINDINGS:
            return "JOB-REPORT-BINDINGS";
        case PRTE_JOB_CPUSET:
            return "JOB-CPUSET";
        case PRTE_JOB_NOTIFICATIONS:
            return "JOB-NOTIFICATIONS";
        case PRTE_JOB_ROOM_NUM:
            return "JOB-ROOM-NUM";
        case PRTE_JOB_LAUNCH_PROXY:
            return "JOB-LAUNCH-PROXY";
        case PRTE_JOB_NSPACE_REGISTERED:
            return "JOB-NSPACE-REGISTERED";
        case PRTE_JOB_FIXED_DVM:
            return "PRTE-JOB-FIXED-DVM";
        case PRTE_JOB_DVM_JOB:
            return "PRTE-JOB-DVM-JOB";
        case PRTE_JOB_CANCELLED:
            return "PRTE-JOB-CANCELLED";
        case PRTE_JOB_OUTPUT_TO_FILE:
            return "PRTE-JOB-OUTPUT-TO-FILE";
        case PRTE_JOB_MERGE_STDERR_STDOUT:
            return "PRTE-JOB-MERGE-STDERR-STDOUT";
        case PRTE_JOB_TAG_OUTPUT:
            return "PRTE-JOB-TAG-OUTPUT";
        case PRTE_JOB_RANK_OUTPUT:
            return "PRTE-JOB-RANK-OUTPUT";
        case PRTE_JOB_TIMESTAMP_OUTPUT:
            return "PRTE-JOB-TIMESTAMP-OUTPUT";
        case PRTE_JOB_MULTI_DAEMON_SIM:
            return "PRTE_JOB_MULTI_DAEMON_SIM";
        case PRTE_JOB_NOTIFY_COMPLETION:
            return "PRTE_JOB_NOTIFY_COMPLETION";
        case PRTE_JOB_TRANSPORT_KEY:
            return "PRTE_JOB_TRANSPORT_KEY";
        case PRTE_JOB_INFO_CACHE:
            return "PRTE_JOB_INFO_CACHE";
        case PRTE_JOB_SILENT_TERMINATION:
            return "PRTE_JOB_SILENT_TERMINATION";
        case PRTE_JOB_SET_ENVAR:
            return "PRTE_JOB_SET_ENVAR";
        case PRTE_JOB_UNSET_ENVAR:
            return "PRTE_JOB_UNSET_ENVAR";
        case PRTE_JOB_PREPEND_ENVAR:
            return "PRTE_JOB_PREPEND_ENVAR";
        case PRTE_JOB_APPEND_ENVAR:
            return "PRTE_JOB_APPEND_ENVAR";
        case PRTE_JOB_ADD_ENVAR:
            return "PRTE_APP_ADD_ENVAR";
        case PRTE_JOB_APP_SETUP_DATA:
            return "PRTE_JOB_APP_SETUP_DATA";
        case PRTE_JOB_OUTPUT_TO_DIRECTORY:
            return "PRTE_JOB_OUTPUT_TO_DIRECTORY";
        case PRTE_JOB_STOP_ON_EXEC:
            return "JOB_STOP_ON_EXEC";
        case PRTE_JOB_SPAWN_NOTIFIED:
            return "JOB_SPAWN_NOTIFIED";
        case PRTE_JOB_DISPLAY_MAP:
            return "DISPLAY_JOB_MAP";
        case PRTE_JOB_DISPLAY_DEVEL_MAP:
            return "DISPLAY_DEVEL_JOB_MAP";
        case PRTE_JOB_DISPLAY_TOPO:
            return "DISPLAY_TOPOLOGY";
        case PRTE_JOB_DISPLAY_ALLOC:
            return "DISPLAY_ALLOCATION";
        case PRTE_JOB_DO_NOT_LAUNCH:
            return "DO_NOT_LAUNCH";
        case PRTE_JOB_XML_OUTPUT:
            return "XML_OUTPUT";
        case PRTE_JOB_TIMEOUT:
            return "JOB_TIMEOUT";
        case PRTE_JOB_STACKTRACES:
            return "JOB_STACKTRACES";
        case PRTE_JOB_REPORT_STATE:
            return "JOB_REPORT_STATE";
        case PRTE_JOB_TIMEOUT_EVENT:
            return "JOB_TIMEOUT_EVENT";
        case PRTE_JOB_TRACE_TIMEOUT_EVENT:
            return "JOB_TRACE_TIMEOUT_EVENT";
        case PRTE_JOB_INHERIT:
            return "JOB_INHERIT";
        case PRTE_JOB_PES_PER_PROC:
            return "JOB_PES_PER_PROC";
        case PRTE_JOB_DIST_DEVICE:
            return "JOB_DIST_DEVICE";
        case PRTE_JOB_HWT_CPUS:
            return "JOB_HWT_CPUS";
        case PRTE_JOB_CORE_CPUS:
            return "JOB_CORE_CPUS";
        case PRTE_JOB_PPR:
            return "JOB_PPR";
        case PRTE_JOB_NOINHERIT:
            return "JOB_NOINHERIT";
        case PRTE_JOB_FILE:
            return "JOB-FILE";
        case PRTE_JOB_DO_NOT_RESOLVE:
            return "DO-NOT-RESOLVE";
        case PRTE_JOB_DEBUG_TARGET:
            return "DEBUG-TARGET";
        case PRTE_JOB_DEBUG_DAEMONS_PER_NODE:
            return "DEBUG-DAEMONS-PER-NODE";
        case PRTE_JOB_DEBUG_DAEMONS_PER_PROC:
            return "DEBUG-DAEMONS-PER-PROC";
        case PRTE_JOB_STOP_IN_INIT:
            return "STOP-IN-INIT";
        case PRTE_JOB_STOP_IN_APP:
            return "STOP-IN-APP";
        case PRTE_JOB_ENVARS_HARVESTED:
            return "ENVARS-HARVESTED";
        case PRTE_JOB_OUTPUT_NOCOPY:
            return "DO-NOT-COPY-OUTPUT";
        case PRTE_SPAWN_TIMEOUT:
            return "SPAWN-TIMEOUT";
        case PRTE_JOB_RAW_OUTPUT:
            return "DO-NOT-BUFFER-OUTPUT";
        case PRTE_JOB_EXEC_AGENT:
            return "EXEC-AGENT";
        case PRTE_JOB_NOAGG_HELP:
            return "DO-NOT-AGGREGATE-HELP";
        case PRTE_JOB_COLOCATE_PROCS:
            return "COLOCATE PROCS";
        case PRTE_JOB_COLOCATE_NPERPROC:
            return "NUM PROCS TO COLOCATE PER PROC";
        case PRTE_JOB_COLOCATE_NPERNODE:
            return "NUM PROCS TO COLOCATE PER NODE";
        case PRTE_JOB_TAG_OUTPUT_DETAILED:
            return "DETAILED OUTPUT TAG";
        case PRTE_JOB_TAG_OUTPUT_FULLNAME:
            return "FULL NSPACE IN OUTPUT TAG";
        case PRTE_JOB_ERROR_NONZERO_EXIT:
            return "ERROR IF NONZERO EXIT";
        case PRTE_JOB_CONTROLS:
            return "JOB CONTROLS";
        case PRTE_JOB_SHOW_PROGRESS:
            return "SHOW LAUNCH PROGRESS";
        case PRTE_JOB_RECOVERABLE:
            return "JOB IS RECOVERABLE";
        case PRTE_JOB_NOTIFY_ERRORS:
            return "NOTIFY ERRORS";
        case PRTE_JOB_AUTORESTART:
            return "AUTORESTART";
        case PRTE_JOB_OUTPUT_PROCTABLE:
            return "OUTPUT PROCTABLE";
        case PRTE_JOB_DISPLAY_PROCESSORS:
            return "DISPLAY PROCESSORS";
        case PRTE_JOB_DISPLAY_PARSEABLE_OUTPUT:
            return "DISPLAY PARSEABLE OUTPUT";
        case PRTE_JOB_EXTEND_DVM:
            return "EXTEND DVM";

        case PRTE_PROC_NOBARRIER:
            return "PROC-NOBARRIER";
        case PRTE_PROC_PRIOR_NODE:
            return "PROC-PRIOR-NODE";
        case PRTE_PROC_NRESTARTS:
            return "PROC-NUM-RESTARTS";
        case PRTE_PROC_RESTART_TIME:
            return "PROC-RESTART-TIME";
        case PRTE_PROC_FAST_FAILS:
            return "PROC-FAST-FAILS";
        case PRTE_PROC_CKPT_STATE:
            return "PROC-CKPT-STATE";
        case PRTE_PROC_SNAPSHOT_REF:
            return "PROC-SNAPHOT-REF";
        case PRTE_PROC_SNAPSHOT_LOC:
            return "PROC-SNAPSHOT-LOC";
        case PRTE_PROC_NODENAME:
            return "PROC-NODENAME";
        case PRTE_PROC_CGROUP:
            return "PROC-CGROUP";
        case PRTE_PROC_NBEATS:
            return "PROC-NBEATS";

        case PRTE_RML_TRANSPORT_TYPE:
            return "RML-TRANSPORT-TYPE";
        case PRTE_RML_PROTOCOL_TYPE:
            return "RML-PROTOCOL-TYPE";
        case PRTE_RML_CONDUIT_ID:
            return "RML-CONDUIT-ID";
        case PRTE_RML_INCLUDE_COMP_ATTRIB:
            return "RML-INCLUDE";
        case PRTE_RML_EXCLUDE_COMP_ATTRIB:
            return "RML-EXCLUDE";
        case PRTE_RML_TRANSPORT_ATTRIB:
            return "RML-TRANSPORT";
        case PRTE_RML_QUALIFIER_ATTRIB:
            return "RML-QUALIFIER";
        case PRTE_RML_PROVIDER_ATTRIB:
            return "RML-DESIRED-PROVIDERS";
        case PRTE_RML_PROTOCOL_ATTRIB:
            return "RML-DESIRED-PROTOCOLS";
        case PRTE_RML_ROUTED_ATTRIB:
            return "RML-DESIRED-ROUTED-MODULES";
        default:
            pmix_snprintf(unknownkey, 180, "UNKNOWN-KEY: %d", key);
            return unknownkey;
        }
    }

    /* see if one of the converters can handle it */
    for (i = 0; i < MAX_CONVERTERS; ++i) {
        if (0 != converters[i].init) {
            if (converters[i].key_base < key && key < converters[i].key_max) {
                return converters[i].converter(key);
            }
        }
    }

    /* get here if nobody know what to do */
    pmix_snprintf(unknownkey, 180, "UNKNOWN-KEY: %d", key);
    return unknownkey;
}

int prte_attr_load(prte_attribute_t *kv, void *data, pmix_data_type_t type)
{
    pmix_byte_object_t *boptr;
    struct timeval *tv;
    pmix_envar_t *envar;
    pmix_status_t rc;

    kv->data.type = type;
    if (NULL == data) {
        /* if the type is BOOL, then the user wanted to
         * use the presence of the attribute to indicate
         * "true" - so let's mark it that way just in
         * case a subsequent test looks for the value */
        if (PMIX_BOOL == type) {
            kv->data.data.flag = true;
        } else {
            /* otherwise, check to see if this type has storage
             * that is already allocated, and free it if so */
            if (PMIX_STRING == type && NULL != kv->data.data.string) {
                free(kv->data.data.string);
            } else if (PMIX_BYTE_OBJECT == type && NULL != kv->data.data.bo.bytes) {
                free(kv->data.data.bo.bytes);
            }
            /* just set the fields to zero */
            memset(&kv->data.data, 0, sizeof(kv->data.data));
        }
        return PRTE_SUCCESS;
    }

    switch (type) {
    case PMIX_BOOL:
        kv->data.data.flag = *(bool *) (data);
        break;
    case PMIX_BYTE:
        kv->data.data.byte = *(uint8_t *) (data);
        break;
    case PMIX_STRING:
        if (NULL != kv->data.data.string) {
            free(kv->data.data.string);
        }
        kv->data.data.string = strdup((const char *) data);
        break;
    case PMIX_SIZE:
        kv->data.data.size = *(size_t *) (data);
        break;
    case PMIX_PID:
        kv->data.data.pid = *(pid_t *) (data);
        break;

    case PMIX_INT:
        kv->data.data.integer = *(int *) (data);
        break;
    case PMIX_INT8:
        kv->data.data.int8 = *(int8_t *) (data);
        break;
    case PMIX_INT16:
        kv->data.data.int16 = *(int16_t *) (data);
        break;
    case PMIX_INT32:
        kv->data.data.int32 = *(int32_t *) (data);
        break;
    case PMIX_INT64:
        kv->data.data.int64 = *(int64_t *) (data);
        break;

    case PMIX_UINT:
        kv->data.data.uint = *(unsigned int *) (data);
        break;
    case PMIX_UINT8:
        kv->data.data.uint8 = *(uint8_t *) (data);
        break;
    case PMIX_UINT16:
        kv->data.data.uint16 = *(uint16_t *) (data);
        break;
    case PMIX_UINT32:
        kv->data.data.uint32 = *(uint32_t *) data;
        break;
    case PMIX_UINT64:
        kv->data.data.uint64 = *(uint64_t *) (data);
        break;

    case PMIX_BYTE_OBJECT:
        if (NULL != kv->data.data.bo.bytes) {
            free(kv->data.data.bo.bytes);
        }
        boptr = (pmix_byte_object_t *) data;
        if (NULL != boptr && NULL != boptr->bytes && 0 < boptr->size) {
            kv->data.data.bo.bytes = (char *) malloc(boptr->size);
            memcpy(kv->data.data.bo.bytes, boptr->bytes, boptr->size);
            kv->data.data.bo.size = boptr->size;
        } else {
            kv->data.data.bo.bytes = NULL;
            kv->data.data.bo.size = 0;
        }
        break;

    case PMIX_FLOAT:
        kv->data.data.fval = *(float *) (data);
        break;

    case PMIX_TIMEVAL:
        tv = (struct timeval *) data;
        kv->data.data.tv.tv_sec = tv->tv_sec;
        kv->data.data.tv.tv_usec = tv->tv_usec;
        break;

    case PMIX_POINTER:
        kv->data.data.ptr = data;
        break;

    case PMIX_PROC_RANK:
        kv->data.data.rank = *(pmix_rank_t *) data;
        break;

    case PMIX_PROC_NSPACE:
        PMIX_PROC_CREATE(kv->data.data.proc, 1);
        if (NULL == kv->data.data.proc) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        PMIX_LOAD_NSPACE(kv->data.data.proc->nspace, (char *) data);
        break;

    case PMIX_PROC:
        PMIX_PROC_CREATE(kv->data.data.proc, 1);
        if (NULL == kv->data.data.proc) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        PMIX_XFER_PROCID(kv->data.data.proc, (pmix_proc_t *) data);
        break;

    case PMIX_ENVAR:
        PMIX_ENVAR_CONSTRUCT(&kv->data.data.envar);
        envar = (pmix_envar_t *) data;
        if (NULL != envar) {
            if (NULL != envar->envar) {
                kv->data.data.envar.envar = strdup(envar->envar);
            }
            if (NULL != envar->value) {
                kv->data.data.envar.value = strdup(envar->value);
            }
            kv->data.data.envar.separator = envar->separator;
        }
        break;

    case PMIX_DATA_ARRAY:
        rc = PMIx_Data_copy((void**)&kv->data.data.darray, data, PMIX_DATA_ARRAY);
        return rc;
        break;

    default:
        PRTE_ERROR_LOG(PRTE_ERR_NOT_SUPPORTED);
        return PRTE_ERR_NOT_SUPPORTED;
    }
    return PRTE_SUCCESS;
}

int prte_attr_unload(prte_attribute_t *kv, void **data, pmix_data_type_t type)
{
    pmix_byte_object_t *boptr;
    pmix_envar_t *envar;
    pmix_data_array_t *darray;
    pmix_status_t rc;
    pmix_data_type_t pointers[] = {
        PMIX_STRING,
        PMIX_BYTE_OBJECT,
        PMIX_POINTER,
        PMIX_PROC_NSPACE,
        PMIX_PROC,
        PMIX_ENVAR,
        PMIX_DATA_ARRAY,
        PMIX_UNDEF};
    int n;
    bool found = false;

    if (type != kv->data.type) {
        return PRTE_ERR_TYPE_MISMATCH;
    }
    if (NULL == data) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }
    /* if they didn't give us a storage address
     * and the data type isn't one where we can
     * create storage, then this is an error */
    for (n = 0; PMIX_UNDEF != pointers[n]; n++) {
        if (type == pointers[n]) {
            found = true;
            break;
        }
    }
    if (!found && NULL == *data) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }

    switch (type) {
    case PMIX_BOOL:
        memcpy(*data, &kv->data.data.flag, sizeof(bool));
        break;
    case PMIX_BYTE:
        memcpy(*data, &kv->data.data.byte, sizeof(uint8_t));
        break;
    case PMIX_STRING:
        if (NULL != kv->data.data.string) {
            *data = strdup(kv->data.data.string);
        } else {
            *data = NULL;
        }
        break;
    case PMIX_SIZE:
        memcpy(*data, &kv->data.data.size, sizeof(size_t));
        break;
    case PMIX_PID:
        memcpy(*data, &kv->data.data.pid, sizeof(pid_t));
        break;

    case PMIX_INT:
        memcpy(*data, &kv->data.data.integer, sizeof(int));
        break;
    case PMIX_INT8:
        memcpy(*data, &kv->data.data.int8, sizeof(int8_t));
        break;
    case PMIX_INT16:
        memcpy(*data, &kv->data.data.int16, sizeof(int16_t));
        break;
    case PMIX_INT32:
        memcpy(*data, &kv->data.data.int32, sizeof(int32_t));
        break;
    case PMIX_INT64:
        memcpy(*data, &kv->data.data.int64, sizeof(int64_t));
        break;

    case PMIX_UINT:
        memcpy(*data, &kv->data.data.uint, sizeof(unsigned int));
        break;
    case PMIX_UINT8:
        memcpy(*data, &kv->data.data.uint8, 1);
        break;
    case PMIX_UINT16:
        memcpy(*data, &kv->data.data.uint16, 2);
        break;
    case PMIX_UINT32:
        memcpy(*data, &kv->data.data.uint32, 4);
        break;
    case PMIX_UINT64:
        memcpy(*data, &kv->data.data.uint64, 8);
        break;

    case PMIX_BYTE_OBJECT:
        boptr = (pmix_byte_object_t *) malloc(sizeof(pmix_byte_object_t));
        if (NULL == boptr) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        if (NULL != kv->data.data.bo.bytes && 0 < kv->data.data.bo.size) {
            boptr->bytes = (char *) malloc(kv->data.data.bo.size);
            memcpy(boptr->bytes, kv->data.data.bo.bytes, kv->data.data.bo.size);
            boptr->size = kv->data.data.bo.size;
        } else {
            boptr->bytes = NULL;
            boptr->size = 0;
        }
        *data = boptr;
        break;

    case PMIX_FLOAT:
        memcpy(*data, &kv->data.data.fval, sizeof(float));
        break;

    case PMIX_TIMEVAL:
        memcpy(*data, &kv->data.data.tv, sizeof(struct timeval));
        break;

    case PMIX_POINTER:
        *data = kv->data.data.ptr;
        break;

    case PMIX_PROC_RANK:
        memcpy(*data, &kv->data.data.rank, sizeof(pmix_rank_t));
        break;

    case PMIX_PROC_NSPACE:
        PMIX_PROC_CREATE(*data, 1);
        if (NULL == *data) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        memcpy(*data, kv->data.data.proc->nspace, sizeof(pmix_nspace_t));
        break;

    case PMIX_PROC:
        PMIX_PROC_CREATE(*data, 1);
        if (NULL == *data) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        memcpy(*data, kv->data.data.proc, sizeof(pmix_proc_t));
        break;

    case PMIX_ENVAR:
        PMIX_ENVAR_CREATE(envar, 1);
        if (NULL == envar) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        if (NULL != kv->data.data.envar.envar) {
            envar->envar = strdup(kv->data.data.envar.envar);
        }
        if (NULL != kv->data.data.envar.value) {
            envar->value = strdup(kv->data.data.envar.value);
        }
        envar->separator = kv->data.data.envar.separator;
        *data = envar;
        break;

    case PMIX_DATA_ARRAY:
        rc = PMIx_Data_copy((void**)&darray, kv->data.data.darray, PMIX_DATA_ARRAY);
        if (PMIX_SUCCESS != rc) {
            *data = NULL;
            return prte_pmix_convert_status(rc);
        }
        *data = darray;
        break;

    default:
        PRTE_ERROR_LOG(PRTE_ERR_NOT_SUPPORTED);
        return PRTE_ERR_NOT_SUPPORTED;
    }
    return PRTE_SUCCESS;
}

char* prte_print_proc_flags(struct prte_proc_t *ptr)
{
    prte_proc_t *p = (prte_proc_t*)ptr;
    char **tmp = NULL;
    char *ans;

    // start with the proc name
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, PRTE_NAME_PRINT(&p->name));
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, ": ");

    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_ALIVE)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "ALIVE");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_ABORT)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "ABORT");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_UPDATED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "UPDATED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_LOCAL)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "LOCAL");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_REPORTED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "REPORTED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_REG)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "REGISTERED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_HAS_DEREG)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "DEREGISTERED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_AS_MPI)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "MPI");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_IOF_COMPLETE)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "IOF-COMPLETE");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_WAITPID)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "WAITPID");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_RECORDED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "RECORDED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_DATA_IN_SM)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "DATA-IN-SM");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_DATA_RECVD)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "DATA-RECVD");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_SM_ACCESS)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "SM-ACCESS");
    }
    if (PRTE_FLAG_TEST(p, PRTE_PROC_FLAG_TERM_REPORTED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "TERMINATED");
    }
    ans = PMIX_ARGV_JOIN_COMPAT(tmp, '|');
    PMIX_ARGV_FREE_COMPAT(tmp);
    return ans;
}

char* prte_print_node_flags(struct prte_node_t *ptr)
{
    prte_node_t *p = (prte_node_t*)ptr;
    char **tmp = NULL;
    char *ans;

    // start with the node name
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, p->name);
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, ": ");

    if (PRTE_FLAG_TEST(p, PRTE_NODE_FLAG_DAEMON_LAUNCHED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "DAEMON-LAUNCHED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_NODE_FLAG_LOC_VERIFIED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "LOCATION");
    }
    if (PRTE_FLAG_TEST(p, PRTE_NODE_FLAG_OVERSUBSCRIBED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "OVERSUBSCRIBED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_NODE_FLAG_MAPPED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "MAPPED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_NODE_FLAG_SLOTS_GIVEN)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "SLOTS-GIVEN");
    }
    if (PRTE_FLAG_TEST(p, PRTE_NODE_NON_USABLE)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "NONUSABLE");
    }
    ans = PMIX_ARGV_JOIN_COMPAT(tmp, '|');
    PMIX_ARGV_FREE_COMPAT(tmp);
    return ans;
}

char* prte_print_job_flags(struct prte_job_t *ptr)
{
    prte_job_t *p = (prte_job_t*)ptr;
    char **tmp = NULL;
    char *ans;

    // start with the job name
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, PRTE_JOBID_PRINT(p->nspace));
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, ": ");

    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_UPDATED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "UPDATED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_RESTARTED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "RESTARTED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_ABORTED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "ABORTED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_FORWARD_OUTPUT)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "FORWARD-OUTPUT");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_DO_NOT_MONITOR)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "DO-NOT-MONITOR");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_FORWARD_COMM)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "FWD-COM");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_RESTART)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "RESTART");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_PROCS_MIGRATING)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "MIGRATING");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_OVERSUBSCRIBED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "OVERSUBSCRIBED");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_TOOL)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "TOOL");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_LAUNCHER)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "LAUNCHER");
    }
    if (PRTE_FLAG_TEST(p, PRTE_JOB_FLAG_ERR_REPORTED)) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, "ERROR-REPORTED");
    }
    ans = PMIX_ARGV_JOIN_COMPAT(tmp, '|');
    PMIX_ARGV_FREE_COMPAT(tmp);
    return ans;
}
