/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2024 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "src/include/pmix_config.h"

#include "include/pmix.h"
#include "pmix_common.h"
#include "include/pmix_server.h"

#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/gds/base/base.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_hash.h"

#include "src/common/pmix_attributes.h"
#include "src/include/pmix_dictionary.h"

static bool initialized = false;
static pmix_list_t client_attrs;
static pmix_list_t server_attrs;
static pmix_list_t host_attrs;
static pmix_list_t tool_attrs;

typedef struct {
    char *function;
    char **attrs;
} pmix_attr_init_t;

typedef struct {
    pmix_list_item_t super;
    char *function;
    char **attrs;
} pmix_attribute_trk_t;

static void atrkcon(pmix_attribute_trk_t *p)
{
    p->function = NULL;
    p->attrs = NULL;
}
static void atrkdes(pmix_attribute_trk_t *p)
{
    if (NULL != p->function) {
        free(p->function);
    }
    PMIx_Argv_free(p->attrs);
}
static PMIX_CLASS_INSTANCE(pmix_attribute_trk_t, pmix_list_item_t, atrkcon, atrkdes);

PMIX_EXPORT void pmix_init_registered_attrs(void)
{
    size_t n;
    pmix_regattr_input_t *p;

    if (!initialized) {
        PMIX_CONSTRUCT(&client_attrs, pmix_list_t);
        PMIX_CONSTRUCT(&server_attrs, pmix_list_t);
        PMIX_CONSTRUCT(&host_attrs, pmix_list_t);
        PMIX_CONSTRUCT(&tool_attrs, pmix_list_t);

        /* cycle across the dictionary and load a hash
         * table with translations of key -> index */
        for (n=0; UINT32_MAX != pmix_dictionary[n].index; n++) {
            p = (pmix_regattr_input_t*)pmix_malloc(sizeof(pmix_regattr_input_t));
            p->index = pmix_dictionary[n].index;
            p->name = strdup(pmix_dictionary[n].name);
            p->string = strdup(pmix_dictionary[n].string);
            p->type = pmix_dictionary[n].type;
            p->description = PMIx_Argv_copy(pmix_dictionary[n].description);
            pmix_hash_register_key(p->index, p, &pmix_globals.keyindex);
        }
        initialized = true;
    }
}

static pmix_status_t process_reg(char *level, char *function, char **attrs)
{
    pmix_attribute_trk_t *fnptr;
    pmix_list_t *lst;

    /* select the list this will appear on */
    if (0 == strcmp(level, PMIX_CLIENT_ATTRIBUTES)) {
        lst = &client_attrs;
    } else if (0 == strcmp(level, PMIX_SERVER_ATTRIBUTES)) {
        lst = &server_attrs;
    } else if (0 == strcmp(level, PMIX_HOST_ATTRIBUTES)) {
        lst = &host_attrs;
    } else if (0 == strcmp(level, PMIX_TOOL_ATTRIBUTES)) {
        lst = &tool_attrs;
    } else {
        return PMIX_ERR_BAD_PARAM;
    }

    /* see if we already have this function */
    PMIX_LIST_FOREACH (fnptr, lst, pmix_attribute_trk_t) {
        if (0 == strcmp(function, fnptr->function)) {
            /* we already have this function at this level
             * so we must return an error */
            return PMIX_ERR_REPEAT_ATTR_REGISTRATION;
        }
    }

    fnptr = PMIX_NEW(pmix_attribute_trk_t);
    pmix_list_append(lst, &fnptr->super);
    fnptr->function = strdup(function);
    if (NULL != attrs) {
        fnptr->attrs = PMIx_Argv_copy(attrs);
    }
    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_Register_attributes(char *function, char *attrs[])
{
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    rc = process_reg(PMIX_HOST_ATTRIBUTES, function, attrs);
    PMIX_RELEASE_THREAD(&pmix_global_lock);
    return rc;
}

PMIX_EXPORT void pmix_release_registered_attrs(void)
{
    if (initialized) {
        PMIX_LIST_DESTRUCT(&client_attrs);
        PMIX_LIST_DESTRUCT(&server_attrs);
        PMIX_LIST_DESTRUCT(&host_attrs);
        PMIX_LIST_DESTRUCT(&tool_attrs);
   }
    initialized = false;
}

/* sadly, we cannot dynamically register our supported attributes
 * as that would require the user to first call the function whose
 * attributes they want to know about - which somewhat defeats the
 * purpose. Until someone comes up with a better solution, we will
 * manually maintain the list */
static pmix_attr_init_t client_fns[] = {
    {.function = "PMIx_Init",
     .attrs = (char *[]){"PMIX_GDS_MODULE",
                         "PMIX_EVENT_BASE",
                         "PMIX_HOSTNAME",
                         "PMIX_NODEID",
                         "PMIX_PROGRAMMING_MODEL",
                         "PMIX_MODEL_LIBRARY_NAME",
                         "PMIX_MODEL_LIBRARY_VERSION",
                         "PMIX_THREADING_MODEL",
                         "PMIX_NODE_INFO_ARRAY",
                         "PMIX_EXTERNAL_PROGRESS",
                         "PMIX_HOSTNAME_KEEP_FQDN",
                         "PMIX_TOPOLOGY2",
                         "PMIX_SERVER_URI",
                         "PMIX_DEBUG_STOP_IN_INIT",
                         "PMIX_IOF_TAG_OUTPUT",
                         "PMIX_TAG_OUTPUT",
                         "PMIX_IOF_RANK_OUTPUT",
                         "PMIX_IOF_TIMESTAMP_OUTPUT",
                         "PMIX_TIMESTAMP_OUTPUT",
                         "PMIX_IOF_XML_OUTPUT",
                         "PMIX_IOF_OUTPUT_TO_FILE",
                         "PMIX_OUTPUT_TO_FILE",
                         "PMIX_IOF_OUTPUT_TO_DIRECTORY",
                         "PMIX_OUTPUT_TO_DIRECTORY",
                         "PMIX_IOF_FILE_ONLY",
                         "PMIX_OUTPUT_NOCOPY",
                         "PMIX_IOF_MERGE_STDERR_STDOUT",
                         "PMIX_MERGE_STDERR_STDOUT",
                         "PMIX_IOF_LOCAL_OUTPUT",
                         "PMIX_IOF_FILE_PATTERN",
                         NULL}},
    {.function = "PMIx_Finalize", .attrs = (char *[]){"PMIX_EMBED_BARRIER", NULL}},
    {.function = "PMIx_Initialized", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Abort", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Store_internal", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Put", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Commit", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Fence", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Fence_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Get",
     .attrs = (char *[]){"PMIX_NODE_INFO",
                         "PMIX_HOSTNAME",
                         "PMIX_NODEID",
                         "PMIX_APP_INFO",
                         "PMIX_APPNUM",
                         "PMIX_SESSION_INFO",
                         "PMIX_GET_REFRESH_CACHE",
                         "PMIX_OPTIONAL",
                         "PMIX_DATA_SCOPE",
                         NULL}},
    {.function = "PMIx_Get_nb",
     .attrs = (char *[]){"PMIX_NODE_INFO",
                         "PMIX_HOSTNAME",
                         "PMIX_NODEID",
                         "PMIX_APP_INFO",
                         "PMIX_APPNUM",
                         "PMIX_SESSION_INFO",
                         "PMIX_GET_REFRESH_CACHE",
                         "PMIX_OPTIONAL",
                         "PMIX_DATA_SCOPE",
                         NULL}},
    {.function = "PMIx_Publish", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Publish_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Lookup", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Lookup_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Unpublish", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Unpublish_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Spawn", .attrs = (char *[]){"PMIX_SETUP_APP_ENVARS", NULL}},
    {.function = "PMIx_Spawn_nb", .attrs = (char *[]){"PMIX_SETUP_APP_ENVARS", NULL}},
    {.function = "PMIx_Connect", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Connect_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Disconnect", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Disconnect_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Resolve_peers", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Resolve_nodes", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Query_info",
     .attrs = (char *[]){"PMIX_QUERY_ATTRIBUTE_SUPPORT",
                         "PMIX_QUERY_AVAIL_SERVERS",
                         "PMIX_QUERY_REFRESH_CACHE",
                         "PMIX_QUERY_SUPPORTED_KEYS",
                         "PMIX_QUERY_SUPPORTED_QUALIFIERS",
                         NULL}},
    {.function = "PMIx_Query_info_nb",
     .attrs = (char *[]){"PMIX_QUERY_ATTRIBUTE_SUPPORT",
                         "PMIX_QUERY_AVAIL_SERVERS",
                         "PMIX_QUERY_REFRESH_CACHE",
                         "PMIX_QUERY_SUPPORTED_KEYS",
                         "PMIX_QUERY_SUPPORTED_QUALIFIERS",
                         NULL}},
    {.function = "PMIx_Log",
     .attrs = (char *[]){"PMIX_LOG_GENERATE_TIMESTAMP", "PMIX_LOG_SOURCE", NULL}},
    {.function = "PMIx_Log_nb",
     .attrs = (char *[]){"PMIX_LOG_GENERATE_TIMESTAMP", "PMIX_LOG_SOURCE", NULL}},
    {.function = "PMIx_Allocation_request", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Allocation_request_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Job_control", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Job_control_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Process_monitor", .attrs = (char *[]){"PMIX_SEND_HEARTBEAT", NULL}},
    {.function = "PMIx_Process_monitor_nb", .attrs = (char *[]){"PMIX_SEND_HEARTBEAT", NULL}},
    {.function = "PMIx_Get_credential", .attrs = (char *[]){"PMIX_CRED_TYPE", NULL}},
    {.function = "PMIx_Get_credential_nb", .attrs = (char *[]){"PMIX_CRED_TYPE", NULL}},
    {.function = "PMIx_Validate_credential", .attrs = (char *[]){"PMIX_CRED_TYPE", NULL}},
    {.function = "PMIx_Validate_credential_nb", .attrs = (char *[]){"PMIX_CRED_TYPE", NULL}},
    {.function = "PMIx_Group_construct", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_construct_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_invite", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_invite_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_join", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_join_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_leave", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_leave_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_destruct", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_destruct_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Register_event_handler",
     .attrs = (char *[]){"PMIX_EVENT_HDLR_FIRST",
                         "PMIX_EVENT_HDLR_LAST",
                         "PMIX_EVENT_HDLR_PREPEND",
                         "PMIX_EVENT_HDLR_APPEND",
                         "PMIX_EVENT_HDLR_NAME",
                         "PMIX_EVENT_RETURN_OBJECT",
                         "PMIX_EVENT_HDLR_FIRST_IN_CATEGORY",
                         "PMIX_EVENT_HDLR_LAST_IN_CATEGORY",
                         "PMIX_EVENT_HDLR_BEFORE",
                         "PMIX_EVENT_HDLR_AFTER",
                         "PMIX_RANGE",
                         "PMIX_EVENT_CUSTOM_RANGE",
                         "PMIX_EVENT_AFFECTED_PROC",
                         "PMIX_EVENT_AFFECTED_PROCS",
                         NULL}},
    {.function = "PMIx_Deregister_event_handler", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Notify_event",
     .attrs = (char *[]){"PMIX_EVENT_NON_DEFAULT",
                         "PMIX_EVENT_CUSTOM_RANGE",
                         "PMIX_EVENT_AFFECTED_PROC",
                         "PMIX_EVENT_AFFECTED_PROCS",
                         NULL}},
    {.function = "PMIx_Error_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Proc_state_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Scope_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Persistence_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Data_range_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Info_directives_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Data_type_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Alloc_directive_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_IOF_channel_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Job_state_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Get_attribute_string", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Get_attribute_name", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Get_version", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Data_pack", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Data_unpack", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Data_copy", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Data_print", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Data_copy_payload", .attrs = (char *[]){"N/A", NULL}},
    {.function = ""}};

/*****    REGISTER CLIENT ATTRS    *****/
static bool client_attrs_regd = false;

PMIX_EXPORT pmix_status_t pmix_register_client_attrs(void)
{
    size_t n;
    pmix_status_t rc = PMIX_SUCCESS;

    if (client_attrs_regd) {
        return PMIX_SUCCESS;
    }
    client_attrs_regd = true;

    for (n = 0; 0 != strlen(client_fns[n].function); n++) {
        rc = process_reg(PMIX_CLIENT_ATTRIBUTES, client_fns[n].function, client_fns[n].attrs);
        if (PMIX_SUCCESS != rc) {
            break;
        }
    }

    return rc;
}

static pmix_attr_init_t server_fns[] = {
    {.function = "PMIx_server_init",
     .attrs = (char *[]){"PMIX_SERVER_GATEWAY",
                         "PMIX_SERVER_SCHEDULER",
                         "PMIX_SERVER_TMPDIR",
                         "PMIX_SYSTEM_TMPDIR",
                         "PMIX_SERVER_NSPACE",
                         "PMIX_SERVER_RANK",
                         "PMIX_SERVER_SHARE_TOPOLOGY",
                         "PMIX_TOPOLOGY2",
                         "PMIX_TOPOLOGY",
                         "PMIX_IOF_LOCAL_OUTPUT",
                         "PMIX_GDS_MODULE",
                         "PMIX_EVENT_BASE",
                         "PMIX_HOSTNAME",
                         "PMIX_NODEID",
                         "PMIX_TCP_IF_INCLUDE",
                         "PMIX_TCP_IF_EXCLUDE",
                         "PMIX_TCP_IPV4_PORT",
                         "PMIX_TCP_IPV6_PORT",
                         "PMIX_TCP_DISABLE_IPV4",
                         "PMIX_TCP_DISABLE_IPV6",
                         "PMIX_SERVER_REMOTE_CONNECTIONS",
                         "PMIX_TCP_REPORT_URI",
                         "PMIX_SERVER_SESSION_SUPPORT",
                         "PMIX_SERVER_SYSTEM_SUPPORT",
                         "PMIX_SERVER_TOOL_SUPPORT",
                         "PMIX_LAUNCHER_RENDEZVOUS_FILE",
                         "PMIX_SERVER_ENABLE_MONITORING",
                         "PMIX_IOF_TAG_OUTPUT",
                         "PMIX_TAG_OUTPUT",
                         "PMIX_IOF_RANK_OUTPUT",
                         "PMIX_IOF_TIMESTAMP_OUTPUT",
                         "PMIX_TIMESTAMP_OUTPUT",
                         "PMIX_IOF_XML_OUTPUT",
                         "PMIX_IOF_OUTPUT_TO_FILE",
                         "PMIX_OUTPUT_TO_FILE",
                         "PMIX_IOF_OUTPUT_TO_DIRECTORY",
                         "PMIX_OUTPUT_TO_DIRECTORY",
                         "PMIX_IOF_FILE_ONLY",
                         "PMIX_OUTPUT_NOCOPY",
                         "PMIX_IOF_MERGE_STDERR_STDOUT",
                         "PMIX_MERGE_STDERR_STDOUT",
                         "PMIX_IOF_LOCAL_OUTPUT",
                         "PMIX_IOF_FILE_PATTERN",
                        NULL}},
    {.function = "PMIx_server_finalize", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_generate_regex", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_generate_ppn", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_register_nspace", .attrs = (char *[]){"PMIX_REGISTER_NODATA", NULL}},
    {.function = "PMIx_server_deregister_nspace", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_register_client", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_deregister_client", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_setup_fork", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_dmodex_request", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_setup_application",
     .attrs = (char *[]){"PMIX_SETUP_APP_ENVARS",
                         "PMIX_SETUP_APP_ALL",
                         "PMIX_SETUP_APP_NONENVARS",
                         "PMIX_ALLOC_FABRIC",
                         "PMIX_ALLOC_FABRIC_SEC_KEY",
                         "PMIX_ALLOC_FABRIC_ID",
                         "PMIX_ALLOC_FABRIC_TYPE",
                         "PMIX_ALLOC_FABRIC_PLANE",
                         "PMIX_ALLOC_FABRIC_ENDPTS",
                         NULL}},
    {.function = "PMIx_server_setup_local_support", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_server_IOF_deliver", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_server_collect_inventory", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_server_deliver_inventory", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Register_attributes", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_register_fabric",
     .attrs = (char *[]){"PMIX_FABRIC_PLANE",
                         "PMIX_FABRIC_IDENTIFIER",
                         "PMIX_FABRIC_VENDOR",
                         NULL}},
    {.function = "PMIx_server_update_fabric", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_deregister_fabric", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_get_vertex_info", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_server_get_index",
     .attrs = (char *[]){"PMIX_HOSTNAME",
                         "PMIX_NODEID",
                         "PMIX_FABRIC_DEVICE_NAME",
                         "PMIX_FABRIC_DEVICE_VENDOR",
                         "PMIX_FABRIC_DEVICE_BUS_TYPE",
                         "PMIX_FABRIC_DEVICE_PCI_DEVID",
                         NULL}},
    {.function = ""}};

/*****    REGISTER SERVER ATTRS    *****/
static bool server_attrs_regd = false;

PMIX_EXPORT pmix_status_t pmix_register_server_attrs(void)
{
    pmix_status_t rc = PMIX_SUCCESS;
    size_t n;

    if (server_attrs_regd) {
        return PMIX_SUCCESS;
    }
    server_attrs_regd = true;

    for (n = 0; 0 != strlen(server_fns[n].function); n++) {
        rc = process_reg(PMIX_SERVER_ATTRIBUTES, server_fns[n].function, server_fns[n].attrs);
        if (PMIX_SUCCESS != rc) {
            break;
        }
    }

    return rc;
}

static pmix_attr_init_t tool_fns[]
    = {{.function = "PMIx_tool_init",
        .attrs = (char *[]){"PMIX_GDS_MODULE",
                            "PMIX_EVENT_BASE",
                            "PMIX_HOSTNAME",
                            "PMIX_NODEID",
                            "PMIX_TOOL_DO_NOT_CONNECT",
                            "PMIX_DEBUG_STOP_IN_INIT",
                            "PMIX_TOOL_NSPACE",
                            "PMIX_TOOL_RANK",
                            "PMIX_FWD_STDIN",
                            "PMIX_LAUNCHER",
                            "PMIX_SERVER_TMPDIR",
                            "PMIX_SYSTEM_TMPDIR",
                            "PMIX_TOOL_CONNECT_OPTIONAL",
                            "PMIX_RECONNECT_SERVER",
                            "PMIX_TOOL_ATTACHMENT_FILE",
                            "PMIX_CONNECT_MAX_RETRIES",
                            "PMIX_CONNECT_RETRY_DELAY",
                            "PMIX_CONNECT_TO_SYSTEM",
                            "PMIX_CONNECT_SYSTEM_FIRST",
                            "PMIX_SERVER_PIDINFO",
                            "PMIX_SERVER_NSPACE",
                            "PMIX_SERVER_URI",
                            "PMIX_TCP_IF_INCLUDE",
                            "PMIX_TCP_IF_EXCLUDE",
                            "PMIX_TCP_IPV4_PORT",
                            "PMIX_TCP_IPV6_PORT",
                            "PMIX_TCP_DISABLE_IPV4",
                            "PMIX_TCP_DISABLE_IPV6",
                            "PMIX_IOF_TAG_OUTPUT",
                            "PMIX_TAG_OUTPUT",
                            "PMIX_IOF_RANK_OUTPUT",
                            "PMIX_IOF_TIMESTAMP_OUTPUT",
                            "PMIX_TIMESTAMP_OUTPUT",
                            "PMIX_IOF_XML_OUTPUT",
                            "PMIX_IOF_OUTPUT_TO_FILE",
                            "PMIX_OUTPUT_TO_FILE",
                            "PMIX_IOF_OUTPUT_TO_DIRECTORY",
                            "PMIX_OUTPUT_TO_DIRECTORY",
                            "PMIX_IOF_FILE_ONLY",
                            "PMIX_OUTPUT_NOCOPY",
                            "PMIX_IOF_MERGE_STDERR_STDOUT",
                            "PMIX_MERGE_STDERR_STDOUT",
                            "PMIX_IOF_LOCAL_OUTPUT",
                            "PMIX_IOF_FILE_PATTERN",
                            NULL}},
       {.function = "PMIx_tool_finalize", .attrs = (char *[]){"N/A", NULL}},
       {.function = "PMIx_tool_connect_to_server",
        .attrs = (char *[]){"PMIX_CONNECT_TO_SYSTEM",
                            "PMIX_CONNECT_SYSTEM_FIRST",
                            "PMIX_SERVER_PIDINFO",
                            "PMIX_SERVER_NSPACE",
                            "PMIX_SERVER_URI",
                            "PMIX_CONNECT_RETRY_DELAY",
                            "PMIX_CONNECT_MAX_RETRIES",
                            "PMIX_RECONNECT_SERVER",
                            "PMIX_TOOL_ATTACHMENT_FILE",
                            NULL}},
       {.function = "PMIx_IOF_pull", .attrs = (char *[]){"NONE", NULL}},
       {.function = "PMIx_IOF_deregister", .attrs = (char *[]){"NONE", NULL}},
       {.function = "PMIx_IOF_push",
        .attrs = (char *[]){"PMIX_IOF_PUSH_STDIN",
                            "PMIX_IOF_COMPLETE",
                            NULL}},
       {.function = ""}};

/*****    REGISTER TOOL ATTRS    *****/
static bool tool_attrs_regd = false;

PMIX_EXPORT pmix_status_t pmix_register_tool_attrs(void)
{
    pmix_status_t rc = PMIX_SUCCESS;
    size_t n;

    if (tool_attrs_regd) {
        return PMIX_SUCCESS;
    }
    tool_attrs_regd = true;

    for (n = 0; 0 != strlen(tool_fns[n].function); n++) {
        rc = process_reg(PMIX_TOOL_ATTRIBUTES, tool_fns[n].function, tool_fns[n].attrs);
        if (PMIX_SUCCESS != rc) {
            break;
        }
    }

    return rc;
}

/*****   PROCESS QUERY ATTRS    *****/
static void _get_attrs(pmix_list_t *lst, pmix_info_t *info, pmix_list_t *attrs)
{
    pmix_attribute_trk_t *trk, *tptr;
    pmix_infolist_t *ip;
    pmix_data_array_t *darray;
    pmix_regattr_t *regarray;
    size_t m, nattr;
    char **fns;
    const pmix_regattr_input_t *dptr;

    /* the value in the info is a comma-delimited list of
     * functions whose attributes are being requested */
    fns = PMIx_Argv_split(info->value.data.string, ',');

    /* search the list for these functions */
    PMIX_LIST_FOREACH (tptr, attrs, pmix_attribute_trk_t) {
        trk = NULL;
        for (m = 0; NULL != fns[m] && NULL == trk; m++) {
            if (0 == strcmp(fns[m], tptr->function) || 0 == strcmp(fns[m], "all")) {
                trk = tptr;
                break;
            }
        }
        if (NULL == trk || NULL == trk->attrs) {
            /* function wasn't found - no attrs
             * registered for it */
            continue;
        }
        /* add the found attrs to the results */
        ip = PMIX_NEW(pmix_infolist_t);
        PMIX_LOAD_KEY(ip->info.key, tptr->function);
        /* create the data array to hold the results */
        nattr = PMIx_Argv_count(tptr->attrs);
        if (0 == nattr || (1 == nattr &&
                           (0 == strcmp("N/A", tptr->attrs[0]) ||
                            0 == strcmp("NONE", tptr->attrs[0])))) {
            nattr = 1;
            PMIX_DATA_ARRAY_CREATE(darray, nattr, PMIX_REGATTR);
            ip->info.value.type = PMIX_DATA_ARRAY;
            ip->info.value.data.darray = darray;
            regarray = (pmix_regattr_t *) darray->array;
            regarray[0].name = strdup("NONE");
        } else {
            PMIX_DATA_ARRAY_CREATE(darray, nattr, PMIX_REGATTR);
            ip->info.value.type = PMIX_DATA_ARRAY;
            ip->info.value.data.darray = darray;
            regarray = (pmix_regattr_t *) darray->array;
            for (m = 0; m < nattr; m++) {
                regarray[m].name = strdup(tptr->attrs[m]);
                PMIX_LOAD_KEY(regarray[m].string, pmix_attributes_lookup(tptr->attrs[m]));
                dptr = pmix_attributes_lookup_term(tptr->attrs[m]);
                if (NULL == dptr) {
                    PMIX_RELEASE(ip);
                    return;
                }
                regarray[m].type = dptr->type;
                regarray[m].description = PMIx_Argv_copy(dptr->description);
            }
        }
        pmix_list_append(lst, &ip->super);
    }
    PMIx_Argv_free(fns);
}

static void _get_fns(pmix_list_t *lst, pmix_info_t *info, pmix_list_t *attrs)
{
    pmix_attribute_trk_t *tptr;
    pmix_infolist_t *ip;
    char **fns = NULL, *tmp;

    /* search the list for these functions */
    PMIX_LIST_FOREACH (tptr, attrs, pmix_attribute_trk_t) {
        PMIx_Argv_append_nosize(&fns, tptr->function);
    }
    if (0 < PMIx_Argv_count(fns)) {
        ip = PMIX_NEW(pmix_infolist_t);
        tmp = PMIx_Argv_join(fns, ',');
        PMIX_INFO_LOAD(&ip->info, info->key, tmp, PMIX_STRING);
        pmix_list_append(lst, &ip->super);
        PMIx_Argv_free(fns);
    }
}

static void _local_relcb(void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;
    PMIX_RELEASE(cd);
}

static void relcbfunc(void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:query release callback");

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}
static void query_cbfunc(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr,
                         pmix_buffer_t *buf, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;
    pmix_status_t rc;
    pmix_shift_caddy_t *results;
    int cnt;
    PMIX_HIDE_UNUSED_PARAMS(hdr);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:attrs:query cback from server");

    results = PMIX_NEW(pmix_shift_caddy_t);

    /* unpack the status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &results->status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        results->status = rc;
        goto complete;
    }
    if (PMIX_SUCCESS != results->status) {
        goto complete;
    }

    /* unpack any returned data */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &results->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        results->status = rc;
        goto complete;
    }
    if (0 < results->ninfo) {
        PMIX_INFO_CREATE(results->info, results->ninfo);
        cnt = results->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, results->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            results->status = rc;
            goto complete;
        }
    }

complete:
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server releasing");
    /* release the caller */
    if (NULL != cd->cbfunc) {
        cd->cbfunc(results->status, results->info, results->ninfo, cd->cbdata, relcbfunc, results);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT void pmix_attrs_query_support(int sd, short args, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;
    pmix_infolist_t *info, *head;
    pmix_list_t kyresults;
    size_t n, m, p;
    pmix_info_t *iptr;
    pmix_data_array_t *darray;
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_QUERY_CMD;
    pmix_status_t rc;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    for (n = 0; n < cd->nqueries; n++) {
        if (0 != strcmp(cd->queries[n].keys[0], PMIX_QUERY_ATTRIBUTE_SUPPORT)) {
            /* skip this one */
            continue;
        }
        head = NULL;
        for (m = 0; m < cd->queries[n].nqual; m++) {
            PMIX_CONSTRUCT(&kyresults, pmix_list_t);
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_CLIENT_ATTRIBUTES)) {
                /* everyone has access to the client attrs */
                _get_attrs(&kyresults, &cd->queries[n].qualifiers[m], &client_attrs);
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_CLIENT_FUNCTIONS)) {
                /* everyone has access to the client functions */
                _get_fns(&kyresults, &cd->queries[n].qualifiers[m], &client_attrs);
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_SERVER_ATTRIBUTES)) {
                /* if I am a server, add in my attrs */
                if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
                    _get_attrs(&kyresults, &cd->queries[n].qualifiers[m], &server_attrs);
                } else {
                    /* we need to ask our server for them */
                    PMIX_LIST_DESTRUCT(&kyresults);
                    goto query;
                }
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_SERVER_FUNCTIONS)) {
                /* if I am a server, add in my fns */
                if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
                    _get_fns(&kyresults, &cd->queries[n].qualifiers[m], &server_attrs);
                } else {
                    /* we need to ask our server for them */
                    PMIX_LIST_DESTRUCT(&kyresults);
                    goto query;
                }
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_TOOL_ATTRIBUTES)) {
                if (PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) {
                    _get_attrs(&kyresults, &cd->queries[n].qualifiers[m], &tool_attrs);
                }
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_TOOL_FUNCTIONS)) {
                if (PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) {
                    _get_fns(&kyresults, &cd->queries[n].qualifiers[m], &tool_attrs);
                }
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_HOST_ATTRIBUTES)) {

                /* if I am a server, add in the host's */
                if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
                    _get_attrs(&kyresults, &cd->queries[n].qualifiers[m], &host_attrs);
                } else {
                    /* we need to ask our server for them */
                    PMIX_LIST_DESTRUCT(&kyresults);
                    goto query;
                }
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_HOST_FUNCTIONS)) {
                /* if I am a server, add in the host's */
                if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
                    _get_fns(&kyresults, &cd->queries[n].qualifiers[m], &host_attrs);
                } else {
                    /* we need to ask our server for them */
                    PMIX_LIST_DESTRUCT(&kyresults);
                    goto query;
                }
            }
            if (0 < (p = pmix_list_get_size(&kyresults))) {
                head = PMIX_NEW(pmix_infolist_t);
                PMIX_LOAD_KEY(head->info.key, cd->queries[n].keys[m]);
                head->info.value.type = PMIX_DATA_ARRAY;
                /* create the data array to hold the results */
                PMIX_DATA_ARRAY_CREATE(darray, p, PMIX_INFO);
                head->info.value.data.darray = darray;
                iptr = (pmix_info_t *) darray->array;
                p = 0;
                PMIX_LIST_FOREACH (info, &kyresults, pmix_infolist_t) {
                    PMIX_INFO_XFER(&iptr[p], &info->info);
                    ++p;
                }
                pmix_list_append(&cd->results, &head->super);
            }
            PMIX_LIST_DESTRUCT(&kyresults);
        }
    }
    /* prep the response by converting the list
     * of results into an array */
    if (0 < (cd->ninfo = pmix_list_get_size(&cd->results))) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        n = 0;
        PMIX_LIST_FOREACH (info, &cd->results, pmix_infolist_t) {
            PMIX_INFO_XFER(&cd->info[n], &info->info);
            ++n;
        }
        cd->status = PMIX_SUCCESS;
    } else {
        cd->status = PMIX_ERR_NOT_FOUND;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);
    goto release;

query:
   /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        cd->status = PMIX_ERR_NOT_FOUND;
        goto release;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* relay this request to the server */
    msg = PMIX_NEW(pmix_buffer_t);
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        cd->status = rc;
        goto release;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cd->nqueries, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        cd->status = rc;
        goto release;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, cd->queries, cd->nqueries, PMIX_QUERY);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        cd->status = rc;
        goto release;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query sending to server");
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, query_cbfunc, (void *) cd);
    if (PMIX_SUCCESS != rc) {
        cd->status = rc;
        goto release;
    }
    return;

release:
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query releasing");
    if (NULL != cd->cbfunc) {
        cd->cbfunc(cd->status, cd->info, cd->ninfo, cd->cbdata, _local_relcb, cd);
        return;
    }

    PMIX_RELEASE(cd);
}

/*****   LOCATE A GIVEN ATTRIBUTE    *****/
PMIX_EXPORT const char *pmix_attributes_lookup(const char *attr)
{
    pmix_keyindex_t *const kidx = &pmix_globals.keyindex;

    for (int i = 0; i < kidx->table->size; ++i) {
        pmix_regattr_input_t *ra = pmix_pointer_array_get_item(kidx->table, i);
        if (NULL == ra) break;
        if (0 == strcasecmp(ra->name, attr)) {
            return ra->string;
        }
    }
    return attr;
}

PMIX_EXPORT const char *pmix_attributes_reverse_lookup(const char *attrstring)
{
    pmix_keyindex_t *const kidx = &pmix_globals.keyindex;

    for (int i = 0; i < kidx->table->size; ++i) {
        pmix_regattr_input_t *ra = pmix_pointer_array_get_item(kidx->table, i);
        if (NULL == ra) break;
        if (0 == strcasecmp(ra->string, attrstring)) {
            return ra->name;
        }
    }
    return attrstring;
}

PMIX_EXPORT const pmix_regattr_input_t *pmix_attributes_lookup_term(char *attr)
{
    pmix_keyindex_t *const kidx = &pmix_globals.keyindex;

    for (int i = 0; i < kidx->table->size; ++i) {
        pmix_regattr_input_t *ra = pmix_pointer_array_get_item(kidx->table, i);
        if (NULL == ra) break;
        if (0 == strcasecmp(ra->name, attr)) {
            return ra;
        }
    }
    return NULL;
}

/*****   PRINT QUERY FUNCTIONS RESULTS   *****/
PMIX_EXPORT char **pmix_attributes_print_functions(char *level)
{
    char *title1 = "CLIENT SUPPORTED FUNCTIONS: ";
    char *title2 = "SERVER SUPPORTED FUNCTIONS: ";
    char *title3 = "HOST SUPPORTED FUNCTIONS: ";
    char *title4 = "TOOL SUPPORTED FUNCTIONS: ";
    char **ans = NULL;
    pmix_list_t *lst;
    pmix_attribute_trk_t *fnptr;

    /* select title */
    if (0 == strcmp(level, PMIX_CLIENT_FUNCTIONS)) {
        PMIx_Argv_append_nosize(&ans, title1);
        lst = &client_attrs;
    } else if (0 == strcmp(level, PMIX_SERVER_FUNCTIONS)) {
        PMIx_Argv_append_nosize(&ans, title2);
        lst = &server_attrs;
    } else if (0 == strcmp(level, PMIX_HOST_FUNCTIONS)) {
        PMIx_Argv_append_nosize(&ans, title3);
        lst = &host_attrs;
    } else if (0 == strcmp(level, PMIX_TOOL_FUNCTIONS)) {
        PMIx_Argv_append_nosize(&ans, title4);
        lst = &tool_attrs;
    } else {
        return NULL;
    }

    PMIX_LIST_FOREACH (fnptr, lst, pmix_attribute_trk_t) {
        PMIx_Argv_append_nosize(&ans, fnptr->function);
    }
    return ans;
}

/*****   PRINT QUERY ATTRS RESULTS   *****/

#define PMIX_PRINT_NAME_COLUMN_WIDTH   35
#define PMIX_PRINT_STRING_COLUMN_WIDTH 25
#define PMIX_PRINT_TYPE_COLUMN_WIDTH   20
#define PMIX_PRINT_ATTR_COLUMN_WIDTH   141

void pmix_attributes_print_attrs(char ***ans, char *function,
                                 pmix_regattr_t *attrs, size_t nattrs)
{
    char line[PMIX_PRINT_ATTR_COLUMN_WIDTH], *tmp;
    size_t n, m, len;

    /* print the function */
    memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
    m = 0;
    for (n = 0; n < strlen(function); n++) {
        line[m] = function[n];
        ++m;
    }
    line[m++] = ':';
    line[m] = '\0';
    PMIx_Argv_append_nosize(ans, line);

    for (n = 0; n < nattrs; n++) {
        memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
        line[PMIX_PRINT_ATTR_COLUMN_WIDTH - 1] = '\0';
        len = strlen(attrs[n].name);
        if (PMIX_PRINT_NAME_COLUMN_WIDTH < len) {
            len = PMIX_PRINT_NAME_COLUMN_WIDTH;
        }
        memcpy(line, attrs[n].name, len);

        if (0 == strlen(attrs[n].string)) {
            line[PMIX_PRINT_ATTR_COLUMN_WIDTH - 1] = '\0'; // ensure NULL termination
            PMIx_Argv_append_nosize(ans, line);
            continue;
        }

        len = strlen(attrs[n].string);
        if (PMIX_PRINT_STRING_COLUMN_WIDTH < len) {
            len = PMIX_PRINT_STRING_COLUMN_WIDTH;
        }
        memcpy(&line[PMIX_PRINT_NAME_COLUMN_WIDTH + 2], attrs[n].string, len);

        tmp = (char *) PMIx_Data_type_string(attrs[n].type);
        len = strlen(tmp);
        if (PMIX_PRINT_STRING_COLUMN_WIDTH < len) {
            len = PMIX_PRINT_STRING_COLUMN_WIDTH;
        }
        memcpy(&line[PMIX_PRINT_NAME_COLUMN_WIDTH + PMIX_PRINT_STRING_COLUMN_WIDTH + 4], tmp, len);

        for (m = 0; NULL != attrs[n].description[m]; m++) {
            len = strlen(attrs[n].description[m]);
            memcpy(&line[PMIX_PRINT_NAME_COLUMN_WIDTH + PMIX_PRINT_STRING_COLUMN_WIDTH
                         + PMIX_PRINT_TYPE_COLUMN_WIDTH + 6],
                   attrs[n].description[m], len);
            line[PMIX_PRINT_ATTR_COLUMN_WIDTH - 1] = '\0'; // ensure NULL termination
            PMIx_Argv_append_nosize(ans, line);
            memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
            line[PMIX_PRINT_ATTR_COLUMN_WIDTH - 1] = '\0';
        }
    }
}

void pmix_attributes_print_headers(char ***ans, char *level)
{
    size_t n, m, left;
    char *title1 = "CLIENT SUPPORTED ATTRIBUTES: ";
    char *title2 = "SERVER SUPPORTED ATTRIBUTES: ";
    char *title3 = "HOST SUPPORTED ATTRIBUTES: ";
    char *title4 = "TOOL SUPPORTED ATTRIBUTES: ";
    char line[PMIX_PRINT_ATTR_COLUMN_WIDTH];

    /* select title */
    if (0 == strcmp(level, PMIX_CLIENT_ATTRIBUTES)) {
        PMIx_Argv_append_nosize(ans, title1);
    } else if (0 == strcmp(level, PMIX_SERVER_ATTRIBUTES)) {
        PMIx_Argv_append_nosize(ans, title2);
    } else if (0 == strcmp(level, PMIX_HOST_ATTRIBUTES)) {
        PMIx_Argv_append_nosize(ans, title3);
    } else if (0 == strcmp(level, PMIX_TOOL_ATTRIBUTES)) {
        PMIx_Argv_append_nosize(ans, title4);
    } else {
        return;
    }

    /* print the column headers */
    memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
    line[PMIX_PRINT_ATTR_COLUMN_WIDTH - 1] = '\0';
    left = PMIX_PRINT_NAME_COLUMN_WIDTH / 2 - 1;
    memcpy(&line[left], "NAME", 4);

    left = 3 + PMIX_PRINT_NAME_COLUMN_WIDTH + (PMIX_PRINT_STRING_COLUMN_WIDTH / 2) - 2;
    memcpy(&line[left], "STRING", 6);

    left = 3 + PMIX_PRINT_NAME_COLUMN_WIDTH + PMIX_PRINT_STRING_COLUMN_WIDTH
           + (PMIX_PRINT_TYPE_COLUMN_WIDTH / 2) - 2;
    memcpy(&line[left], "TYPE", 4);

    left = PMIX_PRINT_NAME_COLUMN_WIDTH + PMIX_PRINT_STRING_COLUMN_WIDTH
           + PMIX_PRINT_TYPE_COLUMN_WIDTH
           + ((PMIX_PRINT_ATTR_COLUMN_WIDTH - PMIX_PRINT_NAME_COLUMN_WIDTH
               - PMIX_PRINT_STRING_COLUMN_WIDTH - PMIX_PRINT_TYPE_COLUMN_WIDTH)
              / 2)
           - 3 - strlen("DESCRIPTION") / 2;
    memcpy(&line[left], "DESCRIPTION", strlen("DESCRIPTION"));
    left += strlen("DESCRIPTION") + 1;
    line[left] = '\0';
    PMIx_Argv_append_nosize(ans, line);

    /* print the dashes under the column headers */
    memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
    line[PMIX_PRINT_ATTR_COLUMN_WIDTH - 1] = '\0';
    m = 0;
    for (n = 0; n < PMIX_PRINT_NAME_COLUMN_WIDTH; n++) {
        line[m] = '-';
        ++m;
    }
    m += 2; // leave gap
    for (n = 0; n < PMIX_PRINT_STRING_COLUMN_WIDTH; n++) {
        line[m] = '-';
        ++m;
    }
    m += 2; // leave gap
    for (n = 0; n < PMIX_PRINT_TYPE_COLUMN_WIDTH; n++) {
        line[m] = '-';
        ++m;
    }
    m += 2; // leave gap
    while (m < PMIX_PRINT_ATTR_COLUMN_WIDTH - 1) {
        line[m] = '-';
        ++m;
    }
    PMIx_Argv_append_nosize(ans, line);
}

PMIX_EXPORT char **pmix_attributes_print_attr(char *level, char *function)
{
    size_t n, m, nattr;
    char **tmp, **ans = NULL;
    pmix_list_t *lst;
    pmix_attribute_trk_t *fnptr;
    char line[PMIX_PRINT_ATTR_COLUMN_WIDTH];
    pmix_regattr_t *rptr;
    const pmix_regattr_input_t *dptr;

    /* select title */
    if (0 == strcmp(level, PMIX_CLIENT_ATTRIBUTES)) {
        lst = &client_attrs;
    } else if (0 == strcmp(level, PMIX_SERVER_ATTRIBUTES)) {
        lst = &server_attrs;
    } else if (0 == strcmp(level, PMIX_HOST_ATTRIBUTES)) {
        lst = &host_attrs;
    } else if (0 == strcmp(level, PMIX_TOOL_ATTRIBUTES)) {
        lst = &tool_attrs;
    } else {
        return NULL;
    }

    /* print the column headers */
    pmix_attributes_print_headers(&ans, level);

    memset(line, '=', PMIX_PRINT_ATTR_COLUMN_WIDTH);
    line[PMIX_PRINT_ATTR_COLUMN_WIDTH - 1] = '\0';

    /* can be comma-delimited list of functions */
    tmp = PMIx_Argv_split(function, ',');
    for (n = 0; NULL != tmp[n]; n++) {
        PMIX_LIST_FOREACH (fnptr, lst, pmix_attribute_trk_t) {
            if (0 == strcmp(tmp[n], "all") || 0 == strcmp(tmp[n], fnptr->function)) {
                /* create an array of pmix_regattr_t for this function's attributes */
                nattr = PMIx_Argv_count(fnptr->attrs);
                if (0 == nattr || (1 == nattr &&
                                   (0 == strcmp("N/A", fnptr->attrs[0]) ||
                                    0 == strcmp("NONE", fnptr->attrs[0])))) {
                    nattr = 1;
                    PMIX_REGATTR_CREATE(rptr, nattr);
                    rptr[0].name = strdup("NONE");
                } else {
                    PMIX_REGATTR_CREATE(rptr, nattr);
                    for (m = 0; m < nattr; m++) {
                        rptr[m].name = strdup(fnptr->attrs[m]);
                        PMIX_LOAD_KEY(rptr[m].string, pmix_attributes_lookup(fnptr->attrs[m]));
                        dptr = pmix_attributes_lookup_term(fnptr->attrs[m]);
                        if (NULL == dptr) {
                            PMIx_Argv_free(tmp);
                            PMIx_Argv_free(ans);
                            PMIX_REGATTR_FREE(rptr, nattr);
                            return NULL;
                        }
                        rptr[m].type = dptr->type;
                        rptr[m].description = PMIx_Argv_copy(dptr->description);
                    }
                }
                pmix_attributes_print_attrs(&ans, fnptr->function, rptr, nattr);
                PMIX_REGATTR_FREE(rptr, nattr);
                if (0 == strcmp(tmp[n], fnptr->function)) {
                    break;
                }
                /* add a spacer between functions */
                PMIx_Argv_append_nosize(&ans, "   ");
                PMIx_Argv_append_nosize(&ans, line);
                PMIx_Argv_append_nosize(&ans, "   ");
            }
        }
    }
    PMIx_Argv_free(tmp);

    return ans;
}
