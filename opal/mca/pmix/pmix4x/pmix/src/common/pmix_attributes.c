/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <src/include/pmix_config.h>

#include <pmix.h>
#include <pmix_common.h>
#include <pmix_server.h>
#include <pmix_rename.h>

#include "src/mca/bfrops/bfrops.h"
#include "src/mca/gds/base/base.h"
#include "src/include/pmix_globals.h"
#include "src/threads/threads.h"
#include "src/client/pmix_client_ops.h"

#include "src/common/pmix_attributes.h"

static bool initialized = false;
static pmix_list_t client_attrs;
static pmix_list_t server_attrs;
static pmix_list_t host_attrs;
static pmix_list_t tool_attrs;

typedef struct {
    pmix_list_item_t super;
    char *function;
    pmix_regattr_t *attrs;
    size_t nattrs;
} pmix_attribute_trk_t;

static void atrkcon(pmix_attribute_trk_t *p)
{
    p->function = NULL;
    p->attrs = NULL;
    p->nattrs = 0;
}
static void atrkdes(pmix_attribute_trk_t *p)
{
    if (NULL != p->function) {
        free(p->function);
    }
    if (NULL != p->attrs) {
        PMIX_REGATTR_FREE(p->attrs, p->nattrs);
    }
}
static PMIX_CLASS_INSTANCE(pmix_attribute_trk_t,
                           pmix_list_item_t,
                           atrkcon, atrkdes);

PMIX_EXPORT void pmix_init_registered_attrs(void)
{
    if (!initialized) {
        PMIX_CONSTRUCT(&client_attrs, pmix_list_t);
        PMIX_CONSTRUCT(&server_attrs, pmix_list_t);
        PMIX_CONSTRUCT(&host_attrs, pmix_list_t);
        PMIX_CONSTRUCT(&tool_attrs, pmix_list_t);
        initialized = true;
    }
}

static pmix_status_t process_reg(char *level, char *function,
                                 pmix_regattr_t attrs[], size_t nattrs)
{
    pmix_attribute_trk_t *fnptr;
    pmix_list_t *lst;
    size_t n;

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
    PMIX_LIST_FOREACH(fnptr, lst, pmix_attribute_trk_t) {
        if (0 == strcmp(function, fnptr->function)) {
            /* we already have this function at this level
             * so we must return an error */
            return PMIX_ERR_REPEAT_ATTR_REGISTRATION;
        }
    }

    fnptr = PMIX_NEW(pmix_attribute_trk_t);
    pmix_list_append(lst, &fnptr->super);
    fnptr->function = strdup(function);
    if (0 < nattrs) {
        fnptr->nattrs = nattrs;
        PMIX_REGATTR_CREATE(fnptr->attrs, fnptr->nattrs);
        for (n=0; n < nattrs; n++) {
            fnptr->attrs[n].name = strdup(attrs[n].name);
            PMIX_LOAD_KEY(fnptr->attrs[n].string, attrs[n].string);
            fnptr->attrs[n].type = attrs[n].type;
            PMIX_ARGV_COPY(fnptr->attrs[n].description, attrs[n].description);
        }
    }
    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_Register_attributes(char *function,
                                                   pmix_regattr_t attrs[], size_t nattrs)
{
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    rc = process_reg(PMIX_HOST_ATTRIBUTES, function, attrs, nattrs);
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
}

/* sadly, we cannot dynamically register our supported attributes
 * as that would require the user to first call the function whose
 * attributes they want to know about - which somewhat defeats the
 * purpose. Until someone comes up with a better solution, we will
 * manually maintain the list */
static char *client_fns[] = {
    "PMIx_Init",
    "PMIx_Finalize",
    "PMIx_Put",
    "PMIx_Get",
    "PMIx_Get_nb",
    "PMIx_Store_internal",
    "PMIx_Commit",
    "PMIx_Fence",
    "PMIx_Fence_nb",
    "PMIx_Publish",
    "PMIx_Group_construct",
    "PMIx_Group_construct_nb",
    "PMIx_Group_destruct",
    "PMIx_Group_destruct_nb",
    "PMIx_Group_invite",
    "PMIx_Group_invite_nb",
    "PMIx_Group_join",
    "PMIx_Group_join_nb",
    "PMIx_Spawn",
    "PMIx_Spawn_nb",
    "PMIx_Log",
    "PMIx_Log_nb"
};

typedef struct {
    char *name;
    char *string;
    pmix_data_type_t type;
    char **description;
} pmix_regattr_input_t;

static pmix_regattr_input_t client_attributes[] = {
        // init
        {.name = "PMIX_GDS_MODULE", .string = PMIX_GDS_MODULE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_EVENT_BASE", .string = PMIX_EVENT_BASE, .type = PMIX_POINTER, .description = (char *[]){"VALID MEMORY REFERENCE", NULL}},
        {.name = "PMIX_HOSTNAME", .string = PMIX_HOSTNAME, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_NODEID", .string = PMIX_NODEID, .type = PMIX_UINT32, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = "PMIX_PROGRAMMING_MODEL", .string = PMIX_PROGRAMMING_MODEL, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_MODEL_LIBRARY_NAME", .string = PMIX_MODEL_LIBRARY_NAME, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_MODEL_LIBRARY_VERSION", .string = PMIX_MODEL_LIBRARY_VERSION, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_THREADING_MODEL", .string = PMIX_THREADING_MODEL, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_USOCK_DISABLE", .string = PMIX_USOCK_DISABLE, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Disable usock messaging interface", NULL}},
        {.name = "PMIX_SOCKET_MODE", .string = PMIX_SOCKET_MODE, .type = PMIX_UINT32, .description = (char *[]){"Valid POSIX mode_t value", NULL}},
        {.name = "PMIX_TCP_REPORT_URI", .string = PMIX_TCP_REPORT_URI, .type = PMIX_STRING, .description = (char *[]){"-, +, or filename", NULL}},
        {.name = "PMIX_TCP_IF_INCLUDE", .string = PMIX_TCP_IF_INCLUDE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Comma-separated list of", "TCP interfaces to include", NULL}},
        {.name = "PMIX_TCP_IF_EXCLUDE", .string = PMIX_TCP_IF_EXCLUDE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Comma-separated list of", "TCP interfaces to exclude", NULL}},
        {.name = "PMIX_TCP_IPV4_PORT", .string = PMIX_TCP_IPV4_PORT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", "IPv4 port to be used", NULL}},
        {.name = "PMIX_TCP_IPV6_PORT", .string = PMIX_TCP_IPV6_PORT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", "IPv6 port to be used", NULL}},
        {.name = "PMIX_TCP_DISABLE_IPV4", .string = PMIX_TCP_DISABLE_IPV4, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Disable IPv4 messaging interface", NULL}},
        {.name = "PMIX_TCP_DISABLE_IPV6", .string = PMIX_TCP_DISABLE_IPV6, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Disable IPv6 messaging interface", NULL}},
        {.name = ""},
        // finalize
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Execute fence during finalize", NULL}},
        {.name = ""},
        // put
        {.name = ""},
        // get
        {.name = "PMIX_DATA_SCOPE", .string = PMIX_DATA_SCOPE, .type = PMIX_SCOPE, .description = (char *[]){"PMIX_SCOPE_UNDEF,PMIX_LOCAL,", "PMIX_REMOTE,PMIX_GLOBAL,", "PMIX_INTERNAL", NULL}},
        {.name = "PMIX_OPTIONAL", .string = PMIX_OPTIONAL, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_IMMEDIATE", .string = PMIX_IMMEDIATE, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_SESSION_INFO", .string = PMIX_SESSION_INFO, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Requesting session-level value", NULL}},
        {.name = "PMIX_JOB_INFO", .string = PMIX_JOB_INFO, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Requesting job-level value", NULL}},
        {.name = "PMIX_APP_INFO", .string = PMIX_APP_INFO, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Requesting app-level value", NULL}},
        {.name = "PMIX_NODE_INFO", .string = PMIX_NODE_INFO, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Requesting node-level value", NULL}},
        {.name = ""},
        // get_nb
        {.name = "PMIX_DATA_SCOPE", .string = PMIX_DATA_SCOPE, .type = PMIX_SCOPE, .description = (char *[]){"PMIX_SCOPE_UNDEF,PMIX_LOCAL,", "PMIX_REMOTE,PMIX_GLOBAL,", "PMIX_INTERNAL", NULL}},
        {.name = "PMIX_OPTIONAL", .string = PMIX_OPTIONAL, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_IMMEDIATE", .string = PMIX_IMMEDIATE, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_SESSION_INFO", .string = PMIX_SESSION_INFO, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Requesting session-level value", NULL}},
        {.name = "PMIX_JOB_INFO", .string = PMIX_JOB_INFO, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Requesting job-level value", NULL}},
        {.name = "PMIX_APP_INFO", .string = PMIX_APP_INFO, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Requesting app-level value", NULL}},
        {.name = "PMIX_NODE_INFO", .string = PMIX_NODE_INFO, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Requesting node-level value", NULL}},
        {.name = ""},
        // store_internal
        {.name = ""},
        // commit
        {.name = ""},
        // fence
        {.name = ""},
        // fence_nb
        {.name = ""},
        // publish
        {.name = ""},
        // group_construct
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
        // group_construct_nb
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
        // group_destruct
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
        // group_destruct_nb
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
        // group_invite
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
        // group_invite_nb
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
        // group_join
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
        // group_join_nb
        {.name = "PMIX_SETUP_APP_ENVARS", .string = PMIX_SETUP_APP_ENVARS, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = ""},
        // spawn
        {.name = "PMIX_SETUP_APP_ENVARS", .string = PMIX_SETUP_APP_ENVARS, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = ""},
        // spawn_nb
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
        // log
        {.name = "PMIX_LOG_GENERATE_TIMESTAMP", .string = PMIX_LOG_GENERATE_TIMESTAMP, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
        // log_nb
        {.name = "PMIX_LOG_GENERATE_TIMESTAMP", .string = PMIX_LOG_GENERATE_TIMESTAMP, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
};

/*****    REGISTER CLIENT ATTRS    *****/
static bool client_attrs_regd = false;

PMIX_EXPORT pmix_status_t pmix_register_client_attrs(void)
{
    size_t nregs, nattrs, n, m;
    size_t cnt = 0;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_regattr_t *attrs;

    if (client_attrs_regd) {
        return PMIX_SUCCESS;
    }
    client_attrs_regd = true;

    nregs = sizeof(client_fns) / sizeof(char*);

    /* we know we have to prep the GDS, PTL, BFROPS, and SEC
     * entries as these are dynamically defined */
    client_attributes[0].description[0] = pmix_gds_base_get_available_modules();

    for (n=0; n < nregs; n++) {
        nattrs = 0;
        while (0 != strlen(client_attributes[cnt+nattrs].name)) {
            ++nattrs;
        }
        PMIX_REGATTR_CREATE(attrs, nattrs);
        for (m=0; m < nattrs; m++) {
            attrs[m].name = strdup(client_attributes[m+cnt].name);
            PMIX_LOAD_KEY(attrs[m].string, client_attributes[m+cnt].string);
            attrs[m].type = client_attributes[m+cnt].type;
            PMIX_ARGV_COPY(attrs[m].description, client_attributes[m+cnt].description);
        }
        rc = process_reg(PMIX_CLIENT_ATTRIBUTES,
                         client_fns[n],
                         attrs, nattrs);
        PMIX_REGATTR_FREE(attrs, nattrs);
        if (PMIX_SUCCESS != rc) {
            break;
        }
        cnt += nattrs + 1;
    }

    if (NULL != client_attributes[0].description[0]) {
        free(client_attributes[0].description[0]);
        client_attributes[0].description[0] = NULL;
    }
    return PMIX_SUCCESS;
}

static char *server_fns[] = {
    "PMIx_server_init",
    "PMIx_server_finalize",
    "PMIx_generate_regex",
    "PMIx_generate_ppn",
    "PMIx_server_register_nspace",
    "PMIx_server_deregister_nspace",
    "PMIx_server_register_client",
    "PMIx_server_deregister_client",
    "PMIx_server_setup_fork",
    "PMIx_server_dmodex_request",
    "PMIx_server_setup_application",
    "PMIx_Register_attributes",
    "PMIx_server_setup_local_support",
    "PMIx_server_IOF_deliver",
    "PMIx_server_collect_inventory",
    "PMIx_server_deliver_inventory",
    "PMIx_Get",
    "PMIx_Get_nb",
    "PMIx_Fence",
    "PMIx_Fence_nb",
    "PMIx_Spawn",
    "PMIx_Spawn_nb",
    "PMIx_Connect",
    "PMIx_Connect_nb",
    "PMIx_Register_event_handler",
    "PMIx_Query_info_nb",
    "PMIx_Job_control",
    "PMIx_Job_control_nb"
};

static pmix_regattr_input_t server_attributes[] = {
    // init
        {.name = "PMIX_GDS_MODULE", .string = PMIX_GDS_MODULE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_EVENT_BASE", .string = PMIX_EVENT_BASE, .type = PMIX_POINTER, .description = (char *[]){"VALID MEMORY REFERENCE", NULL}},
        {.name = "PMIX_HOSTNAME", .string = PMIX_HOSTNAME, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_NODEID", .string = PMIX_NODEID, .type = PMIX_UINT32, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = "PMIX_SINGLE_LISTENER", .string = PMIX_SINGLE_LISTENER, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Only use one messaging interface", NULL}},
        {.name = "PMIX_USOCK_DISABLE", .string = PMIX_USOCK_DISABLE, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Disable usock messaging interface", NULL}},
        {.name = "PMIX_SOCKET_MODE", .string = PMIX_SOCKET_MODE, .type = PMIX_UINT32, .description = (char *[]){"Valid POSIX mode_t value", NULL}},
        {.name = "PMIX_TCP_REPORT_URI", .string = PMIX_TCP_REPORT_URI, .type = PMIX_STRING, .description = (char *[]){"-, +, or filename", NULL}},
        {.name = "PMIX_TCP_IF_INCLUDE", .string = PMIX_TCP_IF_INCLUDE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Comma-separated list of", "TCP interfaces to include", NULL}},
        {.name = "PMIX_TCP_IF_EXCLUDE", .string = PMIX_TCP_IF_EXCLUDE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Comma-separated list of", "TCP interfaces to exclude", NULL}},
        {.name = "PMIX_TCP_IPV4_PORT", .string = PMIX_TCP_IPV4_PORT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", "IPv4 port to be used", NULL}},
        {.name = "PMIX_TCP_IPV6_PORT", .string = PMIX_TCP_IPV6_PORT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", "IPv6 port to be used", NULL}},
        {.name = "PMIX_TCP_DISABLE_IPV4", .string = PMIX_TCP_DISABLE_IPV4, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Disable IPv4 messaging interface", NULL}},
        {.name = "PMIX_TCP_DISABLE_IPV6", .string = PMIX_TCP_DISABLE_IPV6, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Disable IPv6 messaging interface", NULL}},
        {.name = "PMIX_SERVER_REMOTE_CONNECTIONS", .string = PMIX_SERVER_REMOTE_CONNECTIONS, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Allow connections from", "remote tools", NULL}},
        {.name = "PMIX_SERVER_NSPACE", .string = PMIX_SERVER_NSPACE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Namespace assigned to server", NULL}},
        {.name = "PMIX_SERVER_RANK", .string = PMIX_SERVER_RANK, .type = PMIX_PROC_RANK, .description = (char *[]){"POSITIVE INTEGERS", "Rank assigned to server", NULL}},
        {.name = "PMIX_SERVER_TMPDIR", .string = PMIX_SERVER_TMPDIR, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Path to temp directory", "assigned to server", NULL}},
        {.name = "PMIX_SYSTEM_TMPDIR", .string = PMIX_SYSTEM_TMPDIR, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Path to system temp directory", NULL}},
        {.name = "PMIX_SERVER_TOOL_SUPPORT", .string = PMIX_SERVER_TOOL_SUPPORT, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Allow tool connections", NULL}},
        {.name = "PMIX_SERVER_SYSTEM_SUPPORT", .string = PMIX_SERVER_SYSTEM_SUPPORT, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Declare server as being the", "local system server for PMIx", "connection requests", NULL}},
        {.name = ""},
    // finalize
        {.name = ""},
    // regex
        {.name = "PMIX_DATA_SCOPE", .string = PMIX_DATA_SCOPE, .type = PMIX_SCOPE, .description = (char *[]){"PMIX_SCOPE_UNDEF,PMIX_LOCAL,","PMIX_REMOTE,PMIX_GLOBAL,", "PMIX_INTERNAL", NULL}},
        {.name = "PMIX_OPTIONAL", .string = PMIX_OPTIONAL, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_IMMEDIATE", .string = PMIX_IMMEDIATE, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // ppn
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // register_nspace
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // deregister_nspace
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // register_client
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // deregister_client
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // setup_fork
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // dmodex_request
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // setup_application
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // register_attributes
        {.name = "PMIX_SETUP_APP_ENVARS", .string = PMIX_SETUP_APP_ENVARS, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = ""},
    // setup_local_support
        {.name = "PMIX_SETUP_APP_ENVARS", .string = PMIX_SETUP_APP_ENVARS, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = ""},
    // IOF deliver
        {.name = "PMIX_EMBED_BARRIER", .string = PMIX_EMBED_BARRIER, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // collect_inventory
        {.name = "PMIX_LOG_GENERATE_TIMESTAMP", .string = PMIX_LOG_GENERATE_TIMESTAMP, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // deliver_inventory
        {.name = "PMIX_LOG_GENERATE_TIMESTAMP", .string = PMIX_LOG_GENERATE_TIMESTAMP, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // get
        {.name = "PMIX_IMMEDIATE", .string = PMIX_IMMEDIATE, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // get_nb
        {.name = "PMIX_IMMEDIATE", .string = PMIX_IMMEDIATE, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // fence
        {.name = "PMIX_COLLECT_DATA", .string = PMIX_COLLECT_DATA, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // fence_nb
        {.name = "PMIX_COLLECT_DATA", .string = PMIX_COLLECT_DATA, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // spawn
        {.name = "PMIX_FWD_STDIN", .string = PMIX_FWD_STDIN, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_FWD_STDOUT", .string = PMIX_FWD_STDOUT, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_FWD_STDERR", .string = PMIX_FWD_STDERR, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_FWD_STDDIAG", .string = PMIX_FWD_STDDIAG, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // spawn_nb
        {.name = "PMIX_FWD_STDIN", .string = PMIX_FWD_STDIN, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_FWD_STDOUT", .string = PMIX_FWD_STDOUT, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_FWD_STDERR", .string = PMIX_FWD_STDERR, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_FWD_STDDIAG", .string = PMIX_FWD_STDDIAG, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // connect
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // connect_nb
        {.name = "PMIX_TIMEOUT", .string = PMIX_TIMEOUT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = ""},
    // register_event
        {.name = "PMIX_EVENT_AFFECTED_PROC", .string = PMIX_EVENT_AFFECTED_PROC, .type = PMIX_PROC, .description = (char *[]){"pmix_proc_t*", NULL}},
        {.name = "PMIX_EVENT_AFFECTED_PROCS", .string = PMIX_EVENT_AFFECTED_PROCS, .type = PMIX_DATA_ARRAY, .description = (char *[]){"Array of pmix_proc_t", NULL}},
        {.name = ""},
    // query
        {.name = "PMIX_QUERY_REFRESH_CACHE", .string = PMIX_QUERY_REFRESH_CACHE, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_PROCID", .string = PMIX_PROCID, .type = PMIX_PROC, .description = (char *[]){"pmix_proc_t*", NULL}},
        {.name = "PMIX_NSPACE", .string = PMIX_NSPACE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_RANK", .string = PMIX_RANK, .type = PMIX_PROC_RANK, .description = (char *[]){"UNSIGNED INT32", NULL}},
        {.name = "PMIX_HOSTNAME", .string = PMIX_HOSTNAME, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = ""},
    // job_ctrl
        {.name = "PMIX_REGISTER_CLEANUP", .string = PMIX_REGISTER_CLEANUP, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_REGISTER_CLEANUP_DIR", .string = PMIX_REGISTER_CLEANUP_DIR, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_CLEANUP_RECURSIVE", .string = PMIX_CLEANUP_RECURSIVE, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_CLEANUP_IGNORE", .string = PMIX_CLEANUP_IGNORE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_CLEANUP_LEAVE_TOPDIR", .string = PMIX_CLEANUP_LEAVE_TOPDIR, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
    // job_ctrl_nb
        {.name = "PMIX_REGISTER_CLEANUP", .string = PMIX_REGISTER_CLEANUP, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_REGISTER_CLEANUP_DIR", .string = PMIX_REGISTER_CLEANUP_DIR, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_CLEANUP_RECURSIVE", .string = PMIX_CLEANUP_RECURSIVE, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = "PMIX_CLEANUP_IGNORE", .string = PMIX_CLEANUP_IGNORE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_CLEANUP_LEAVE_TOPDIR", .string = PMIX_CLEANUP_LEAVE_TOPDIR, .type = PMIX_BOOL, .description = (char *[]){"True,False", NULL}},
        {.name = ""},
};

/*****    REGISTER SERVER ATTRS    *****/
static bool server_attrs_regd = false;

PMIX_EXPORT pmix_status_t pmix_register_server_attrs(void)
{
    size_t nregs, nattrs, n, m;
    size_t cnt = 0;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_regattr_t *attrs;

    if (server_attrs_regd) {
        return PMIX_SUCCESS;
    }
    server_attrs_regd = true;

    nregs = sizeof(server_fns) / sizeof(char*);

    /* we know we have to prep the GDS, PTL, BFROPS, and SEC
     * entries as these are dynamically defined */
    server_attributes[0].description[0] = pmix_gds_base_get_available_modules();

    for (n=0; n < nregs; n++) {
        nattrs = 0;
        while (0 != strlen(server_attributes[cnt+nattrs].name)) {
            ++nattrs;
        }
        PMIX_REGATTR_CREATE(attrs, nattrs);
        for (m=0; m < nattrs; m++) {
            attrs[m].name = strdup(server_attributes[m+cnt].name);
            PMIX_LOAD_KEY(attrs[m].string, server_attributes[m+cnt].string);
            attrs[m].type = server_attributes[m+cnt].type;
            PMIX_ARGV_COPY(attrs[m].description, server_attributes[m+cnt].description);
        }
        rc = process_reg(PMIX_SERVER_ATTRIBUTES,
                         server_fns[n],
                         attrs, nattrs);
        PMIX_REGATTR_FREE(attrs, nattrs);
        if (PMIX_SUCCESS != rc) {
            break;
        }
        cnt += nattrs + 1;
    }

    return PMIX_SUCCESS;
}

static char *tool_fns[] = {
    "PMIx_tool_init",
    "PMIx_tool_finalize",
    "PMIx_tool_connect_to_server"
};

static pmix_regattr_input_t tool_attributes[] = {
    // init
        {.name = "PMIX_GDS_MODULE", .string = PMIX_GDS_MODULE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_EVENT_BASE", .string = PMIX_EVENT_BASE, .type = PMIX_POINTER, .description = (char *[]){"VALID MEMORY REFERENCE", NULL}},
        {.name = "PMIX_HOSTNAME", .string = PMIX_HOSTNAME, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_NODEID", .string = PMIX_NODEID, .type = PMIX_UINT32, .description = (char *[]){"POSITIVE INTEGERS", NULL}},
        {.name = "PMIX_TOOL_NSPACE", .string = PMIX_TOOL_NSPACE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", NULL}},
        {.name = "PMIX_TOOL_RANK", .string = PMIX_TOOL_RANK, .type = PMIX_PROC_RANK, .description = (char *[]){"POSITIVE INTEGERS", "Rank assigned to tool", NULL}},
        {.name = "PMIX_TOOL_DO_NOT_CONNECT", .string = PMIX_TOOL_DO_NOT_CONNECT, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Do not connect to server", NULL}},
        {.name = "PMIX_CONNECT_TO_SYSTEM", .string = PMIX_CONNECT_TO_SYSTEM, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Connect to system PMIx server", NULL}},
        {.name = "PMIX_CONNECT_SYSTEM_FIRST", .string = PMIX_CONNECT_SYSTEM_FIRST, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Try system PMIx server first", NULL}},
        {.name = "PMIX_SERVER_PIDINFO", .string = PMIX_SERVER_PIDINFO, .type = PMIX_PID, .description = (char *[]){"Valid pid_t value", "PID of target PMIx server", NULL}},
        {.name = "PMIX_TCP_URI", .string = PMIX_TCP_URI, .type = PMIX_STRING, .description = (char *[]){"Valid PMIx URI", "URI of PMIx server to connect to", NULL}},
        {.name = "PMIX_SERVER_URI", .string = PMIX_SERVER_URI, .type = PMIX_STRING, .description = (char *[]){"Valid PMIx URI", "URI of PMIx server to connect to", NULL}},
        {.name = "PMIX_SERVER_NSPACE", .string = PMIX_SERVER_NSPACE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Namespace of PMIx server to", "connect to", NULL}},
        {.name = "PMIX_CONNECT_RETRY_DELAY", .string = PMIX_CONNECT_RETRY_DELAY, .type = PMIX_UINT32, .description = (char *[]){"POSITIVE INTEGERS", "Seconds between connection", "attempts", NULL}},
        {.name = "PMIX_CONNECT_MAX_RETRIES", .string = PMIX_CONNECT_MAX_RETRIES, .type = PMIX_UINT32, .description = (char *[]){"POSITIVE INTEGERS", "Max number of connection retries", NULL}},
        {.name = "PMIX_SOCKET_MODE", .string = PMIX_SOCKET_MODE, .type = PMIX_UINT32, .description = (char *[]){"Valid POSIX mode_t value", NULL}},
        {.name = "PMIX_TCP_REPORT_URI", .string = PMIX_TCP_REPORT_URI, .type = PMIX_STRING, .description = (char *[]){"-, +, or filename", NULL}},
        {.name = "PMIX_TCP_IF_INCLUDE", .string = PMIX_TCP_IF_INCLUDE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Comma-separated list of", "TCP interfaces to include", NULL}},
        {.name = "PMIX_TCP_IF_EXCLUDE", .string = PMIX_TCP_IF_EXCLUDE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Comma-separated list of", "TCP interfaces to exclude", NULL}},
        {.name = "PMIX_TCP_IPV4_PORT", .string = PMIX_TCP_IPV4_PORT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", "IPv4 port to be used", NULL}},
        {.name = "PMIX_TCP_IPV6_PORT", .string = PMIX_TCP_IPV6_PORT, .type = PMIX_INT, .description = (char *[]){"POSITIVE INTEGERS", "IPv6 port to be used", NULL}},
        {.name = "PMIX_TCP_DISABLE_IPV4", .string = PMIX_TCP_DISABLE_IPV4, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Disable IPv4 messaging interface", NULL}},
        {.name = "PMIX_TCP_DISABLE_IPV6", .string = PMIX_TCP_DISABLE_IPV6, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Disable IPv6 messaging interface", NULL}},
        {.name = "PMIX_FWD_STDIN", .string = PMIX_FWD_STDIN, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Forward stdin of the tool", NULL}},
        {.name = "PMIX_LAUNCHER", .string = PMIX_LAUNCHER, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Tool is a job launcher", NULL}},
        {.name = "PMIX_SERVER_TMPDIR", .string = PMIX_SERVER_TMPDIR, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Path to temp directory", "assigned to tool", NULL}},
        {.name = "PMIX_SYSTEM_TMPDIR", .string = PMIX_SYSTEM_TMPDIR, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Path to system temp directory", NULL}},
        {.name = ""},
    // finalize
        {.name = ""},
    // connect_to_server
        {.name = "PMIX_CONNECT_TO_SYSTEM", .string = PMIX_CONNECT_TO_SYSTEM, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Connect to system PMIx server", NULL}},
        {.name = "PMIX_CONNECT_SYSTEM_FIRST", .string = PMIX_CONNECT_SYSTEM_FIRST, .type = PMIX_BOOL, .description = (char *[]){"True,False", "Try system PMIx server first", NULL}},
        {.name = "PMIX_SERVER_PIDINFO", .string = PMIX_SERVER_PIDINFO, .type = PMIX_PID, .description = (char *[]){"Valid pid_t value", "PID of target PMIx server", NULL}},
        {.name = "PMIX_TCP_URI", .string = PMIX_TCP_URI, .type = PMIX_STRING, .description = (char *[]){"Valid PMIx URI", "URI of PMIx server to connect to", NULL}},
        {.name = "PMIX_SERVER_URI", .string = PMIX_SERVER_URI, .type = PMIX_STRING, .description = (char *[]){"Valid PMIx URI", "URI of PMIx server to connect to", NULL}},
        {.name = "PMIX_SERVER_NSPACE", .string = PMIX_SERVER_NSPACE, .type = PMIX_STRING, .description = (char *[]){"UNRESTRICTED", "Namespace of PMIx server to", "connect to", NULL}},
        {.name = ""},
};

/*****    REGISTER TOOL ATTRS    *****/
static bool tool_attrs_regd = false;

PMIX_EXPORT pmix_status_t pmix_register_tool_attrs(void)
{
    size_t nregs, nattrs, n, m;
    size_t cnt = 0;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_regattr_t *attrs;

    if (tool_attrs_regd) {
        return PMIX_SUCCESS;
    }
    tool_attrs_regd = true;

    nregs = sizeof(tool_fns) / sizeof(char*);

    /* we know we have to prep the GDS, PTL, BFROPS, and SEC
     * entries as these are dynamically defined */
    tool_attributes[0].description[0] = pmix_gds_base_get_available_modules();

    for (n=0; n < nregs; n++) {
        nattrs = 0;
        while (0 != strlen(tool_attributes[cnt+nattrs].name)) {
            ++nattrs;
        }
        PMIX_REGATTR_CREATE(attrs, nattrs);
        for (m=0; m < nattrs; m++) {
            attrs[m].name = strdup(tool_attributes[m+cnt].name);
            PMIX_LOAD_KEY(attrs[m].string, tool_attributes[m+cnt].string);
            attrs[m].type = tool_attributes[m+cnt].type;
            PMIX_ARGV_COPY(attrs[m].description, tool_attributes[m+cnt].description);
        }
        rc = process_reg(PMIX_TOOL_ATTRIBUTES,
                         tool_fns[n],
                         attrs, nattrs);
        PMIX_REGATTR_FREE(attrs, nattrs);
        if (PMIX_SUCCESS != rc) {
            break;
        }
        cnt += nattrs + 1;
    }

    return PMIX_SUCCESS;
}

/*****   PROCESS QUERY ATTRS    *****/
static void _get_attrs(pmix_list_t *lst,
                       pmix_info_t *info,
                       pmix_list_t *attrs)
{
    pmix_attribute_trk_t *trk, *tptr;
    pmix_infolist_t *ip;
    pmix_data_array_t *darray;
    pmix_regattr_t *regarray;
    size_t m;
    char **fns;

    /* the value in the info is a comma-delimited list of
     * functions whose attributes are being requested */
    fns = pmix_argv_split(info->value.data.string, ',');

    /* search the list for these functions */
    PMIX_LIST_FOREACH(tptr, attrs, pmix_attribute_trk_t) {
        trk = NULL;
        for (m=0; NULL != fns[m] && NULL == trk; m++) {
            if (0 == strcmp(fns[m], tptr->function) ||
                0 == strcmp(fns[m], "all")) {
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
        PMIX_DATA_ARRAY_CREATE(darray, trk->nattrs, PMIX_REGATTR);
        ip->info.value.type = PMIX_DATA_ARRAY;
        ip->info.value.data.darray = darray;
        regarray = (pmix_regattr_t*)darray->array;
        for (m=0; m < trk->nattrs; m++) {
            PMIX_REGATTR_XFER(&regarray[m], &trk->attrs[m]);
        }
        pmix_list_append(lst, &ip->super);
    }
    pmix_argv_free(fns);
}

static void _get_fns(pmix_list_t *lst,
                     pmix_info_t *info,
                     pmix_list_t *attrs)
{
    pmix_attribute_trk_t *tptr;
    pmix_infolist_t *ip;
    char **fns = NULL, *tmp;

    /* search the list for these functions */
    PMIX_LIST_FOREACH(tptr, attrs, pmix_attribute_trk_t) {
        pmix_argv_append_nosize(&fns, tptr->function);
    }
    if (0 < pmix_argv_count(fns)) {
        ip = PMIX_NEW(pmix_infolist_t);
        tmp = pmix_argv_join(fns, ',');
        PMIX_INFO_LOAD(&ip->info, info->key, tmp, PMIX_STRING);
        pmix_list_append(lst, &ip->super);
        pmix_argv_free(fns);
    }
}

static void _local_relcb(void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    PMIX_RELEASE(cd);
}

static void relcbfunc(void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query release callback");

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}
static void query_cbfunc(struct pmix_peer_t *peer,
                         pmix_ptl_hdr_t *hdr,
                         pmix_buffer_t *buf, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    pmix_status_t rc;
    pmix_shift_caddy_t *results;
    int cnt;

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
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    pmix_infolist_t *info, *head;
    pmix_list_t kyresults;
    size_t n, m, p;
    pmix_info_t *iptr;
    pmix_data_array_t *darray;
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_QUERY_CMD;
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    for (n=0; n < cd->nqueries; n++) {
        if (0 != strcmp(cd->queries[n].keys[0], PMIX_QUERY_ATTRIBUTE_SUPPORT)) {
            /* skip this one */
            continue;
        }
        head = NULL;
        for (m=0; m < cd->queries[n].nqual; m++) {
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
                if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
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
                if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
                    _get_fns(&kyresults, &cd->queries[n].qualifiers[m], &server_attrs);
                 } else {
                    /* we need to ask our server for them */
                    PMIX_LIST_DESTRUCT(&kyresults);
                    goto query;
                }
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_TOOL_ATTRIBUTES)) {
                if (PMIX_PROC_IS_TOOL(pmix_globals.mypeer)) {
                    _get_attrs(&kyresults, &cd->queries[n].qualifiers[m], &tool_attrs);
                }
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_TOOL_FUNCTIONS)) {
                if (PMIX_PROC_IS_TOOL(pmix_globals.mypeer)) {
                    _get_fns(&kyresults, &cd->queries[n].qualifiers[m], &tool_attrs);
                }
            }
            if (NULL == cd->queries[n].qualifiers ||
                PMIX_CHECK_KEY(&cd->queries[n].qualifiers[m], PMIX_HOST_ATTRIBUTES)) {
                /* if I am a server, add in the host's */
                if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
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
                if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
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
                iptr = (pmix_info_t*)darray->array;
                p = 0;
                PMIX_LIST_FOREACH(info, &kyresults, pmix_infolist_t) {
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
        PMIX_LIST_FOREACH(info, &cd->results, pmix_infolist_t) {
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
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        cd->status = rc;
        goto release;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cd->nqueries, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        cd->status = rc;
        goto release;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, cd->queries, cd->nqueries, PMIX_QUERY);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        cd->status = rc;
        goto release;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query sending to server");
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, query_cbfunc, (void*)cd);
    if (PMIX_SUCCESS != rc) {
        cd->status = rc;
        goto release;
    }
    return;

  release:
    if (NULL != cd->cbfunc) {
        cd->cbfunc(cd->status, cd->info, cd->ninfo, cd, _local_relcb, cd);
        return;
    }

    PMIX_RELEASE(cd);
}

/*****   PRINT QUERY FUNCTIONS RESULTS   *****/
PMIX_EXPORT char** pmix_attributes_print_functions(char *level)
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
        pmix_argv_append_nosize(&ans, title1);
        lst = &client_attrs;
    } else if (0 == strcmp(level, PMIX_SERVER_FUNCTIONS)) {
        pmix_argv_append_nosize(&ans, title2);
        lst = &server_attrs;
    } else if (0 == strcmp(level, PMIX_HOST_FUNCTIONS)) {
        pmix_argv_append_nosize(&ans, title3);
        lst = &host_attrs;
    } else if (0 == strcmp(level, PMIX_TOOL_FUNCTIONS)) {
        pmix_argv_append_nosize(&ans, title4);
        lst = &tool_attrs;
    } else {
        return NULL;
    }

    PMIX_LIST_FOREACH(fnptr, lst, pmix_attribute_trk_t) {
        pmix_argv_append_nosize(&ans, fnptr->function);
    }
    return ans;
}

/*****   PRINT QUERY ATTRS RESULTS   *****/

#define PMIX_PRINT_NAME_COLUMN_WIDTH      30
#define PMIX_PRINT_STRING_COLUMN_WIDTH    30
#define PMIX_PRINT_TYPE_COLUMN_WIDTH      20
#define PMIX_PRINT_ATTR_COLUMN_WIDTH     120

void pmix_attributes_print_attrs(char ***ans, char *function,
                                 pmix_regattr_t *attrs,
                                 size_t nattrs)
{
    char line[PMIX_PRINT_ATTR_COLUMN_WIDTH], *tmp;
    size_t n, m, len;

    /* print the function */
    memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
    m = 0;
    for (n=0; n < strlen(function); n++) {
        line[m] = function[n];
        ++m;
    }
    line[m++] = ':';
    line[m] = '\0';
    pmix_argv_append_nosize(ans, line);

    for (n=0; n < nattrs; n++) {
        memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
        line[PMIX_PRINT_ATTR_COLUMN_WIDTH-1] = '\0';
        len = strlen(attrs[n].name);
        if (PMIX_PRINT_NAME_COLUMN_WIDTH < len) {
            len = PMIX_PRINT_NAME_COLUMN_WIDTH;
        }
        memcpy(line, attrs[n].name, len);

        len = strlen(attrs[n].string);
        if (PMIX_PRINT_STRING_COLUMN_WIDTH < len) {
            len = PMIX_PRINT_STRING_COLUMN_WIDTH;
        }
        memcpy(&line[PMIX_PRINT_NAME_COLUMN_WIDTH+2], attrs[n].string, len);

        tmp = (char*)PMIx_Data_type_string(attrs[n].type);
        len = strlen(tmp);
        if (PMIX_PRINT_STRING_COLUMN_WIDTH < len) {
            len = PMIX_PRINT_STRING_COLUMN_WIDTH;
        }
        memcpy(&line[PMIX_PRINT_NAME_COLUMN_WIDTH+PMIX_PRINT_STRING_COLUMN_WIDTH+4], tmp, len);

        len = strlen(attrs[n].description[0]);
        if ((PMIX_PRINT_ATTR_COLUMN_WIDTH-PMIX_PRINT_NAME_COLUMN_WIDTH-PMIX_PRINT_STRING_COLUMN_WIDTH-PMIX_PRINT_TYPE_COLUMN_WIDTH-6) < len) {
            len = PMIX_PRINT_ATTR_COLUMN_WIDTH-PMIX_PRINT_NAME_COLUMN_WIDTH-PMIX_PRINT_STRING_COLUMN_WIDTH-PMIX_PRINT_TYPE_COLUMN_WIDTH-6;
        }
        memcpy(&line[PMIX_PRINT_NAME_COLUMN_WIDTH+PMIX_PRINT_STRING_COLUMN_WIDTH+PMIX_PRINT_TYPE_COLUMN_WIDTH+6], attrs[n].description[0], len);
        line[PMIX_PRINT_ATTR_COLUMN_WIDTH-1] = '\0';  // ensure NULL termination
        pmix_argv_append_nosize(ans, line);

        for (m=1; NULL != attrs[n].description[m]; m++) {
            memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
            line[PMIX_PRINT_ATTR_COLUMN_WIDTH-1] = '\0';
            len = strlen(attrs[n].description[m]);
            if ((PMIX_PRINT_ATTR_COLUMN_WIDTH-PMIX_PRINT_NAME_COLUMN_WIDTH-PMIX_PRINT_STRING_COLUMN_WIDTH-PMIX_PRINT_TYPE_COLUMN_WIDTH-6) < len) {
                len = PMIX_PRINT_ATTR_COLUMN_WIDTH-PMIX_PRINT_NAME_COLUMN_WIDTH-PMIX_PRINT_STRING_COLUMN_WIDTH-PMIX_PRINT_TYPE_COLUMN_WIDTH-6;
            }
            memcpy(&line[PMIX_PRINT_NAME_COLUMN_WIDTH+PMIX_PRINT_STRING_COLUMN_WIDTH+PMIX_PRINT_TYPE_COLUMN_WIDTH+6], attrs[n].description[m], len);
            line[PMIX_PRINT_ATTR_COLUMN_WIDTH-1] = '\0';  // ensure NULL termination
            pmix_argv_append_nosize(ans, line);
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
        pmix_argv_append_nosize(ans, title1);
    } else if (0 == strcmp(level, PMIX_SERVER_ATTRIBUTES)) {
        pmix_argv_append_nosize(ans, title2);
    } else if (0 == strcmp(level, PMIX_HOST_ATTRIBUTES)) {
        pmix_argv_append_nosize(ans, title3);
    } else if (0 == strcmp(level, PMIX_TOOL_ATTRIBUTES)) {
        pmix_argv_append_nosize(ans, title4);
    } else {
        return;
    }

    /* print the column headers */
    memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
    line[PMIX_PRINT_ATTR_COLUMN_WIDTH-1] = '\0';
    left = PMIX_PRINT_NAME_COLUMN_WIDTH/2 - 1;
    memcpy(&line[left], "NAME", 4);

    left = 3 + PMIX_PRINT_NAME_COLUMN_WIDTH + (PMIX_PRINT_STRING_COLUMN_WIDTH/2) - 2;
    memcpy(&line[left], "STRING", 6);

    left = 3 + PMIX_PRINT_NAME_COLUMN_WIDTH + PMIX_PRINT_STRING_COLUMN_WIDTH + (PMIX_PRINT_TYPE_COLUMN_WIDTH/2) - 2;
    memcpy(&line[left], "TYPE", 4);

    left = PMIX_PRINT_NAME_COLUMN_WIDTH + PMIX_PRINT_STRING_COLUMN_WIDTH + PMIX_PRINT_TYPE_COLUMN_WIDTH +
           ((PMIX_PRINT_ATTR_COLUMN_WIDTH-PMIX_PRINT_NAME_COLUMN_WIDTH-PMIX_PRINT_STRING_COLUMN_WIDTH-PMIX_PRINT_TYPE_COLUMN_WIDTH)/2) - 3 - strlen("DESCRIPTION")/2;
    memcpy(&line[left], "DESCRIPTION", strlen("DESCRIPTION"));
    pmix_argv_append_nosize(ans, line);

    /* print the dashes under the column headers */
    memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
    line[PMIX_PRINT_ATTR_COLUMN_WIDTH-1] = '\0';
    m=0;
    for (n=0; n < PMIX_PRINT_NAME_COLUMN_WIDTH; n++) {
        line[m] = '-';
        ++m;
    }
    m += 2; // leave gap
    for (n=0; n < PMIX_PRINT_STRING_COLUMN_WIDTH; n++) {
        line[m] = '-';
        ++m;
    }
    m += 2; // leave gap
    for (n=0; n < PMIX_PRINT_TYPE_COLUMN_WIDTH; n++) {
        line[m] = '-';
        ++m;
    }
    m += 2; // leave gap
    while (m < PMIX_PRINT_ATTR_COLUMN_WIDTH-1) {
        line[m] = '-';
        ++m;
    }
    pmix_argv_append_nosize(ans, line);
}

PMIX_EXPORT char** pmix_attributes_print_attr(char *level, char *function)
{
    size_t n;
    char **tmp, **ans=NULL;
    pmix_list_t *lst;
    pmix_attribute_trk_t *fnptr;
    char line[PMIX_PRINT_ATTR_COLUMN_WIDTH];

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
    memset(line, ' ', PMIX_PRINT_ATTR_COLUMN_WIDTH);
    line[1] = '\0';

    /* can be comma-delimited list of functions */
    tmp = pmix_argv_split(function, ',');
    for (n=0; NULL != tmp[n]; n++) {
        PMIX_LIST_FOREACH(fnptr, lst, pmix_attribute_trk_t) {
            if (0 == strcmp(tmp[n], "all")) {
                pmix_attributes_print_attrs(&ans, fnptr->function, fnptr->attrs, fnptr->nattrs);
                pmix_argv_append_nosize(&ans, line);
            } else if (0 == strcmp(tmp[n], fnptr->function)) {
                pmix_attributes_print_attrs(&ans, fnptr->function, fnptr->attrs, fnptr->nattrs);
                break;
            }
        }
    }
    pmix_argv_free(tmp);


    return ans;
}
