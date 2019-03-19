/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "pmix_config.h"
#include "pmix_common.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>

#include <pmix_tool.h>
#include "src/common/pmix_attributes.h"
#include "src/mca/base/base.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/threads/threads.h"
#include "src/util/cmd_line.h"
#include "src/util/keyval_parse.h"
#include "src/util/show_help.h"
#include "src/runtime/pmix_rte.h"

typedef struct {
    pmix_lock_t lock;
    pmix_status_t status;
} mylock_t;

static pmix_proc_t myproc;

/* define a structure for collecting returned
 * info from a query */
typedef struct {
    pmix_lock_t lock;
    pmix_status_t status;
    pmix_info_t *info;
    size_t ninfo;
} myquery_data_t;


/* this is a callback function for the PMIx_Query
 * API. The query will callback with a status indicating
 * if the request could be fully satisfied, partially
 * satisfied, or completely failed. The info parameter
 * contains an array of the returned data, with the
 * info->key field being the key that was provided in
 * the query call. Thus, you can correlate the returned
 * data in the info->value field to the requested key.
 *
 * Once we have dealt with the returned data, we must
 * call the release_fn so that the PMIx library can
 * cleanup */
static void cbfunc(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    myquery_data_t *mq = (myquery_data_t*)cbdata;
    size_t n;

    mq->status = status;
    /* save the returned info - the PMIx library "owns" it
     * and will release it and perform other cleanup actions
     * when release_fn is called */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    PMIX_WAKEUP_THREAD(&mq->lock);
}

/* this is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
 * handler. We don't technically need to register one, but it
 * is usually good practice to catch any events that occur */
static void notification_fn(size_t evhdlr_registration_id,
                            pmix_status_t status,
                            const pmix_proc_t *source,
                            pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc,
                            void *cbdata)
{
    /* this example doesn't do anything with default events */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

/* event handler registration is done asynchronously because it
 * may involve the PMIx server registering with the host RM for
 * external events. So we provide a callback function that returns
 * the status of the request (success or an error), plus a numerical index
 * to the registered event. The index is used later on to deregister
 * an event handler - if we don't explicitly deregister it, then the
 * PMIx server will do so when it see us exit */
static void evhandler_reg_callbk(pmix_status_t status,
                                 size_t evhandler_ref,
                                 void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   myproc.nspace, myproc.rank, status, (unsigned long)evhandler_ref);
    }
    lock->status = status;
    PMIX_WAKEUP_THREAD(&lock->lock);
}

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    bool verbose;
    pid_t pid;
    char *nspace;
    char *uri;
    bool sysfirst;
    bool system;
    char *client;
    char *server;
    char *tool;
    char *host;
    bool clientfns;
    bool serverfns;
    bool toolfns;
    bool hostfns;
} pmix_pattrs_globals_t;

pmix_pattrs_globals_t pmix_pattrs_globals = {0};

pmix_cmd_line_init_t cmd_line_opts[] = {
    { NULL,
      'h', NULL, "help",
      0,
      &pmix_pattrs_globals.help, PMIX_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL,
      'v', NULL, "verbose",
      0,
      &pmix_pattrs_globals.verbose, PMIX_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL,
      'p', NULL, "pid",
      1,
      &pmix_pattrs_globals.pid, PMIX_CMD_LINE_TYPE_INT,
      "Specify server pid to connect to" },

    { NULL,
      'n', NULL, "nspace",
      1,
      &pmix_pattrs_globals.nspace, PMIX_CMD_LINE_TYPE_STRING,
      "Specify server nspace to connect to" },

    { NULL,
      '\0', NULL, "uri",
      1,
      &pmix_pattrs_globals.uri, PMIX_CMD_LINE_TYPE_STRING,
      "Specify URI of server to connect to" },

    { NULL,
      '\0', NULL, "system-server-first",
      0,
      &pmix_pattrs_globals.sysfirst, PMIX_CMD_LINE_TYPE_BOOL,
      "Look for the system server first" },

    { NULL,
      '\0', NULL, "system-server",
      0,
      &pmix_pattrs_globals.system, PMIX_CMD_LINE_TYPE_BOOL,
      "Specifically connect to the system server" },

    { NULL,
      'c', NULL, "client",
      1,
      &pmix_pattrs_globals.client, PMIX_CMD_LINE_TYPE_STRING,
      "Comma-delimited list of client function whose attributes are to be printed (function or all)" },

    { NULL,
      's', NULL, "server",
      1,
      &pmix_pattrs_globals.server, PMIX_CMD_LINE_TYPE_STRING,
      "Comma-delimited list of server function whose attributes are to be printed (function or all)" },

    { NULL,
      't', NULL, "tool",
      1,
      &pmix_pattrs_globals.tool, PMIX_CMD_LINE_TYPE_STRING,
      "Comma-delimited list of tool function whose attributes are to be printed (function or all)" },

    { NULL,
      'h', NULL, "host",
      1,
      &pmix_pattrs_globals.host, PMIX_CMD_LINE_TYPE_STRING,
      "Comma-delimited list of host function whose attributes are to be printed (function or all)" },

    { NULL,
      '\0', NULL, "client-fns",
      0,
      &pmix_pattrs_globals.clientfns, PMIX_CMD_LINE_TYPE_BOOL,
      "List the functions supported in this client library" },

    { NULL,
      '\0', NULL, "server-fns",
      0,
      &pmix_pattrs_globals.serverfns, PMIX_CMD_LINE_TYPE_BOOL,
      "List the functions supported in this server library" },

    { NULL,
      '\0', NULL, "tool-fns",
      0,
      &pmix_pattrs_globals.toolfns, PMIX_CMD_LINE_TYPE_BOOL,
      "List the functions supported in this tool library" },

    { NULL,
      '\0', NULL, "host-fns",
      0,
      &pmix_pattrs_globals.hostfns, PMIX_CMD_LINE_TYPE_BOOL,
      "List the functions supported by this host environment" },


    /* End of list */
    { NULL,
      '\0', NULL, NULL,
      0,
      NULL, PMIX_CMD_LINE_TYPE_NULL,
      NULL }
};

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_info_t *info;
    mylock_t mylock;
    pmix_cmd_line_t cmd_line;
    char **fns;
    size_t n, m;
    myquery_data_t mq;
    pmix_query_t query;
    pmix_regattr_t *reg;
    char **ans = NULL;

    /* protect against problems if someone passes us thru a pipe
     * and then abnormally terminates the pipe early */
    signal(SIGPIPE, SIG_IGN);

    /* initialize the output system */
    if (!pmix_output_init()) {
        return PMIX_ERROR;
    }

    /* initialize install dirs code */
    if (PMIX_SUCCESS != (rc = pmix_mca_base_framework_open(&pmix_pinstalldirs_base_framework, 0))) {
        fprintf(stderr, "pmix_pinstalldirs_base_open() failed -- process will likely abort (%s:%d, returned %d instead of PMIX_SUCCESS)\n",
                __FILE__, __LINE__, rc);
        return rc;
    }

    /* initialize the help system */
    pmix_show_help_init();

    /* keyval lex-based parser */
    if (PMIX_SUCCESS != (rc = pmix_util_keyval_parse_init())) {
        fprintf(stderr, "pmix_util_keyval_parse_init failed with %d\n", rc);
        return PMIX_ERROR;
    }

    /* Setup the parameter system */
    if (PMIX_SUCCESS != (rc = pmix_mca_base_var_init())) {
        fprintf(stderr, "pmix_mca_base_var_init failed with %d\n", rc);
        return PMIX_ERROR;
    }

    /* register params for pmix */
    if (PMIX_SUCCESS != (rc = pmix_register_params())) {
        fprintf(stderr, "pmix_register_params failed with %d\n", rc);
        return PMIX_ERROR;
    }

    /* Parse the command line options */
    pmix_cmd_line_create(&cmd_line, cmd_line_opts);

    rc = pmix_cmd_line_parse(&cmd_line, false, false, argc, argv);

    if (PMIX_SUCCESS != rc) {
        if (PMIX_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    PMIx_Error_string(rc));
        }
        return rc;
    }

    if (pmix_pattrs_globals.help) {
        char *str, *args = NULL;
        args = pmix_cmd_line_get_usage_msg(&cmd_line);
        str = pmix_show_help_string("help-pattrs.txt", "usage", true,
                                    args);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);
        /* If we show the help message, that should be all we do */
        exit(0);
    }

    /* cannot list functions and get attributes at same time */
    if ((pmix_pattrs_globals.clientfns || pmix_pattrs_globals.serverfns || pmix_pattrs_globals.toolfns || pmix_pattrs_globals.hostfns)
        && (pmix_pattrs_globals.client || pmix_pattrs_globals.server || pmix_pattrs_globals.tool || NULL != pmix_pattrs_globals.host)) {
        fprintf(stderr, "Cannot request both a list of functions and attributes at same time\n");
        exit(1);
    }

    /* if they are asking for client, server, or tool attrs, then
     * we don't need to connect to anyone - just register the
     * attrs and report */
    if (pmix_pattrs_globals.clientfns || pmix_pattrs_globals.serverfns || pmix_pattrs_globals.toolfns ||
        pmix_pattrs_globals.client || pmix_pattrs_globals.server || pmix_pattrs_globals.tool) {
        PMIX_INFO_CREATE(info, 1);
        PMIX_INFO_LOAD(&info[0], PMIX_TOOL_DO_NOT_CONNECT, NULL, PMIX_BOOL);
        PMIx_tool_init(&myproc, info, 1);
        if (pmix_pattrs_globals.clientfns) {
            pmix_register_client_attrs();
            fns = pmix_attributes_print_functions(PMIX_CLIENT_FUNCTIONS);
        } else if (pmix_pattrs_globals.serverfns) {
            pmix_register_server_attrs();
            fns = pmix_attributes_print_functions(PMIX_SERVER_FUNCTIONS);
        } else if (pmix_pattrs_globals.toolfns) {
            pmix_register_tool_attrs();
            fns = pmix_attributes_print_functions(PMIX_TOOL_FUNCTIONS);
        } else if (NULL != pmix_pattrs_globals.client) {
            pmix_register_client_attrs();
            fns = pmix_attributes_print_attr(PMIX_CLIENT_ATTRIBUTES, pmix_pattrs_globals.client);
        } else if (NULL != pmix_pattrs_globals.server) {
            pmix_register_server_attrs();
            fns = pmix_attributes_print_attr(PMIX_SERVER_ATTRIBUTES, pmix_pattrs_globals.server);
        } else if (NULL != pmix_pattrs_globals.tool) {
            pmix_register_tool_attrs();
            fns = pmix_attributes_print_attr(PMIX_TOOL_ATTRIBUTES, pmix_pattrs_globals.tool);
        }
        if (NULL != fns) {
            for (n=0; NULL != fns[n]; n++) {
                fprintf(stderr, "%s\n", fns[n]);
            }
        }
        goto done;
    }

    /* if they didn't give us an option, then we can't do anything */
    if (!pmix_pattrs_globals.hostfns && NULL == pmix_pattrs_globals.host) {
        char *str, *args = NULL;
        args = pmix_cmd_line_get_usage_msg(&cmd_line);
        str = pmix_show_help_string("help-pattrs.txt", "usage", true,
                                    args);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);
        exit(1);
    }

    /* if we were given the pid of a starter, then direct that
     * we connect to it */
    n = 1;
    PMIX_INFO_CREATE(info, n);
    if (0 < pmix_pattrs_globals.pid) {
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_PIDINFO, &pmix_pattrs_globals.pid, PMIX_PID);
    } else if (NULL != pmix_pattrs_globals.nspace) {
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_NSPACE, pmix_pattrs_globals.nspace, PMIX_STRING);
    } else if (NULL != pmix_pattrs_globals.uri) {
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_URI, pmix_pattrs_globals.uri, PMIX_STRING);
    } else if (pmix_pattrs_globals.sysfirst) {
        /* otherwise, use the system connection first, if available */
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECT_SYSTEM_FIRST, NULL, PMIX_BOOL);
    } else if (pmix_pattrs_globals.system) {
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECT_TO_SYSTEM, NULL, PMIX_BOOL);
    } else {
        PMIX_INFO_FREE(info, 1);
        n = 0;
    }
    /* init as a tool */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, n))) {
        fprintf(stderr, "PMIx_tool_init failed: %s\n", PMIx_Error_string(rc));
        exit(rc);
    }
    PMIX_INFO_FREE(info, 1);

    /* register a default event handler */
    PMIX_CONSTRUCT_LOCK(&mylock.lock);
    PMIx_Register_event_handler(NULL, 0, NULL, 0,
                                notification_fn, evhandler_reg_callbk, (void*)&mylock);
    PMIX_WAIT_THREAD(&mylock.lock);
    if (PMIX_SUCCESS != mylock.status) {
        fprintf(stderr, "PMIx_Register_event_handler returned bad status: %d\n", rc);
        PMIX_DESTRUCT_LOCK(&mylock.lock);
        goto done;
    }
    PMIX_DESTRUCT_LOCK(&mylock.lock);

    /* generate the query */
    PMIX_QUERY_CONSTRUCT(&query);
    pmix_argv_append_nosize(&query.keys, PMIX_QUERY_ATTRIBUTE_SUPPORT);
    PMIX_QUERY_QUALIFIERS_CREATE(&query, 1);
    if (pmix_pattrs_globals.hostfns) {
        PMIX_INFO_LOAD(&query.qualifiers[0], PMIX_HOST_FUNCTIONS, NULL, PMIX_BOOL);
    } else {
        PMIX_INFO_LOAD(&query.qualifiers[0], PMIX_HOST_ATTRIBUTES, pmix_pattrs_globals.host, PMIX_STRING);
    }
    PMIX_CONSTRUCT_LOCK(&mq.lock);
    rc = PMIx_Query_info_nb(&query, 1, cbfunc,(void*)&mq);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx_Notify_event failed: %d\n", rc);
        goto done;
    }
    PMIX_WAIT_THREAD(&mq.lock);
    PMIX_DESTRUCT_LOCK(&mq.lock);
    if (PMIX_SUCCESS != mq.status) {
        fprintf(stderr, "PMIx_Query returned: %s\n", PMIx_Error_string(mq.status));
    } else {
        /* print out the returned value(s) */
        for (n=0; n < mq.ninfo; n++) {
            if (PMIX_CHECK_KEY(&mq.info[n], PMIX_HOST_FUNCTIONS)) {
                fns = pmix_argv_split(mq.info[n].value.data.string, ',');
                fprintf(stderr, "HOST SUPPORTED FUNCTIONS:\n");
                for (m=0; NULL != fns[m]; m++) {
                    fprintf(stderr, "\t%s\n", fns[m]);
                }
                pmix_argv_free(fns);
            } else {
                pmix_attributes_print_headers(&ans, PMIX_HOST_ATTRIBUTES);
                if (PMIX_DATA_ARRAY == mq.info[n].value.type) {
                    info = (pmix_info_t*)mq.info[n].value.data.darray->array;
                    for (m=0; m < mq.info[n].value.data.darray->size; m++) {
                        reg = (pmix_regattr_t*)info[m].value.data.darray->array;
                        pmix_attributes_print_attrs(&ans, info[m].key, reg, info[0].value.data.darray->size);
                    }
                } else {
                    reg = (pmix_regattr_t*)mq.info[n].value.data.ptr;
                    pmix_attributes_print_attrs(&ans, mq.info[n].key, reg, 1);
                }
                for (m=0; NULL != ans[m]; m++) {
                    fprintf(stderr, "%s\n", ans[m]);
                }
                pmix_argv_free(ans);
                ans = NULL;
            }
        }
    }

  done:
    PMIx_tool_finalize();

    return(rc);
}
