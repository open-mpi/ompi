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
 * Copyright (c) 2013-2018 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>

#include <pmix_tool.h>
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
    bool wait;
    int timeout;
} pmix_plookup_globals_t;

pmix_plookup_globals_t pmix_plookup_globals = {0};

pmix_cmd_line_init_t cmd_line_opts[] = {
    { NULL,
      'h', NULL, "help",
      0,
      &pmix_plookup_globals.help, PMIX_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL,
      'v', NULL, "verbose",
      0,
      &pmix_plookup_globals.verbose, PMIX_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL,
      'p', NULL, "pid",
      1,
      &pmix_plookup_globals.pid, PMIX_CMD_LINE_TYPE_INT,
      "Specify starter pid" },

    { NULL,
      'w', NULL, "wait",
      0,
      &pmix_plookup_globals.wait, PMIX_CMD_LINE_TYPE_BOOL,
      "Wait for data to be available" },

    { NULL,
      't', NULL, "timeout",
      0,
      &pmix_plookup_globals.timeout, PMIX_CMD_LINE_TYPE_INT,
      "Max number of seconds to wait for data to become available" },

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
    pmix_info_t *info = NULL;
    size_t ninfo = 0, n;
    mylock_t mylock;
    pmix_cmd_line_t cmd_line;
    pmix_pdata_t *pdata = NULL;
    size_t ndata;
    int count;
    char **keys = NULL;

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

    pmix_mca_base_open();
    pmix_mca_base_cmd_line_setup(&cmd_line);
    rc = pmix_cmd_line_parse(&cmd_line, false, false, argc, argv);

    if (PMIX_SUCCESS != rc) {
        if (PMIX_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    PMIx_Error_string(rc));
        }
        return rc;
    }

    if (pmix_plookup_globals.help) {
        char *str, *args = NULL;
        args = pmix_cmd_line_get_usage_msg(&cmd_line);
        str = pmix_show_help_string("help-plookup.txt", "usage", true,
                                    args);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);
        /* If we show the help message, that should be all we do */
        exit(0);
    }

    if (pmix_plookup_globals.wait) {
        ++ninfo;
        if (0 < pmix_plookup_globals.timeout) {
            ++ninfo;
        }
    }

    /* determine how many keys were given */
    pmix_cmd_line_get_tail(&cmd_line, &count, &keys);
    if (0 == count) {
        /* must give us at least one key */
        fprintf(stderr, "%s: Must provide at least one key to lookup\n", argv[0]);
        exit(1);
    }
    ndata = count;

    /* if we were given the pid of a starter, then direct that
     * we connect to it */

    /* otherwise, use the system connection first, if available */
    PMIX_INFO_CREATE(info, 1);
    PMIX_INFO_LOAD(&info[0], PMIX_CONNECT_SYSTEM_FIRST, NULL, PMIX_BOOL);
    /* init as a tool */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, 1))) {
        fprintf(stderr, "PMIx_tool_init failed: %d\n", rc);
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

    /* setup any info for the lookup */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        PMIX_INFO_LOAD(&info[0], PMIX_WAIT, NULL, PMIX_BOOL);
        if (1 < ninfo) {
            PMIX_INFO_LOAD(&info[1], PMIX_TIMEOUT, &pmix_plookup_globals.timeout, PMIX_INT);
        }
    }

    /* setup the keys */
    PMIX_PDATA_CREATE(pdata, ndata);
    for (n=0; n < ndata; n++) {
        pmix_strncpy(pdata[n].key, keys[n], PMIX_MAX_KEYLEN);
    }
    /* perform the lookup */
    rc = PMIx_Lookup(pdata, ndata, info, ninfo);

    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx_Lookup failed: %d\n", rc);
        goto done;
    }

    for (n=0; n < ndata; n++) {
        fprintf(stderr, "Key: %s\n", pdata[n].key);
    }
    PMIX_PDATA_FREE(pdata, ndata);

  done:
    PMIx_tool_finalize();

    return(rc);
}
