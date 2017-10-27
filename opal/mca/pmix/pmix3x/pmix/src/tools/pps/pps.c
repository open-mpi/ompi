/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @fie
 * PMIX PS command
 *
 */

#include "pmix_config.h"
#include "pmix_common.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <stdlib.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#include <string.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */

#include "src/threads/threads.h"
#include "src/util/basename.h"
#include "src/util/cmd_line.h"
#include "src/util/keyval_parse.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/util/show_help.h"
#include "src/mca/base/base.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/runtime/pmix_rte.h"

#include "src/include/pmix_globals.h"
#include <pmix_tool.h>
#include <pmix.h>

typedef struct {
    pmix_lock_t lock;
    pmix_status_t status;
} mylock_t;

/* define a structure for collecting returned
 * info from a query */
typedef struct {
    mylock_t lock;
    pmix_info_t *info;
    size_t ninfo;
} myquery_data_t;

static pmix_proc_t myproc;

/******************
 * Local Functions
 ******************/
#if 0
static int gather_information(pmix_ps_mpirun_info_t *hnpinfo);
static int gather_active_jobs(pmix_ps_mpirun_info_t *hnpinfo);
static int gather_nodes(pmix_ps_mpirun_info_t *hnpinfo);
static int gather_vpid_info(pmix_ps_mpirun_info_t *hnpinfo);

static int pretty_print(pmix_ps_mpirun_info_t *hnpinfo);
static int pretty_print_nodes(pmix_node_t **nodes, pmix_std_cntr_t num_nodes);
static int pretty_print_jobs(pmix_job_t **jobs, pmix_std_cntr_t num_jobs);
static int pretty_print_vpids(pmix_job_t *job);
static void pretty_print_dashed_line(int len);

static char *pretty_node_state(pmix_node_state_t state);

static int parseable_print(pmix_ps_mpirun_info_t *hnpinfo);
#endif

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    bool parseable;
    bool nodes;
    char *nspace;
    pid_t pid;
} pmix_ps_globals_t;

pmix_ps_globals_t pmix_ps_globals = {0};

pmix_cmd_line_init_t cmd_line_opts[] = {
    { NULL,
      'h', NULL, "help",
      0,
      &pmix_ps_globals.help, PMIX_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL,
      '\0', NULL, "parseable",
      0,
      &pmix_ps_globals.parseable, PMIX_CMD_LINE_TYPE_BOOL,
      "Provide parseable output" },

    { NULL,
      '\0', NULL, "nspace",
      0,
     &pmix_ps_globals.nspace, PMIX_CMD_LINE_TYPE_STRING,
      "Nspace of job whose status is being requested" },

    { NULL,
      'p', NULL, "pid",
      1,
      &pmix_ps_globals.pid, PMIX_CMD_LINE_TYPE_INT,
      "Specify pid of starter to be contacted (default is to system server" },

    { NULL,
      'n', NULL, "nodes",
      0,
      &pmix_ps_globals.nodes, PMIX_CMD_LINE_TYPE_BOOL,
      "Display Node Information" },

    /* End of list */
    { NULL,
      '\0', NULL, NULL,
      0,
      NULL, PMIX_CMD_LINE_TYPE_NULL,
      NULL }
};

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

    /* save the returned info - the PMIx library "owns" it
     * and will release it and perform other cleanup actions
     * when release_fn is called */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n=0; n < ninfo; n++) {
            fprintf(stderr, "Transferring %s\n", info[n].key);
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    PMIX_WAKEUP_THREAD(&mq->lock.lock);
}

/* this is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
 * handler. */
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

int
main(int argc, char *argv[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_info_t *info;
    pmix_query_t *query;
    size_t nq;
    myquery_data_t myquery_data;
    mylock_t mylock;
    pmix_cmd_line_t cmd_line;

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

    if (pmix_ps_globals.help) {
        char *str, *args = NULL;
        args = pmix_cmd_line_get_usage_msg(&cmd_line);
        str = pmix_show_help_string("help-pps.txt", "usage", true,
                                    args);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);
        /* If we show the help message, that should be all we do */
        exit(0);
    }

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
    PMIX_DESTRUCT_LOCK(&mylock.lock);

    /* if we were given a specific nspace to ask about, then do so */

    /* if we were asked to provide the status of the nodes, then do that */

    /* otherwise, query the active nspaces */
    nq = 1;
    PMIX_QUERY_CREATE(query, nq);
    PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_NAMESPACES);
    /* setup the caddy to retrieve the data */
    PMIX_CONSTRUCT_LOCK(&myquery_data.lock.lock);
    myquery_data.info = NULL;
    myquery_data.ninfo = 0;
    /* execute the query */
    fprintf(stderr, "pps: querying nspaces\n");
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&myquery_data))) {
        fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
        goto done;
    }
    PMIX_WAIT_THREAD(&myquery_data.lock.lock);
    PMIX_DESTRUCT_LOCK(&myquery_data.lock.lock);

    /* we should have received back one info struct containing
     * a comma-delimited list of nspaces */
    if (1 != myquery_data.ninfo) {
        /* this is an error */
        fprintf(stderr, "PMIx Query returned an incorrect number of results: %lu\n", myquery_data.ninfo);
        PMIX_INFO_FREE(myquery_data.info, myquery_data.ninfo);
        goto done;
    }

    fprintf(stderr, "Active nspaces: %s\n", myquery_data.info[0].value.data.string);

    /***************
     * Cleanup
     ***************/
  done:
    PMIx_tool_finalize();

    return rc;
}

#if 0
static int pretty_print(pmix_ps_mpirun_info_t *hnpinfo) {
    char *header;
    int len_hdr;

    /*
     * Print header and remember header length
     */
    len_hdr = asprintf(&header, "Information from mpirun %s", PMIX_JOBID_PRINT(hnpinfo->hnp->name.jobid));

    printf("\n\n%s\n", header);
    free(header);
    pretty_print_dashed_line(len_hdr);

    /*
     * Print Node Information
     */
    if( pmix_ps_globals.nodes )
        pretty_print_nodes(hnpinfo->nodes, hnpinfo->num_nodes);

    /*
     * Print Job Information
     */
    pretty_print_jobs(hnpinfo->jobs, hnpinfo->num_jobs);

    return PMIX_SUCCESS;
}

static int pretty_print_nodes(pmix_node_t **nodes, pmix_std_cntr_t num_nodes) {
    int line_len;
    int len_name    = 0,
        len_state   = 0,
        len_slots   = 0,
        len_slots_i = 0,
        len_slots_m = 0;
    pmix_node_t *node;
    pmix_std_cntr_t i;

    /*
     * Caculate segment lengths
     */
    len_name    = (int) strlen("Node Name");
    len_state   = (int) strlen("State");
    len_slots   = (int) strlen("Slots");
    len_slots_i = (int) strlen("Slots In Use");
    len_slots_m = (int) strlen("Slots Max");

    for(i=0; i < num_nodes; i++) {
        node = nodes[i];

        if( NULL != node->name &&
            (int)strlen(node->name) > len_name)
            len_name = (int) strlen(node->name);

        if( (int)strlen(pretty_node_state(node->state)) > len_state )
            len_state = (int)strlen(pretty_node_state(node->state));
    }

    line_len = (len_name    + 3 +
                len_state   + 3 +
                len_slots   + 3 +
                len_slots_i + 3 +
                len_slots_m) + 2;

    /*
     * Print the header
     */
    printf("%*s | ", len_name,    "Node Name");
    printf("%*s | ", len_state,   "State");
    printf("%*s | ", len_slots,   "Slots");
    printf("%*s | ", len_slots_m, "Slots Max");
    printf("%*s | ", len_slots_i, "Slots In Use");
    printf("\n");

    pretty_print_dashed_line(line_len);

    /*
     * Print Info
     */
    for(i=0; i < num_nodes; i++) {
        node = nodes[i];

        printf("%*s | ", len_name,    node->name);
        printf("%*s | ", len_state,   pretty_node_state(node->state));
        printf("%*d | ", len_slots,   (uint)node->slots);
        printf("%*d | ", len_slots_m, (uint)node->slots_max);
        printf("%*d | ", len_slots_i, (uint)node->slots_inuse);
        printf("\n");

    }

    return PMIX_SUCCESS;
}

static int pretty_print_jobs(pmix_job_t **jobs, pmix_std_cntr_t num_jobs) {
    int len_jobid = 0,
        len_state = 0,
        len_slots = 0,
        len_vpid_r = 0,
        len_ckpt_s = 0,
        len_ckpt_r = 0,
        len_ckpt_l = 0;
    int line_len;
    pmix_job_t *job;
    pmix_std_cntr_t i;
    char *jobstr;
    pmix_jobid_t mask=0x0000ffff;

    for(i=0; i < num_jobs; i++) {
        job = jobs[i];

        /* check the jobid to see if this is the daemons' job */
        if ((0 == (mask & job->jobid)) && !pmix_ps_globals.daemons) {
            continue;
        }

        /* setup the printed name - do -not- free this! */
        jobstr = PMIX_JOBID_PRINT(job->jobid);

        /*
         * Caculate segment lengths
         */
        len_jobid  = strlen(jobstr);;
        len_state  = (int) (strlen(pmix_job_state_to_str(job->state)) < strlen("State") ?
                            strlen("State") :
                            strlen(pmix_job_state_to_str(job->state)));
        len_slots  = 6;
        len_vpid_r = (int) strlen("Num Procs");
        len_ckpt_s = -3;
        len_ckpt_r = -3;
        len_ckpt_l = -3;

        line_len = (len_jobid  + 3 +
                    len_state  + 3 +
                    len_slots  + 3 +
                    len_vpid_r + 3 +
                    len_ckpt_s + 3 +
                    len_ckpt_r + 3 +
                    len_ckpt_l)
                    + 2;

        /*
         * Print Header
         */
        printf("\n");
        printf("%*s | ", len_jobid  , "JobID");
        printf("%*s | ", len_state  , "State");
        printf("%*s | ", len_slots  , "Slots");
        printf("%*s | ", len_vpid_r , "Num Procs");
        printf("\n");

        pretty_print_dashed_line(line_len);

        /*
         * Print Info
         */
        printf("%*s | ",  len_jobid ,  PMIX_JOBID_PRINT(job->jobid));
        printf("%*s | ",  len_state ,  pmix_job_state_to_str(job->state));
        printf("%*d | ",  len_slots ,  (uint)job->total_slots_alloc);
        printf("%*d | ",  len_vpid_r,  job->num_procs);
        printf("\n");


        pretty_print_vpids(job);
        printf("\n\n"); /* give a little room between job outputs */
    }

    return PMIX_SUCCESS;
}

static int pretty_print_vpids(pmix_job_t *job) {
    int len_o_proc_name = 0,
        len_proc_name   = 0,
        len_rank        = 0,
        len_pid         = 0,
        len_state       = 0,
        len_node        = 0,
        len_ckpt_s      = 0,
        len_ckpt_r      = 0,
        len_ckpt_l      = 0;
    int i, line_len;
    pmix_vpid_t v;
    pmix_proc_t *vpid;
    pmix_app_context_t *app;
    char *o_proc_name;
    char **nodename = NULL;

    if (0 == job->num_procs) {
        return PMIX_SUCCESS;
    }

    /*
     * Caculate segment lengths
     */
    len_o_proc_name = (int)strlen("PMIX Name");
    len_proc_name   = (int)strlen("Process Name");
    len_rank        = (int)strlen("Local Rank");
    len_pid         = 6;
    len_state       = 0;
    len_node        = 0;
    len_ckpt_s      = -3;
    len_ckpt_r      = -3;
    len_ckpt_l      = -3;

    nodename = (char **) malloc(job->num_procs * sizeof(char *));
    for(v=0; v < job->num_procs; v++) {
        char *rankstr;
        vpid = (pmix_proc_t*)job->procs->addr[v];

        /*
         * Find my app context
         */
        if( 0 >= (int)job->num_apps ) {
            if( 0 == vpid->name.vpid ) {
                if( (int)strlen("pmixrun") > len_proc_name)
                    len_proc_name = strlen("pmixrun");
            }
            else {
                if( (int)strlen("pmixd") > len_proc_name)
                    len_proc_name = strlen("pmixd");
            }
        }
        for( i = 0; i < (int)job->num_apps; ++i) {
            app = (pmix_app_context_t*)job->apps->addr[i];
            if( app->idx == vpid->app_idx ) {
                if( (int)strlen(app->app) > len_proc_name)
                    len_proc_name = strlen(app->app);
                break;
            }
        }

        o_proc_name = pmix_util_print_name_args(&vpid->name);
        if ((int)strlen(o_proc_name) > len_o_proc_name)
            len_o_proc_name = strlen(o_proc_name);

        asprintf(&rankstr, "%u", (uint)vpid->local_rank);
        if ((int)strlen(rankstr) > len_rank)
            len_rank = strlen(rankstr);
        free(rankstr);

        nodename[v] = NULL;
        if( pmix_get_attribute(&vpid->attributes, PMIX_PROC_NODENAME, (void**)&nodename[v], PMIX_STRING) &&
            (int)strlen(nodename[v]) > len_node) {
            len_node = strlen(nodename[v]);
        } else if ((int)strlen("Unknown") > len_node) {
            len_node = strlen("Unknown");
        }

        if( (int)strlen(pmix_proc_state_to_str(vpid->state)) > len_state)
            len_state = strlen(pmix_proc_state_to_str(vpid->state));

    }

    line_len = (len_o_proc_name + 3 +
                len_proc_name   + 3 +
                len_rank        + 3 +
                len_pid         + 3 +
                len_state       + 3 +
                len_node        + 3 +
                len_ckpt_s      + 3 +
                len_ckpt_r      + 3 +
                len_ckpt_l)
                + 2;

    /*
     * Print Header
     */
    printf("\t");
    printf("%*s | ", len_proc_name   , "Process Name");
    printf("%*s | ", len_o_proc_name , "PMIX Name");
    printf("%*s | ", len_rank        , "Local Rank");
    printf("%*s | ", len_pid         , "PID");
    printf("%*s | ", len_node        , "Node");
    printf("%*s | ", len_state       , "State");
    printf("\n");

    printf("\t");
    pretty_print_dashed_line(line_len);

    /*
     * Print Info
     */
    for(v=0; v < job->num_procs; v++) {
        vpid = (pmix_proc_t*)job->procs->addr[v];

        printf("\t");

        if( 0 >= (int)job->num_apps ) {
            if( 0 == vpid->name.vpid ) {
                printf("%*s | ", len_proc_name, "pmixrun");
            } else {
                printf("%*s | ", len_proc_name, "pmixd");
            }
        }
        for( i = 0; i < (int)job->num_apps; ++i) {
            app = (pmix_app_context_t*)job->apps->addr[i];
            if( app->idx == vpid->app_idx ) {
                printf("%*s | ", len_proc_name, app->app);
                break;
            }
        }

        o_proc_name = pmix_util_print_name_args(&vpid->name);

        printf("%*s | ",  len_o_proc_name, o_proc_name);
        printf("%*u | ",  len_rank       , (uint)vpid->local_rank);
        printf("%*d | ",  len_pid        , vpid->pid);
        printf("%*s | ",  len_node       , (NULL == nodename[v]) ? "Unknown" : nodename[v]);
        printf("%*s | ",  len_state      , pmix_proc_state_to_str(vpid->state));

        if (NULL != nodename[v]) {
            free(nodename[v]);
        }
        printf("\n");

    }
    if (NULL != nodename) {
        free(nodename);
    }
    return PMIX_SUCCESS;
}

static void pretty_print_dashed_line(int len) {
    static const char dashes[9] = "--------";

    while (len >= 8) {
        printf("%8.8s", dashes);
        len -= 8;
    }
    printf("%*.*s\n", len, len, dashes);
}

static int gather_information(pmix_ps_mpirun_info_t *hnpinfo) {
    int ret;

    if( PMIX_SUCCESS != (ret = gather_active_jobs(hnpinfo) )) {
        goto cleanup;
    }

    if( PMIX_SUCCESS != (ret = gather_nodes(hnpinfo) )) {
        goto cleanup;
    }

    if( PMIX_SUCCESS != (ret = gather_vpid_info(hnpinfo) )) {
        goto cleanup;
    }

 cleanup:
    return ret;
}

static int gather_active_jobs(pmix_ps_mpirun_info_t *hnpinfo) {
    int ret;

    if (PMIX_SUCCESS != (ret = pmix_util_comm_query_job_info(&(hnpinfo->hnp->name), pmix_ps_globals.jobid,
                                                             &hnpinfo->num_jobs, &hnpinfo->jobs))) {
        PMIX_ERROR_LOG(ret);
    }

    return ret;
}

static int gather_nodes(pmix_ps_mpirun_info_t *hnpinfo) {
    int ret;

    if (PMIX_SUCCESS != (ret = pmix_util_comm_query_node_info(&(hnpinfo->hnp->name), NULL,
                                                             &hnpinfo->num_nodes, &hnpinfo->nodes))) {
        PMIX_ERROR_LOG(ret);
    }
    pmix_output(0, "RECEIVED %d NODES", hnpinfo->num_nodes);
    return ret;
}

static int gather_vpid_info(pmix_ps_mpirun_info_t *hnpinfo) {
    int ret;
    pmix_std_cntr_t i;
    int cnt;
    pmix_job_t *job;
    pmix_proc_t **procs;

    /*
     * For each Job in the HNP
     */
    for(i=0; i < hnpinfo->num_jobs; i++) {
        job = hnpinfo->jobs[i];

        /*
         * Skip getting the vpid's for the HNP, unless asked to do so
         * The HNP is always the first in the array
         */
        if( 0 == i && !pmix_ps_globals.daemons) {
            continue;
        }

        /* query the HNP for info on the procs in this job */
        if (PMIX_SUCCESS != (ret = pmix_util_comm_query_proc_info(&(hnpinfo->hnp->name),
                                                                  job->jobid,
                                                                  PMIX_VPID_WILDCARD,
                                                                  &cnt,
                                                                  &procs))) {
            PMIX_ERROR_LOG(ret);
        }
        job->procs->addr = (void**)procs;
        job->procs->size = cnt;
        job->num_procs = cnt;
    }

    return PMIX_SUCCESS;
}

static char *pretty_node_state(pmix_node_state_t state) {
    switch(state) {
    case PMIX_NODE_STATE_DOWN:
        return strdup("Down");
        break;
    case PMIX_NODE_STATE_UP:
        return strdup("Up");
        break;
    case PMIX_NODE_STATE_REBOOT:
        return strdup("Reboot");
        break;
    case PMIX_NODE_STATE_UNKNOWN:
    default:
        return strdup("Unknown");
        break;
    }
}

static int parseable_print(pmix_ps_mpirun_info_t *hnpinfo)
{
    pmix_job_t **jobs;
    pmix_node_t **nodes;
    pmix_proc_t *proc;
    pmix_app_context_t *app;
    char *appname;
    int i, j;
    char *nodename;

    /* don't include the daemon job in the number of jobs reppmixd */
    printf("mpirun:%lu:num nodes:%d:num jobs:%d\n",
           (unsigned long)hnpinfo->hnp->pid, hnpinfo->num_nodes, hnpinfo->num_jobs-1);

    if (pmix_ps_globals.nodes) {
        nodes = hnpinfo->nodes;
        for (i=0; i < hnpinfo->num_nodes; i++) {
            printf("node:%s:state:%s:slots:%d:in use:%d\n",
                   nodes[i]->name, pretty_node_state(nodes[i]->state),
                   nodes[i]->slots, nodes[i]->slots_inuse);
        }
    }

    jobs = hnpinfo->jobs;
    /* skip job=0 as that's the daemon job */
    for (i=1; i < hnpinfo->num_jobs; i++) {
        printf("jobid:%d:state:%s:slots:%d:num procs:%d\n",
               PMIX_LOCAL_JOBID(jobs[i]->jobid),
               pmix_job_state_to_str(jobs[i]->state),
               jobs[i]->total_slots_alloc,
               jobs[i]->num_procs);
        /* print the proc info */
        for (j=0; j < jobs[i]->procs->size; j++) {
            if (NULL == (proc = (pmix_proc_t*)pmix_pointer_array_get_item(jobs[i]->procs, j))) {
                continue;
            }
            app = (pmix_app_context_t*)pmix_pointer_array_get_item(jobs[i]->apps, proc->app_idx);
            if (NULL == app) {
                appname = strdup("NULL");
            } else {
                appname = pmix_basename(app->app);
            }
            nodename = NULL;
            pmix_get_attribute(&proc->attributes, PMIX_PROC_NODENAME, (void**)&nodename, PMIX_STRING);
            printf("process:%s:rank:%s:pid:%lu:node:%s:state:%s\n",
                   appname, PMIX_VPID_PRINT(proc->name.vpid),
                   (unsigned long)proc->pid,
                   (NULL == nodename) ? "unknown" : nodename,
                   pmix_proc_state_to_str(proc->state));
            free(appname);
            if (NULL != nodename) {
                free(nodename);
            }
        }
    }

    return PMIX_SUCCESS;
}
#endif
