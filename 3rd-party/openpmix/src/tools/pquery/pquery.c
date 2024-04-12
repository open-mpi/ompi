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
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "pmix_config.h"
#include "include/pmix.h"
#include "pmix_common.h"
#include "include/pmix_server.h"

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "include/pmix_tool.h"
#include "src/common/pmix_attributes.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/runtime/pmix_init_util.h"
#include "src/runtime/pmix_rte.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_cmd_line.h"
#include "src/util/pmix_keyval_parse.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

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
static void querycbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                        pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    myquery_data_t *mq = (myquery_data_t *) cbdata;
    size_t n;

    mq->status = status;
    /* save the returned info - the PMIx library "owns" it
     * and will release it and perform other cleanup actions
     * when release_fn is called */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n = 0; n < ninfo; n++) {
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
static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(evhdlr_registration_id, status,
                            source, info, ninfo, results, nresults);

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
static void evhandler_reg_callbk(pmix_status_t status, size_t evhandler_ref, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                myproc.nspace, myproc.rank, status, (unsigned long) evhandler_ref);
    }
    lock->status = status;
    PMIX_WAKEUP_THREAD(&lock->lock);
}

static struct option pqoptions[] = {
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),

    PMIX_OPTION_DEFINE(PMIX_CLI_SYS_SERVER_FIRST, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_SYSTEM_SERVER, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_WAIT_TO_CONNECT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_NUM_CONNECT_RETRIES, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_PID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_NAMESPACE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_TMPDIR, PMIX_ARG_REQD),

    PMIX_OPTION_END
};
static char *pqshorts = "h::vV";

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_info_t *info;
    mylock_t mylock;
    pmix_cli_result_t results;
    pmix_cli_item_t *opt;
    size_t n, m, nqueries;
    myquery_data_t mq = {
        .lock = PMIX_LOCK_STATIC_INIT,
        .status = 0,
        .info = NULL,
        .ninfo = 0
    };
    char **qkeys = NULL;
    const char *attr;
    pmix_list_t querylist, qlist;
    pmix_querylist_t *qry;
    char **qprs;
    char *strt, *endp, *kptr;
    pmix_infolist_t *iptr;
    char *str, *result, **ans;
    pmix_query_t *queries;
    pmix_rank_t rank = 0;
    char hostname[PMIX_PATH_MAX];

    PMIX_HIDE_UNUSED_PARAMS(argc);

    /* protect against problems if someone passes us thru a pipe
     * and then abnormally terminates the pipe early */
    signal(SIGPIPE, SIG_IGN);

    /* init globals */
    pmix_tool_basename = "pquery";
    gethostname(hostname, sizeof(hostname));

    // setup the base infrastructure
    if (PMIX_SUCCESS != pmix_init_util(NULL, 0, NULL)) {
        return PMIX_ERROR;
    }

    /* Parse the command line options */
    PMIX_CONSTRUCT(&results, pmix_cli_result_t);
    rc = pmix_cmd_line_parse(argv, pqshorts, pqoptions,
                             NULL, &results, "help-pquery.txt");

    if (PMIX_SUCCESS != rc) {
        if (PMIX_ERR_SILENT != rc && PMIX_OPERATION_SUCCEEDED != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0], PMIx_Error_string(rc));
        }
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            rc = PMIX_SUCCESS;
        }
        exit(rc);
    }

    // handle relevant MCA params
    PMIX_LIST_FOREACH(opt, &results.instances, pmix_cli_item_t) {
        if (0 == strcmp(opt->key, PMIX_CLI_PMIXMCA)) {
            for (n=0; NULL != opt->values[n]; n++) {
                pmix_expose_param(opt->values[n]);
            }
        }
    }

    // setup the base infrastructure
    if (PMIX_SUCCESS != pmix_init_util(NULL, 0, NULL)) {
        return PMIX_ERROR;
    }

    /* get the argv array of keys they want us to query */
    qkeys = results.tail;

    /* if they didn't give us an option, then we can't do anything */
    if (NULL == qkeys) {
        str = pmix_show_help_string("help-pquery.txt", "usage", false,
                                    pmix_tool_basename, "PMIx",
                                    PMIX_PROXY_VERSION,
                                    pmix_tool_basename,
                                    PMIX_PROXY_BUGREPORT);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        exit(1);
    }

    /* if we were given the pid of a starter, then direct that
     * we connect to it */
    n = 3;
    PMIX_INFO_CREATE(info, n);
    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_PID))) {
        /* see if it is an integer value */
        char *leftover, *param;
        pid_t pid;
        leftover = NULL;
        pid = strtol(opt->values[0], &leftover, 10);
        if (NULL == leftover || 0 == strlen(leftover)) {
            /* it is an integer */
            PMIX_INFO_LOAD(&info[0], PMIX_SERVER_PIDINFO, &pid, PMIX_PID);
        } else if (0 == strncasecmp(opt->values[0], "file", 4)) {
            FILE *fp;
            /* step over the file: prefix */
            param = strchr(opt->values[0], ':');
            if (NULL == param) {
                /* malformed input */
                pmix_show_help("help-pquery.txt", "bad-option-input", true, pmix_tool_basename,
                               "--pid", opt->values[0], "file:path");
                return PMIX_ERR_BAD_PARAM;
            }
            ++param;
            fp = fopen(param, "r");
            if (NULL == fp) {
                pmix_show_help("help-pquery.txt", "file-open-error", true, pmix_tool_basename,
                               "--pid", opt->values[0], param);
                return PMIX_ERR_BAD_PARAM;
            }
            rc = fscanf(fp, "%lu", (unsigned long *) &pid);
            if (1 != rc) {
                /* if we were unable to obtain the single conversion we
                 * require, then error out */
                pmix_show_help("help-pquery.txt", "bad-file", true, pmix_tool_basename,
                               "--pid", opt->values[0], param);
                fclose(fp);
                return PMIX_ERR_BAD_PARAM;
            }
            fclose(fp);
            PMIX_INFO_LOAD(&info[0], PMIX_SERVER_PIDINFO, &pid, PMIX_PID);
        } else { /* a string that's neither an integer nor starts with 'file:' */
            pmix_show_help("help-pquery.txt", "bad-option-input", true,
                           pmix_tool_basename, "--pid",
                           opt->values[0], "file:path");
            return PMIX_ERR_BAD_PARAM;
        }
    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_NAMESPACE))) {
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_NSPACE, opt->values[0], PMIX_STRING);
    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_URI))) {
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_URI, opt->values[0], PMIX_STRING);
    } else if (pmix_cmd_line_is_taken(&results, PMIX_CLI_SYS_SERVER_FIRST)) {
        /* otherwise, use the system connection first, if available */
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECT_SYSTEM_FIRST, NULL, PMIX_BOOL);
    } else if (pmix_cmd_line_is_taken(&results, PMIX_CLI_SYSTEM_SERVER)) {
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECT_TO_SYSTEM, NULL, PMIX_BOOL);
    } else {
        /* we set ourselves up as a tool, but no connections required */
        PMIX_INFO_LOAD(&info[0], PMIX_TOOL_CONNECT_OPTIONAL, NULL, PMIX_BOOL);
    }

    /* assign our own name */
    pmix_asprintf(&kptr, "%s.%s.%lu", pmix_tool_basename, hostname, (unsigned long)getpid());
    PMIX_INFO_LOAD(&info[1], PMIX_TOOL_NSPACE, kptr, PMIX_STRING);
    free(kptr);
    PMIX_INFO_LOAD(&info[2], PMIX_TOOL_RANK, &rank, PMIX_PROC_RANK);

    /* init as a tool */
    rc = PMIx_tool_init(&myproc, info, n);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx_tool_init failed: %s\n", PMIx_Error_string(rc));
        exit(rc);
    }
    PMIX_INFO_FREE(info, 1);

    /* they might be querying the system about a client, server, or tool
     * attribute, so register those - this will allow us to compare the
     * provided key with the actual name of the attribute so we can
     * identify it */
    pmix_init_registered_attrs();
    pmix_register_client_attrs();
    pmix_register_server_attrs();
    pmix_register_tool_attrs();

    /* register a default event handler */
    PMIX_CONSTRUCT_LOCK(&mylock.lock);
    PMIx_Register_event_handler(NULL, 0, NULL, 0, notification_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    PMIX_WAIT_THREAD(&mylock.lock);
    if (PMIX_SUCCESS != mylock.status) {
        fprintf(stderr, "PMIx_Register_event_handler returned bad status: %d\n", mylock.status);
        PMIX_DESTRUCT_LOCK(&mylock.lock);
        rc = mylock.status;
        goto done;
    }
    PMIX_DESTRUCT_LOCK(&mylock.lock);

    /* generate the queries */
    PMIX_CONSTRUCT(&querylist, pmix_list_t);
    for (n = 0; NULL != qkeys[n]; n++) {
        qry = PMIX_NEW(pmix_querylist_t);
        PMIX_CONSTRUCT(&qlist, pmix_list_t);
        /* check for qualifiers: key[qual=foo;qual=bar] */
        if (NULL != (strt = strchr(qkeys[n], '['))) {
            /* we have qualifiers - find the end */
            *strt = '\0';
            ++strt;
            if (NULL == (endp = strrchr(strt, ']'))) {
                str = pmix_show_help_string("help-pquery.txt", "bad-quals", true, qkeys[n]);
                if (NULL != str) {
                    printf("%s", str);
                    free(str);
                }
                exit(1);
            }
            *endp = '\0';
            /* break into qual=val pairs */
            qprs = PMIx_Argv_split(strt, ';');
            for (m = 0; NULL != qprs[m]; m++) {
                /* break each pair */
                if (NULL == (kptr = strchr(qprs[m], '='))) {
                    str = pmix_show_help_string("help-pquery.txt", "bad-qual", true, qkeys[n],
                                                qprs[m]);
                    if (NULL != str) {
                        printf("%s", str);
                        free(str);
                    }
                    exit(1);
                }
                *kptr = '\0';
                kptr++;
                iptr = PMIX_NEW(pmix_infolist_t);
                if (NULL == (attr = pmix_attributes_lookup(qprs[m]))) {
                    fprintf(stderr, "Failed to lookup %s\n", qprs[m]);
                    exit(1);
                }
                PMIX_INFO_LOAD(&iptr->info, attr, kptr, PMIX_STRING);
                pmix_list_append(&qlist, &iptr->super);
            }
        }
        /* convert the key */
        if (NULL == (attr = pmix_attributes_lookup(qkeys[n]))) {
            fprintf(stderr, "Failed to lookup %s\n", qkeys[n]);
            exit(1);
        }
        PMIx_Argv_append_nosize(&qry->query.keys, attr);
        /* add in any qualifiers */
        m = pmix_list_get_size(&qlist);
        if (0 < m) {
            PMIX_QUERY_QUALIFIERS_CREATE(&qry->query, m);
            m = 0;
            PMIX_LIST_FOREACH (iptr, &qlist, pmix_infolist_t) {
                PMIX_INFO_XFER(&qry->query.qualifiers[m], &iptr->info);
                ++m;
            }
        }
        pmix_list_append(&querylist, &qry->super);
        PMIX_LIST_DESTRUCT(&qlist);
    }
    /* convert the list of queries into an array */
    nqueries = pmix_list_get_size(&querylist);
    PMIX_QUERY_CREATE(queries, nqueries);
    m = 0;
    PMIX_LIST_FOREACH (qry, &querylist, pmix_querylist_t) {
        /* move the queries across */
        queries[m].keys = qry->query.keys;
        queries[m].nqual = qry->query.nqual;
        queries[m].qualifiers = qry->query.qualifiers;
        ++m;
    }
    PMIX_LIST_DESTRUCT(&querylist);

    PMIX_CONSTRUCT_LOCK(&mq.lock);
    rc = PMIx_Query_info_nb(queries, nqueries, querycbfunc, (void *) &mq);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
        goto done;
    }
    PMIX_WAIT_THREAD(&mq.lock);
    PMIX_DESTRUCT_LOCK(&mq.lock);
    if (PMIX_SUCCESS != mq.status) {
        fprintf(stderr, "PMIx_Query_info returned: %s\n", PMIx_Error_string(mq.status));
        rc = mq.status;
    } else {
        if (0 == mq.ninfo) {
            fprintf(stderr, "Query returned zero results\n");
            goto done;
        }
        /* print out the returned value(s) */
        for (n = 0; n < mq.ninfo; n++) {
            if (NULL == (attr = pmix_attributes_reverse_lookup(mq.info[n].key))) {
                fprintf(stdout, "%s: ", mq.info[n].key);
            } else {
                fprintf(stdout, "%s: ", attr);
            }
            fprintf(stdout, "\n");
            if (PMIX_STRING == mq.info[n].value.type) {
                ans = PMIx_Argv_split(mq.info[n].value.data.string, ',');
                for (m=0; NULL != ans[m]; m++) {
                    if (NULL == (attr = pmix_attributes_reverse_lookup(ans[m]))) {
                        fprintf(stdout, "    %s\n", ans[m]);
                    } else {
                        fprintf(stdout, "    %s\n", attr);
                    }
                }
                PMIx_Argv_free(ans);
            } else {
                result = PMIx_Value_string(&mq.info[n].value);
                fprintf(stderr, "  %s\n", (NULL == result) ? "NULL" : result);
                free(result);
            }
        }
    }

done:
    PMIx_tool_finalize();

    return (rc);
}
