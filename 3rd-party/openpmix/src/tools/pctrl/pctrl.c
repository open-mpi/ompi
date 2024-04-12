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

static struct option pctrlptions[] = {
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),
    PMIX_OPTION_DEFINE(PMIX_CLI_PMIXMCA, PMIX_ARG_REQD),

    PMIX_OPTION_DEFINE(PMIX_CLI_SYS_SERVER_FIRST, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_SYS_SERVER_ONLY, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_WAIT_TO_CONNECT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_NUM_CONNECT_RETRIES, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_PID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_NAMESPACE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_NSPACE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_TMPDIR, PMIX_ARG_REQD),

    PMIX_OPTION_DEFINE(PMIX_CLI_REQ_ID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_PAUSE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_RESUME, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_CANCEL, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_KILL, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_TERMINATE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_RESTART, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_CHKPT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_PSET_NAME, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_TARGETS, PMIX_ARG_REQD),

    PMIX_OPTION_END
};
static char *pctrlshorts = "h::vV";

static pmix_status_t convert_procs(const char *vals,
                                   pmix_data_array_t *array);
static int convert_signal(const char *val);

static void cbfunc(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    pmix_shift_caddy_t *req = (pmix_shift_caddy_t*)cbdata;
    PMIX_HIDE_UNUSED_PARAMS(info, ninfo);

    PMIX_ACQUIRE_OBJECT(req);

    req->status = status;
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    PMIX_POST_OBJECT(req);
    PMIX_WAKEUP_THREAD(&req->lock);
}
int main(int argc, char **argv)
{
    pmix_proc_t myproc, *targets = NULL;
    pmix_status_t rc;
    pmix_shift_caddy_t *req = NULL;
    pmix_info_t *info;
    pmix_data_array_t darray;
    void *options;
    pmix_cli_result_t results;
    pmix_cli_item_t *opt;
    size_t n, ntargets;
    pmix_rank_t rank = 0;
    char hostname[PMIX_PATH_MAX], *kptr;
    bool donotwait = false;
    int sigval;
    char *key = NULL;
    PMIX_HIDE_UNUSED_PARAMS(argc);

    /* protect against problems if someone passes us thru a pipe
     * and then abnormally terminates the pipe early */
    signal(SIGPIPE, SIG_IGN);

    /* init globals */
    pmix_tool_basename = "pctrl";
    gethostname(hostname, sizeof(hostname));

    /* Parse the command line options */
    PMIX_CONSTRUCT(&results, pmix_cli_result_t);
    rc = pmix_cmd_line_parse(argv, pctrlshorts, pctrlptions,
                             NULL, &results, "help-pctrl.txt");

    if (PMIX_SUCCESS != rc) {
        if (PMIX_ERR_SILENT == rc) {
            exit(rc);
        }
        if (PMIX_OPERATION_SUCCEEDED != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0], PMIx_Error_string(rc));
            exit(rc);
        }
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

    // check for common required command line option
    if (NULL == (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_TARGETS))) {
        // must tell us the targets for the operation
        exit(1);
    }
    // save the targets
    // convert the provided string to an array of pmix_proc_t
    rc = convert_procs(opt->values[0], &darray);
    if (PMIX_SUCCESS != rc) {
        // report the error
        exit(rc);
    }
    targets = (pmix_proc_t*)darray.array;
    ntargets = darray.size;

    // collect options for init
    options = PMIx_Info_list_start();
     /* if we were given the pid of a starter, then direct that
     * we connect to it */
    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_PID))) {
        /* see if it is an integer value */
        char *leftover, *param;
        pid_t pid;
        leftover = NULL;
        pid = strtol(opt->values[0], &leftover, 10);
        if (NULL == leftover || 0 == strlen(leftover)) {
            /* it is an integer */
            rc = PMIx_Info_list_add(options, PMIX_SERVER_PIDINFO, &pid, PMIX_PID);
            if (PMIX_SUCCESS != rc) {
                fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
                PMIx_Info_list_release(options);
                free(targets);
                exit(rc);
            }
        } else if (0 == strncasecmp(opt->values[0], "file", 4)) {
            FILE *fp;
            /* step over the file: prefix */
            param = strchr(opt->values[0], ':');
            if (NULL == param) {
                /* malformed input */
                pmix_show_help("help-pquery.txt", "bad-option-input", true, pmix_tool_basename,
                               "--pid", opt->values[0], "file:path");
                PMIx_Info_list_release(options);
                free(targets);
                return PMIX_ERR_BAD_PARAM;
            }
            ++param;
            fp = fopen(param, "r");
            if (NULL == fp) {
                pmix_show_help("help-pquery.txt", "file-open-error", true, pmix_tool_basename,
                               "--pid", opt->values[0], param);
                PMIx_Info_list_release(options);
                free(targets);
                return PMIX_ERR_BAD_PARAM;
            }
            rc = fscanf(fp, "%lu", (unsigned long *) &pid);
            if (1 != rc) {
                /* if we were unable to obtain the single conversion we
                 * require, then error out */
                pmix_show_help("help-pquery.txt", "bad-file", true, pmix_tool_basename,
                               "--pid", opt->values[0], param);
                fclose(fp);
                PMIx_Info_list_release(options);
                free(targets);
                return PMIX_ERR_BAD_PARAM;
            }
            fclose(fp);
            rc = PMIx_Info_list_add(options, PMIX_SERVER_PIDINFO, &pid, PMIX_PID);
            if (PMIX_SUCCESS != rc) {
                fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
                PMIx_Info_list_release(options);
                free(targets);
                exit(rc);
            }
        } else { /* a string that's neither an integer nor starts with 'file:' */
            pmix_show_help("help-pquery.txt", "bad-option-input", true,
                           pmix_tool_basename, "--pid",
                           opt->values[0], "file:path");
            PMIx_Info_list_release(options);
            free(targets);
            return PMIX_ERR_BAD_PARAM;
        }

    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_NAMESPACE))) {
        rc = PMIx_Info_list_add(options, PMIX_SERVER_NSPACE, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            free(targets);
            exit(rc);
        }

    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_NSPACE))) {
        rc = PMIx_Info_list_add(options, PMIX_SERVER_NSPACE, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            free(targets);
            exit(rc);
        }

    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_URI))) {
        rc = PMIx_Info_list_add(options, PMIX_SERVER_URI, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            free(targets);
            exit(rc);
        }

    } else if (pmix_cmd_line_is_taken(&results, PMIX_CLI_SYS_SERVER_FIRST)) {
        rc = PMIx_Info_list_add(options, PMIX_CONNECT_SYSTEM_FIRST, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            free(targets);
            exit(rc);
        }

    } else if (pmix_cmd_line_is_taken(&results, PMIX_CLI_SYS_SERVER_ONLY)) {
        rc = PMIx_Info_list_add(options, PMIX_CONNECT_TO_SYSTEM, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            free(targets);
            exit(rc);
        }
    }

    /* assign our own name */
    pmix_asprintf(&kptr, "%s.%s.%lu", pmix_tool_basename, hostname, (unsigned long)getpid());
    rc = PMIx_Info_list_add(options, PMIX_TOOL_NSPACE, kptr, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
        PMIx_Info_list_release(options);
        free(targets);
        exit(rc);
    }
    free(kptr);
    rc = PMIx_Info_list_add(options, PMIX_TOOL_RANK, &rank, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
        PMIx_Info_list_release(options);
        free(targets);
        exit(rc);
    }
    rc = PMIx_Info_list_convert(options, &darray);
    info = (pmix_info_t *) darray.array;
    n  = darray.size;
    PMIx_Info_list_release(options);

    /* init as a tool */
    rc = PMIx_tool_init(&myproc, info, n);
    PMIX_INFO_FREE(info, n);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx_tool_init failed: %s\n", PMIx_Error_string(rc));
        free(targets);
        exit(rc);
    }

    /* construct the job control request */
    options = PMIx_Info_list_start();

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_REQ_ID))) {
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_ID, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_PAUSE))) {
        key = "PAUSE";
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_PAUSE, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_RESUME))) {
        key = "RESUME";
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_RESUME, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_CANCEL))) {
        key = "CANCEL";
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_CANCEL, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_KILL))) {
        key = "KILL";
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_KILL, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_RESTART))) {
        key = "RESTART";
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_RESTART, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_CHKPT))) {
        key = "CHECKPOINT";
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_CHECKPOINT, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_TERMINATE))) {
        key = "TERMINATE";
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_TERMINATE, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_PSET_NAME))) {
        key = "DEFINE PSET";
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_DEFINE_PSET, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_SIGNAL))) {
        key = "SIGNAL";
        sigval = convert_signal(opt->values[0]);
        if (0 == sigval) {
            // unrecognized signal
            fprintf(stderr, "Unrecognized signal name: %s\n", opt->values[0]);
            PMIx_Info_list_release(options);
            rc = -1;
            goto done;
        }
        rc = PMIx_Info_list_add(options, PMIX_JOB_CTRL_SIGNAL, &sigval, PMIX_INT);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIx_Info_list_release(options);
            goto done;
        }
    }

    req = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL != key) {
        req->key = strdup(key);
    } else {
        req->key = strdup("N/A");
    }
    rc = PMIx_Info_list_convert(options, &darray);
    if (PMIX_ERR_EMPTY == rc) {
        req->info = NULL;
        req->ninfo = 0;
    } else if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    } else {
        req->info = (pmix_info_t *) darray.array;
        req->ninfo = darray.size;
    }
    PMIx_Info_list_release(options);

    rc = PMIx_Job_control_nb(targets, ntargets,
                             req->info, req->ninfo,
                             cbfunc, req);
    if (PMIX_SUCCESS != rc) {
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            fprintf(stderr, "Job control request %s granted\n", req->key);
            PMIX_RELEASE(req);
            rc = PMIX_SUCCESS;
            goto done;
        }
        fprintf(stderr, "Job control request failed: %s\n", PMIx_Error_string(rc));
        goto done;
    }

    if (donotwait) {
        fprintf(stderr, "Job control request being processed\n");
        goto done;
    }

    PMIX_WAIT_THREAD(&req->lock);
    if (PMIX_SUCCESS == req->status) {
        fprintf(stderr, "Job control %s granted\n", req->key);
    } else {
        fprintf(stderr, "Job control request failed: %s\n", PMIx_Error_string(req->status));
    }

done:
    if (NULL != req) {
        PMIX_RELEASE(req);
    }
    if (NULL != targets) {
        free(targets);
    }
    PMIx_tool_finalize();

    return (rc);
}

static pmix_status_t convert_procs(const char *vals,
                                   pmix_data_array_t *array)
{
    char **p, *r;
    size_t cnt, n;
    pmix_proc_t *procs;

    // split on any commas
    p = PMIx_Argv_split(vals, ',');
    // count how many we have
    cnt = PMIx_Argv_count(p);
    // setup the array
    PMIx_Data_array_construct(array, cnt, PMIX_PROC);
    // load the array
    procs = (pmix_proc_t*)array->array;
    for (n=0; NULL != p[n]; n++) {
        // find the nspace/rank delimiting ':'
        r = strrchr(p[n], ':');
        *r = '\0';
        ++r;  // step over the colon
        PMIX_LOAD_NSPACE(procs[n].nspace, p[n]);
        if ('*' == *r) {
            procs[n].rank = PMIX_RANK_WILDCARD;
        } else {
            procs[n].rank = strtoul(r, NULL, 10);
        }
    }
    return PMIX_SUCCESS;

}

typedef struct {
    char *name;
    int value;
} pmix_signal_t;

static pmix_signal_t sigs[] = {
#ifdef SIGHUP
    {"SIGHUP", SIGHUP},
#endif
#ifdef SIGABRT
    {"SIGABRT", SIGABRT},
#endif
#ifdef SIGALRM
    {"SIGALRM", SIGALRM},
#endif
#ifdef SIGKILL
    {"SIGKILL", SIGKILL},
#endif
#ifdef SIGPIPE
    {"SIGPIPE", SIGPIPE},
#endif
#ifdef SIGTERM
    {"SIGTERM", SIGTERM},
#endif
#ifdef SIGSTOP
    {"SIGSTOP", SIGSTOP},
#endif
#ifdef SIGTSTP
    {"SIGTSTP", SIGTSTP},
#endif
#ifdef SIGCONT
    {"SIGCONT", SIGCONT},
#endif
#ifdef SIGCHLD
    {"SIGCHLD", SIGCHLD},
#endif
#ifdef SIGINFO
    {"SIGINFO", SIGINFO},
#endif
#ifdef SIGUSR1
    {"SIGUSR1", SIGUSR1},
#endif
#ifdef SIGUSR2
    {"SIGUSR2", SIGUSR2},
#endif
#ifdef SIGINT
    {"SIGINT", SIGINT},
#endif
#ifdef SIGTRAP
    {"SIGTRAP", SIGTRAP},
#endif
    {NULL, 0}
};

static int convert_signal(const char *val)
{
    int n;

    n = 0;
    while (NULL != sigs[n].name) {
        if (0 == strcasecmp(val, sigs[n].name)) {
            return sigs[n].value;
        }
        ++n;
    }
    return 0;
}
