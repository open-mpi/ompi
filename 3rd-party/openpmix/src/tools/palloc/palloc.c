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

static struct option pallocptions[] = {
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PMIX_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),
    PMIX_OPTION_DEFINE(PMIX_CLI_PMIXMCA, PMIX_ARG_REQD),

    PMIX_OPTION_DEFINE(PMIX_CLI_SYS_SERVER_FIRST, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_SYSTEM_SERVER, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_WAIT_TO_CONNECT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_NUM_CONNECT_RETRIES, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_PID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_NAMESPACE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_TMPDIR, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_CONNECTION_ORDER, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_SYS_CONTROLLER, PMIX_ARG_NONE),

    PMIX_OPTION_DEFINE(PMIX_CLI_REQ_ID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_QUEUE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_NODES, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_IMAGE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_EXCLUDE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_WAIT_ALL_NODES, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_NODELIST, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_UID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_GID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_TIME, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_SIGNAL, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_SHARE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_EXTEND, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_SHRINK, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_NO_SHELL, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PMIX_CLI_BEGIN, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_IMMEDIATE, PMIX_ARG_OPTIONAL),
    PMIX_OPTION_DEFINE(PMIX_CLI_DEPENDENCY, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PMIX_CLI_DO_NOT_WAIT, PMIX_ARG_NONE),

    PMIX_OPTION_END
};
static char *pallocshorts = "h::vVq:N:i:x:w:t:I::d:";

static void cbfunc(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    pmix_shift_caddy_t *req = (pmix_shift_caddy_t*)cbdata;
    size_t n;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(req);

    req->status = status;
    if (PMIX_SUCCESS == status && 0 < ninfo) {
        req->ninfo = ninfo;
        PMIX_INFO_CREATE(req->info, req->ninfo);
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&req->info[n], &info[n]);
            if (PMIX_CHECK_KEY(&info[n], PMIX_SESSION_ID)) {
                PMIX_VALUE_GET_NUMBER(rc, &info[n].value, req->sessionid, uint32_t);
                if (PMIX_SUCCESS != rc) {
                    req->status = rc;
                }
            }
        }
    }

    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    PMIX_POST_OBJECT(req);
    PMIX_WAKEUP_THREAD(&req->lock);
}
int main(int argc, char **argv)
{
    pmix_proc_t myproc;
    pmix_status_t rc;
    pmix_shift_caddy_t *req = NULL;
    pmix_info_t *info;
    pmix_data_array_t darray;
    void *options;
    pmix_cli_result_t results;
    pmix_cli_item_t *opt;
    size_t n;
    pmix_rank_t rank = 0;
    char hostname[PMIX_PATH_MAX], *kptr;
    bool donotwait = false;
    uint32_t ui32;
    uint64_t ui64;
    pmix_alloc_directive_t directive = PMIX_ALLOC_NEW;
    PMIX_HIDE_UNUSED_PARAMS(argc);

    /* protect against problems if someone passes us thru a pipe
     * and then abnormally terminates the pipe early */
    signal(SIGPIPE, SIG_IGN);

    /* init globals */
    pmix_tool_basename = "palloc";
    gethostname(hostname, sizeof(hostname));

    /* Parse the command line options */
    PMIX_CONSTRUCT(&results, pmix_cli_result_t);
    rc = pmix_cmd_line_parse(argv, pallocshorts, pallocptions,
                             NULL, &results, "help-palloc.txt");

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
                PMIX_INFO_FREE(info, n);
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
                PMIX_INFO_FREE(info, n);
                return PMIX_ERR_BAD_PARAM;
            }
            fclose(fp);
            PMIX_INFO_LOAD(&info[0], PMIX_SERVER_PIDINFO, &pid, PMIX_PID);
        } else { /* a string that's neither an integer nor starts with 'file:' */
            pmix_show_help("help-pquery.txt", "bad-option-input", true,
                           pmix_tool_basename, "--pid",
                           opt->values[0], "file:path");
            PMIX_INFO_FREE(info, n);
            return PMIX_ERR_BAD_PARAM;
        }

    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_NAMESPACE))) {
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_NSPACE, opt->values[0], PMIX_STRING);

    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_NSPACE))) {
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_NSPACE, opt->values[0], PMIX_STRING);

    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_URI))) {
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_URI, opt->values[0], PMIX_STRING);

    } else if (pmix_cmd_line_is_taken(&results, PMIX_CLI_SYS_SERVER_FIRST)) {
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECT_SYSTEM_FIRST, NULL, PMIX_BOOL);

    } else if (pmix_cmd_line_is_taken(&results, PMIX_CLI_SYSTEM_SERVER)) {
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECT_TO_SYSTEM, NULL, PMIX_BOOL);

    } else if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_CONNECTION_ORDER))) {
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECTION_ORDER, opt->values[0], PMIX_STRING);

    } else if (pmix_cmd_line_is_taken(&results, PMIX_CLI_SYS_CONTROLLER)) {
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECT_TO_SYS_CONTROLLER, NULL, PMIX_BOOL);

    } else {
        /* if none of the above, setup a common "order" to check */
        char **tmp = NULL, *t2;
        PMIx_Argv_append_nosize(&tmp, PMIX_CONNECT_TO_SCHEDULER);
        PMIx_Argv_append_nosize(&tmp, PMIX_CONNECT_TO_SYS_CONTROLLER);
        t2 = PMIx_Argv_join(tmp, ',');
        PMIx_Argv_free(tmp);
        PMIX_INFO_LOAD(&info[0], PMIX_CONNECTION_ORDER, t2, PMIX_STRING);
        free(t2);
    }

    /* assign our own name */
    pmix_asprintf(&kptr, "%s.%s.%lu", pmix_tool_basename, hostname, (unsigned long)getpid());
    PMIX_INFO_LOAD(&info[1], PMIX_TOOL_NSPACE, kptr, PMIX_STRING);
    free(kptr);
    PMIX_INFO_LOAD(&info[2], PMIX_TOOL_RANK, &rank, PMIX_PROC_RANK);

    /* init as a tool */
    rc = PMIx_tool_init(&myproc, info, n);
    PMIX_INFO_FREE(info, n);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx_tool_init failed: %s\n", PMIx_Error_string(rc));
        exit(rc);
    }

    /* construct the allocation request */
    options = PMIx_Info_list_start();

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_REQ_ID))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_REQ_ID, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_QUEUE))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_QUEUE, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_NODES))) {
        ui64 = strtoul(opt->values[0], NULL, 10);
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_NUM_NODES, &ui64, PMIX_UINT64);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_IMAGE))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_IMAGE, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_EXCLUDE))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_EXCLUDE, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_WAIT_ALL_NODES))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_WAIT_ALL_NODES, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_NODELIST))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_NODE_LIST, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_UID))) {
        ui32 = strtoul(opt->values[0], NULL, 10);
        PMIX_INFO_LIST_ADD(rc, options, PMIX_USERID, &ui32, PMIX_UINT32);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_GID))) {
        ui32 = strtoul(opt->values[0], NULL, 10);
        PMIX_INFO_LIST_ADD(rc, options, PMIX_GRPID, &ui32, PMIX_UINT32);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_TIME))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_TIME, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_SIGNAL))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_NODE_LIST, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_SHARE))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_SHARE, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_EXTEND))) {
        directive = PMIX_ALLOC_EXTEND;
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_SHRINK))) {
        directive = PMIX_ALLOC_RELEASE;
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_NO_SHELL))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_NOSHELL, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_BEGIN))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_BEGIN, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }
    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_IMMEDIATE))) {
        if (NULL == opt->values || NULL == opt->values[0]) {
            ui32 = 0;
        } else {
            ui32 = PMIX_CONVERT_TIME(opt->values[0]);
        }
        PMIX_INFO_LIST_ADD(rc, options, PMIX_TIMEOUT, &ui32, PMIX_UINT);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_DEPENDENCY))) {
        PMIX_INFO_LIST_ADD(rc, options, PMIX_ALLOC_DEPENDENCY, opt->values[0], PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "PMIx info list add failed: %s\n", PMIx_Error_string(rc));
            PMIX_INFO_LIST_RELEASE(options);
            goto done;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(&results, PMIX_CLI_DO_NOT_WAIT))) {
        donotwait = true;
    }

    req = PMIX_NEW(pmix_shift_caddy_t);
    PMIX_INFO_LIST_CONVERT(rc, options, &darray);
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
    PMIX_INFO_LIST_RELEASE(options);

    rc = PMIx_Allocation_request_nb(directive, req->info, req->ninfo,
                                    cbfunc, req);
    if (PMIX_SUCCESS != rc) {
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            fprintf(stderr, "Allocation %s granted\n", req->key);
            PMIX_RELEASE(req);
            rc = PMIX_SUCCESS;
            goto done;
        }
        fprintf(stderr, "Allocation request failed: %s\n", PMIx_Error_string(rc));
        goto done;
    }

    if (donotwait) {
        fprintf(stderr, "Allocation request being processed\n");
        goto done;
    }

    PMIX_WAIT_THREAD(&req->lock);
    if (PMIX_SUCCESS == req->status) {
        fprintf(stderr, "Allocation %s granted\n", req->key);
    } else {
        fprintf(stderr, "Allocation request failed: %s\n", PMIx_Error_string(req->status));
    }

done:
    if (NULL != req) {
        PMIX_RELEASE(req);
    }
    PMIx_tool_finalize();

    return (rc);
}
