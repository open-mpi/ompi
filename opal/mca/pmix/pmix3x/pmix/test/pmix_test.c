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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>

#include "src/util/pmix_environ.h"
#include "src/util/output.h"

#include "server_callbacks.h"
#include "utils.h"
#include "test_server.h"
#include "test_common.h"

bool spawn_wait = false;

int main(int argc, char **argv)
{
    char **client_env=NULL;
    char **client_argv=NULL;
    int rc, i;
    struct stat stat_buf;
    char *tmp;
    int ns_nprocs;
    sigset_t unblock;

    INIT_TEST_PARAMS(params);

    /* smoke test */
    if (PMIX_SUCCESS != 0) {
        TEST_ERROR(("ERROR IN COMPUTING CONSTANTS: PMIX_SUCCESS = %d", PMIX_SUCCESS));
        exit(1);
    }

    TEST_VERBOSE(("Testing version %s", PMIx_Get_version()));

    parse_cmd(argc, argv, &params);
    TEST_VERBOSE(("Start PMIx_lite smoke test (timeout is %d)", params.timeout));

    /* set common argv and env */
    client_env = pmix_argv_copy(environ);
    set_client_argv(&params, &client_argv);

    tmp = pmix_argv_join(client_argv, ' ');
    TEST_VERBOSE(("Executing test: %s", tmp));
    free(tmp);

    /* verify executable */
    if( 0 > ( rc = stat(params.binary, &stat_buf) ) ){
        TEST_ERROR(("Cannot stat() executable \"%s\": %d: %s", params.binary, errno, strerror(errno)));
        FREE_TEST_PARAMS(params);
        return 0;
    } else if( !S_ISREG(stat_buf.st_mode) ){
        TEST_ERROR(("Client executable \"%s\": is not a regular file", params.binary));
        FREE_TEST_PARAMS(params);
        return 0;
    }else if( !(stat_buf.st_mode & S_IXUSR) ){
        TEST_ERROR(("Client executable \"%s\": has no executable flag", params.binary));
        FREE_TEST_PARAMS(params);
        return 0;
    }

    /* ensure that SIGCHLD is unblocked as we need to capture it */
    if (0 != sigemptyset(&unblock)) {
        fprintf(stderr, "SIGEMPTYSET FAILED\n");
        exit(1);
    }
    if (0 != sigaddset(&unblock, SIGCHLD)) {
        fprintf(stderr, "SIGADDSET FAILED\n");
        exit(1);
    }
    if (0 != sigprocmask(SIG_UNBLOCK, &unblock, NULL)) {
        fprintf(stderr, "SIG_UNBLOCK FAILED\n");
        exit(1);
    }

    if (PMIX_SUCCESS != (rc = server_init(&params))) {
        FREE_TEST_PARAMS(params);
        return rc;
    }

    cli_init(params.lsize);

    int launched = 0;
    /* set namespaces and fork clients */
    if (NULL == params.ns_dist) {
        uint32_t i;
        int base_rank = 0;

        /* compute my start counter */
        for(i = 0; i < (uint32_t)my_server_id; i++) {
            base_rank += (params.nprocs % params.nservers) > (uint32_t)i ?
                        params.nprocs / params.nservers + 1 :
                        params.nprocs / params.nservers;
        }
        /* we have a single namespace for all clients */
        ns_nprocs = params.nprocs;
        launched += server_launch_clients(params.lsize, params.nprocs, base_rank,
                                   &params, &client_env, &client_argv);
    } else {
        char *pch;
        pch = strtok(params.ns_dist, ":");
        while (NULL != pch) {
            ns_nprocs = (int)strtol(pch, NULL, 10);
            if (params.nprocs < (uint32_t)(launched+ns_nprocs)) {
                TEST_ERROR(("Total number of processes doesn't correspond number specified by ns_dist parameter."));
                FREE_TEST_PARAMS(params);
                return PMIX_ERROR;
            }
            if (0 < ns_nprocs) {
                launched += server_launch_clients(ns_nprocs, ns_nprocs, 0, &params,
                                           &client_env, &client_argv);
            }
            pch = strtok (NULL, ":");
        }
    }
    if (params.lsize != (uint32_t)launched) {
        TEST_ERROR(("Total number of processes doesn't correspond number specified by ns_dist parameter."));
        cli_kill_all();
        test_fail = 1;
        goto done;
    }

    /* hang around until the client(s) finalize */
    while (!test_complete) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }

    if( test_abort ){
        TEST_ERROR(("Test was aborted!"));
        /* do not simply kill the clients as that generates
         * event notifications which these tests then print
         * out, flooding the log */
      //  cli_kill_all();
        test_fail = 1;
    }

    if (0 != params.test_spawn) {
        PMIX_WAIT_FOR_COMPLETION(spawn_wait);
    }
    for(i=0; i < cli_info_cnt; i++){
        if (cli_info[i].exit_code != 0) {
            ++test_fail;
        }
    }

    /* deregister the errhandler */
//    PMIx_Deregister_event_handler(0, op_callbk, NULL);

  done:
    TEST_VERBOSE(("srv #%d: call server_finalize!", my_server_id));
    test_fail += server_finalize(&params);

    FREE_TEST_PARAMS(params);
    pmix_argv_free(client_argv);
    pmix_argv_free(client_env);

    if (0 == test_fail) {
        TEST_OUTPUT(("Test SUCCEEDED!"));
    }
    return test_fail;
}
