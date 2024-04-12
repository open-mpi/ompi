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
 * Copyright (c) 2020-2021 Triad National Security, LLC.
 *                         All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"

#include "server_callbacks.h"
#include "test_common.h"
#include "test_server.h"

bool spawn_wait = false;

int main(int argc, char **argv)
{
    char **client_env = NULL;
    char **client_argv = NULL;
    int rc, i;
    test_params params;
    validation_params val_params;
    struct stat stat_buf;
    int test_fail = 0;
    char *tmp;
    sigset_t unblock;
    char **test_argv = NULL;

    default_params(&params, &val_params);

    /* smoke test */
    if (PMIX_SUCCESS != 0) {
        TEST_ERROR_EXIT(("ERROR IN COMPUTING CONSTANTS: PMIX_SUCCESS = %d", PMIX_SUCCESS));
    }

    TEST_VERBOSE(("Testing version %s", PMIx_Get_version()));

    parse_cmd_server(argc, argv, &params, &val_params, &test_argv);
    TEST_VERBOSE(("Start PMIx_lite smoke test (timeout is %d)", params.timeout));

    /* set common argv and env */
    client_env = PMIx_Argv_copy(environ);
    if (NULL != test_argv) {
        TEST_VERBOSE(("Before set_client_argv, test_argv[0] = %s", test_argv[0]));
    }
    set_client_argv(&params, &client_argv, test_argv);
    //set_client_argv(&params, &client_argv);
    TEST_VERBOSE(("After set_client_argv"));
    tmp = PMIx_Argv_join(client_argv, ' ');
    TEST_VERBOSE(("Executing test: %s", tmp));
    free(tmp);

    /* verify executable */
    if (0 > (rc = stat(params.binary, &stat_buf))) {
        TEST_ERROR_EXIT(
            ("Cannot stat() executable \"%s\": %d: %s", params.binary, errno, strerror(errno)));
    } else if (!S_ISREG(stat_buf.st_mode)) {
        TEST_ERROR_EXIT(("Client executable \"%s\": is not a regular file", params.binary));
    } else if (!(stat_buf.st_mode & S_IXUSR)) {
        TEST_ERROR_EXIT(("Client executable \"%s\": has no executable flag", params.binary));
    }

    /* ensure that SIGCHLD is unblocked as we need to capture it */
    if (0 != sigemptyset(&unblock)) {
        TEST_ERROR_EXIT(("SIGEMPTYSET FAILED"));
    }
    if (0 != sigaddset(&unblock, SIGCHLD)) {
        TEST_ERROR_EXIT(("SIGADDSET FAILED"));
    }
    if (0 != sigprocmask(SIG_UNBLOCK, &unblock, NULL)) {
        TEST_ERROR_EXIT(("SIG_UNBLOCK FAILED"));
    }

    // fork of other servers happens below in server_init
    if (PMIX_SUCCESS != (rc = server_init(&val_params))) {
        free_params(&params, &val_params);
        return rc;
    }
    // at this point, we can have multiple servers executing this code
    cli_init(val_params.pmix_local_size);

    /* set namespaces and fork clients
     * we always have a single namespace for all clients, unlike original version of this test */
    int launched = 0;
    launched += server_launch_clients(&params, &val_params, &client_env, &client_argv);

    if (val_params.pmix_local_size != (uint32_t) launched) {
        TEST_ERROR(
            ("srv #%d: Total number of processes doesn't correspond to pmix_local_size parameter.",
             my_server_id));
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

    if (test_abort) {
        TEST_ERROR(("srv #%d: Test was aborted!", my_server_id));
        /* do not simply kill the clients as that generates
         * event notifications which these tests then print
         * out, flooding the log */
        //  cli_kill_all();
        test_fail = 1;
    }

    for (i = 0; i < cli_info_cnt; i++) {
        if (cli_info[i].exit_code != 0) {
            ++test_fail;
        }
        TEST_VERBOSE(
            ("client %d exit code = %d, test_fail = %d", i, cli_info[i].exit_code, test_fail));
    }

    /* deregister the errhandler */
    PMIx_Deregister_event_handler(0, op_callbk, NULL);

done:
    TEST_VERBOSE(("srv #%d: calling server_finalize", my_server_id));
    test_fail += server_finalize(&val_params, test_fail);

    TEST_VERBOSE(("srv #%d: exit sequence", my_server_id));
    free_params(&params, &val_params);
    PMIx_Argv_free(client_argv);
    PMIx_Argv_free(client_env);

    if (!test_fail && !test_timeout) {
        if (0 == my_server_id) {
            TEST_OUTPUT(("Test SUCCEEDED! All servers completed normally."));
        }
    }
    else if (!test_timeout) {
        if (PMIX_ERR_TIMEOUT == test_fail){
            TEST_OUTPUT(("Test TIMED OUT for server id: %d", my_server_id));
        }
        else {
            TEST_OUTPUT(("Test FAILED for server id: %d, failure code = %d", my_server_id, test_fail));
        }
    }
    else {
        TEST_OUTPUT(("Test TIMED OUT for server id: %d", my_server_id));
        return test_timeout;
    }

    return test_fail;
}
