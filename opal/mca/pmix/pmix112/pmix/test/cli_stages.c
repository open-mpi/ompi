/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "cli_stages.h"

cli_info_t *cli_info = NULL;
int cli_info_cnt = 0;
bool test_abort = false;

int cli_rank(cli_info_t *cli)
{
    int i;
    for(i=0; i < cli_info_cnt; i++){
        if( cli == &cli_info[i] ){
            return i;
        }
    }
    return -1;
}

void cli_init(int nprocs, cli_state_t order[])
{
    int n, i;
    cli_info = malloc( sizeof(cli_info_t) * nprocs);
    cli_info_cnt = nprocs;

    for (n=0; n < nprocs; n++) {
        cli_info[n].sd = -1;
        cli_info[n].ev = NULL;
        cli_info[n].pid = -1;
        cli_info[n].state = CLI_UNINIT;
        PMIX_CONSTRUCT(&(cli_info[n].modex), pmix_list_t);
        for (i = 0; i < CLI_TERM+1; i++) {
            cli_info[n].next_state[i] = order[i];
        }
        cli_info[n].rank = -1;
        cli_info[n].ns = NULL;
    }
}

void cli_connect(cli_info_t *cli, int sd, struct event_base * ebase, event_callback_fn callback)
{
    if( CLI_CONNECTED != cli->next_state[cli->state] ){
        TEST_ERROR(("Rank %d has bad next state: expect %d have %d!",
                     cli_rank(cli), CLI_CONNECTED, cli->next_state[cli->state]));
        test_abort = true;
        return;
    }

    cli->sd = sd;
    cli->ev = event_new(ebase, sd,
                      EV_READ|EV_PERSIST, callback, cli);
    event_add(cli->ev,NULL);
    pmix_usock_set_nonblocking(sd);
    TEST_VERBOSE(("Connection accepted from rank %d", cli_rank(cli) ));
    cli->state = CLI_CONNECTED;
}

void cli_finalize(cli_info_t *cli)
{
    if( CLI_FIN != cli->next_state[cli->state] ){
        TEST_ERROR(("rank %d: bad client next state: expect %d have %d!",
                     cli_rank(cli), CLI_FIN, cli->next_state[cli->state]));
        test_abort = true;
    }

    cli->state = CLI_FIN;
}

void cli_disconnect(cli_info_t *cli)
{
    if( CLI_DISCONN != cli->next_state[cli->state] ){
        TEST_ERROR(("rank %d: bad client next state: expect %d have %d!",
                     cli_rank(cli), CLI_DISCONN, cli->next_state[cli->state]));
        test_abort = true;
    }

    if( 0 > cli->sd ){
        TEST_ERROR(("Bad sd = %d of rank = %d ", cli->sd, cli_rank(cli)));
        test_abort = true;
    } else {
        TEST_VERBOSE(("close sd = %d for rank = %d", cli->sd, cli_rank(cli)));
        close(cli->sd);
        cli->sd = -1;
    }

    if( NULL == cli->ev ){
        TEST_ERROR(("Bad ev = NULL of rank = %d ", cli->sd, cli_rank(cli)));
        test_abort = true;
    } else {
        TEST_VERBOSE(("remove event of rank %d from event queue", cli_rank(cli)));
        event_del(cli->ev);
        event_free(cli->ev);
        cli->ev = NULL;
    }

    TEST_VERBOSE(("Destruct modex list for the rank %d", cli_rank(cli)));
    PMIX_LIST_DESTRUCT(&(cli->modex));

    cli->state = CLI_DISCONN;
}

void cli_terminate(cli_info_t *cli)
{
    if( CLI_TERM != cli->next_state[cli->state] ){
        TEST_ERROR(("rank %d: bad client next state: expect %d have %d!",
                     cli_rank(cli), CLI_TERM, cli->next_state[cli->state]));
        test_abort = true;
    }
    cli->pid = -1;
    TEST_VERBOSE(("Client rank = %d terminated", cli_rank(cli)));
    cli->state = CLI_TERM;
    if (NULL != cli->ns) {
        free(cli->ns);
    }
}

void cli_cleanup(cli_info_t *cli)
{
    if (CLI_TERM < cli->state) {
        TEST_ERROR(("Bad rank %d state %d", cli_rank(cli), cli->state));
        test_abort = true;
        return;
    }
    switch( cli->next_state[cli->state] ){
    case CLI_FORKED:
        break;
    case CLI_CONNECTED:
        /* error - means that process terminated w/o calling finalize */
        if (!test_abort) {
            TEST_ERROR(("rank %d with state %d unexpectedly terminated.", cli_rank(cli), cli->state));
        }
        cli->state = CLI_TERM;
        test_abort = true;
        break;
    case CLI_FIN:
        /* error - means that process terminated w/o calling finalize */
        if (!test_abort) {
            TEST_ERROR(("rank %d with state %d unexpectedly terminated.", cli_rank(cli), cli->state));
        }
        cli_finalize(cli);
        cli_cleanup(cli);
        test_abort = true;
        break;
    case CLI_DISCONN:
        cli_disconnect(cli);
        cli_cleanup(cli);
        break;
    case CLI_TERM:
        cli_terminate(cli);
        break;
    default:
        TEST_ERROR(("Bad rank %d next state %d", cli_rank(cli), cli->next_state[cli->state]));
        test_abort = true;
        return;
    }
}


bool test_terminated(void)
{
    bool ret = true;
    int i;

    // All clients should disconnect
    for(i=0; i < cli_info_cnt; i++){
        ret = ret && (CLI_TERM <= cli_info[i].state);
    }
    return (ret || test_abort);
}

void cli_wait_all(double timeout)
{
    struct timeval tv;
    double start_time, cur_time;

    gettimeofday(&tv, NULL);
    start_time = tv.tv_sec + 1E-6*tv.tv_usec;
    cur_time = start_time;

    //TEST_VERBOSE(("Wait for all children to terminate"))

    // Wait for all children to cleanup after the test.
    while( !test_terminated() && ( timeout >= (cur_time - start_time) ) ){
        struct timespec ts;
        int status, i;
        pid_t pid;
        while( 0 < (pid = waitpid(-1, &status, WNOHANG) ) ){
            TEST_VERBOSE(("waitpid = %d", pid));
            for(i=0; i < cli_info_cnt; i++){
                if( cli_info[i].pid == pid ){
                    TEST_VERBOSE(("the child with pid = %d has rank = %d\n"
                                "\t\texited = %d, signalled = %d", pid, i,
                                WIFEXITED(status), WIFSIGNALED(status) ));
                    if( WIFEXITED(status) || WIFSIGNALED(status) ){
                        cli_cleanup(&cli_info[i]);
                    }
                }
            }
        }
        if( pid < 0 ){
            if( errno == ECHILD ){
                TEST_VERBOSE(("No more children to wait. Happens on the last cli_wait_all call "
                            "which is used to ensure that all children terminated.\n"));
                break;
            } else {
                TEST_ERROR(("waitpid(): %d : %s", errno, strerror(errno)));
                exit(0);
            }
        }
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
        // calculate current timestamp
        gettimeofday(&tv, NULL);
        cur_time = tv.tv_sec + 1E-6*tv.tv_usec;
    }
}

void cli_kill_all(void)
{
    int i;
    for(i = 0; i < cli_info_cnt; i++){
        if( CLI_UNINIT == cli_info[i].state ){
            TEST_ERROR(("Skip rank %d as it wasn't ever initialized (shouldn't happe)",
                          i));
            continue;
        } else if( CLI_TERM <= cli_info[i].state ){
            TEST_VERBOSE(("Skip rank %d as it was already terminated.", i));
            continue;

        }
        TEST_VERBOSE(("Kill rank %d (pid = %d).", i, cli_info[i].pid));
        kill(cli_info[i].pid, SIGKILL);
        cli_cleanup(&cli_info[i]);
    }
}

void errhandler(pmix_status_t status,
                pmix_proc_t procs[], size_t nprocs,
                pmix_info_t info[], size_t ninfo)
{
    TEST_ERROR((" PMIX server Error handler with status = %d", status));
    /* notify clients of error */
    PMIx_Notify_error(status,
                      NULL, 0,
                      NULL, 0, NULL, 0,
                      op_callbk, NULL);
}

void op_callbk(pmix_status_t status,
                      void *cbdata)
{
    TEST_VERBOSE(( "OP CALLBACK CALLED WITH STATUS %d", status));
}

void errhandler_reg_callbk (pmix_status_t status,
                                   int errhandler_ref,
                                   void *cbdata)
{
    TEST_VERBOSE(("ERRHANDLER REGISTRATION CALLBACK CALLED WITH STATUS %d, ref=%d",
                status, errhandler_ref));
}

