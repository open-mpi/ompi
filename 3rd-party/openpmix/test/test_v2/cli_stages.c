/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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
bool test_complete = false;
int test_timeout = 0;

int cli_rank(cli_info_t *cli)
{
    int i;
    for (i = 0; i < cli_info_cnt; i++) {
        if (cli == &cli_info[i]) {
            return cli->rank;
        }
    }
    return -1;
}

void cli_init(int nprocs)
{
    int n, i;
    cli_state_t order[CLI_TERM + 1];

    cli_info = malloc(sizeof(cli_info_t) * nprocs);
    cli_info_cnt = nprocs;

    order[CLI_UNINIT] = CLI_FORKED;
    order[CLI_FORKED] = CLI_FIN;
    order[CLI_CONNECTED] = CLI_UNDEF;
    order[CLI_FIN] = CLI_TERM;
    order[CLI_DISCONN] = CLI_UNDEF;
    order[CLI_TERM] = CLI_UNDEF;

    for (n = 0; n < nprocs; n++) {
        cli_info[n].sd = -1;
        cli_info[n].ev = NULL;
        cli_info[n].pid = -1;
        cli_info[n].state = CLI_UNINIT;
        PMIX_CONSTRUCT(&(cli_info[n].modex), pmix_list_t);
        for (i = 0; i < CLI_TERM + 1; i++) {
            cli_info[n].next_state[i] = order[i];
        }
        cli_info[n].rank = -1;
        cli_info[n].ns = NULL;
    }
}

void cli_connect(cli_info_t *cli, int sd, pmix_event_base_t *ebase, event_callback_fn callback)
{
    if (CLI_CONNECTED != cli->next_state[cli->state]) {
        TEST_ERROR(("Rank %d has bad next state: expect %d have %d!", cli_rank(cli), CLI_CONNECTED,
                    cli->next_state[cli->state]));
        test_abort = true;
        return;
    }

    cli->sd = sd;
    cli->ev = pmix_event_new(ebase, sd, EV_READ | EV_PERSIST, callback, cli);
    pmix_event_add(cli->ev, NULL);
    pmix_ptl_base_set_nonblocking(sd);
    TEST_VERBOSE(("Connection accepted from rank %d", cli_rank(cli)));
    cli->state = CLI_CONNECTED;
}

void cli_finalize(cli_info_t *cli)
{
    if (CLI_FIN != cli->next_state[cli->state]) {
        TEST_ERROR(("rank %d: bad client next state: expect %d have %d!", cli_rank(cli), CLI_FIN,
                    cli->next_state[cli->state]));
        test_abort = true;
    }

    cli->state = CLI_FIN;
}

void cli_disconnect(cli_info_t *cli)
{
    if (CLI_DISCONN != cli->next_state[cli->state]) {
        TEST_ERROR(("rank %d: bad client next state: expect %d have %d!", cli_rank(cli),
                    CLI_DISCONN, cli->next_state[cli->state]));
        test_abort = true;
    }

    if (0 > cli->sd) {
        TEST_ERROR(("Bad sd = %d of rank = %d ", cli->sd, cli_rank(cli)));
        test_abort = true;
    } else {
        TEST_VERBOSE(("close sd = %d for rank = %d", cli->sd, cli_rank(cli)));
        close(cli->sd);
        cli->sd = -1;
    }

    if (NULL == cli->ev) {
        TEST_ERROR(("Bad ev = NULL of rank = %d ", cli_rank(cli)));
        test_abort = true;
    } else {
        TEST_VERBOSE(("remove event of rank %d from event queue", cli_rank(cli)));
        pmix_event_del(cli->ev);
        pmix_event_free(cli->ev);
        cli->ev = NULL;
    }

    TEST_VERBOSE(("Destruct modex list for the rank %d", cli_rank(cli)));
    PMIX_LIST_DESTRUCT(&(cli->modex));

    cli->state = CLI_DISCONN;
}

void cli_terminate(cli_info_t *cli)
{
    if (CLI_TERM != cli->next_state[cli->state]) {
        TEST_ERROR(("rank %d: bad client next state: expect %d have %d!", cli_rank(cli), CLI_TERM,
                    cli->next_state[cli->state]));
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
    switch (cli->next_state[cli->state]) {
    case CLI_FORKED:
        break;
    case CLI_CONNECTED:
        /* error - means that process terminated w/o calling finalize */
        if (!test_abort) {
            TEST_ERROR(
                ("rank %d with state %d unexpectedly terminated.", cli_rank(cli), cli->state));
        }
        cli->state = CLI_TERM;
        test_abort = true;
        break;
    case CLI_FIN:
        /* error - means that process terminated w/o calling finalize */
        if (!test_abort) {
            TEST_VERBOSE(("Rank %d cli->exit_code %d", cli_rank(cli), cli->exit_code));
            if (PMIX_ERR_TIMEOUT == cli->exit_code) {
                TEST_VERBOSE(("Rank %d timed out", cli_rank(cli)));
                test_timeout = PMIX_ERR_TIMEOUT;
            }
            else {
                TEST_ERROR(
                    ("rank %d with state %d unexpectedly terminated.", cli_rank(cli), cli->state));
                test_abort = true;
            }
        }
        cli_finalize(cli);
        cli_cleanup(cli);
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

void cli_kill_all(void)
{
    int i;
    for (i = 0; i < cli_info_cnt; i++) {
        if (CLI_UNINIT == cli_info[i].state) {
            TEST_ERROR(("Skip rank %d as it wasn't ever initialized (shouldn't happen)", i));
            continue;
        } else if (CLI_TERM <= cli_info[i].state) {
            TEST_VERBOSE(("Skip rank %d as it was already terminated.", i));
            continue;
        }
        TEST_VERBOSE(("Kill rank %d (pid = %d).", i, cli_info[i].pid));
        kill(cli_info[i].pid, SIGKILL);
        cli_cleanup(&cli_info[i]);
    }
}

void errhandler(size_t evhdlr_registration_id, pmix_status_t status, const pmix_proc_t *source,
                pmix_info_t info[], size_t ninfo, pmix_info_t results[], size_t nresults,
                pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(evhdlr_registration_id, info, ninfo, results, nresults);
    TEST_ERROR((" PMIX server event handler for %s:%d with status = %d", source->nspace,
                source->rank, status));
    cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
}

void op_callbk(pmix_status_t status, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(cbdata);
    TEST_VERBOSE(("OP CALLBACK CALLED WITH STATUS %d", status));
}

void errhandler_reg_callbk(pmix_status_t status, size_t errhandler_ref, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(cbdata);
    TEST_VERBOSE(("ERRHANDLER REGISTRATION CALLBACK CALLED WITH STATUS %d, ref=%lu", status,
                  (unsigned long) errhandler_ref));
}
