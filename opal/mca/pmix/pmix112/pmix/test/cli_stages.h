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

#include <private/autogen/config.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <time.h>
#include PMIX_EVENT_HEADER
#include <errno.h>
#include "src/include/pmix_globals.h"
#include "pmix_server.h"
#include "src/class/pmix_list.h"
#include "src/usock/usock.h"

#include "test_common.h"

// In correct scenario each client has to sequentially pass all of this stages
typedef enum {
    CLI_UNINIT, CLI_FORKED, CLI_CONNECTED, CLI_FIN, CLI_DISCONN, CLI_TERM, CLI_UNDEF
} cli_state_t;

typedef struct {
    pmix_list_t modex;
    pid_t pid;
    int sd;
    pmix_event_t *ev;
    cli_state_t state;
    cli_state_t next_state[CLI_TERM+1];
    int rank;
    char *ns;
} cli_info_t;

extern cli_info_t *cli_info;
extern int cli_info_cnt;
extern bool test_abort;

int cli_rank(cli_info_t *cli);
void cli_init(int nprocs, cli_state_t order[]);
void cli_connect(cli_info_t *cli, int sd, struct event_base * ebase, event_callback_fn callback);
void cli_finalize(cli_info_t *cli);
void cli_disconnect(cli_info_t *cli);
void cli_terminate(cli_info_t *cli);
void cli_cleanup(cli_info_t *cli);
void cli_wait_all(double timeout);
void cli_kill_all(void);

bool test_terminated(void);

void errhandler(pmix_status_t status,
                pmix_proc_t procs[], size_t nprocs,
                pmix_info_t info[], size_t ninfo);

void op_callbk(pmix_status_t status,
               void *cbdata);

void errhandler_reg_callbk (pmix_status_t status,
                            int errhandler_ref,
                            void *cbdata);


