/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>
#include <private/pmix_socket_errno.h>

#include <pmix.h>
#include <pmix/pmix_common.h>
#include <pmix_server.h>
#include "src/include/pmix_globals.h"
#include "src/util/output.h"

typedef struct {
    pmix_event_t ev;
    pmix_notification_fn_t errhandler;
    pmix_errhandler_reg_cbfunc_t cbfunc;
    int ref;
    pmix_op_cbfunc_t opcbfunc;
    void *cbdata;
} shifter_t;

 #define PMIX_THREADSHIFT(r, c)                       \
 do {                                                 \
    event_assign(&((r)->ev), pmix_globals.evbase,     \
                 -1, EV_WRITE, (c), (r));             \
    event_active(&((r)->ev), EV_WRITE, 1);            \
} while(0);


void pmix_default_errhdlr(pmix_status_t status,
                          pmix_proc_t procs[], size_t nprocs,
                          pmix_info_t info[], size_t ninfo)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client default errhandler activated");
}

static void _register_err(int sd, short args, void *cbdata)
{
    shifter_t *st = (shifter_t*)cbdata;

    pmix_globals.errhandler = st->errhandler;
    if (NULL != st->cbfunc) {
        st->cbfunc(PMIX_SUCCESS, 1, st->cbdata);
    }
    free(st);
}
void PMIx_Register_errhandler(pmix_info_t info[], size_t ninfo,
                              pmix_notification_fn_t errhandler,
                              pmix_errhandler_reg_cbfunc_t cbfunc,
                              void *cbdata)
{
    shifter_t *st;
    st = (shifter_t*)malloc(sizeof(shifter_t));
    st->errhandler = errhandler;
    st->cbfunc = cbfunc;
    st->cbdata = cbdata;
    PMIX_THREADSHIFT(st, _register_err);
}

static void _dereg_err(int sd, short args, void *cbdata)
{
    shifter_t *st = (shifter_t*)cbdata;

    pmix_globals.errhandler = pmix_default_errhdlr;
    if (NULL != st->opcbfunc) {
        st->opcbfunc(PMIX_SUCCESS, st->cbdata);
    }
    free(st);
}
void PMIx_Deregister_errhandler(int errhandler_ref,
                                 pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    shifter_t *st;
    st = (shifter_t*)malloc(sizeof(shifter_t));
    st->opcbfunc = cbfunc;
    st->cbdata = cbdata;
    PMIX_THREADSHIFT(st, _dereg_err);
}

pmix_status_t PMIx_Notify_error(pmix_status_t status,
                                pmix_proc_t procs[], size_t nprocs,
                                pmix_proc_t error_procs[], size_t error_nprocs,
                                pmix_info_t info[], size_t ninfo,
                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    /* common err notify goes here */
   return PMIX_SUCCESS;
}
