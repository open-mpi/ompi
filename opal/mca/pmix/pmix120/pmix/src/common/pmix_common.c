/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
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

#include "src/util/output.h"

#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/include/pmix_globals.h"

void PMIx_Register_errhandler(pmix_info_t info[], size_t ninfo,
                              pmix_notification_fn_t errhandler,
                              pmix_errhandler_reg_cbfunc_t cbfunc,
                              void *cbdata)
{
    /* common err handler registration */
    if (pmix_globals.server) {
        /* PMIX server: store the error handler, process info keys and call
         * cbfunc with reference to the errhandler */
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "registering server err handler");
        pmix_server_register_errhandler(info, ninfo,
                                        errhandler,
                                        cbfunc,cbdata);

    } else {
        /* PMIX client: store the error handler, process info keys &
         * call pmix_server_register_for_events, and call cbfunc with
         * reference to the errhandler */
         pmix_output_verbose(2, pmix_globals.debug_output,
                             "registering client err handler with %d info", (int)ninfo);
         pmix_client_register_errhandler(info, ninfo,
                                         errhandler,
                                         cbfunc, cbdata);
    }
}

void PMIx_Deregister_errhandler(int errhandler_ref,
                                pmix_op_cbfunc_t cbfunc,
                                void *cbdata)
{
    /* common err handler registration */
    if (pmix_globals.server) {
        /* PMIX server: store the error handler, process info keys and call
         * cbfunc with reference to the errhandler */
        pmix_server_deregister_errhandler(errhandler_ref,cbfunc,cbdata);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "deregistering server err handler");
    } else {
        /* PMIX client: store the error handler, process info keys &
         * call pmix_server_register_for_events, and call cbfunc with
         * reference to the errhandler */
        pmix_client_deregister_errhandler(errhandler_ref, cbfunc, cbdata);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "deregistering client err handler");
    }
}

pmix_status_t PMIx_Notify_error(pmix_status_t status,
                                pmix_proc_t procs[], size_t nprocs,
                                pmix_proc_t error_procs[], size_t error_nprocs,
                                pmix_info_t info[], size_t ninfo,
                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;

    if (pmix_globals.server) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix_server_notify_error error =%d, rc=%d", status, rc);
        rc = pmix_server_notify_error(status, procs, nprocs, error_procs,
                                      error_nprocs, info, ninfo,
                                      cbfunc, cbdata);
    } else {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix_client_notify_error error =%d, rc=%d", status, rc);
        rc = pmix_client_notify_error(status, procs, nprocs, error_procs,
                                      error_nprocs, info, ninfo,
                                      cbfunc, cbdata);
    }
    return rc;
}
