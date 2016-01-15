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
#include "src/include/pmix_globals.h"

void PMIx_Register_errhandler(pmix_info_t info[], size_t ninfo,
                              pmix_notification_fn_t errhandler,
                              pmix_errhandler_reg_cbfunc_t cbfunc,
                              void *cbdata)
{
    /* common err handler registration to be added */
}

void PMIx_Deregister_errhandler(int errhandler_ref,
                                 pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    /* common err handler deregistration goes here */
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
