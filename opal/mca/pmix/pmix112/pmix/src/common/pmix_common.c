/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation, All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <src/include/pmix_config.h>

#include <src/include/types.h>
#include <pmix/autogen/pmix_stdint.h>
#include <src/include/pmix_socket_errno.h>

#include <pmix.h>
#include <pmix/pmix_common.h>
#include <pmix_server.h>

#include "src/util/output.h"

#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/include/pmix_globals.h"

PMIX_EXPORT void PMIx_Register_errhandler(pmix_info_t info[], size_t ninfo,
                              pmix_notification_fn_t errhandler,
                              pmix_errhandler_reg_cbfunc_t cbfunc,
                              void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_ERR_NOT_SUPPORTED, -1, cbdata);
    }
}

PMIX_EXPORT void PMIx_Deregister_errhandler(int errhandler_ref,
                                pmix_op_cbfunc_t cbfunc,
                                void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_ERR_NOT_SUPPORTED, cbdata);
    }
}

PMIX_EXPORT pmix_status_t PMIx_Notify_error(pmix_status_t status,
                                pmix_proc_t procs[], size_t nprocs,
                                pmix_proc_t error_procs[], size_t error_nprocs,
                                pmix_info_t info[], size_t ninfo,
                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{

    return PMIX_ERR_NOT_SUPPORTED;
}
