/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef PMIX_SERVER_CALLBACK_H
#define PMIX_SERVER_CALLBACK_H

#include "cli_stages.h"

pmix_status_t connected(const pmix_proc_t *proc, void *server_object);
pmix_status_t finalized(const pmix_proc_t *proc, void *server_object,
                        pmix_op_cbfunc_t cbfunc, void *cbdata);
pmix_status_t abort_fn(const pmix_proc_t *proc,
                       void *server_object,
                       int status, const char msg[],
                       pmix_proc_t procs[], size_t nprocs,
                       pmix_op_cbfunc_t cbfunc, void *cbdata);
pmix_status_t fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                         const pmix_info_t info[], size_t ninfo,
                         char *data, size_t ndata,
                         pmix_modex_cbfunc_t cbfunc, void *cbdata);
pmix_status_t dmodex_fn(const pmix_proc_t *proc,
                        const pmix_info_t info[], size_t ninfo,
                        pmix_modex_cbfunc_t cbfunc, void *cbdata);
pmix_status_t publish_fn(const pmix_proc_t *proc,
                         const pmix_info_t info[], size_t ninfo,
                         pmix_op_cbfunc_t cbfunc, void *cbdata);
pmix_status_t lookup_fn(const pmix_proc_t *proc, char **keys,
                        const pmix_info_t info[], size_t ninfo,
                        pmix_lookup_cbfunc_t cbfunc, void *cbdata);
pmix_status_t unpublish_fn(const pmix_proc_t *proc, char **keys,
                           const pmix_info_t info[], size_t ninfo,
                           pmix_op_cbfunc_t cbfunc, void *cbdata);
pmix_status_t spawn_fn(const pmix_proc_t *proc,
                       const pmix_info_t job_info[], size_t ninfo,
                       const pmix_app_t apps[], size_t napps,
                       pmix_spawn_cbfunc_t cbfunc, void *cbdata);
pmix_status_t connect_fn(const pmix_proc_t procs[], size_t nprocs,
                         const pmix_info_t info[], size_t ninfo,
                         pmix_op_cbfunc_t cbfunc, void *cbdata);
pmix_status_t disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                            const pmix_info_t info[], size_t ninfo,
                            pmix_op_cbfunc_t cbfunc, void *cbdata);
pmix_status_t regevents_fn(const pmix_info_t info[], size_t ninfo,
                            pmix_op_cbfunc_t cbfunc, void *cbdata);
pmix_status_t deregevents_fn(const pmix_info_t info[], size_t ninfo,
                           pmix_op_cbfunc_t cbfunc, void *cbdata);
extern pmix_server_module_t mymodule;

#endif
