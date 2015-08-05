/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
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

int finalized(const char nspace[], int rank, void *server_object,
              pmix_op_cbfunc_t cbfunc, void *cbdata);
int abort_fn(const char nspace[], int rank,
             void *server_object,
             int status, const char msg[],
             pmix_proc_t procs[], size_t nprocs,
             pmix_op_cbfunc_t cbfunc, void *cbdata);
int fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
               char *data, size_t ndata,
               pmix_modex_cbfunc_t cbfunc, void *cbdata);
int dmodex_fn(const char nspace[], int rank,
              pmix_modex_cbfunc_t cbfunc, void *cbdata);
int publish_fn(const char nspace[], int rank,
               pmix_scope_t scope, pmix_persistence_t persist,
               const pmix_info_t info[], size_t ninfo,
               pmix_op_cbfunc_t cbfunc, void *cbdata);
int lookup_fn(pmix_scope_t scope, int wait, char **keys,
              pmix_lookup_cbfunc_t cbfunc, void *cbdata);
int unpublish_fn(pmix_scope_t scope, char **keys,
                 pmix_op_cbfunc_t cbfunc, void *cbdata);
int spawn_fn(const pmix_app_t apps[], size_t napps,
             pmix_spawn_cbfunc_t cbfunc, void *cbdata);
int connect_fn(const pmix_proc_t procs[], size_t nprocs,
               pmix_op_cbfunc_t cbfunc, void *cbdata);
int disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                  pmix_op_cbfunc_t cbfunc, void *cbdata);

extern pmix_server_module_t mymodule;

#endif
