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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <private/autogen/config.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <pmix_server.h>
#include "src/util/pmix_environ.h"
#include "src/util/output.h"
#include "src/util/printf.h"
#include "src/util/argv.h"
#include "src/buffer_ops/buffer_ops.h"

static int connected(const pmix_proc_t *proc, void *server_object);
static int finalized(const pmix_proc_t *proc, void *server_object,
                     pmix_op_cbfunc_t cbfunc, void *cbdata);
static int abort_fn(const pmix_proc_t *proc,
                    void *server_object,
                    int status, const char msg[],
                    pmix_proc_t procs[], size_t nprocs,
                    pmix_op_cbfunc_t cbfunc, void *cbdata);
static int fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                      const pmix_info_t info[], size_t ninfo,
                      char *data, size_t ndata,
                      pmix_modex_cbfunc_t cbfunc, void *cbdata);
static int dmodex_fn(const pmix_proc_t *proc,
                     const pmix_info_t info[], size_t ninfo,
                     pmix_modex_cbfunc_t cbfunc, void *cbdata);
static int publish_fn(const pmix_proc_t *proc,
                      const pmix_info_t info[], size_t ninfo,
                      pmix_op_cbfunc_t cbfunc, void *cbdata);
static int lookup_fn(const pmix_proc_t *proc, char **keys,
                     const pmix_info_t info[], size_t ninfo,
                     pmix_lookup_cbfunc_t cbfunc, void *cbdata);
static int unpublish_fn(const pmix_proc_t *proc, char **keys,
                        const pmix_info_t info[], size_t ninfo,
                        pmix_op_cbfunc_t cbfunc, void *cbdata);
static int spawn_fn(const pmix_proc_t *proc,
                    const pmix_info_t job_info[], size_t ninfo,
                    const pmix_app_t apps[], size_t napps,
                    pmix_spawn_cbfunc_t cbfunc, void *cbdata);
static int connect_fn(const pmix_proc_t procs[], size_t nprocs,
                      const pmix_info_t info[], size_t ninfo,
                      pmix_op_cbfunc_t cbfunc, void *cbdata);
static int disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                         const pmix_info_t info[], size_t ninfo,
                         pmix_op_cbfunc_t cbfunc, void *cbdata);
static int register_events_fn(const pmix_info_t info[], size_t ninfo,
                              pmix_op_cbfunc_t cbfunc, void *cbdata);

/* this example doesn't use a listener function to centralize
 * connection accept support into a single thread. Thus, the
 * PMIx server library will spawn its own listener thread */

static pmix_server_module_t mymodule = {
    connected,
    finalized,
    abort_fn,
    fencenb_fn,
    dmodex_fn,
    publish_fn,
    lookup_fn,
    unpublish_fn,
    spawn_fn,
    connect_fn,
    disconnect_fn,
    register_events_fn,
    NULL
};

typedef struct {
    pmix_list_item_t super;
    pmix_pdata_t pdata;
} pmix_locdat_t;
PMIX_CLASS_INSTANCE(pmix_locdat_t,
                    pmix_list_item_t,
                    NULL, NULL);

typedef struct {
    pmix_object_t super;
    volatile bool completed;
    pmix_proc_t caller;
    pmix_info_t *info;
    size_t ninfo;
    pmix_op_cbfunc_t cbfunc;
    pmix_spawn_cbfunc_t spcbfunc;
    void *cbdata;
} myxfer_t;
static void xfcon(myxfer_t *p)
{
    p->info = NULL;
    p->ninfo = 0;
    p->completed = false;
    p->cbfunc = NULL;
    p->spcbfunc = NULL;
    p->cbdata = NULL;
}
static void xfdes(myxfer_t *p)
{
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
}
PMIX_CLASS_INSTANCE(myxfer_t,
                    pmix_object_t,
                    xfcon, xfdes);

static volatile int wakeup;
static pmix_list_t pubdata;

static void set_namespace(int nprocs, char *ranks, char *nspace,
                          pmix_op_cbfunc_t cbfunc, myxfer_t *x);
static void errhandler(pmix_status_t status,
                       pmix_proc_t procs[], size_t nprocs,
                       pmix_info_t info[], size_t ninfo);
static void op_callbk(pmix_status_t status, void *cbdata);
static void errhandler_reg_callbk (pmix_status_t status,
                                   int errhandler_ref,
                                   void *cbdata);
static void opcbfunc(pmix_status_t status, void *cbdata)
{
    myxfer_t *x = (myxfer_t*)cbdata;

    x->completed = true;
    /* release the caller, if necessary - note that
     * this may result in release of x, so this must
     * be the last thing we do with it here */
    if (NULL != x->cbfunc) {
        x->cbfunc(PMIX_SUCCESS, x->cbdata);
    }
    PMIX_RELEASE(x);
}

int main(int argc, char **argv)
{
    char **client_env=NULL;
    char **client_argv=NULL;
    char *tmp, **atmp, *executable=NULL;
    int rc, nprocs=1, n;
    uid_t myuid;
    gid_t mygid;
    pid_t pid;
    myxfer_t *x;
    pmix_proc_t proc;

    fprintf(stderr, "Running version %s\n", PMIx_Get_version());

    /* setup the server library */
    if (PMIX_SUCCESS != (rc = PMIx_server_init(&mymodule, NULL, 0))) {
        fprintf(stderr, "Init failed with error %d\n", rc);
        return rc;
    }
    /* register the errhandler */
    PMIx_Register_errhandler(NULL, 0, errhandler, errhandler_reg_callbk, NULL);

    /* setup the pub data, in case it is used */
    PMIX_CONSTRUCT(&pubdata, pmix_list_t);

    /* see if we were passed the number of procs to run or
     * the executable to use */
    for (n=1; n < (argc-1); n++) {
        if (0 == strcmp("-n", argv[n])) {
            if (argc-2 <= n) {
                exit(1);
            }
            nprocs = strtol(argv[n+1], NULL, 10);
            ++n;  // step over the argument
        } else if (0 == strcmp("-e", argv[n])) {
            if (argc-2 <= n) {
                exit(1);
            }
            executable = strdup(argv[n+1]);
            ++n;
        }
    }
    if (NULL == executable) {
        executable = strdup("client");
    }

    /* we have a single namespace for all clients */
    atmp = NULL;
    for (n=0; n < nprocs; n++) {
        asprintf(&tmp, "%d", n);
        pmix_argv_append_nosize(&atmp, tmp);
        free(tmp);
    }
    tmp = pmix_argv_join(atmp, ',');
    x = PMIX_NEW(myxfer_t);
    set_namespace(nprocs, tmp, "foobar", opcbfunc, x);
    free(tmp);

    /* set common argv and env */
    client_env = pmix_argv_copy(environ);
    pmix_argv_append_nosize(&client_argv, executable);

    wakeup = nprocs;
    myuid = getuid();
    mygid = getgid();

    /* if the nspace registration hasn't completed yet,
     * wait for it here */
    while (!x->completed) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }
    PMIX_RELEASE(x);

    /* fork/exec the test */
    (void)strncpy(proc.nspace, "foobar", PMIX_MAX_NSLEN);
    for (n = 0; n < nprocs; n++) {
        proc.rank = n;
        if (PMIX_SUCCESS != (rc = PMIx_server_setup_fork(&proc, &client_env))) {//n
            fprintf(stderr, "Server fork setup failed with error %d\n", rc);
            PMIx_server_finalize();
            return rc;
        }
        x = PMIX_NEW(myxfer_t);
        if (PMIX_SUCCESS != (rc = PMIx_server_register_client(&proc, myuid, mygid,
                                                              NULL, opcbfunc, x))) {
            fprintf(stderr, "Server fork setup failed with error %d\n", rc);
            PMIx_server_finalize();
            return rc;
        }
        /* don't fork/exec the client until we know it is registered
         * so we avoid a potential race condition in the server */
        while (!x->completed) {
            struct timespec ts;
            ts.tv_sec = 0;
            ts.tv_nsec = 100000;
            nanosleep(&ts, NULL);
        }
        PMIX_RELEASE(x);
        pid = fork();
        if (pid < 0) {
            fprintf(stderr, "Fork failed\n");
            PMIx_server_finalize();
            return -1;
        }

        if (pid == 0) {
            execve(executable, client_argv, client_env);
            /* Does not return */
            exit(0);
        }
    }

    /* hang around until the client(s) finalize */
    while (0 < wakeup) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }

    pmix_argv_free(client_argv);
    pmix_argv_free(client_env);

    /* deregister the errhandler */
    PMIx_Deregister_errhandler(0, op_callbk, NULL);

    /* release any pub data */
    PMIX_LIST_DESTRUCT(&pubdata);

    /* finalize the server library */
    if (PMIX_SUCCESS != (rc = PMIx_server_finalize())) {
        fprintf(stderr, "Finalize failed with error %d\n", rc);
    }

    fprintf(stderr, "Server finished!\n");

    return rc;
}

static void set_namespace(int nprocs, char *ranks, char *nspace,
                          pmix_op_cbfunc_t cbfunc, myxfer_t *x)
{
    char *regex, *ppn;
    char hostname[1024];

    gethostname(hostname, 1024);

    PMIX_INFO_CREATE(x->info, x->ninfo);
    (void)strncpy(x->info[0].key, PMIX_UNIV_SIZE, PMIX_MAX_KEYLEN);
    x->info[0].value.type = PMIX_UINT32;
    x->info[0].value.data.uint32 = nprocs;

    (void)strncpy(x->info[1].key, PMIX_SPAWNED, PMIX_MAX_KEYLEN);
    x->info[1].value.type = PMIX_UINT32;
    x->info[1].value.data.uint32 = 0;

    (void)strncpy(x->info[2].key, PMIX_LOCAL_SIZE, PMIX_MAX_KEYLEN);
    x->info[2].value.type = PMIX_UINT32;
    x->info[2].value.data.uint32 = nprocs;

    (void)strncpy(x->info[3].key, PMIX_LOCAL_PEERS, PMIX_MAX_KEYLEN);
    x->info[3].value.type = PMIX_STRING;
    x->info[3].value.data.string = strdup(ranks);

    PMIx_generate_regex(hostname, &regex);
    (void)strncpy(x->info[4].key, PMIX_NODE_MAP, PMIX_MAX_KEYLEN);
    x->info[4].value.type = PMIX_STRING;
    x->info[4].value.data.string = regex;

    PMIx_generate_ppn(ranks, &ppn);
    (void)strncpy(x->info[5].key, PMIX_PROC_MAP, PMIX_MAX_KEYLEN);
    x->info[5].value.type = PMIX_STRING;
    x->info[5].value.data.string = ppn;

    PMIx_server_register_nspace(nspace, nprocs, x->info, x->ninfo,
                                cbfunc, x);
}

static void errhandler(pmix_status_t status,
                       pmix_proc_t procs[], size_t nprocs,
                       pmix_info_t info[], size_t ninfo)
{
    pmix_output_verbose(0, pmix_globals.debug_output, "SERVER: ERRHANDLER CALLED WITH STATUS %d", status);
}

static void op_callbk(pmix_status_t status,
                      void *cbdata)
{
    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: OP CALLBACK CALLED WITH STATUS %d", status);
}

static void errhandler_reg_callbk (pmix_status_t status,
                                   int errhandler_ref,
                                   void *cbdata)
{
    pmix_output_verbose(1, pmix_globals.debug_output, "SERVER: ERRHANDLER REGISTRATION CALLBACK CALLED WITH STATUS %d, ref=%d",
                status, errhandler_ref);
}

static int connected(const pmix_proc_t *proc, void *server_object)
{
    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: CONNECTED %s:%d", proc->nspace, proc->rank);
    return PMIX_SUCCESS;

}

static int finalized(const pmix_proc_t *proc, void *server_object,
                     pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: FINALIZED %s:%d", proc->nspace, proc->rank);
    --wakeup;
    /* ensure we call the cbfunc so the proc can exit! */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

static void abcbfunc(pmix_status_t status, void *cbdata)
{
    myxfer_t *x = (myxfer_t*)cbdata;

    /* be sure to release the caller */
    if (NULL != x->cbfunc) {
        x->cbfunc(status, x->cbdata);
    }
    PMIX_RELEASE(x);
}

static pmix_status_t abort_fn(const pmix_proc_t *proc,
                              void *server_object,
                              pmix_status_t status, const char msg[],
                              pmix_proc_t procs[], size_t nprocs,
                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    myxfer_t *x;

    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: ABORT on %s:%d", procs[0].nspace, procs[0].rank);

    /* instead of aborting the specified procs, notify them
     * (if they have registered their errhandler) */

    /* use the myxfer_t object to ensure we release
     * the caller when notification has been queued */
    x = PMIX_NEW(myxfer_t);
    (void)strncpy(x->caller.nspace, proc->nspace, PMIX_MAX_NSLEN);
    x->caller.rank = proc->rank;

    PMIX_INFO_CREATE(x->info, 2);
    (void)strncpy(x->info[0].key, "DARTH", PMIX_MAX_KEYLEN);
    x->info[0].value.type = PMIX_INT8;
    x->info[0].value.data.int8 = 12;
    (void)strncpy(x->info[1].key, "VADER", PMIX_MAX_KEYLEN);
    x->info[1].value.type = PMIX_DOUBLE;
    x->info[1].value.data.dval = 12.34;
    x->cbfunc = cbfunc;
    x->cbdata = cbdata;

    if (PMIX_SUCCESS != (rc = PMIx_Notify_error(status, procs, nprocs,
                                                       &x->caller, 1, x->info, 2,
                                                       abcbfunc, x))) {
        pmix_output_verbose(0, pmix_globals.debug_output, "SERVER: FAILED NOTIFY ERROR %d", (int)rc);
    }

    return PMIX_SUCCESS;
}


static int fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                      const pmix_info_t info[], size_t ninfo,
                      char *data, size_t ndata,
                      pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: FENCENB");
    /* pass the provided data back to each participating proc */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, data, ndata, cbdata, NULL, NULL);
    }
    return PMIX_SUCCESS;
}


static int dmodex_fn(const pmix_proc_t *proc,
                     const pmix_info_t info[], size_t ninfo,
                     pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: DMODEX");

    /* we don't have any data for remote procs as this
     * test only runs one server - so report accordingly */
    if (NULL != cbfunc) {
        cbfunc(PMIX_ERR_NOT_FOUND, NULL, 0, cbdata, NULL, NULL);
    }
    return PMIX_SUCCESS;
}


static int publish_fn(const pmix_proc_t *proc,
                      const pmix_info_t info[], size_t ninfo,
                      pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_locdat_t *p;
    size_t n;

    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: PUBLISH");

    for (n=0; n < ninfo; n++) {
        p = PMIX_NEW(pmix_locdat_t);
        (void)strncpy(p->pdata.proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
        p->pdata.proc.rank = proc->rank;
        (void)strncpy(p->pdata.key, info[n].key, PMIX_MAX_KEYLEN);
        pmix_value_xfer(&p->pdata.value, (pmix_value_t*)&info[n].value);
        pmix_list_append(&pubdata, &p->super);
    }
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}


static int lookup_fn(const pmix_proc_t *proc, char **keys,
                     const pmix_info_t info[], size_t ninfo,
                     pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_locdat_t *p, *p2;
    pmix_list_t results;
    size_t i, n;
    pmix_pdata_t *pd;
    pmix_status_t ret = PMIX_ERR_NOT_FOUND;

    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: LOOKUP");

    PMIX_CONSTRUCT(&results, pmix_list_t);

    for (n=0; NULL != keys[n]; n++) {
        PMIX_LIST_FOREACH(p, &pubdata, pmix_locdat_t) {
            if (0 == strncmp(keys[n], p->pdata.key, PMIX_MAX_KEYLEN)) {
                p2 = PMIX_NEW(pmix_locdat_t);
                (void)strncpy(p2->pdata.proc.nspace, p->pdata.proc.nspace, PMIX_MAX_NSLEN);
                p2->pdata.proc.rank = p->pdata.proc.rank;
                (void)strncpy(p2->pdata.key, p->pdata.key, PMIX_MAX_KEYLEN);
                pmix_value_xfer(&p2->pdata.value, &p->pdata.value);
                pmix_list_append(&results, &p2->super);
                break;
            }
        }
    }
    if (0 < (n = pmix_list_get_size(&results))) {
        ret = PMIX_SUCCESS;
        PMIX_PDATA_CREATE(pd, n);
        for (i=0; i < n; i++) {
            p = (pmix_locdat_t*)pmix_list_remove_first(&results);
            (void)strncpy(pd[i].proc.nspace, p->pdata.proc.nspace, PMIX_MAX_NSLEN);
            pd[i].proc.rank = p->pdata.proc.rank;
            (void)strncpy(pd[i].key, p->pdata.key, PMIX_MAX_KEYLEN);
            pmix_value_xfer(&pd[i].value, &p->pdata.value);
        }
    }
    PMIX_LIST_DESTRUCT(&results);
    if (NULL != cbfunc) {
        cbfunc(ret, pd, n, cbdata);
    }
    if (0 < n) {
        PMIX_PDATA_FREE(pd, n);
    }
    return PMIX_SUCCESS;
}


static int unpublish_fn(const pmix_proc_t *proc, char **keys,
                        const pmix_info_t info[], size_t ninfo,
                        pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_locdat_t *p, *p2;
    size_t n;

    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: UNPUBLISH");

    for (n=0; NULL != keys[n]; n++) {
        PMIX_LIST_FOREACH_SAFE(p, p2, &pubdata, pmix_locdat_t) {
            if (0 == strncmp(keys[n], p->pdata.key, PMIX_MAX_KEYLEN)) {
                pmix_list_remove_item(&pubdata, &p->super);
                PMIX_RELEASE(p);
                break;
            }
        }
    }
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

static void spcbfunc(pmix_status_t status, void *cbdata)
{
    myxfer_t *x = (myxfer_t*)cbdata;

    if (NULL != x->spcbfunc) {
        x->spcbfunc(PMIX_SUCCESS, "DYNSPACE", x->cbdata);
    }
}

static int spawn_fn(const pmix_proc_t *proc,
                    const pmix_info_t job_info[], size_t ninfo,
                    const pmix_app_t apps[], size_t napps,
                    pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    myxfer_t *x;

    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: SPAWN");

    /* in practice, we would pass this request to the local
     * resource manager for launch, and then have that server
     * execute our callback function. For now, we will fake
     * the spawn and just pretend */

    /* must register the nspace for the new procs before
     * we return to the caller */
    x = PMIX_NEW(myxfer_t);
    x->spcbfunc = cbfunc;
    x->cbdata = cbdata;

    set_namespace(2, "0,1", "DYNSPACE", spcbfunc, x);

    return PMIX_SUCCESS;
}


static int connect_fn(const pmix_proc_t procs[], size_t nprocs,
                      const pmix_info_t info[], size_t ninfo,
                      pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: CONNECT");

    /* in practice, we would pass this request to the local
     * resource manager for handling */

    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }

    return PMIX_SUCCESS;
}


static int disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                         const pmix_info_t info[], size_t ninfo,
                         pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output_verbose(2, pmix_globals.debug_output,"SERVER: DISCONNECT");

    /* in practice, we would pass this request to the local
     * resource manager for handling */

    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }

    return PMIX_SUCCESS;
}

static int register_events_fn(const pmix_info_t info[], size_t ninfo,
                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output_verbose(2, pmix_globals.debug_output, "SERVER: REGISTER EVENTS");

    /* in practice, we would pass this request to the local
     * resource manager for handling */

    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }

    return PMIX_SUCCESS;
}

