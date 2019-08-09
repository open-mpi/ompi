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
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <src/include/pmix_config.h>
#include <pmix.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "src/class/pmix_object.h"
#include "src/util/output.h"
#include "src/util/printf.h"

static uint32_t nprocs;
static pmix_proc_t myproc;
static uint32_t getcount = 0;

#define PMIX_WAIT_FOR_COMPLETION(a)             \
    do {                                        \
        while ((a)) {                           \
            usleep(10);                         \
        }                                       \
    } while (0)

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    bool *active = (bool*)cbdata;

    pmix_output(0, "%s:%d completed fence_nb", myproc.nspace, myproc.rank);
    *active = false;
}

static void valcbfunc(pmix_status_t status,
                      pmix_value_t *val, void *cbdata)
{
    char *key = (char*)cbdata;

    if (PMIX_SUCCESS == status) {
        if (NULL != strstr(key, "local")) {
            if (PMIX_UINT64 != val->type) {
                pmix_output(0, "%s:%d: PMIx_Get_nb Key %s returned wrong type: %d", myproc.nspace, myproc.rank, key, val->type);
                goto done;
            }
            if (1234 != val->data.uint64) {
                pmix_output(0, "%s:%d: PMIx_Get_nb Key %s returned wrong value: %d", myproc.nspace, myproc.rank, key, (int)val->data.uint64);
                goto done;
            }
        } else if (NULL != strstr(key, "remote")) {
            if (PMIX_STRING != val->type) {
                pmix_output(0, "%s:%d: PMIx_Get_nb Key %s returned wrong type: %d", myproc.nspace, myproc.rank, key, val->type);
                goto done;
            }
            if (0 != strcmp(val->data.string, "1234")) {
                pmix_output(0, "%s:%d: PMIx_Get_nb Key %s returned wrong value: %s", myproc.nspace, myproc.rank, key, val->data.string);
                goto done;
            }
        } else {
            pmix_output(0, "%s:%d PMIx_Get_nb returned wrong key: %s", myproc.nspace, myproc.rank, key);
            goto done;
        }
        pmix_output(0, "%s:%d PMIx_Get_nb Key %s returned correctly", myproc.nspace, myproc.rank, key);
    } else {
        pmix_output(0, "%s:%d PMIx_Get_nb Key %s failed: %s", myproc.nspace, myproc.rank, key, PMIx_Error_string(status));
    }
 done:
    free(key);
    getcount++;
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    uint32_t n, num_gets, k, nlocal;
    bool active;
    bool dofence = true;
    bool local, all_local;
    char **peers;
    pmix_rank_t *locals;

    if (NULL != getenv("PMIX_SIMPDMODEX_ASYNC")) {
        dofence = false;
    }

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %d", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    pmix_output(0, "Client ns %s rank %d: Running", myproc.nspace, myproc.rank);

    /* get our job size */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get job size failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d job size %d", myproc.nspace, myproc.rank, nprocs);

    /* put a few values */
    (void)asprintf(&tmp, "%s-%d-internal", myproc.nspace, myproc.rank);
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&myproc, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Store_internal failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    (void)asprintf(&tmp, "%s-%d-local", myproc.nspace, myproc.rank);
    value.type = PMIX_UINT64;
    value.data.uint64 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    (void)asprintf(&tmp, "%s-%d-remote", myproc.nspace, myproc.rank);
    value.type = PMIX_STRING;
    value.data.string = "1234";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* introduce a delay by one rank so we can check what happens
     * if a "get" is received prior to data being provided */
    if (0 == myproc.rank) {
        sleep(2);
        pmix_output(0, "\n\n\nWOKE UP");
    }

    /* commit the data to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Commit failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    if (dofence) {
        /* call fence_nb, but don't return any data */
        PMIX_PROC_CONSTRUCT(&proc);
        (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
        proc.rank = PMIX_RANK_WILDCARD;
        active = true;
        if (PMIX_SUCCESS != (rc = PMIx_Fence_nb(&proc, 1, NULL, 0, opcbfunc, &active))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Fence failed: %d", myproc.nspace, myproc.rank, rc);
            goto done;
        }
    }

    /* get a list of our local peers */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_PEERS, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get local peers failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    /* split the returned string to get the rank of each local peer */
    peers = pmix_argv_split(val->data.string, ',');
    PMIX_VALUE_RELEASE(val);
    nlocal = pmix_argv_count(peers);
    if (nprocs == nlocal) {
        all_local = true;
    } else {
        all_local = false;
        locals = (pmix_rank_t*)malloc(pmix_argv_count(peers) * sizeof(pmix_rank_t));
        for (n=0; NULL != peers[n]; n++) {
            locals[n] = strtoul(peers[n], NULL, 10);
        }
    }
    pmix_argv_free(peers);

    /* get the committed data - ask for someone who doesn't exist as well */
    num_gets = 0;
    for (n=0; n < nprocs; n++) {
        if (all_local) {
            local = true;
        } else {
            local = false;
            /* see if this proc is local to us */
            for (k=0; k < nlocal; k++) {
                if (proc.rank == locals[k]) {
                    local = true;
                    break;
                }
            }
        }
        if (local) {
            (void)asprintf(&tmp, "%s-%d-local", myproc.nspace, n);
            proc.rank = n;
            if (PMIX_SUCCESS != (rc = PMIx_Get_nb(&proc, tmp,
                                                  NULL, 0, valcbfunc, tmp))) {
                pmix_output(0, "Client ns %s rank %d: PMIx_Get %s failed: %d", myproc.nspace, n, tmp, rc);
                goto done;
            }
            ++num_gets;
        } else {
            (void)asprintf(&tmp, "%s-%d-remote", myproc.nspace, n);
            if (PMIX_SUCCESS != (rc = PMIx_Get_nb(&proc, tmp,
                                                  NULL, 0, valcbfunc, tmp))) {
                pmix_output(0, "Client ns %s rank %d: PMIx_Get %s failed: %d", myproc.nspace, n, tmp, rc);
                goto done;
            }
            ++num_gets;
        }
    }

    if (dofence) {
        /* wait for the first fence to finish */
        PMIX_WAIT_FOR_COMPLETION(active);
    }

    /* wait for all my "get" calls to complete */
    while (getcount < num_gets) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }

    /* call fence again so everyone waits before leaving */
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Fence failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

 done:
    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
