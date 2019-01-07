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

#include <stdbool.h>

#define _GNU_SOURCE
#include <stdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <pmix.h>
#include "examples.h"

static uint32_t nprocs;
static pmix_proc_t myproc;
static uint32_t getcount = 0;

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;

    fprintf(stderr, "%s:%d completed fence_nb\n", myproc.nspace, myproc.rank);
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

static void valcbfunc(pmix_status_t status,
                      pmix_value_t *val, void *cbdata)
{
    char *key = (char*)cbdata;

    if (PMIX_SUCCESS == status) {
        if (NULL != strstr(key, "local")) {
            if (PMIX_UINT64 != val->type) {
                fprintf(stderr, "%s:%d: PMIx_Get_nb Key %s returned wrong type: %d\n", myproc.nspace, myproc.rank, key, val->type);
                goto done;
            }
            if (1234 != val->data.uint64) {
                fprintf(stderr, "%s:%d: PMIx_Get_nb Key %s returned wrong value: %d\n", myproc.nspace, myproc.rank, key, (int)val->data.uint64);
                goto done;
            }
        } else if (NULL != strstr(key, "remote")) {
            if (PMIX_STRING != val->type) {
                fprintf(stderr, "%s:%d: PMIx_Get_nb Key %s returned wrong type: %d\n", myproc.nspace, myproc.rank, key, val->type);
                goto done;
            }
            if (0 != strcmp(val->data.string, "1234")) {
                fprintf(stderr, "%s:%d: PMIx_Get_nb Key %s returned wrong value: %s\n", myproc.nspace, myproc.rank, key, val->data.string);
                goto done;
            }
        } else {
            fprintf(stderr, "%s:%d PMIx_Get_nb returned wrong key: %s\n", myproc.nspace, myproc.rank, key);
            goto done;
        }
        fprintf(stderr, "%s:%d PMIx_Get_nb Key %s returned correctly\n", myproc.nspace, myproc.rank, key);
    } else {
        fprintf(stderr, "%s:%d PMIx_Get_nb Key %s failed\n", myproc.nspace, myproc.rank, key);
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
    uint32_t n, num_gets;
    mylock_t mylock;

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    /* get our universe size */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d universe size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* put a few values */
    if (0 > asprintf(&tmp, "%s-%d-internal", myproc.nspace, myproc.rank)) {
        exit(1);
    }
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&myproc, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Store_internal failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    free(tmp);

    if (0 > asprintf(&tmp, "%s-%d-local", myproc.nspace, myproc.rank)) {
        exit(1);
    }
    value.type = PMIX_UINT64;
    value.data.uint64 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put internal failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    free(tmp);

    if (0 > asprintf(&tmp, "%s-%d-remote", myproc.nspace, myproc.rank)) {
        exit(1);
    }
    value.type = PMIX_STRING;
    value.data.string = "1234";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_REMOTE, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put internal failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    free(tmp);

    /* introduce a delay by one rank so we can check what happens
     * if a "get" is received prior to data being provided */

    if (0 == myproc.rank) {
        sleep(2);
    }

    /* commit the data to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Commit failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* call fence_nb, but don't return any data */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    DEBUG_CONSTRUCT_LOCK(&mylock);
    if (PMIX_SUCCESS != (rc = PMIx_Fence_nb(&proc, 1, NULL, 0, opcbfunc, &mylock))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        DEBUG_DESTRUCT_LOCK(&mylock);
        goto done;
    }

    /* get the committed data - ask for someone who doesn't exist as well */
    num_gets = 0;
    for (n=0; n <= nprocs; n++) {
        if (0 > asprintf(&tmp, "%s-%d-local", myproc.nspace, n)) {
            exit(1);
        }
        (void)strncpy(proc.nspace, tmp, PMIX_MAX_NSLEN);
        proc.rank = n;
        if (PMIX_SUCCESS != (rc = PMIx_Get_nb(&proc, tmp,
                                              NULL, 0, valcbfunc, tmp))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d\n", myproc.nspace, n, tmp, rc);
            goto done;
        }
        ++num_gets;
        if (0 > asprintf(&tmp, "%s-%d-remote", myproc.nspace, n)) {
            exit(1);
        }
        (void)strncpy(proc.nspace, tmp, PMIX_MAX_NSLEN);
        if (PMIX_SUCCESS != (rc = PMIx_Get_nb(&proc, tmp,
                                              NULL, 0, valcbfunc, tmp))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d\n", myproc.nspace, n, tmp, rc);
            goto done;
        }
        ++num_gets;
    }

    /* wait for the first fence to finish */
    DEBUG_WAIT_THREAD(&mylock);

    /* wait for all my "get" calls to complete */
    while (getcount < num_gets) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }

    /* call fence again so everyone waits before leaving */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

 done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
