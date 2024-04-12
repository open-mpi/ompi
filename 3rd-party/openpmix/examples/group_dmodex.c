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
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/*
 * This test simulates the way Open MPI uses the PMIx_Group_construct to
 * implement MPI4 functions:
 * - MPI_Comm_create_from_group
 * - MPI_Intercomm_create_from_groups
 */

#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <pmix.h>
#include "examples.h"

static pmix_proc_t myproc;
static uint32_t get_timeout = 600; /* default 600 secs to get remote data */

static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    EXAMPLES_HIDE_UNUSED_PARAMS(evhdlr_registration_id, source,
                                info, ninfo, results, nresults,
                                cbfunc, cbdata);

    fprintf(stderr, "Client %s:%d NOTIFIED with status %d\n", myproc.nspace, myproc.rank, status);
}

static void op_callbk(pmix_status_t status, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;

    fprintf(stderr, "Client %s:%d OP CALLBACK CALLED WITH STATUS %d\n", myproc.nspace, myproc.rank,
            status);
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

static void errhandler_reg_callbk(pmix_status_t status, size_t errhandler_ref, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;

    fprintf(stderr,
            "Client %s:%d ERRHANDLER REGISTRATION CALLBACK CALLED WITH STATUS %d, ref=%lu\n",
            myproc.nspace, myproc.rank, status, (unsigned long) errhandler_ref);
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

int main(int argc, char **argv)
{
    int rc;
    size_t n;
    pmix_value_t *val = NULL;
    pmix_value_t value;
    pmix_proc_t proc, *procs;
    uint32_t nprocs;
    mylock_t lock;
    pmix_info_t *results, info, tinfo;
    size_t nresults, cid;
    char tmp[1024];

    EXAMPLES_HIDE_UNUSED_PARAMS(argc, argv);

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %s\n", myproc.nspace, myproc.rank,
                PMIx_Error_string(rc));
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    PMIX_PROC_CONSTRUCT(&proc);
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);

    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d job size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* register our default errhandler */
    DEBUG_CONSTRUCT_LOCK(&lock);
    PMIx_Register_event_handler(NULL, 0, NULL, 0, notification_fn, errhandler_reg_callbk,
                                (void *) &lock);
    DEBUG_WAIT_THREAD(&lock);
    rc = lock.status;
    DEBUG_DESTRUCT_LOCK(&lock);
    if (PMIX_SUCCESS != rc) {
        goto done;
    }

    /* call fence to sync */
    PMIX_PROC_CONSTRUCT(&proc);
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        goto done;
    }

    PMIX_PROC_CREATE(procs, nprocs);
    for (n = 0; n < nprocs; n++) {
        PMIX_PROC_LOAD(&procs[n], myproc.nspace, n);
    }
    PMIX_INFO_LOAD(&info, PMIX_GROUP_ASSIGN_CONTEXT_ID, NULL, PMIX_BOOL);
    rc = PMIx_Group_construct("ourgroup", procs, nprocs, &info, 1, &results, &nresults);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Group_construct failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    /* we should have a single results object */
    if (NULL != results) {
        cid = 0;
        PMIX_VALUE_GET_NUMBER(rc, &results[0].value, cid, size_t);
        fprintf(stderr, "%d Group construct complete with status %s KEY %s CID %ld\n",
                myproc.rank, PMIx_Error_string(rc), results[0].key, cid);
    } else {
        fprintf(stderr, "%d Group construct complete, but no CID returned\n", myproc.rank);
        goto done;
    }
    PMIX_PROC_FREE(procs, nprocs);

    /*
     * put some data
     */
    (void) snprintf(tmp, 1024, "%s-%lu-%d-remote", myproc.nspace, cid, myproc.rank);
    value.type = PMIX_UINT64;
    value.data.uint64 = 1234UL + (unsigned long) myproc.rank;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put internal failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        goto done;
    }

    /* commit the data to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Commit failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        goto done;
    }

    /*
     * destruct the group
     */
    rc = PMIx_Group_destruct("ourgroup", NULL, 0);
    if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Group_destruct failed: %s\n", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc));
            goto done;
    }

    PMIX_INFO_CONSTRUCT(&tinfo);
    PMIX_INFO_LOAD(&tinfo, PMIX_TIMEOUT, &get_timeout, PMIX_UINT32);
    for (n = 0; n < nprocs; n++) {
        proc.rank = n;
        (void)snprintf(tmp, 1024, "%s-%lu-%d-remote", myproc.nspace, cid, (int)n);
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, &tinfo, 1, &val))) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d\n",
                        myproc.nspace, (int)n, tmp, rc);
            goto done;
        }
        if (PMIX_UINT64 != val->type) {
           fprintf(stderr, "%s:%d: PMIx_Get Key %s returned wrong type: %d\n",
                   myproc.nspace, myproc.rank, tmp, val->type);
            PMIX_VALUE_RELEASE(val);
            goto done;
        }
        if ((1234UL + (unsigned long)n) != val->data.uint64) {
            fprintf(stderr, "%s:%d: PMIx_Get Key %s returned wrong value: %lu\n",
                    myproc.nspace, myproc.rank, tmp, (unsigned long)val->data.uint64);
            PMIX_VALUE_RELEASE(val);
            goto done;
        }
        PMIX_VALUE_RELEASE(val);
    }

done:
    /* finalize us */
    DEBUG_CONSTRUCT_LOCK(&lock);
    PMIx_Deregister_event_handler(1, op_callbk, &lock);
    DEBUG_WAIT_THREAD(&lock);
    DEBUG_DESTRUCT_LOCK(&lock);

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    }
    fprintf(stderr, "%s:%d COMPLETE\n", myproc.nspace, myproc.rank);
    fflush(stderr);
    return (0);
}
