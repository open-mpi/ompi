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
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "examples.h"
#include <pmix.h>

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    uint32_t nprocs, n;
    pmix_info_t *info;
    bool flag;
    mylock_t mylock;
    myrel_t myrel;
    pmix_status_t dbg = PMIX_ERR_DEBUGGER_RELEASE;
    pid_t pid;
    char hostname[1024];

    pid = getpid();
    gethostname(hostname, 1024);
    fprintf(stderr, "Client %lu: Running on node %s\n", (unsigned long) pid, hostname);

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes any
     * debugger flag instructing us to stop-in-init. If such a directive
     * is included, then the process will be stopped in this call until
     * the "debugger release" notification arrives */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d pid %lu: Running\n", myproc.nspace, myproc.rank,
            (unsigned long) pid);

    /* job-related info is found in our nspace, assigned to the
     * wildcard rank as it doesn't relate to a specific rank. Setup
     * a name to retrieve such values */
    PMIX_PROC_CONSTRUCT(&proc);
    (void) strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

   /* get our universe size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        goto done;
    }
    PMIX_VALUE_GET_NUMBER(rc, val, n, uint32_t);
    fprintf(stderr, "Client %s:%d universe size %u\n", myproc.nspace, myproc.rank, n);

    /* get the number of procs in our job - univ size is the total number of allocated
     * slots, not the number of procs in the job */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d num procs %d\n", myproc.nspace, myproc.rank, nprocs);

    if (0 > asprintf(&tmp, "uniq-key")) {
        exit(1);
    }

    if (myproc.rank == 0) {
        /* put a few values */
        value.type = PMIX_UINT64;
        value.data.uint64 = 1234;
        if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, tmp, &value))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Put internal failed: %d\n", myproc.nspace,
                    myproc.rank, rc);
            goto done;
        }

        /* push the data to our PMIx server */
        if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Commit failed: %d\n", myproc.nspace,
                    myproc.rank, rc);
            goto done;
        }
    }

    /* call fence to synchronize with our peers - instruct
     * the fence operation to collect and return all "put"
     * data from our peers */
    PMIX_INFO_CREATE(info, 1);
    flag = true;
    PMIX_INFO_LOAD(info, PMIX_COLLECT_DATA, &flag, PMIX_BOOL);
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, info, 1))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        goto done;
    }
    PMIX_INFO_FREE(info, 1);

    proc.rank = PMIX_RANK_UNDEF;

    rc = PMIx_Get(&proc, tmp, NULL, 0, &val);

    if (rc != PMIX_SUCCESS) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d\n", myproc.nspace,
                myproc.rank, tmp, rc);
        goto done;
    }
    if (PMIX_UINT64 != val->type) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d\n",
                myproc.nspace, myproc.rank, tmp, val->type);
        PMIX_VALUE_RELEASE(val);
        goto done;
    }
    if (1234 != val->data.uint64) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned wrong value: %d\n",
                myproc.nspace, myproc.rank, tmp, (int) val->data.uint64);
        PMIX_VALUE_RELEASE(val);
        goto done;
    }
    fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned correct\n", myproc.nspace,
            myproc.rank, tmp);
    PMIX_VALUE_RELEASE(val);


done:
    free(tmp);

    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace,
                myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return (0);
}
