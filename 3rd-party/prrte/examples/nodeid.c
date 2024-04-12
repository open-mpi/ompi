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
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
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
#include <unistd.h>

#include "examples.h"
#include <pmix.h>

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pid_t pid;
    char hostname[1024];
    pmix_value_t *val;
    uint32_t jobsize, nodeid;
    size_t n;
    pmix_proc_t proc, wildcard;

    pid = getpid();
    gethostname(hostname, 1024);

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "[%s:%lu] PMIx_Init failed: %s\n",
                hostname, (unsigned long)pid, PMIx_Error_string(rc));
        exit(0);
    }
    PMIX_LOAD_PROCID(&wildcard, myproc.nspace, PMIX_RANK_WILDCARD);

    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&wildcard, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "[%s:%u] PMIx_Get job size failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    PMIX_VALUE_GET_NUMBER(rc, val, jobsize, uint32_t);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "[%s:%u] Got bad job size: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    PMIX_VALUE_RELEASE(val);

    /* get the nodeid of all our peers */
    PMIX_LOAD_NSPACE(proc.nspace, myproc.nspace);
    for (n=0; n < jobsize; n++) {
        proc.rank = n;
        rc = PMIx_Get(&proc, PMIX_NODEID, NULL, 0, &val);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "[%s:%u] PMIx_Get failed for nodeid on rank %zd: %s\n",
                    myproc.nspace, myproc.rank, n, PMIx_Error_string(rc));
            break;
        }
        PMIX_VALUE_GET_NUMBER(rc, val, nodeid, uint32_t);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "[%s:%u] Got bad nodeid for rank %zd: %s\n",
                    myproc.nspace, myproc.rank, n, PMIx_Error_string(rc));
            goto done;
        }
        if (0 == myproc.rank) {
            fprintf(stderr, "[%s:%u] Peer %zd is running on node %u\n",
                    myproc.nspace, myproc.rank, n, nodeid);
        }
        PMIX_VALUE_RELEASE(val);
    }

    fprintf(stderr, "[%s:%u]: Successfully retrieved all nodeids\n",
            myproc.nspace, myproc.rank);

done:
    /* finalize us */
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    }
    fflush(stderr);
    return (0);
}
