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

#include <pmix.h>

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pid_t pid;
    char hostname[1024];
    pmix_value_t *val;
    uint16_t localrank;
    size_t n;
    pmix_info_t optional;

    pid = getpid();
    gethostname(hostname, 1024);

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes any
     * debugger flag instructing us to stop-in-init. If such a directive
     * is included, then the process will be stopped in this call until
     * the "debugger release" notification arrives */
    rc = PMIx_Init(&myproc, NULL, 0);
    if (PMIX_SUCCESS != rc && PMIX_ERR_UNREACH != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(0);
    }
    /* get our local rank */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_LOCAL_RANK, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get local rank failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    localrank = val->data.uint16;
    PMIX_VALUE_RELEASE(val);

    fprintf(stderr, "Client ns %s rank %d pid %lu: Running on host %s localrank %d\n",
            myproc.nspace, myproc.rank, (unsigned long)pid, hostname , (int)localrank);

    pmix_proc_t wild;
    PMIX_LOAD_PROCID(&wild, myproc.nspace, PMIX_RANK_WILDCARD);
    rc = PMIx_Get(&wild, PMIX_LOCAL_PEERS, NULL, 0, &val);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Unable to get local peers\n");
    } else {
        fprintf(stderr, "%s:%u - local peers %s\n", myproc.nspace, myproc.rank, val->data.string);
    }

    PMIX_LOAD_PROCID(&wild, myproc.nspace, PMIX_RANK_WILDCARD);
    PMIX_INFO_LOAD(&optional, PMIX_OPTIONAL, NULL, PMIX_BOOL);
    rc = PMIx_Get(&wild, PMIX_NODE_OVERSUBSCRIBED, &optional, 1, &val);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Not oversubscribed\n");
    } else {
        fprintf(stderr, "%s:%u - oversubscribed\n", myproc.nspace, myproc.rank);
    }

  done:
    /* finalize us */

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
    }
    fflush(stderr);
    return(0);
}
