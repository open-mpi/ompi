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
    pmix_value_t *val;
    pmix_proc_t proc;
    uint32_t nprocs;
    char *nodelist, **nodes, *hostname;
    pmix_info_t info[2];

    EXAMPLES_HIDE_UNUSED_PARAMS(argc, argv);

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes any
     * debugger flag instructing us to stop-in-init. If such a directive
     * is included, then the process will be stopped in this call until
     * the "debugger release" notification arrives */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    /* job-related info is found in our nspace, assigned to the
     * wildcard rank as it doesn't relate to a specific rank. Setup
     * a name to retrieve such values */
    PMIX_PROC_CONSTRUCT(&proc);
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);

    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d job size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* get the list of nodes being used */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_NODE_LIST, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get node list failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nodelist = strdup(val->data.string);
    if (0 == myproc.rank) {
        fprintf(stderr, "Client ns %s rank %d: Host list %s\n",
                myproc.nspace, myproc.rank, val->data.string);
    }
    PMIX_VALUE_RELEASE(val);
    PMIX_ARGV_SPLIT(nodes, nodelist, ',');
    free(nodelist);
    if (NULL == nodes) {
        goto done;
    }
    /* get some node-specific info */
    if (1 < nprocs) {
        proc.rank = myproc.rank + 1;
        if (nprocs == proc.rank) {
            proc.rank = 0;
        }
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_HOSTNAME, NULL, 0, &val))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get hostname for rank %u failed: %s\n",
                    myproc.nspace, myproc.rank, proc.rank, PMIx_Error_string(rc));
            goto done;
        }
        fprintf(stderr, "Client ns %s rank %d: Rank %u is on host %s\n",
                myproc.nspace, myproc.rank, proc.rank, val->data.string);
        hostname = strdup(val->data.string);
        PMIX_VALUE_RELEASE(val);

        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_FABRIC_COORDINATES, NULL, 0, &val))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get coordinates for rank %u failed: %s\n",
                    myproc.nspace, myproc.rank, proc.rank, PMIx_Error_string(rc));
        } else {
            fprintf(stderr, "Client ns %s rank %d: Rank %u has coordinates\n    %s\n",
                    myproc.nspace, myproc.rank, proc.rank, PMIx_Value_string(val));
            PMIX_VALUE_RELEASE(val);
        }

        // try with directive
        proc.rank = PMIX_RANK_WILDCARD;
        PMIX_INFO_LOAD(&info[0], PMIX_NODE_INFO, NULL, PMIX_BOOL);
        PMIX_INFO_LOAD(&info[1], PMIX_HOSTNAME, hostname, PMIX_STRING);
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_FABRIC_COORDINATES, info, 2, &val))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get coordinates with directive for host %s failed: %s\n",
                    myproc.nspace, myproc.rank, hostname, PMIx_Error_string(rc));
            goto done;
        }
        fprintf(stderr, "Client ns %s rank %d: Host %s has coordinates\n    %s\n",
                myproc.nspace, myproc.rank, hostname, PMIx_Value_string(val));
        PMIX_VALUE_RELEASE(val);
    }

done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return (0);
}
