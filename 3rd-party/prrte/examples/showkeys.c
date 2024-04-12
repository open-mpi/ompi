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
#include <unistd.h>

#include "examples.h"
#include <pmix.h>

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_value_t *val;
    pmix_info_t *info;
    size_t ninfo, n;

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes any
     * debugger flag instructing us to stop-in-init. If such a directive
     * is included, then the process will be stopped in this call until
     * the "debugger release" notification arrives */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %s\n", myproc.nspace, myproc.rank,
                PMIx_Error_string(rc));
        exit(0);
    }
    /* get all keys provided to us */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, NULL, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get local rank failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        exit(0);
    }
    fprintf(stderr, "Returned type %s\n", PMIx_Data_type_string(val->type));
    info = (pmix_info_t *) val->data.darray->array;
    ninfo = val->data.darray->size;

    for (n = 0; n < ninfo; n++) {
        fprintf(stderr, "KEY: %s\n", info[n].key);
    }

    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_PACKAGE_RANK, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get numa rank failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        exit(0);
    }
    fprintf(stderr, "NUMA rank: %d\n", (int) val->data.uint16);

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
