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
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
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
#include <time.h>

#include <pmi.h>

int main(int argc, char **argv)
{
    int rc, spawned;
    char *tmp;
    int nprocs, rank;
    bool flag;

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes any
     * debugger flag instructing us to stop-in-init. If such a directive
     * is included, then the process will be stopped in this call until
     * the "debugger release" notification arrives */
    if (PMI_SUCCESS != (rc = PMI_Init(&spawned))) {
        fprintf(stderr, "Client: PMI_Init failed: %d\n", rc);
        exit(0);
    }
    fprintf(stderr, "Client: Running\n");


    /* check to see if we have been instructed to wait for a debugger
     * to attach to us. We won't get both a stop-in-init AND a
     * wait-for-notify directive, so we should never stop twice. This
     * directive is provided so that something like an MPI implementation
     * can do some initial setup in MPI_Init prior to pausing for the
     * debugger */
    if (PMI_SUCCESS != (rc = PMI_Get_size(&nprocs))) {
        fprintf(stderr, "PMI_Get_size failed: %d\n", rc);
        exit(1);
    }

    /* get our universe size */
    if (PMI_SUCCESS != (rc = PMI_Get_rank(&rank))) {
        fprintf(stderr, "PMI_Get_rank failed: %d\n", rc);
        exit(1);
    }

    /* finalize us */
    fprintf(stderr, "Client %d: Finalizing\n", rank);
    if (PMI_SUCCESS != (rc = PMI_Finalize())) {
        fprintf(stderr, "Client %d: PMI_Finalize failed: %d\n", rank, rc);
    }
    fflush(stderr);
    return(0);
}
