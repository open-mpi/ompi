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
#include <pmi.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#define MAXCNT 3

int main(int argc, char **argv)
{
    int rc, j, n;
    char *tmp;
    int spawned;
    int rank;
    int nprocs;
    char value[1024];

    fprintf(stderr, "Client calling init\n");
    if (PMI_SUCCESS != (rc = PMI_Init(&spawned))) {
        fprintf(stderr, "Client PMI_Init failed: %d\n", rc);
        exit(rc);
    }
    fprintf(stderr, "Client Running\n");

    /* test something */
    if (PMI_SUCCESS != (rc = PMI_Get_rank(&rank))) {
        fprintf(stderr, "Client PMI_Get_rank failed: %d\n", rc);
        exit(rc);
    }
    if (PMI_SUCCESS != (rc = PMI_Get_universe_size(&nprocs))) {
        fprintf(stderr, "Client %d: PMI_Get_universe_size failed: %d\n", rank, rc);
        exit(rc);
    }
    fprintf(stderr, "Client %d job size %d\n", rank, nprocs);

    for (j=0; j < 10; j++) {
        (void)asprintf(&tmp, "%d-gasnet-0-%d", rank, j);
        if (PMI_SUCCESS != (rc = PMI_KVS_Put("foobar", tmp, "myvalue"))) {
            fprintf(stderr, "Client %d: j %d PMI_KVS_Put failed: %d\n",
                    rank, j, rc);
            goto done;
        }
        free(tmp);
    }

    if (PMIX_SUCCESS != (rc = PMI_KVS_Commit("foobar"))) {
            fprintf(stderr, "Client %d: PMI_KVS_Commit failed: %d\n", rank, rc);
            goto done;
    }

    fprintf(stderr, "Client rank %d: CALLING PMI_Barrier\n", rank);

    /* call fence to ensure the data is received */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
            fprintf(stderr, "Client %d: PMI_Barrier failed: %d\n", rank, rc);
        goto done;
    }

    /* check the returned data */
    for (j=0; j < 10; j++) {
        for (n=0; n < nprocs; n++) {
            (void)asprintf(&tmp, "%d-gasnet-0-%d", n, j);
            fprintf(stderr, "Client %d: Calling get\n", rank);
            if (PMI_SUCCESS != (rc = PMI_KVS_Get("foobar", tmp, value, 1024))) {
             fprintf(stderr, "Client %d: PMI_Get failed: %d\n", rank, rc);
                continue;
            }
            if (0 == strcmp(value, "myvalue")) {
                fprintf(stderr, "Client %d: PMI_Get returned correct value\n", rank);
            } else {
                fprintf(stderr, "Client %d: PMI_Get returned incorrect value\n", rank);
            }
            free(tmp);
        }
    }

  done:
    /* finalize us */
    fprintf(stderr, "Client rank %d: Finalizing\n", rank);
    if (PMI_SUCCESS != (rc = PMI_Finalize())) {
        fprintf(stderr, "Client rank %d: finalize failed %d\n", rank, rc);
    } else {
        fprintf(stderr, "Client %d:PMI_Finalize successfully completed\n", rank);
    }
    fflush(stderr);
    return(rc);
}
