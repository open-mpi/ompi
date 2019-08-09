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
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include <sys/param.h>

#include <pmix.h>
#include "examples.h"

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;
    uint32_t nprocs;
    char nsp2[PMIX_MAX_NSLEN+1];
    pmix_app_t *app;
    char hostname[1024], dir[1024];
    size_t ntmp=0;

    if (0 > gethostname(hostname, sizeof(hostname))) {
        exit(1);
    }
    if (NULL == getcwd(dir, 1024)) {
        exit(1);
    }

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d job size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* call fence to sync */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* rank=0 calls spawn */
    if (0 == myproc.rank) {
        PMIX_APP_CREATE(app, 1);
        if (0 > asprintf(&app->cmd, "%s/client", dir)) {
            exit(1);
        }
        app->maxprocs = 2;
        app->argv = (char**)malloc(2 * sizeof(char*));
        if (0 > asprintf(&app->argv[0], "%s/client", dir)) {
            exit(1);
        }
        app->argv[1] = NULL;
        app->env = (char**)malloc(2 * sizeof(char*));
        app->env[0] = strdup("PMIX_ENV_VALUE=3");
        app->env[1] = NULL;

        fprintf(stderr, "Client ns %s rank %d: calling PMIx_Spawn\n", myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != (rc = PMIx_Spawn(NULL, 0, app, 1, nsp2))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Spawn failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        PMIX_APP_FREE(app, 1);

        /* get their universe size */
        val = NULL;
        (void)strncpy(proc.nspace, nsp2, PMIX_MAX_NSLEN);
        proc.rank = PMIX_RANK_WILDCARD;
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val)) ||
            NULL == val) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        ntmp = val->data.uint32;
        PMIX_VALUE_RELEASE(val);
        fprintf(stderr, "Client %s:%d job %s size %d\n", myproc.nspace, myproc.rank, nsp2, (int)ntmp);

        /* get a proc-specific value */
        val = NULL;
        (void)strncpy(proc.nspace, nsp2, PMIX_MAX_NSLEN);
        proc.rank = 1;
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_RANK, NULL, 0, &val)) ||
            NULL == val) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get local rank failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        ntmp = (int)val->data.uint16;
        PMIX_VALUE_RELEASE(val);
        fprintf(stderr, "Client %s:%d job %s local rank %d\n", myproc.nspace, myproc.rank, nsp2, (int)ntmp);
    }

 done:
    /* call fence to sync */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
