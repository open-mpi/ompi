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
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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
#include <sys/param.h>
#include <time.h>
#include <unistd.h>

#include "examples.h"
#include <pmix.h>

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    int rc, exitcode;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;
    uint32_t nprocs;
    char nsp2[PMIX_MAX_NSLEN + 1];
    pmix_app_t *app;
    char hostname[1024], dir[1024];
    pmix_proc_t *peers;
    size_t npeers, ntmp = 0;
    char *nodelist;
    char *cmd;

    if (0 > gethostname(hostname, sizeof(hostname))) {
        exit(1);
    }
    if (NULL == getcwd(dir, 1024)) {
        exit(1);
    }

    if (1 < argc) {
        if (0 == strcmp(argv[1], "fail")) {
            cmd = "client-does-not-exist";
        } else {
            fprintf(stderr, "usage: dynamic [fail]\n\tSpecify fail if you want the spawn command to fail\n");
            exit(1);
        }
    } else {
        cmd = "client";
    }

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        exit(rc);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    PMIX_PROC_CONSTRUCT(&proc);
    (void) strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        exitcode = rc;
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d universe size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* call fence to sync */
    (void) strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        exitcode = rc;
        goto done;
    }

    /* rank=0 calls spawn */
    if (0 == myproc.rank) {
        PMIX_APP_CREATE(app, 1);
        if (0 > asprintf(&app->cmd, "%s/%s", dir, cmd)) {
            exitcode = 1;
            goto done;
        }
        app->maxprocs = 2;
        PMIX_ARGV_APPEND(rc, app->argv, app->cmd);
        PMIX_ARGV_APPEND(rc, app->env, "PMIX_ENV_VALUE=3");

        fprintf(stderr, "Client ns %s rank %d: calling PMIx_Spawn\n", myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != (rc = PMIx_Spawn(NULL, 0, app, 1, nsp2))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Spawn failed: %s(%d)\n", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc), rc);
            exitcode = rc;
            /* terminate our peers */
            PMIx_Abort(rc, "FAILED TO START CHILD JOB", &proc, 1);
            goto done;
        } else {
          fprintf(stderr, "Spawn success.\n");
        }
        PMIX_APP_FREE(app, 1);

        /* get their job size */
        val = NULL;
        (void) strncpy(proc.nspace, nsp2, PMIX_MAX_NSLEN);
        proc.rank = PMIX_RANK_WILDCARD;
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val)) || NULL == val) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %d\n", myproc.nspace,
                    myproc.rank, rc);
            goto done;
        }
        ntmp = val->data.uint32;
        PMIX_VALUE_RELEASE(val);
        fprintf(stderr, "Client %s:%d universe %s size %d\n", myproc.nspace, myproc.rank, nsp2,
                (int) ntmp);
    }

    /* just cycle the connect/disconnect functions */
    (void) strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Connect(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Connect failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        exitcode = rc;
        goto done;
    }
    fprintf(stderr, "Client ns %s rank %d: PMIx_Connect succeeded\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Disconnect(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Disonnect failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        exitcode = rc;
        goto done;
    }
    fprintf(stderr, "Client ns %s rank %d: PMIx_Disconnect succeeded\n", myproc.nspace,
            myproc.rank);

    /* finally, test the resolve functions */
    if (0 == myproc.rank) {
        if (PMIX_SUCCESS != (rc = PMIx_Resolve_peers(hostname, NULL, &peers, &npeers))) {
            fprintf(stderr,
                    "Client ns %s rank %d: PMIx_Resolve_peers failed for nspace %s: %s(%d)\n",
                    myproc.nspace, myproc.rank, nsp2, PMIx_Error_string(rc), rc);
            exitcode = rc;
            goto done;
        }
        if ((nprocs + ntmp) != npeers) {
            fprintf(stderr,
                    "Client ns %s rank %d: PMIx_Resolve_peers returned incorrect npeers: %d vs %d\n",
                    myproc.nspace, myproc.rank, (int) (nprocs + ntmp), (int) npeers);
            exitcode = 1;
            goto done;
        }
        fprintf(stderr, "Client ns %s rank %d: PMIx_Resolve_peers returned %d npeers\n",
                myproc.nspace, myproc.rank, (int) npeers);
        if (PMIX_SUCCESS != (rc = PMIx_Resolve_nodes(nsp2, &nodelist))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Resolve_nodes failed for nspace %s: %d\n",
                    myproc.nspace, myproc.rank, nsp2, rc);
            exitcode = rc;
            goto done;
        }
        fprintf(stderr, "Client ns %s rank %d: PMIx_Resolve_nodes %s\n", myproc.nspace, myproc.rank,
                nodelist);
    } else {
        if (PMIX_SUCCESS != (rc = PMIx_Resolve_peers(hostname, myproc.nspace, &peers, &npeers))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Resolve_peers failed for nspace %s: %d\n",
                    myproc.nspace, myproc.rank, myproc.nspace, rc);
            exitcode = rc;
            goto done;
        }
        if (nprocs != npeers) {
            fprintf(stderr,
                    "Client ns %s rank %d: PMIx_Resolve_peers returned incorrect npeers: %d vs %d\n",
                    myproc.nspace, myproc.rank, nprocs, (int) npeers);
            exitcode = rc;
            goto done;
        }
        fprintf(stderr, "Client ns %s rank %d: PMIx_Resolve_peers returned %d npeers\n",
                myproc.nspace, myproc.rank, (int) npeers);
        if (PMIX_SUCCESS != (rc = PMIx_Resolve_nodes(NULL, &nodelist))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Resolve_nodes failed: %d\n", myproc.nspace,
                    myproc.rank, rc);
            exitcode = rc;
            goto done;
        }
        fprintf(stderr, "Client ns %s rank %d: PMIx_Resolve_nodes %s\n", myproc.nspace, myproc.rank,
                nodelist);
    }
    PMIX_PROC_FREE(peers, npeers);
    free(nodelist);

done:
    /* call fence to sync */
    (void) strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        return(rc);
    }

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
    printf("exit\n");
    return (exitcode);
}
