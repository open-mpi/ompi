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
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <src/include/pmix_config.h>
#include <pmix.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "src/class/pmix_object.h"
#include "src/buffer_ops/types.h"
#include "src/util/argv.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/util/printf.h"

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
    char hostname[PMIX_MAXHOSTNAMELEN];
    pmix_proc_t *peers;
    size_t npeers, ntmp=0;
    char *nodelist;

    gethostname(hostname, sizeof(hostname));

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %d", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    pmix_output(0, "Client ns %s rank %d: Running", myproc.nspace, myproc.rank);

    /* get our universe size */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get universe size failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d universe size %d", myproc.nspace, myproc.rank, nprocs);

    /* call fence to sync */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Fence failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* rank=0 calls spawn */
    if (0 == myproc.rank) {
        PMIX_APP_CREATE(app, 1);
        app->cmd = strdup("gumby");
        app->maxprocs = 2;
        pmix_argv_append(&app->argc, &app->argv, "gumby");
        pmix_argv_append(&app->argc, &app->argv, "-n");
        pmix_argv_append(&app->argc, &app->argv, "2");
        pmix_setenv("PMIX_ENV_VALUE", "3", true, &app->env);
        PMIX_INFO_CREATE(app->info, 2);
        (void)strncpy(app->info[0].key, "DARTH", PMIX_MAX_KEYLEN);
        app->info[0].value.type = PMIX_INT8;
        app->info[0].value.data.int8 = 12;
        (void)strncpy(app->info[1].key, "VADER", PMIX_MAX_KEYLEN);
        app->info[1].value.type = PMIX_DOUBLE;
        app->info[1].value.data.dval = 12.34;

        pmix_output(0, "Client ns %s rank %d: calling PMIx_Spawn", myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != (rc = PMIx_Spawn(NULL, 0, app, 1, nsp2))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Spawn failed: %d", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        PMIX_APP_FREE(app, 1);

        /* check to see if we got the expected info back */
        if (0 != strncmp(nsp2, "DYNSPACE", PMIX_MAX_NSLEN)) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Spawn returned incorrect nspace: %s", myproc.nspace, myproc.rank, nsp2);
            goto done;
        } else {
            pmix_output(0, "Client ns %s rank %d: PMIx_Spawn succeeded returning nspace: %s", myproc.nspace, myproc.rank, nsp2);
        }
        /* get their universe size */
        (void)strncpy(proc.nspace, nsp2, PMIX_MAX_NSLEN);
        proc.rank = PMIX_RANK_WILDCARD;
        val = NULL;
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val)) ||
            NULL == val) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get universe size failed: %d", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        ntmp = val->data.uint32;
        PMIX_VALUE_RELEASE(val);
        pmix_output(0, "Client %s:%d universe %s size %d", myproc.nspace, myproc.rank, nsp2, (int)ntmp);
    }

    /* just cycle the connect/disconnect functions */
    if (PMIX_SUCCESS != (rc = PMIx_Connect(&proc, 1, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Connect failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    pmix_output(0, "Client ns %s rank %d: PMIx_Connect succeeded", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Disconnect(&proc, 1, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Disonnect failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    pmix_output(0, "Client ns %s rank %d: PMIx_Disconnect succeeded", myproc.nspace, myproc.rank);

    /* finally, test the resolve functions */
    if (0 == myproc.rank) {
        if (PMIX_SUCCESS != (rc = PMIx_Resolve_peers(hostname, NULL, &peers, &npeers))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_peers failed for nspace %s: %d", myproc.nspace, myproc.rank, nsp2, rc);
            goto done;
        }
        if ((nprocs+ntmp) != npeers) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_peers returned incorrect npeers: %d vs %d", myproc.nspace, myproc.rank, (int)(nprocs+ntmp), (int)npeers);
            goto done;
        }
        pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_peers returned %d npeers", myproc.nspace, myproc.rank, (int)npeers);
        if (PMIX_SUCCESS != (rc = PMIx_Resolve_nodes(nsp2, &nodelist))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_nodes failed for nspace %s: %d", myproc.nspace, myproc.rank, nsp2, rc);
            goto done;
        }
        pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_nodes %s", myproc.nspace, myproc.rank, nodelist);
    } else {
        if (PMIX_SUCCESS != (rc = PMIx_Resolve_peers(hostname, myproc.nspace, &peers, &npeers))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_peers failed for nspace %s: %d", myproc.nspace, myproc.rank, myproc.nspace, rc);
            goto done;
        }
        if (nprocs != npeers) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_peers returned incorrect npeers: %d vs %d", myproc.nspace, myproc.rank, nprocs, (int)npeers);
            goto done;
        }
        pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_peers returned %d npeers", myproc.nspace, myproc.rank, (int)npeers);
        if (PMIX_SUCCESS != (rc = PMIx_Resolve_nodes(myproc.nspace, &nodelist))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_nodes failed: %d", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        pmix_output(0, "Client ns %s rank %d: PMIx_Resolve_nodes %s", myproc.nspace, myproc.rank, nodelist);
    }
    PMIX_PROC_FREE(peers, npeers);
    free(nodelist);

 done:
    /* call fence to sync */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Fence failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank);

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
