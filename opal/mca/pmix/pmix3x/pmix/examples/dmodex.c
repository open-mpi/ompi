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
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "src/include/pmix_config.h"
#include "../include/pmix.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

static uint32_t nprocs;
static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = NULL;
    char *tmp;
    pmix_proc_t proc;
    uint32_t n, k, nlocal;
    bool local, all_local = false;
    char **peers;
    pmix_rank_t *locals = NULL;
    uint8_t j;

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    /* get our job size */
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %s\n",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d job size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* put a few values */
    (void)asprintf(&tmp, "%s-%d-internal", myproc.nspace, myproc.rank);
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&myproc, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Store_internal failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    (void)asprintf(&tmp, "%s-%d-local", myproc.nspace, myproc.rank);
    value.type = PMIX_UINT64;
    value.data.uint64 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put internal failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    (void)asprintf(&tmp, "%s-%d-remote", myproc.nspace, myproc.rank);
    value.type = PMIX_STRING;
    value.data.string = "1234";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put internal failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    value.type = PMIX_BYTE_OBJECT;
    value.data.bo.bytes = (char*)malloc(128);
    for (j=0; j < 128; j++) {
        value.data.bo.bytes[j] = j;
    }
    value.data.bo.size = 128;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, "ghex", &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put ghex failed: %d\n", myproc.nspace, myproc.rank, rc);
        PMIX_VALUE_DESTRUCT(&value);
        goto done;
    }
    PMIX_VALUE_DESTRUCT(&value);

    /* commit the data to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Commit failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* get a list of our local peers */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_PEERS, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get local peers failed: %s\n",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    /* split the returned string to get the rank of each local peer */
    peers = pmix_argv_split(val->data.string, ',');
    PMIX_VALUE_RELEASE(val);
    nlocal = pmix_argv_count(peers);
    if (nprocs == nlocal) {
        all_local = true;
    } else {
        all_local = false;
        locals = (pmix_rank_t*)malloc(pmix_argv_count(peers) * sizeof(pmix_rank_t));
        for (n=0; NULL != peers[n]; n++) {
            locals[n] = strtoul(peers[n], NULL, 10);
        }
    }
    pmix_argv_free(peers);

    /* get the committed data - ask for someone who doesn't exist as well */
    for (n=0; n < nprocs; n++) {
        if (all_local) {
            local = true;
        } else {
            local = false;
            /* see if this proc is local to us */
            for (k=0; k < nlocal; k++) {
                if (proc.rank == locals[k]) {
                    local = true;
                    break;
                }
            }
        }
        if (local) {
            if( 0 > asprintf(&tmp, "%s-%d-local", myproc.nspace, n)) {
                exit(1);
            }
            proc.rank = n;
            if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d\n", myproc.nspace, n, tmp, rc);
                free(tmp);
                goto done;
            }
            if (PMIX_UINT64 != val->type) {
                fprintf(stderr, "%s:%d: PMIx_Get Key %s returned wrong type: %d\n", myproc.nspace, myproc.rank, tmp, val->type);
                PMIX_VALUE_RELEASE(val);
                free(tmp);
                goto done;
            }
            if (1234 != val->data.uint64) {
                fprintf(stderr, "%s:%d: PMIx_Get Key %s returned wrong value: %d\n", myproc.nspace, myproc.rank, tmp, (int)val->data.uint64);
                PMIX_VALUE_RELEASE(val);
                free(tmp);
                goto done;
            }
            fprintf(stderr, "%s:%d Local value for %s:%d successfully retrieved\n", myproc.nspace, myproc.rank, proc.nspace, proc.rank);
            PMIX_VALUE_RELEASE(val);
                free(tmp);
        } else {
            if( 0 > asprintf(&tmp, "%s-%d-remote", myproc.nspace, n)) {
                exit(1);
            }
            if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d\n", myproc.nspace, n, tmp, rc);
                free(tmp);
                goto done;
            }
            if (PMIX_STRING != val->type) {
                fprintf(stderr, "%s:%d: PMIx_Get Key %s returned wrong type: %d\n", myproc.nspace, myproc.rank, tmp, val->type);
                PMIX_VALUE_RELEASE(val);
                free(tmp);
                goto done;
            }
            if (0 != strcmp(val->data.string, "1234")) {
                fprintf(stderr, "%s:%d: PMIx_Get Key %s returned wrong value: %s\n", myproc.nspace, myproc.rank, tmp, val->data.string);
                PMIX_VALUE_RELEASE(val);
                free(tmp);
                goto done;
            }
            fprintf(stderr, "%s:%d Remote value for %s:%d successfully retrieved\n", myproc.nspace, myproc.rank, proc.nspace, proc.rank);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
        }
        /* if this isn't us, then get the ghex key */
        if (n != myproc.rank) {
            if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, "ghex", NULL, 0, &val))) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get ghex failed: %d\n", myproc.nspace, n, rc);
                goto done;
            }
            if (PMIX_BYTE_OBJECT != val->type) {
                fprintf(stderr, "%s:%d: PMIx_Get ghex returned wrong type: %d\n", myproc.nspace, myproc.rank, val->type);
                PMIX_VALUE_RELEASE(val);
                goto done;
            }
            if (128 != val->data.bo.size) {
                fprintf(stderr, "%s:%d: PMIx_Get ghex returned wrong size: %d\n", myproc.nspace, myproc.rank, (int)val->data.bo.size);
                PMIX_VALUE_RELEASE(val);
                goto done;
            }
            fprintf(stderr, "%s:%d Ghex for %s:%d successfully retrieved\n", myproc.nspace, myproc.rank, proc.nspace, proc.rank);
            PMIX_VALUE_RELEASE(val);
        }
    }

    /* call fence so everyone waits before leaving */
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

 done:
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
