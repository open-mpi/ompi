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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <private/autogen/config.h>
#include <pmix.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "src/class/pmix_object.h"
#include "src/buffer_ops/types.h"
#include "src/util/output.h"
#include "src/util/printf.h"

static uint32_t nprocs;
static char nspace[PMIX_MAX_NSLEN+1];
static int rank;
static uint32_t getcount = 0;

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    bool *release = (bool*)cbdata;

    pmix_output(0, "%s:%d completed fence_nb", nspace, rank);
    *release = true;
}

static void valcbfunc(pmix_status_t status,
                      pmix_value_t *val, void *cbdata)
{
    char *key = (char*)cbdata;

    if (PMIX_SUCCESS == status) {
        if (NULL != strstr(key, "local")) {
            if (PMIX_UINT64 != val->type) {
                pmix_output(0, "%s:%d: PMIx_Get_nb Key %s returned wrong type: %d", nspace, rank, key, val->type);
                goto done;
            }
            if (1234 != val->data.uint64) {
                pmix_output(0, "%s:%d: PMIx_Get_nb Key %s returned wrong value: %d", nspace, rank, key, (int)val->data.uint64);
                goto done;
            }
        } else if (NULL != strstr(key, "remote")) {
            if (PMIX_STRING != val->type) {
                pmix_output(0, "%s:%d: PMIx_Get_nb Key %s returned wrong type: %d", nspace, rank, key, val->type);
                goto done;
            }
            if (0 != strcmp(val->data.string, "1234")) {
                pmix_output(0, "%s:%d: PMIx_Get_nb Key %s returned wrong value: %s", nspace, rank, key, val->data.string);
                goto done;
            }
        } else {
            pmix_output(0, "%s:%d PMIx_Get_nb returned wrong key: %s", nspace, rank, key);
            goto done;
        }
        pmix_output(0, "%s:%d PMIx_Get_nb Key %s returned correctly", nspace, rank, key);
    } else {
        pmix_output(0, "%s:%d PMIx_Get_nb Key %s failed", nspace, rank, key);
    }
 done:
    free(key);
    getcount++;
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    uint32_t n, num_gets;
    bool completed;
    
    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(nspace, &rank))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %d", nspace, rank, rc);
        exit(0);
    }
    pmix_output(0, "Client ns %s rank %d: Running", nspace, rank);

    /* get our universe size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(nspace, rank, PMIX_UNIV_SIZE, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get universe size failed: %d", nspace, rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d universe size %d", nspace, rank, nprocs);
    
    /* put a few values */
    (void)asprintf(&tmp, "%s-%d-internal", nspace, rank);
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_INTERNAL, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %d", nspace, rank, rc);
        goto done;
    }

    (void)asprintf(&tmp, "%s-%d-local", nspace, rank);
    value.type = PMIX_UINT64;
    value.data.uint64 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %d", nspace, rank, rc);
        goto done;
    }

    (void)asprintf(&tmp, "%s-%d-remote", nspace, rank);
    value.type = PMIX_STRING;
    value.data.string = "1234";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_REMOTE, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %d", nspace, rank, rc);
        goto done;
    }

    /* introduce a delay by one rank so we can check what happens
     * if a "get" is received prior to data being provided */
    if (0 == rank) {
        sleep(2);
    }

    /* commit the data to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Commit failed: %d", nspace, rank, rc);
        goto done;
    }

    /* call fence_nb, but don't return any data */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    completed = false;
    if (PMIX_SUCCESS != (rc = PMIx_Fence_nb(&proc, 1, false, opcbfunc, &completed))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Fence failed: %d", nspace, rank, rc);
        goto done;
    }
    
    /* get the committed data - ask for someone who doesn't exist as well */
    num_gets = 0;
    for (n=0; n <= nprocs; n++) {
        (void)asprintf(&tmp, "%s-%d-local", nspace, n);
        if (PMIX_SUCCESS != (rc = PMIx_Get_nb(nspace, n, tmp,
                                              valcbfunc, tmp))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s failed: %d", nspace, n, tmp, rc);
            goto done;
        }
        ++num_gets;
        (void)asprintf(&tmp, "%s-%d-remote", nspace, n);
        if (PMIX_SUCCESS != (rc = PMIx_Get_nb(nspace, n, tmp,
                                              valcbfunc, tmp))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s failed: %d", nspace, n, tmp, rc);
            goto done;
        }
        ++num_gets;
    }

    /* wait for the first fence to finish */
    while (!completed) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }

    /* wait for all my "get" calls to complete */
    while (getcount < num_gets) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }

    /* call fence again so everyone waits before leaving */
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, false))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Fence failed: %d", nspace, rank, rc);
        goto done;
    }

 done:
    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing", nspace, rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize())) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", nspace, rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", nspace, rank);
    }
    fflush(stderr);
    return(0);
}
