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

#include <pmix/autogen/config.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <pmix.h>

int main(int argc, char **argv)
{
    char nspace[PMIX_MAX_NSLEN+1];
    int rank;
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    uint32_t nprocs, n;
    
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

    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Commit failed: %d", nspace, rank, rc);
        goto done;
    }

    /* call fence to ensure the data is received */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, true))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Fence failed: %d", nspace, rank, rc);
        goto done;
    }
    
    /* check the returned data */
    for (n=0; n < nprocs; n++) {
        (void)asprintf(&tmp, "%s-%d-local", nspace, rank);
        if (PMIX_SUCCESS != (rc = PMIx_Get(nspace, rank, tmp, &val))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s failed: %d", nspace, rank, tmp, rc);
            goto done;
        }
        if (PMIX_UINT64 != val->type) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d", nspace, rank, tmp, val->type);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
            goto done;
        }
        if (1234 != val->data.uint64) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned wrong value: %d", nspace, rank, tmp, (int)val->data.uint64);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
            goto done;
        }
        pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned correct", nspace, rank, tmp);
        PMIX_VALUE_RELEASE(val);
        free(tmp);
        (void)asprintf(&tmp, "%s-%d-remote", nspace, rank);
        if (PMIX_SUCCESS != (rc = PMIx_Get(nspace, rank, tmp, &val))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s failed: %d", nspace, rank, tmp, rc);
            goto done;
        }
        if (PMIX_STRING != val->type) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d", nspace, rank, tmp, val->type);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
            goto done;
        }
        if (0 != strcmp(val->data.string, "1234")) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned wrong value: %s", nspace, rank, tmp, val->data.string);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
            goto done;
        }
        pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned correct", nspace, rank, tmp);
        PMIX_VALUE_RELEASE(val);
        free(tmp);
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
