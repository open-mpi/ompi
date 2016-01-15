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

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc, myproc;
    uint32_t nprocs, n;

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %d", myproc.nspace, myproc.rank, rc);
        exit(rc);
    }
    pmix_output(0, "Client ns %s rank %d: Running", myproc.nspace, myproc.rank);

    /* get our universe size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get universe size failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d universe size %d", myproc.nspace, myproc.rank, nprocs);

    /* put a few values */
    (void)asprintf(&tmp, "%s-%d-internal", myproc.nspace, myproc.rank);
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&myproc, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Store_internal failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    (void)asprintf(&tmp, "%s-%d-local", myproc.nspace, myproc.rank);
    value.type = PMIX_UINT64;
    value.data.uint64 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    (void)asprintf(&tmp, "%s-%d-remote", myproc.nspace, myproc.rank);
    value.type = PMIX_STRING;
    value.data.string = "1234";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_REMOTE, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Commit failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* call fence to ensure the data is received */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Fence failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* check the returned data */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    for (n=0; n < nprocs; n++) {
        proc.rank = n;
        (void)asprintf(&tmp, "%s-%d-local", myproc.nspace, n);
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s failed: %d", myproc.nspace, myproc.rank, tmp, rc);
            goto done;
        }
        if (PMIX_UINT64 != val->type) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d", myproc.nspace, myproc.rank, tmp, val->type);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
            goto done;
        }
        if (1234 != val->data.uint64) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned wrong value: %d", myproc.nspace, myproc.rank, tmp, (int)val->data.uint64);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
            goto done;
        }
        pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned correct", myproc.nspace, myproc.rank, tmp);
        PMIX_VALUE_RELEASE(val);
        free(tmp);

        (void)asprintf(&tmp, "%s-%d-remote", proc.nspace, n);
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s failed: %d", myproc.nspace, myproc.rank, tmp, rc);
            goto done;
        }
        if (PMIX_STRING != val->type) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d", myproc.nspace, myproc.rank, tmp, val->type);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
            goto done;
        }
        if (0 != strcmp(val->data.string, "1234")) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned wrong value: %s", myproc.nspace, myproc.rank, tmp, val->data.string);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
            goto done;
        }
        pmix_output(0, "Client ns %s rank %d: PMIx_Get %s returned correct", myproc.nspace, myproc.rank, tmp);
        PMIX_VALUE_RELEASE(val);
        free(tmp);
    }

 done:
    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize())) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(rc);
}
