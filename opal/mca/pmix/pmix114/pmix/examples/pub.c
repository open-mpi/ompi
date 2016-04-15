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

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <pmix.h>

int main(int argc, char **argv)
{
    pmix_proc_t myproc;
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;
    uint32_t nprocs;
    pmix_info_t *info;
    pmix_pdata_t *pdata;

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    /* get our universe size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d universe size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* call fence to ensure the data is received */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* publish something */
    if (0 == myproc.rank) {
        PMIX_INFO_CREATE(info, 2);
        (void)strncpy(info[0].key, "FOOBAR", PMIX_MAX_KEYLEN);
        info[0].value.type = PMIX_UINT8;
        info[0].value.data.uint8 = 1;
        (void)strncpy(info[1].key, "PANDA", PMIX_MAX_KEYLEN);
        info[1].value.type = PMIX_SIZE;
        info[1].value.data.size = 123456;
        if (PMIX_SUCCESS != (rc = PMIx_Publish(info, 2))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Publish failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        PMIX_INFO_FREE(info, 2);
    }

    /* call fence again so all procs know the data
     * has been published */
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* lookup something */
    if (0 != myproc.rank) {
        PMIX_PDATA_CREATE(pdata, 1);
        (void)strncpy(pdata[0].key, "FOOBAR", PMIX_MAX_KEYLEN);
        if (PMIX_SUCCESS != (rc = PMIx_Lookup(pdata, 1, NULL, 0))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Lookup failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        /* check the return for value and source */
        if (0 != strncmp(myproc.nspace, pdata[0].proc.nspace, PMIX_MAX_NSLEN)) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Lookup returned wrong nspace: %s\n",
                        myproc.nspace, myproc.rank, pdata[0].proc.nspace);
            goto done;
        }
        if (0 != pdata[0].proc.rank) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Lookup returned wrong rank: %d\n",
                        myproc.nspace, myproc.rank, pdata[0].proc.rank);
            goto done;
        }
        if (PMIX_UINT8 != pdata[0].value.type) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Lookup returned wrong type: %d\n",
                        myproc.nspace, myproc.rank, pdata[0].value.type);
            goto done;
        }
        if (1 != pdata[0].value.data.uint8) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Lookup returned wrong value: %d\n",
                        myproc.nspace, myproc.rank, (int)pdata[0].value.data.uint8);
            goto done;
        }
        PMIX_PDATA_FREE(pdata, 1);
        fprintf(stderr, "PUBLISH-LOOKUP SUCCEEDED\n");
    }

    /* call fence again so rank 0 waits before leaving */
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    if (0 == myproc.rank) {
        char **keys;
        keys = (char**)malloc(3 * sizeof(char*));
        keys[0] = "FOOBAR";
        keys[1] = "PANDA";
        keys[2] = NULL;

        if (PMIX_SUCCESS != (rc = PMIx_Unpublish(keys, NULL, 0))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Unpublish failed: %d\n", myproc.nspace, myproc.rank, rc);
            free(keys);
            goto done;
        }
        free(keys);
        fprintf(stderr, "UNPUBLISH SUCCEEDED\n");
    }

    /* call fence again so everyone waits for rank 0 before leaving */
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

 done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize())) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
