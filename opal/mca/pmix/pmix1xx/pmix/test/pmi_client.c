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
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include "pmi.h"


int main(int argc, char **argv)
{
    int spawned;
    int rc;

    /* init us */
    if (PMI_SUCCESS != (rc = PMI_Init(&spawned))) {
        fprintf(stderr, "PMI_Init failed: %d\n", rc);
        return rc;
    }
#if 0
    key = "local-key";
    PMIX_VAL_SET(&value, int, 12345, rc, kvp_error );
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, key, &value))) {
        fprintf(stderr, "PMIx_Put failed: %d\n", rc);
    }

    key = "remote-key";
    char *ptr = "Test string";
    PMIX_VAL_SET(&value, string, ptr, rc, kvp_error );
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_REMOTE, key, &value))) {
        fprintf(stderr, "PMIx_Put failed: %d\n", rc);
    }

    key = "global-key";
    PMIX_VAL_SET(&value, float, 10.15, rc, kvp_error );
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, key, &value))) {
        fprintf(stderr, "PMIx_Put failed: %d\n", rc);
    }

    /* Submit the data */
    pmix_range_t range;
    range.ranks = NULL;
    range.nranks = 0;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(NULL, 0))) {
        fprintf(stderr, "PMIx_Fence failed: %d\n", rc);
        return rc;
    }
#endif
    /* finalize us */
    if (PMI_SUCCESS != (rc = PMI_Finalize())) {
        fprintf(stderr, "PMI_Finalize failed: %d\n", rc);
    }
    
    return rc;
}
