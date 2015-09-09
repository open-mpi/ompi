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

#include "pmi2.h"


int main(int argc, char **argv)
{
    int spawned, size, rank, appnum;
    int rc;
    char *key;
    
    /* init us */
    if (PMI2_SUCCESS != (rc = PMI2_Init(&spawned, &size, &rank, &appnum))) {
        fprintf(stderr, "PMI2_Init failed: %d\n", rc);
        return rc;
    }

    key = "local-key";
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Put(key, "my-local-value"))) {
        fprintf(stderr, "PMI2_Put failed: %d\n", rc);
    }

    key = "remote-key";
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Put(key, "remote-value"))) {
        fprintf(stderr, "PMI2_Put failed: %d\n", rc);
    }

    key = "global-key";
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Put(key, "global-value"))) {
        fprintf(stderr, "PMI2_Put failed: %d\n", rc);
    }

    /* Submit the data */
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
        fprintf(stderr, "PMI2_Fence failed: %d\n", rc);
        return rc;
    }

    /* finalize us */
    if (PMI2_SUCCESS != (rc = PMI2_Finalize())) {
        fprintf(stderr, "PMI2_Finalize failed: %d\n", rc);
        return rc;
    }
    
    return 0;
}
