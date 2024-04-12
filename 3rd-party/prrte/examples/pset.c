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
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <pmix.h>

int main(int argc, char **argv)
{
    int rc;
    size_t n;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc, *pptr;
    pid_t pid;
    pmix_proc_t myproc;

    pid = getpid();

    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d pid %lu: Running\n", myproc.nspace, myproc.rank,
            (unsigned long) pid);

    /* get our pset name */
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_PSET_NAME, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get pset name failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    fprintf(stderr, "Client %s:%d pset name %s\n",
            myproc.nspace, myproc.rank, val->data.string);
    PMIX_VALUE_FREE(val, 1);

    /* since this is our pset, get our membership */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_PSET_MEMBERS, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get of pset members failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    /* must return a pmix_data_array_t of members */
    if (PMIX_DATA_ARRAY != val->type) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get of pset members returned incorrect data type: %s\n",
                myproc.nspace, myproc.rank, PMIx_Data_type_string(val->type));
        goto done;
    }
    fprintf(stderr, "Client %s:%d PMIx_Get returned %zd members\n", myproc.nspace, myproc.rank,
            val->data.darray->size);
    pptr = (pmix_proc_t*)val->data.darray->array;
    for (n=0; n < val->data.darray->size; n++) {
        fprintf(stderr, "\t%s:%d\n", pptr[n].nspace, pptr[n].rank);
    }
    PMIX_VALUE_FREE(val, 1);

done:
    return (rc);
}
