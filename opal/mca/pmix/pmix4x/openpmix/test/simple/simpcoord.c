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
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "src/util/output.h"
#include "src/util/printf.h"

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    uint32_t nprocs, n, *u32;
    size_t ninfo, m;
    pmix_coord_t *coords;
    char *hostname;

    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }
    pmix_output(0, "Client ns %s rank %d: Running", myproc.nspace, myproc.rank);

    /* test something */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }
    PMIX_VALUE_RELEASE(val);

    /* get our job size */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get job size failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d job size %d", myproc.nspace, myproc.rank, nprocs);

    /* get our assumed hostname */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_HOSTNAME, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get hostname failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    hostname = strdup(val->data.string);
    PMIX_VALUE_RELEASE(val);

    /* get our assigned network endpts */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_NETWORK_ENDPT, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get network endpt failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    pmix_output(0, "Client %s:%d was assigned %lu endpts",
                myproc.nspace, myproc.rank,
                (unsigned long)val->data.darray->size);

    u32 = (uint32_t*)val->data.darray->array;
    ninfo = val->data.darray->size;
    /* print them out for diagnostics - someday we can figure
     * out an automated way of testing the answer */
    {
        char **foo = NULL;
        for (n=0; n < ninfo; n++) {
            asprintf(&tmp, "%d", u32[n]);
            pmix_argv_append_nosize(&foo, tmp);
            free(tmp);
        }
        tmp = pmix_argv_join(foo, ',');
        pmix_argv_free(foo);
        pmix_output(0, "Rank %u[%s]: ASSIGNED ENDPTS: %s", myproc.rank, hostname, tmp);
        free(tmp);
    }

    /* get our assigned network coordinates */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_NETWORK_COORDINATE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get network endpt failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    coords = (pmix_coord_t*)val->data.darray->array;
    ninfo = val->data.darray->size;
    /* print them out for diagnostics - someday we can figure
     * out an automated way of testing the answer */
    for (m=0; m < ninfo; m++) {
        char **foo = NULL;
        char *view;
        for (n=0; n < coords[m].dims; n++) {
            asprintf(&tmp, "%d", coords[m].coord[n]);
            pmix_argv_append_nosize(&foo, tmp);
            free(tmp);
        }
        tmp = pmix_argv_join(foo, ',');
        pmix_argv_free(foo);
        if (PMIX_COORD_LOGICAL_VIEW == coords[m].view) {
            view = "LOGICAL";
        } else {
            view = "PHYSICAL";
        }
        pmix_output(0, "Rank %u[%s]: COORD[%d] VIEW %s: %s", myproc.rank, hostname, (int)m, view, tmp);
        free(tmp);
    }

 done:
    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(rc);
}
