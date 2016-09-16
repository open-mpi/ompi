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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <pmix_tool.h>

static void cbfunc(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;

    /* do something with the returned info - it will be
     * released in the release_fn */

    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    *active = false;
}

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_proc_t myproc;
    pmix_query_t *query;
    size_t nq;
    volatile bool active;

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, NULL, 0))) {
        fprintf(stderr, "PMIx_tool_init failed: %d\n", rc);
        exit(rc);
    }

    /* query something */
    nq = 2;
    PMIX_QUERY_CREATE(query, nq);
    query[0].keys = (char**)malloc(2 * sizeof(char*));
    query[0].keys[0] = strdup("foobar");
    query[0].keys[1] = NULL;
    query[1].keys = (char**)malloc(2 * sizeof(char*));
    query[1].keys[0] = strdup("spastic");
    query[1].keys[1] = NULL;
    active = true;
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&active))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    while(active) {
        usleep(10);
    }

 done:
    /* finalize us */
    PMIx_Finalize(NULL, 0);
    return(rc);
}
