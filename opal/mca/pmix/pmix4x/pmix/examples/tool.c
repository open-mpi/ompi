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
#include "examples.h"

static void cbfunc(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    myquery_data_t *mydata = (myquery_data_t*)cbdata;

    /* do something with the returned info - it will be
     * released in the release_fn */
    fprintf(stderr, "Query returned %s\n", PMIx_Error_string(status));

    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    DEBUG_WAKEUP_THREAD(&mydata->lock);
}

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_proc_t myproc;
    pmix_query_t *query;
    size_t nq;
    myquery_data_t mydata;
    pmix_info_t info;

    if (argc != 2) {
        fprintf(stderr, "Must provide server URI as argument\n");
        exit(1);
    }

    PMIX_INFO_LOAD(&info, PMIX_SERVER_URI, argv[1], PMIX_STRING);
    fprintf(stderr, "Connecting to %s\n", argv[1]);

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, &info, 1))) {
        fprintf(stderr, "PMIx_tool_init failed: %d\n", rc);
        exit(rc);
    }
    fprintf(stderr, "Connected\n");

    /* query something */
    nq = 2;
    PMIX_QUERY_CREATE(query, nq);
    query[0].keys = (char**)malloc(2 * sizeof(char*));
    query[0].keys[0] = strdup("foobar");
    query[0].keys[1] = NULL;
    query[1].keys = (char**)malloc(2 * sizeof(char*));
    query[1].keys[0] = strdup("spastic");
    query[1].keys[1] = NULL;
    DEBUG_CONSTRUCT_MYQUERY(&mydata);
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&mydata))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    DEBUG_WAIT_THREAD(&mydata.lock);

 done:
    /* finalize us */
    PMIx_Finalize(NULL, 0);
    return(rc);
}
