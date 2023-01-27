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

#define _GNU_SOURCE
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <pmix.h>
#include "examples.h"

static pmix_proc_t myproc;

/* this is a callback function for the PMIx_Query
 * API. The query will callback with a status indicating
 * if the request could be fully satisfied, partially
 * satisfied, or completely failed. The info parameter
 * contains an array of the returned data, with the
 * info->key field being the key that was provided in
 * the query call. Thus, you can correlate the returned
 * data in the info->value field to the requested key.
 *
 * Once we have dealt with the returned data, we must
 * call the release_fn so that the PMIx library can
 * cleanup */
static void cbfunc(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;
    size_t n;
    char *tmp;
    pmix_status_t rc;

    lock->status = status;

    fprintf(stderr, "Query returned %d values status %s\n", (int)ninfo, PMIx_Error_string(status));
    /* print out the returned keys and pmix_info_t structs */
    for (n=0; n < ninfo; n++) {
        fprintf(stderr, "KEY: %s\n", info[n].key);
        rc = PMIx_Data_print(&tmp, NULL, &info[n].value, info[n].value.type);
        if (PMIX_SUCCESS != rc) {
            lock->status = rc;
            goto done;
        }
        rc = PMIx_Data_print(&tmp, NULL, &info[n].value, info[n].value.type);
        if (PMIX_SUCCESS != rc) {
            lock->status = rc;
            goto done;
        }
        fprintf(stderr, "Key %s Type %s(%d)\n", info[n].key, PMIx_Data_type_string(info[n].value.type), info[n].value.type);
        free(tmp);
    }

  done:
    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    DEBUG_WAKEUP_THREAD(lock);
}

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pid_t pid;
    char hostname[1024];
    pmix_value_t *val;
    uint16_t localrank;
    size_t n;
    pmix_query_t query;
    mylock_t mylock;
    bool refresh = false;

    if (1 < argc) {
        if (NULL != strstr(argv[1], "true")) {
            refresh = true;
        }
    }

    pid = getpid();
    gethostname(hostname, 1024);

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes any
     * debugger flag instructing us to stop-in-init. If such a directive
     * is included, then the process will be stopped in this call until
     * the "debugger release" notification arrives */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(0);
    }
    /* get our local rank */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_LOCAL_RANK, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get local rank failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    localrank = val->data.uint16;
    PMIX_VALUE_RELEASE(val);

    fprintf(stderr, "Client ns %s rank %d pid %lu: Running on host %s localrank %d\n",
            myproc.nspace, myproc.rank, (unsigned long)pid, hostname , (int)localrank);

#if PMIX_VERSION_MAJOR >= 4
    n = 1;
    PMIX_QUERY_CONSTRUCT(&query);
    PMIX_ARGV_APPEND(rc, query.keys, PMIX_QUERY_NUM_PSETS);
    PMIX_ARGV_APPEND(rc, query.keys, PMIX_QUERY_PSET_NAMES);
    if (refresh) {
        PMIX_INFO_CREATE(query.qualifiers, 1);
        query.nqual = 1;
        PMIX_INFO_LOAD(&query.qualifiers[0], PMIX_QUERY_REFRESH_CACHE, &refresh, PMIX_BOOL);
    }
    /* setup the caddy to retrieve the data */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    /* execute the query */
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(&query, 1, cbfunc, (void*)&mylock))) {
        fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
        goto done;
    }
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);

#endif

  done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
