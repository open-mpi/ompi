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
    myquery_data_t *mq = (myquery_data_t*)cbdata;
    size_t n;

    mq->lock.status = status;

    /* save the returned info - it will be
     * released in the release_fn */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }

    /* let the library release the data */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    DEBUG_WAKEUP_THREAD(&mq->lock);
}

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_proc_t myproc;
    pmix_query_t *query;
    size_t nq, ninfo = 0, n, m;
    myquery_data_t mydata;
    pmix_info_t *info = NULL, *iptr;
    char *server_uri = NULL;
    char *nspace = NULL;
    char *nodename = NULL;
    pmix_data_array_t *darray, *dptr;
    bool geturi = false;
    char hostname[1024];

    gethostname(hostname, 1024);
    for (n=1; n < (size_t)argc; n++) {
        if (0 == strcmp("-u", argv[n]) || 0 == strcmp("--url", argv[n])) {
            if (NULL == argv[n+1]) {
                fprintf(stderr, "Must provide URI argument to %s option\n", argv[n]);
                exit(1);
            }
            server_uri = argv[n+1];
        } else if (0 == strcmp("-nspace", argv[n]) || 0 == strcmp("--nspace", argv[n])) {
            if (NULL == argv[n+1]) {
                fprintf(stderr, "Must provide nspace argument to %s option\n", argv[n]);
                exit(1);
            }
            nspace = argv[n+1];
        } else if (0 == strcmp("-uri", argv[n]) || 0 == strcmp("--uri", argv[n])) {
            /* retrieve the PMIx server's uri from the indicated node */
            nodename = argv[n+1];
            geturi = true;
        }
    }

    if (NULL != server_uri) {
        ninfo = 1;
        PMIX_INFO_CREATE(info, ninfo);
        PMIX_INFO_LOAD(&info[0], PMIX_SERVER_URI, server_uri, PMIX_STRING);
        fprintf(stderr, "Connecting to %s\n", server_uri);
    }

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, ninfo))) {
        fprintf(stderr, "PMIx_tool_init failed: %d\n", rc);
        exit(rc);
    }
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }

    if (geturi) {
        nq = 1;
        PMIX_QUERY_CREATE(query, nq);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_SERVER_URI);
        if (NULL != nodename) {
            PMIX_QUERY_QUALIFIERS_CREATE(&query[0], 1);
            PMIX_INFO_LOAD(&query[0].qualifiers[0], PMIX_HOSTNAME, nodename, PMIX_STRING);
        }
        DEBUG_CONSTRUCT_MYQUERY(&mydata);
        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&mydata))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        DEBUG_WAIT_THREAD(&mydata.lock);
        /* find the response */
        if (PMIX_SUCCESS == mydata.lock.status) {
            /* should be in the first key */
            if (PMIX_CHECK_KEY(&mydata.info[0], PMIX_SERVER_URI)) {
                fprintf(stderr, "PMIx server URI for node %s: %s\n",
                        (NULL == nodename) ? hostname : nodename,
                        mydata.info[0].value.data.string);
            } else {
                fprintf(stderr, "Query returned wrong info key at first posn: %s\n", mydata.info[0].key);
            }
        } else {
            fprintf(stderr, "Query returned error: %s\n", PMIx_Error_string(mydata.lock.status));
        }
        DEBUG_DESTRUCT_MYQUERY(&mydata);
        goto done;
    }

    if (NULL == nspace) {
        /* query the list of active nspaces */
        nq = 1;
        PMIX_QUERY_CREATE(query, nq);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_NAMESPACE_INFO);
        DEBUG_CONSTRUCT_MYQUERY(&mydata);
        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&mydata))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        DEBUG_WAIT_THREAD(&mydata.lock);
        /* find the response */
        if (PMIX_SUCCESS == mydata.lock.status) {
            /* should be in the first key */
            if (PMIX_CHECK_KEY(&mydata.info[0], PMIX_QUERY_NAMESPACE_INFO)) {
                darray = mydata.info[0].value.data.darray;
                fprintf(stderr, "ACTIVE NSPACES:\n");
                if (NULL == darray || 0 == darray->size || NULL == darray->array) {
                    fprintf(stderr, "\tNone\n");
                } else {
                    info = (pmix_info_t*)darray->array;
                    if (NULL == info) {
                        fprintf(stderr, "Error\n");
                    } else {
                        for (n=0; n < darray->size; n++) {
                            dptr = info[n].value.data.darray;
                            if (NULL == dptr || 0 == dptr->size || NULL == dptr->array) {
                                fprintf(stderr, "Error in array %s\n", (NULL == dptr) ? "NULL" : "NON-NULL");
                                break;
                            }
                            iptr = (pmix_info_t*)dptr->array;
                            for (m=0; m < dptr->size; m++) {
                                fprintf(stderr, "\t%s", iptr[m].value.data.string);
                            }
                            fprintf(stderr, "\n");
                        }
                    }
                }
            } else {
                fprintf(stderr, "Query returned wrong info key at first posn: %s\n", mydata.info[0].key);
            }
        } else {
            fprintf(stderr, "Query returned error: %s\n", PMIx_Error_string(mydata.lock.status));
        }
        DEBUG_DESTRUCT_MYQUERY(&mydata);
    } else {
        nq = 1;
        PMIX_QUERY_CREATE(query, nq);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_JOB_SIZE);
        PMIX_INFO_CREATE(query[0].qualifiers, 1);
        query[0].nqual = 1;
        PMIX_INFO_LOAD(&query[0].qualifiers[0], PMIX_NSPACE, nspace, PMIX_STRING);
        DEBUG_CONSTRUCT_MYQUERY(&mydata);
        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&mydata))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        DEBUG_WAIT_THREAD(&mydata.lock);
        /* find the response */
        if (PMIX_SUCCESS == mydata.lock.status) {
            /* should be in the first key */
            if (PMIX_CHECK_KEY(&mydata.info[0], PMIX_JOB_SIZE)) {
                fprintf(stderr, "JOB SIZE FOR NSPACE %s: %lu\n", nspace, (unsigned long)mydata.info[0].value.data.uint32);
            } else {
                fprintf(stderr, "Query returned wrong info key at first posn: %s\n", mydata.info[0].key);
            }
        } else {
            fprintf(stderr, "Query returned error: %s\n", PMIx_Error_string(mydata.lock.status));
        }
        DEBUG_DESTRUCT_MYQUERY(&mydata);
    }

 done:
    /* finalize us */
    PMIx_Finalize(NULL, 0);
    return(rc);
}
