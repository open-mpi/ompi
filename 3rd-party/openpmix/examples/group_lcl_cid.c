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
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/*
 * This test simulates the way Open MPI uses the PMIx_Group_construct to
 * implement MPI4 functions:
 * - MPI_Comm_create_from_group
 * - MPI_Intercomm_create_from_groups
 */

#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <pmix.h>
#include "examples.h"

static pmix_proc_t myproc;
static uint32_t get_timeout = 600; /* default 600 secs to get remote data */

static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    EXAMPLES_HIDE_UNUSED_PARAMS(evhdlr_registration_id, source,
                                info, ninfo, results, nresults,
                                cbfunc, cbdata);

    fprintf(stderr, "Client %s:%d NOTIFIED with status %d\n", myproc.nspace, myproc.rank, status);
}

static void op_callbk(pmix_status_t status, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;

    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

static void errhandler_reg_callbk(pmix_status_t status, size_t errhandler_ref, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;
    EXAMPLES_HIDE_UNUSED_PARAMS(errhandler_ref);
    
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t *val = NULL;
    pmix_proc_t proc, *procs;
    uint32_t nprocs, n;
    mylock_t lock;
    pmix_info_t *results, *info, tinfo[2];
    size_t nresults, cid, lcid, ninfo;
    pmix_data_array_t darray;
    void *grpinfo, *list;

    EXAMPLES_HIDE_UNUSED_PARAMS(argc, argv);

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %s\n", myproc.nspace, myproc.rank,
                PMIx_Error_string(rc));
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d pid %lu: Running\n", myproc.nspace, myproc.rank, (unsigned long)getpid());

    PMIX_PROC_CONSTRUCT(&proc);
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);

    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);

    /* register our default errhandler */
    DEBUG_CONSTRUCT_LOCK(&lock);
    PMIx_Register_event_handler(NULL, 0, NULL, 0, notification_fn, errhandler_reg_callbk,
                                (void *) &lock);
    DEBUG_WAIT_THREAD(&lock);
    rc = lock.status;
    DEBUG_DESTRUCT_LOCK(&lock);
    if (PMIX_SUCCESS != rc) {
        goto done;
    }

    /* call fence to sync */
    PMIX_PROC_CONSTRUCT(&proc);
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        goto done;
    }

    PMIX_PROC_CREATE(procs, nprocs);
    for (n = 0; n < nprocs; n++) {
        PMIX_PROC_LOAD(&procs[n], myproc.nspace, n);
    }

    grpinfo = PMIx_Info_list_start();
    rc = PMIx_Info_list_add(grpinfo, PMIX_GROUP_ASSIGN_CONTEXT_ID, NULL, PMIX_BOOL);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Info_list_add failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }

    list = PMIx_Info_list_start();
    lcid = 1234UL + (unsigned long) myproc.rank;
    rc = PMIx_Info_list_add(list, PMIX_GROUP_LOCAL_CID, &lcid, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Info_list_add failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    rc = PMIx_Info_list_convert(list, &darray);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Info_list_convert failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    rc = PMIx_Info_list_add(grpinfo, PMIX_GROUP_INFO, &darray, PMIX_DATA_ARRAY);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Info_list_add failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    PMIx_Info_list_release(list);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);

    rc = PMIx_Info_list_convert(grpinfo, &darray);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Info_list_convert failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    info = (pmix_info_t*)darray.array;
    ninfo = darray.size;
    PMIx_Info_list_release(grpinfo);

    rc = PMIx_Group_construct("ourgroup", procs, nprocs, info, ninfo, &results, &nresults);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Group_construct failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    /* we should have a single results object */
    if (NULL != results) {
        cid = 0;
        PMIX_VALUE_GET_NUMBER(rc, &results[0].value, cid, size_t);
        fprintf(stderr, "Rank %d Group construct complete with status %s KEY %s CID %lu\n",
                myproc.rank, PMIx_Error_string(rc), results[0].key, (unsigned long) cid);
    } else {
        fprintf(stderr, "Rank %d Group construct complete, but no CID returned\n", myproc.rank);
    }
    PMIX_PROC_FREE(procs, nprocs);

    /*
     * destruct the group
     */
    rc = PMIx_Group_destruct("ourgroup", NULL, 0);
    if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Group_destruct failed: %s\n", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc));
            goto done;
    }

    PMIX_INFO_CONSTRUCT(&tinfo[0]);
    PMIX_INFO_LOAD(&tinfo[0], PMIX_GROUP_CONTEXT_ID, &cid, PMIX_SIZE);
    PMIX_INFO_SET_QUALIFIER(&tinfo[0]);
    PMIX_INFO_CONSTRUCT(&tinfo[1]);
    PMIX_INFO_LOAD(&tinfo[1], PMIX_TIMEOUT, &get_timeout, PMIX_UINT32);

    for (n = 0; n < nprocs; n++) {
        proc.rank = n;
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_GROUP_LOCAL_CID, tinfo, 2, &val))) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get of LOCAL CID for rank %d failed: %s\n",
                        myproc.nspace, myproc.rank, n, PMIx_Error_string(rc));
            continue;
        }
        if (PMIX_SIZE != val->type) {
           fprintf(stderr, "%s:%d: PMIx_Get LOCAL CID for rank %d returned wrong type: %s\n", myproc.nspace,
                    myproc.rank, n, PMIx_Data_type_string(val->type));
            PMIX_VALUE_RELEASE(val);
            continue;
        }
        if ((1234UL + (unsigned long)n) != val->data.size) {
            fprintf(stderr, "%s:%d: PMIx_Get LOCAL CID for rank %d returned wrong value: %s\n",
                    myproc.nspace, myproc.rank, n, PMIx_Value_string(val));
            PMIX_VALUE_RELEASE(val);
            continue;
        }
        fprintf(stderr, "%s:%d: PMIx_Get LOCAL CID for rank %u SUCCESS value: %s\n",
                myproc.nspace, myproc.rank, n, PMIx_Value_string(val));
        PMIX_VALUE_RELEASE(val);
    }

done:
    /* finalize us */
    DEBUG_CONSTRUCT_LOCK(&lock);
    PMIx_Deregister_event_handler(1, op_callbk, &lock);
    DEBUG_WAIT_THREAD(&lock);
    DEBUG_DESTRUCT_LOCK(&lock);

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    }
    fprintf(stderr, "%s:%d COMPLETE\n", myproc.nspace, myproc.rank);
    fflush(stderr);
    return (0);
}

