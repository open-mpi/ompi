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

#include <src/include/pmix_config.h>
#include <pmix.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "src/class/pmix_object.h"
#include "src/util/output.h"
#include "src/util/printf.h"

#define MAXCNT 1

static volatile bool completed = false;
static pmix_proc_t myproc;

static void notification_fn(size_t evhdlr_registration_id,
                            pmix_status_t status,
                            const pmix_proc_t *source,
                            pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc,
                            void *cbdata)
{
    pmix_output(0, "Client %s:%d NOTIFIED with status %s", myproc.nspace, myproc.rank, PMIx_Error_string(status));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
    completed = true;
}

static void errhandler_reg_callbk(pmix_status_t status,
                                  size_t errhandler_ref,
                                  void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;

    pmix_output(0, "Client: ERRHANDLER REGISTRATION CALLBACK CALLED WITH STATUS %d, ref=%lu",
                status, (unsigned long)errhandler_ref);
    *active = false;
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;
    *active = false;
}

/* this is an event notification function that we explicitly request
 * be called when the PMIX_MODEL_DECLARED notification is issued.
 * We could catch it in the general event notification function and test
 * the status to see if the status matched, but it often is simpler
 * to declare a use-specific notification callback point. In this case,
 * we are asking to know whenever a model is declared as a means
 * of testing server self-notification */
static void model_callback(size_t evhdlr_registration_id,
                           pmix_status_t status,
                           const pmix_proc_t *source,
                           pmix_info_t info[], size_t ninfo,
                           pmix_info_t results[], size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc,
                           void *cbdata)
{
    size_t n;

    /* just let us know it was received */
    fprintf(stderr, "%s:%d Model event handler called with status %d(%s)\n",
            myproc.nspace, myproc.rank, status, PMIx_Error_string(status));
    for (n=0; n < ninfo; n++) {
        if (PMIX_STRING == info[n].value.type) {
            fprintf(stderr, "%s:%d\t%s:\t%s\n",
                    myproc.nspace, myproc.rank,
                    info[n].key, info[n].value.data.string);
        }
    }

    /* we must NOT tell the event handler state machine that we
     * are the last step as that will prevent it from notifying
     * anyone else that might be listening for declarations */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

/* event handler registration is done asynchronously */
static void model_registration_callback(pmix_status_t status,
                                        size_t evhandler_ref,
                                        void *cbdata)
{
    volatile int *active = (volatile int*)cbdata;

    fprintf(stderr, "simpclient EVENT HANDLER REGISTRATION RETURN STATUS %d, ref=%lu\n",
               status, (unsigned long)evhandler_ref);
    *active = false;
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    uint32_t nprocs, n, k, nlocal;
    int cnt, j;
    bool doabort = false;
    volatile bool active;
    pmix_info_t info, *iptr;
    size_t ninfo;
    pmix_status_t code;
    char **peers;
    bool all_local, local;
    pmix_rank_t *locals = NULL;

    if (1 < argc) {
        if (0 == strcmp("-abort", argv[1])) {
            doabort = true;
        }
    }

    /* init us and declare we are a test programming model */
    PMIX_INFO_CREATE(iptr, 2);
    PMIX_INFO_LOAD(&iptr[0], PMIX_PROGRAMMING_MODEL, "TEST", PMIX_STRING);
    PMIX_INFO_LOAD(&iptr[1], PMIX_MODEL_LIBRARY_NAME, "PMIX", PMIX_STRING);
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, iptr, 2))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }
    PMIX_INFO_FREE(iptr, 2);
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

    /* register a handler specifically for when models declare */
    active = true;
    ninfo = 1;
    PMIX_INFO_CREATE(iptr, ninfo);
    PMIX_INFO_LOAD(&iptr[0], PMIX_EVENT_HDLR_NAME, "SIMPCLIENT-MODEL", PMIX_STRING);
    code = PMIX_MODEL_DECLARED;
    PMIx_Register_event_handler(&code, 1, iptr, ninfo,
                                model_callback, model_registration_callback, (void*)&active);
    while (active) {
        usleep(10);
    }
    PMIX_INFO_FREE(iptr, ninfo);

    /* register our errhandler */
    active = true;
    PMIx_Register_event_handler(NULL, 0, NULL, 0,
                                notification_fn, errhandler_reg_callbk, (void*)&active);
    while (active) {
        usleep(10);
    }


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

    /* put a few values */
    (void)asprintf(&tmp, "%s-%d-internal", myproc.nspace, myproc.rank);
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&myproc, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Store_internal failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }

    /* get a list of our local peers */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_PEERS, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get local peers failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    /* split the returned string to get the rank of each local peer */
    peers = pmix_argv_split(val->data.string, ',');
    PMIX_VALUE_RELEASE(val);
    nlocal = pmix_argv_count(peers);
    if (nprocs == nlocal) {
        all_local = true;
    } else {
        all_local = false;
        locals = (pmix_rank_t*)malloc(pmix_argv_count(peers) * sizeof(pmix_rank_t));
        for (cnt=0; NULL != peers[cnt]; cnt++) {
            locals[cnt] = strtoul(peers[cnt], NULL, 10);
        }
    }
    pmix_argv_free(peers);

    for (cnt=0; cnt < MAXCNT; cnt++) {
        (void)asprintf(&tmp, "%s-%d-local-%d", myproc.nspace, myproc.rank, cnt);
        value.type = PMIX_UINT64;
        value.data.uint64 = 1234;
        if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, tmp, &value))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %s",
                        myproc.nspace, myproc.rank, PMIx_Error_string(rc));
            goto done;
        }

        (void)asprintf(&tmp, "%s-%d-remote-%d", myproc.nspace, myproc.rank, cnt);
        value.type = PMIX_STRING;
        value.data.string = "1234";
        if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_REMOTE, tmp, &value))) {
            pmix_output(0, "Client ns %s rank %d: PMIx_Put internal failed: %s",
                        myproc.nspace, myproc.rank, PMIx_Error_string(rc));
            goto done;
        }

        if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
            pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Commit failed: %s",
                        myproc.nspace, myproc.rank, cnt, PMIx_Error_string(rc));
            goto done;
        }

        /* call fence to ensure the data is received */
        PMIX_PROC_CONSTRUCT(&proc);
        (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
        proc.rank = PMIX_RANK_WILDCARD;
        if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
            pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Fence failed: %s",
                        myproc.nspace, myproc.rank, cnt, PMIx_Error_string(rc));
            goto done;
        }

        /* check the returned data */
        (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
        for (j=0; j <= cnt; j++) {
            for (n=0; n < nprocs; n++) {
                proc.rank = n;
                if (all_local) {
                    local = true;
                } else {
                    local = false;
                    /* see if this proc is local to us */
                    for (k=0; k < nlocal; k++) {
                        if (proc.rank == locals[k]) {
                            local = true;
                            break;
                        }
                    }
                }
                if (local) {
                    (void)asprintf(&tmp, "%s-%d-local-%d", myproc.nspace, n, j);
                    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
                        pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Get %s failed: %s",
                                    myproc.nspace, myproc.rank, j, tmp, PMIx_Error_string(rc));
                        continue;
                    }
                    if (NULL == val) {
                        pmix_output(0, "Client ns %s rank %d: NULL value returned",
                                    myproc.nspace, myproc.rank);
                        break;
                    }
                    if (PMIX_UINT64 != val->type) {
                        pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Get %s returned wrong type: %d", myproc.nspace, myproc.rank, j, tmp, val->type);
                        PMIX_VALUE_RELEASE(val);
                        free(tmp);
                        continue;
                    }
                    if (1234 != val->data.uint64) {
                        pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Get %s returned wrong value: %d", myproc.nspace, myproc.rank, j, tmp, (int)val->data.uint64);
                        PMIX_VALUE_RELEASE(val);
                        free(tmp);
                        continue;
                    }
                    pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Get %s returned correct", myproc.nspace, myproc.rank, j, tmp);
                    PMIX_VALUE_RELEASE(val);
                    free(tmp);

                    /* now check that we don't get data for a remote proc - note that we
                     * always can get our own remote data as we published it */
                    if (proc.rank != myproc.rank) {
                        (void)asprintf(&tmp, "%s-%d-remote-%d", proc.nspace, n, j);
                        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
                            /* this data should _not_ be found as we are on the same node
                             * and the data was "put" with a PMIX_REMOTE scope */
                            pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Get %s returned correct", myproc.nspace, myproc.rank, j, tmp);
                        } else {
                            pmix_output(0, "ERROR: Client ns %s rank %d cnt %d: PMIx_Get %s returned remote data for a local proc",
                                        myproc.nspace, myproc.rank, j, tmp);
                        }
                        if (NULL != val) {
                            PMIX_VALUE_RELEASE(val);
                        }
                        free(tmp);
                    }
                } else {
                    (void)asprintf(&tmp, "%s-%d-remote-%d", proc.nspace, n, j);
                    if (PMIX_SUCCESS == (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
                        pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Get %s returned correct", myproc.nspace, myproc.rank, j, tmp);
                    } else {
                        pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Get %s failed for remote proc",
                                    myproc.nspace, myproc.rank, j, tmp);
                    }
                    PMIX_VALUE_RELEASE(val);
                    free(tmp);
                }
            }
        }
    }

    /* now get the data blob for myself */
    pmix_output(0, "Client ns %s rank %d testing internal modex blob",
                myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS == (rc = PMIx_Get(&myproc, NULL, NULL, 0, &val))) {
        if (PMIX_DATA_ARRAY != val->type) {
            pmix_output(0, "Client ns %s rank %d did not return an array for its internal modex blob",
                        myproc.nspace, myproc.rank);
            PMIX_VALUE_RELEASE(val);
        } else if (PMIX_INFO != val->data.darray->type) {
            pmix_output(0, "Client ns %s rank %d returned an internal modex array of type %s instead of PMIX_INFO",
                        myproc.nspace, myproc.rank, PMIx_Data_type_string(val->data.darray->type));
            PMIX_VALUE_RELEASE(val);
        } else if (0 == val->data.darray->size) {
            pmix_output(0, "Client ns %s rank %d returned an internal modex array of zero length",
                        myproc.nspace, myproc.rank);
            PMIX_VALUE_RELEASE(val);
        } else {
            pmix_info_t *iptr = (pmix_info_t*)val->data.darray->array;
            for (n=0; n < val->data.darray->size; n++) {
                pmix_output(0, "\tKey: %s", iptr[n].key);
            }
            PMIX_VALUE_RELEASE(val);
        }
    } else {
        pmix_output(0, "Client ns %s rank %d internal modex blob FAILED with error %s(%d)",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc), rc);
    }

    /* log something */
    PMIX_INFO_CONSTRUCT(&info);
    PMIX_INFO_LOAD(&info, PMIX_LOG_STDERR, "test log msg\n", PMIX_STRING);
    active = true;
    rc = PMIx_Log_nb(&info, 1, NULL, 0, opcbfunc, (void*)&active);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "Client ns %s rank %d - log_nb returned %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
    } else {
        while (active) {
            usleep(10);
        }
    }
    PMIX_INFO_DESTRUCT(&info);

    /* if requested and our rank is 0, call abort */
    if (doabort) {
        if (0 == myproc.rank) {
            PMIx_Abort(PMIX_ERR_PROC_REQUESTED_ABORT, "CALLING ABORT", NULL, 0);
        } else {
            while(!completed) {
                usleep(10);
            }
        }
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
