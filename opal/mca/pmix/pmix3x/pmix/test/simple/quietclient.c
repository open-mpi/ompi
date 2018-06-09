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

    *active = false;
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    uint32_t nprocs, n;
    int cnt, j;
    volatile bool active;
    pmix_info_t *iptr;
    size_t ninfo;
    pmix_status_t code;

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

    /* get our universe size */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get universe size failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);

    /* put a few values */
    (void)asprintf(&tmp, "%s-%d-internal", myproc.nspace, myproc.rank);
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&myproc, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Store_internal failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }

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
                PMIX_VALUE_RELEASE(val);
                free(tmp);

                if (n != myproc.rank) {
                    (void)asprintf(&tmp, "%s-%d-remote-%d", proc.nspace, n, j);
                    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
                        /* this data should _not_ be found as we are on the same node
                         * and the data was "put" with a PMIX_REMOTE scope */
                        continue;
                    }
                    pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Get %s returned remote data for a local proc",
                                myproc.nspace, myproc.rank, j, tmp);
                    PMIX_VALUE_RELEASE(val);
                    free(tmp);
                }
            }
        }
    }

    /* now get the data blob for myself */
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
            PMIX_VALUE_RELEASE(val);
        }
    } else {
        pmix_output(0, "Client ns %s rank %d internal modex blob FAILED with error %s(%d)",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc), rc);
    }

 done:
    /* finalize us */
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
    }
    fflush(stderr);
    return(rc);
}
