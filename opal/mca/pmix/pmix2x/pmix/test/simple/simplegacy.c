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
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
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

#define MAXCNT 3

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
    uint32_t nprocs, n;
    int cnt, j;
    volatile bool active;
    pmix_info_t info, *iptr;
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
    pmix_output(0, "Client ns %s rank %d: Running", myproc.nspace, myproc.rank);

    /* test something */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }
    nprocs = val->data .uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d universe size %d", myproc.nspace, myproc.rank, nprocs);

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

    memset(&info, 0, sizeof(pmix_info_t));
    (void)strncpy(info.key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);
    info.value.type = PMIX_UNDEF;
    info.value.data.flag = 1;

    for (cnt=0; cnt < MAXCNT; cnt++) {
        pmix_output(0, "EXECUTING LOOP %d", cnt);
        for (j=0; j < 10; j++) {
            (void)asprintf(&tmp, "%s-%d-gasnet-%d-%d", myproc.nspace, myproc.rank, cnt, j);
            value.type = PMIX_UINT64;
            value.data.uint64 = 1234;
            if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, tmp, &value))) {
                pmix_output(0, "Client ns %s rank %d: PMIx_Put failed: %s",
                            myproc.nspace, myproc.rank, PMIx_Error_string(rc));
                goto done;
            }
            free(tmp);
        }

        if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
            pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Commit failed: %s",
                        myproc.nspace, myproc.rank, cnt, PMIx_Error_string(rc));
            goto done;
        }

        /* call fence to ensure the data is received */
        if (PMIX_SUCCESS != (rc = PMIx_Fence(NULL, 0, &info, 1))) {
            pmix_output(0, "Client ns %s rank %d cnt %d: PMIx_Fence failed: %s",
                        myproc.nspace, myproc.rank, cnt, PMIx_Error_string(rc));
            goto done;
        }

        /* check the returned data */
        (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
        proc.rank = PMIX_RANK_UNDEF;
        for (j=0; j < 10; j++) {
            for (n=0; n < nprocs; n++) {
                (void)asprintf(&tmp, "%s-%d-gasnet-%d-%d", myproc.nspace, n, cnt, j);
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
