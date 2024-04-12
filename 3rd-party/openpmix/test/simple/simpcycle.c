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
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "src/include/pmix_config.h"
#include "include/pmix.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "src/class/pmix_object.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"

#define MAXCNT 1

static volatile bool completed = false;
static pmix_proc_t myproc;

static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(evhdlr_registration_id, source, info, ninfo,
                            results, nresults);
    pmix_output(0, "Client %s:%d NOTIFIED with status %s", myproc.nspace, myproc.rank,
                PMIx_Error_string(status));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
    completed = true;
}

/* this is an event notification function that we explicitly request
 * be called when the PMIX_MODEL_DECLARED notification is issued.
 * We could catch it in the general event notification function and test
 * the status to see if the status matched, but it often is simpler
 * to declare a use-specific notification callback point. In this case,
 * we are asking to know whenever a model is declared as a means
 * of testing server self-notification */
static void model_callback(size_t evhdlr_registration_id, pmix_status_t status,
                           const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                           pmix_info_t results[], size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    size_t n;
    PMIX_HIDE_UNUSED_PARAMS(evhdlr_registration_id, source, results, nresults);

    /* just let us know it was received */
    fprintf(stderr, "%s:%d Model event handler called with status %d(%s)\n", myproc.nspace,
            myproc.rank, status, PMIx_Error_string(status));
    for (n = 0; n < ninfo; n++) {
        if (PMIX_STRING == info[n].value.type) {
            fprintf(stderr, "%s:%d\t%s:\t%s\n", myproc.nspace, myproc.rank, info[n].key,
                    info[n].value.data.string);
        }
    }

    /* we must NOT tell the event handler state machine that we
     * are the last step as that will prevent it from notifying
     * anyone else that might be listening for declarations */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

int main(int argc, char **argv)
{
    int rc, nprocs;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    pmix_info_t *iptr;
    size_t ninfo;
    pmix_status_t code;
    PMIX_HIDE_UNUSED_PARAMS(argc, argv);

    /* init us and declare we are a test programming model */
    PMIX_INFO_CREATE(iptr, 2);
    PMIX_INFO_LOAD(&iptr[0], PMIX_PROGRAMMING_MODEL, "TEST", PMIX_STRING);
    PMIX_INFO_LOAD(&iptr[1], PMIX_MODEL_LIBRARY_NAME, "PMIX", PMIX_STRING);
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, iptr, 2))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %s", myproc.nspace, myproc.rank,
                    PMIx_Error_string(rc));
        exit(rc);
    }
    PMIX_INFO_FREE(iptr, 2);
    pmix_output(0, "Client ns %s rank %d: Running on node %s", myproc.nspace, myproc.rank,
                pmix_globals.hostname);

    /* test something */
    pmix_strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get job size failed: %s", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d job size %d", myproc.nspace, myproc.rank, nprocs);

    /* register a handler specifically for when models declare */
    ninfo = 1;
    PMIX_INFO_CREATE(iptr, ninfo);
    PMIX_INFO_LOAD(&iptr[0], PMIX_EVENT_HDLR_NAME, "SIMPCLIENT-MODEL", PMIX_STRING);
    code = PMIX_MODEL_DECLARED;
    PMIx_Register_event_handler(&code, 1, iptr, ninfo, model_callback, NULL, NULL);
    PMIX_INFO_FREE(iptr, ninfo);

    /* register our errhandler */
    PMIx_Register_event_handler(NULL, 0, NULL, 0, notification_fn, NULL, NULL);

    /* put a few values */
    if (0 > asprintf(&tmp, "%s-%d-internal", myproc.nspace, myproc.rank)) {
        errno = ENOMEM;
        abort();
    }
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&myproc, tmp, &value))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Store_internal failed: %s", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }

    /* get a list of our local peers */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_PEERS, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get local peers failed: %s", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }
    PMIX_VALUE_RELEASE(val);

    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing(1)", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }

    /* initialize us again */
    fprintf(stderr, "Client Init(2)\n");
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %s", myproc.nspace, myproc.rank,
                    PMIx_Error_string(rc));
        exit(rc);
    }
    pmix_output(0, "Client ns %s rank %d: Finalizing(2)", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }

    fflush(stderr);
    return (rc);
}
