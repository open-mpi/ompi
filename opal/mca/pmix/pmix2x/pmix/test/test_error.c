/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#include <time.h>
#include "test_error.h"
#include "test_common.h"

#define MAX_ERR_HANDLERS 5
#define TEST_NOTIFY PMIX_ERR_TIMEOUT
static bool done;
static void comfail_errhandler(size_t evhdlr_registration_id,
                               pmix_status_t status,
                               const pmix_proc_t *source,
                               pmix_info_t info[], size_t ninfo,
                               pmix_info_t results[], size_t nresults,
                               pmix_event_notification_cbfunc_fn_t cbfunc,
                               void *cbdata)
{
    TEST_ERROR(("comfail errhandler called for error status = %d ninfo = %lu",
                  status, (unsigned long)ninfo));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

static void timeout_errhandler(size_t evhdlr_registration_id,
                               pmix_status_t status,
                               const pmix_proc_t *source,
                               pmix_info_t info[], size_t ninfo,
                               pmix_info_t results[], size_t nresults,
                               pmix_event_notification_cbfunc_fn_t cbfunc,
                               void *cbdata)
{
    TEST_ERROR(("timeout errhandler called for error status = %d ninfo = %d",
                  status, (int)ninfo));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

static void op1_callbk(pmix_status_t status,
                       void *cbdata)
{
    TEST_VERBOSE(( "op1_callbk CALLED WITH STATUS %d", status));
    done = true;
}

static void errhandler_reg_callbk1 (pmix_status_t status,
                                   size_t errhandler_ref,
                                   void *cbdata)
{
    size_t *ref = (size_t*) cbdata;
    *ref = errhandler_ref;
    TEST_VERBOSE(("PMIX client ERRHANDLER REGISTRATION CALLED WITH STATUS %d, ref=%lu",
                  status, (unsigned long)errhandler_ref));

}

int test_error(char *my_nspace, int my_rank, test_params params)
{
    size_t errhandler_refs[MAX_ERR_HANDLERS];
    struct timespec ts;
    pmix_status_t status;
    pmix_proc_t source;

    TEST_VERBOSE(("test-error: running  error handling test cases"));
    /* register specific client error handlers and test their invocation
     * by  trigerring events  from server side*/
    status = PMIX_ERR_TIMEOUT;
    PMIx_Register_event_handler(&status, 1, NULL, 0,
                                timeout_errhandler, errhandler_reg_callbk1, &errhandler_refs[0]);

    /* reg a handler for comm errors */
    status = PMIX_ERR_LOST_PEER_CONNECTION;
    PMIx_Register_event_handler(&status, 1, NULL, 0,
                                comfail_errhandler, errhandler_reg_callbk1, &errhandler_refs[1]);
    /* inject error from client */
    done = false;
    (void)strncpy(source.nspace, my_nspace, PMIX_MAX_NSLEN);
    source.rank = my_rank;
    /* change error value to test other error notifications */
    PMIx_Notify_event(TEST_NOTIFY,
                      &source, PMIX_RANGE_NAMESPACE,
                      NULL, 0,
                      op1_callbk, NULL);
    while(!done) {
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }
    done = false;
    /* dereg all handlers*/
    PMIx_Deregister_event_handler( errhandler_refs[0], op1_callbk, NULL);
    /* loop until we get callback */
    while(!done) {
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }
    done = false;
    PMIx_Deregister_event_handler( errhandler_refs[1], op1_callbk, NULL);
    /* loop until we get callback */
    while(!done) {
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }
    return PMIX_SUCCESS;
}
