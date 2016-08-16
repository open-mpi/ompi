/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
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
static void comfail_errhandler(pmix_status_t status,
                                pmix_proc_t procs[], size_t nprocs,
                                pmix_info_t info[], size_t ninfo)
{
    TEST_ERROR(("comfail errhandler called for error status = %d nprocs =%d ninfo = %d",
                  status, nprocs, ninfo));
}

static void timeout_errhandler(pmix_status_t status,
                               pmix_proc_t procs[], size_t nprocs,
                               pmix_info_t info[], size_t ninfo)
{
    TEST_ERROR(("timeout errhandler called for error status = %d nprocs = %d ninfo = %d",
                  status, nprocs, ninfo));
}

static void op1_callbk(pmix_status_t status,
                                   void *cbdata)
{
    TEST_VERBOSE(( "op1_callbk CALLED WITH STATUS %d", status));
    done = true;
}

static void errhandler_reg_callbk1 (pmix_status_t status,
                                   int errhandler_ref,
                                   void *cbdata)
{
    int *ref = (int*) cbdata;
    *ref = errhandler_ref;
    TEST_VERBOSE(("PMIX client ERRHANDLER REGISTRATION CALLED WITH STATUS %d, ref=%d",
                  status, *ref, errhandler_ref));

}

int test_error(char *my_nspace, int my_rank, test_params params)
{
    pmix_info_t *info;
    size_t ninfo;
    int errhandler_refs[MAX_ERR_HANDLERS];
    int value;
    struct timespec ts;
    TEST_VERBOSE(("test-error: running  error handling test cases"));
    /* register specific client error handlers and test their invocation
     * by  trigerring events  from server side*/
    ninfo = 1;
    value = PMIX_ERR_TIMEOUT;
    PMIX_INFO_CREATE(info, ninfo);
    (void)strncpy(info[0].key, PMIX_ERROR_NAME, PMIX_MAX_KEYLEN);
    pmix_value_load(&info[0].value, &value, PMIX_INT);
    PMIx_Register_errhandler(info, 1, timeout_errhandler, errhandler_reg_callbk1, &errhandler_refs[0]);
    /* reg a handler for comm errors */
    (void)strncpy(info[0].key, PMIX_ERROR_GROUP_COMM, PMIX_MAX_KEYLEN);
    value = 1;
    pmix_value_load(&info[0].value, &value, PMIX_BOOL);
    PMIx_Register_errhandler(info, 1, comfail_errhandler, errhandler_reg_callbk1, &errhandler_refs[1]);
    /* inject error from client */
    done = false;
    /* change error value to test other error notifications */
    PMIx_Notify_error(TEST_NOTIFY,
                      NULL, 0,
                      NULL, 0, NULL, 0,
                      op1_callbk, NULL);
    while(!done) {
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }
    done = false;
    /* dereg all handlers*/
    PMIx_Deregister_errhandler( errhandler_refs[0], op1_callbk, NULL);
    /* loop until we get callback */
    while(!done) {
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }
    done = false;
    PMIx_Deregister_errhandler( errhandler_refs[1], op1_callbk, NULL);
    /* loop until we get callback */
    while(!done) {
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }
    return PMIX_SUCCESS;
}
