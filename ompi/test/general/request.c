/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/request layer (request.c, grequest.c,
 * req_wait.c, req_test.c).  Generalized requests are fully exercisable
 * single-process, and the wait/test family is driven over arrays mixing
 * MPI_REQUEST_NULL with a completed generalized request.  (Point-to-point
 * request progress between ranks is out of scope for single-process and
 * belongs to the np>1 effort.)
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include "support.h"

#include "mpi.h"

static int query_calls = 0;
static int free_calls = 0;
static int cancel_calls = 0;

static int grequest_query(void *extra, MPI_Status *status)
{
    (void) extra;
    ++query_calls;
    MPI_Status_set_elements(status, MPI_INT, 1);
    MPI_Status_set_cancelled(status, 0);
    status->MPI_SOURCE = MPI_ANY_SOURCE;
    status->MPI_TAG = MPI_ANY_TAG;
    return MPI_SUCCESS;
}

static int grequest_free(void *extra)
{
    (void) extra;
    ++free_calls;
    return MPI_SUCCESS;
}

static int grequest_cancel(void *extra, int complete)
{
    (void) extra; (void) complete;
    ++cancel_calls;
    return MPI_SUCCESS;
}

static void test_null_requests(void);
static void test_grequest_wait(void);
static void test_grequest_test(void);
static void test_array_ops(void);
static void test_cancel(void);

int main(int argc, char *argv[])
{
    test_init("ompi request");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_null_requests();
    test_grequest_wait();
    test_grequest_test();
    test_array_ops();
    test_cancel();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_null_requests(void)
{
    MPI_Request req = MPI_REQUEST_NULL;
    MPI_Status status;
    int rc = MPI_Wait(&req, &status);
    test_verify("Wait on MPI_REQUEST_NULL succeeds", MPI_SUCCESS == rc);

    req = MPI_REQUEST_NULL;
    int flag = 0;
    rc = MPI_Test(&req, &flag, &status);
    test_verify("Test on MPI_REQUEST_NULL succeeds", MPI_SUCCESS == rc);
    test_verify("Test on MPI_REQUEST_NULL sets flag", 1 == flag);

    req = MPI_REQUEST_NULL;
    flag = 0;
    rc = MPI_Request_get_status(req, &flag, &status);
    test_verify("Request_get_status(NULL) succeeds", MPI_SUCCESS == rc && 1 == flag);
}

/* ------------------------------------------------------------------ */

static void test_grequest_wait(void)
{
    query_calls = free_calls = 0;
    MPI_Request req = MPI_REQUEST_NULL;
    int rc = MPI_Grequest_start(grequest_query, grequest_free, grequest_cancel,
                                NULL, &req);
    test_verify("Grequest_start succeeds", MPI_SUCCESS == rc);
    test_verify("grequest is not NULL", MPI_REQUEST_NULL != req);

    rc = MPI_Grequest_complete(req);
    test_verify("Grequest_complete succeeds", MPI_SUCCESS == rc);

    MPI_Status status;
    rc = MPI_Wait(&req, &status);
    test_verify("Wait on completed grequest succeeds", MPI_SUCCESS == rc);
    test_verify("query callback was invoked", 1 == query_calls);
    test_verify("free callback was invoked", 1 == free_calls);
    test_verify("Wait NULLed the request handle", MPI_REQUEST_NULL == req);
}

/* ------------------------------------------------------------------ */

static void test_grequest_test(void)
{
    query_calls = free_calls = 0;
    MPI_Request req = MPI_REQUEST_NULL;
    MPI_Grequest_start(grequest_query, grequest_free, grequest_cancel, NULL, &req);

    /* not completed yet: Test must report incomplete */
    int flag = 1;
    MPI_Status status;
    int rc = MPI_Test(&req, &flag, &status);
    test_verify("Test on incomplete grequest succeeds", MPI_SUCCESS == rc);
    test_verify("incomplete grequest Test flag is 0", 0 == flag);

    MPI_Grequest_complete(req);
    flag = 0;
    rc = MPI_Test(&req, &flag, &status);
    test_verify("Test on completed grequest succeeds", MPI_SUCCESS == rc);
    test_verify("completed grequest Test flag is 1", 1 == flag);
    test_verify("query callback invoked by Test", 1 == query_calls);
}

/* ------------------------------------------------------------------ */

static void test_array_ops(void)
{
    /* Build an array: [NULL, completed-grequest, NULL] */
    MPI_Request reqs[3];
    reqs[0] = MPI_REQUEST_NULL;
    reqs[2] = MPI_REQUEST_NULL;
    MPI_Grequest_start(grequest_query, grequest_free, grequest_cancel, NULL, &reqs[1]);
    MPI_Grequest_complete(reqs[1]);

    MPI_Status statuses[3];
    int rc = MPI_Waitall(3, reqs, statuses);
    test_verify("Waitall over mixed array succeeds", MPI_SUCCESS == rc);
    test_verify("Waitall NULLed the grequest", MPI_REQUEST_NULL == reqs[1]);

    /* Testall on an all-NULL array reports done */
    reqs[1] = MPI_REQUEST_NULL;
    int flag = 0;
    rc = MPI_Testall(3, reqs, &flag, statuses);
    test_verify("Testall on all-NULL succeeds", MPI_SUCCESS == rc);
    test_verify("Testall on all-NULL reports complete", 1 == flag);

    /* Waitany on all-NULL returns MPI_UNDEFINED */
    int index = -2;
    MPI_Status st;
    rc = MPI_Waitany(3, reqs, &index, &st);
    test_verify("Waitany on all-NULL succeeds", MPI_SUCCESS == rc);
    test_verify("Waitany on all-NULL yields MPI_UNDEFINED", MPI_UNDEFINED == index);

    /* Waitany picks the completed grequest */
    MPI_Grequest_start(grequest_query, grequest_free, grequest_cancel, NULL, &reqs[1]);
    MPI_Grequest_complete(reqs[1]);
    index = -2;
    rc = MPI_Waitany(3, reqs, &index, &st);
    test_verify("Waitany finds the completed request", MPI_SUCCESS == rc && 1 == index);

    /* Testsome on all-NULL returns MPI_UNDEFINED outcount */
    int outcount = -2;
    int indices[3];
    rc = MPI_Testsome(3, reqs, &outcount, indices, statuses);
    test_verify("Testsome on all-NULL succeeds", MPI_SUCCESS == rc);
    test_verify("Testsome on all-NULL yields MPI_UNDEFINED", MPI_UNDEFINED == outcount);
}

/* ------------------------------------------------------------------ */

static void test_cancel(void)
{
    cancel_calls = 0;
    MPI_Request req = MPI_REQUEST_NULL;
    MPI_Grequest_start(grequest_query, grequest_free, grequest_cancel, NULL, &req);

    int rc = MPI_Cancel(&req);
    test_verify("Cancel on a grequest succeeds", MPI_SUCCESS == rc);
    test_verify("cancel callback was invoked", 1 == cancel_calls);

    /* must still be completed and freed to release it */
    MPI_Grequest_complete(req);
    MPI_Status status;
    MPI_Wait(&req, &status);

    int cancelled = -1;
    MPI_Test_cancelled(&status, &cancelled);
    test_verify("Test_cancelled is callable on the status", cancelled == 0 || cancelled == 1);
}
