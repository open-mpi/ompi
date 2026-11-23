/* SPDX-License-Identifier: BSD-3-Clause-Open-MPI */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Check how the MPI_{Wait,Test}{,any,all,some} and MPI_Request_get_status*
 * functions report a request that completed with an error, with
 * MPI_ERRORS_RETURN on the communicators:
 *
 *  - a receive truncated by a longer message (the error is the one of the
 *    communication),
 *  - a generalized request whose free_fn fails (the error is the one of the
 *    callback).
 *
 * The expectations, from the MPI standard:
 *
 *  - the single-completion functions return the error of the request,
 *  - the functions with an array of statuses return MPI_ERR_IN_STATUS, even
 *    with MPI_STATUSES_IGNORE, and the error of each request is in its status,
 *  - a request that completed with an error is released by MPI_{Wait,Test}*
 *    (the handle is MPI_REQUEST_NULL),
 *  - MPI_Request_get_status* are non-destructive: they report the same errors
 *    but leave the request valid,
 *  - the free_fn error is reported once, free_fn is called once, and the
 *    request is released.
 *
 * usage: request_errors
 *
 * Runs on any number of processes, each of them only talks to itself.  The
 * exit status is non-zero if any check failed.
 */

#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

#define MAX_REQS 3

static int failures = 0;

static void expect(const char *what, int got, int want)
{
    if (got != want) {
        fprintf(stderr, "FAIL: %s: got %d, expected %d\n", what, got, want);
        ++failures;
    }
}

/* All the requests of an array are MPI_REQUEST_NULL */
static int all_null(int count, const MPI_Request *requests)
{
    for (int i = 0; i < count; ++i) {
        if (MPI_REQUEST_NULL != requests[i]) {
            return 0;
        }
    }
    return 1;
}

static int index_of(int count, const int *indices, int wanted)
{
    for (int i = 0; i < count; ++i) {
        if (wanted == indices[i]) {
            return i;
        }
    }
    return -1;
}

/*
 * Truncated receives.  requests[0] is a receive of one int matched by a
 * message of two ints, and fails; requests[1] is a regular receive.  Both
 * are completed when this function returns.
 */
static void post_truncated(MPI_Request requests[2])
{
    int bad_send[2] = {1, 2}, bad_recv, good_send = 3, good_recv, flag = 0;

    MPI_Irecv(&bad_recv, 1, MPI_INT, 0, 1, MPI_COMM_SELF, &requests[0]);
    MPI_Irecv(&good_recv, 1, MPI_INT, 0, 2, MPI_COMM_SELF, &requests[1]);
    MPI_Send(bad_send, 2, MPI_INT, 0, 1, MPI_COMM_SELF);
    MPI_Send(&good_send, 1, MPI_INT, 0, 2, MPI_COMM_SELF);
    for (int i = 0; i < 2; ++i) {
        /* the return code is irrelevant here, wait for the completion */
        for (int spin = 0; !flag && spin < 10000000; ++spin) {
            MPI_Request_get_status(requests[i], &flag, MPI_STATUS_IGNORE);
        }
        flag = 0;
    }
}

static void drain(MPI_Request requests[2])
{
    for (int i = 0; i < 2; ++i) {
        if (MPI_REQUEST_NULL != requests[i]) {
            MPI_Wait(&requests[i], MPI_STATUS_IGNORE);
        }
    }
}

static void test_truncated_completion(void)
{
    MPI_Request r[2];
    MPI_Status st[2];
    int rc, flag, count, indices[2], k;

    post_truncated(r);
    rc = MPI_Wait(&r[0], MPI_STATUS_IGNORE);
    expect("Wait: truncated receive", rc, MPI_ERR_TRUNCATE);
    expect("Wait: handle released", MPI_REQUEST_NULL == r[0], 1);
    drain(r);

    post_truncated(r);
    rc = MPI_Test(&r[0], &flag, MPI_STATUS_IGNORE);
    expect("Test: truncated receive", rc, MPI_ERR_TRUNCATE);
    expect("Test: handle released", MPI_REQUEST_NULL == r[0], 1);
    drain(r);

    post_truncated(r);
    rc = MPI_Waitany(2, r, &k, MPI_STATUS_IGNORE);
    expect("Waitany: error", rc, MPI_ERR_TRUNCATE);
    expect("Waitany: handle released", MPI_REQUEST_NULL == r[k], 1);
    drain(r);

    post_truncated(r);
    rc = MPI_Testany(2, r, &k, &flag, MPI_STATUS_IGNORE);
    expect("Testany: error", rc, MPI_ERR_TRUNCATE);
    expect("Testany: handle released", MPI_REQUEST_NULL == r[k], 1);
    drain(r);

    /* arrays, without statuses */
    post_truncated(r);
    rc = MPI_Waitall(2, r, MPI_STATUSES_IGNORE);
    expect("Waitall/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    expect("Waitall/STATUSES_IGNORE: handles released", all_null(2, r), 1);

    post_truncated(r);
    rc = MPI_Testall(2, r, &flag, MPI_STATUSES_IGNORE);
    expect("Testall/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    expect("Testall/STATUSES_IGNORE: handles released", all_null(2, r), 1);

    post_truncated(r);
    rc = MPI_Waitsome(2, r, &count, indices, MPI_STATUSES_IGNORE);
    expect("Waitsome/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    drain(r);

    post_truncated(r);
    rc = MPI_Testsome(2, r, &count, indices, MPI_STATUSES_IGNORE);
    expect("Testsome/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    drain(r);

    /* arrays, with statuses */
    post_truncated(r);
    rc = MPI_Waitall(2, r, st);
    expect("Waitall/statuses", rc, MPI_ERR_IN_STATUS);
    expect("Waitall/statuses: failed status", st[0].MPI_ERROR, MPI_ERR_TRUNCATE);
    expect("Waitall/statuses: good status", st[1].MPI_ERROR, MPI_SUCCESS);

    post_truncated(r);
    rc = MPI_Testall(2, r, &flag, st);
    expect("Testall/statuses", rc, MPI_ERR_IN_STATUS);
    expect("Testall/statuses: failed status", st[0].MPI_ERROR, MPI_ERR_TRUNCATE);
    expect("Testall/statuses: good status", st[1].MPI_ERROR, MPI_SUCCESS);

    post_truncated(r);
    rc = MPI_Waitsome(2, r, &count, indices, st);
    expect("Waitsome/statuses", rc, MPI_ERR_IN_STATUS);
    k = index_of(count, indices, 0);
    expect("Waitsome/statuses: failed request completed", k >= 0, 1);
    if (k >= 0) {
        expect("Waitsome/statuses: failed status", st[k].MPI_ERROR, MPI_ERR_TRUNCATE);
    }
    drain(r);

    post_truncated(r);
    rc = MPI_Testsome(2, r, &count, indices, st);
    expect("Testsome/statuses", rc, MPI_ERR_IN_STATUS);
    k = index_of(count, indices, 0);
    expect("Testsome/statuses: failed request completed", k >= 0, 1);
    if (k >= 0) {
        expect("Testsome/statuses: failed status", st[k].MPI_ERROR, MPI_ERR_TRUNCATE);
    }
    drain(r);
}

static void test_truncated_get_status(void)
{
    MPI_Request r[2];
    MPI_Status st[2];
    int rc, flag, count, index, indices[2];

    post_truncated(r);

    rc = MPI_Request_get_status(r[0], &flag, MPI_STATUS_IGNORE);
    expect("get_status: failed request", rc, MPI_ERR_TRUNCATE);
    rc = MPI_Request_get_status(r[0], &flag, MPI_STATUS_IGNORE);
    expect("get_status: failed request, second call", rc, MPI_ERR_TRUNCATE);
    rc = MPI_Request_get_status(r[1], &flag, MPI_STATUS_IGNORE);
    expect("get_status: good request", rc, MPI_SUCCESS);

    rc = MPI_Request_get_status_any(2, r, &index, &flag, MPI_STATUS_IGNORE);
    expect("get_status_any: error", rc, MPI_ERR_TRUNCATE);

    rc = MPI_Request_get_status_all(2, r, &flag, MPI_STATUSES_IGNORE);
    expect("get_status_all/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    rc = MPI_Request_get_status_all(2, r, &flag, st);
    expect("get_status_all/statuses", rc, MPI_ERR_IN_STATUS);
    expect("get_status_all/statuses: failed status", st[0].MPI_ERROR, MPI_ERR_TRUNCATE);
    expect("get_status_all/statuses: good status", st[1].MPI_ERROR, MPI_SUCCESS);

    rc = MPI_Request_get_status_some(2, r, &count, indices, MPI_STATUSES_IGNORE);
    expect("get_status_some/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    rc = MPI_Request_get_status_some(2, r, &count, indices, st);
    expect("get_status_some/statuses", rc, MPI_ERR_IN_STATUS);
    expect("get_status_some/statuses: outcount", count, 2);
    if (2 == count) {
        int k = index_of(count, indices, 0);
        expect("get_status_some/statuses: failed status", k >= 0 ? st[k].MPI_ERROR : -1,
               MPI_ERR_TRUNCATE);
    }

    /* none of them released any request */
    expect("get_status*: requests are still valid",
           MPI_REQUEST_NULL != r[0] && MPI_REQUEST_NULL != r[1], 1);
    rc = MPI_Wait(&r[1], MPI_STATUS_IGNORE);
    expect("Wait on the good request after get_status*", rc, MPI_SUCCESS);
    rc = MPI_Request_get_status_all(2, r, &flag, MPI_STATUSES_IGNORE);
    expect("get_status_all: failed request left", rc, MPI_ERR_IN_STATUS);
    rc = MPI_Wait(&r[0], MPI_STATUS_IGNORE);
    expect("Wait on the failed request after get_status*", rc, MPI_ERR_TRUNCATE);

    /* no error, nothing changes */
    {
        int in = 1, out = -1;
        MPI_Request q[2];

        MPI_Irecv(&out, 1, MPI_INT, 0, 5, MPI_COMM_SELF, &q[0]);
        q[1] = MPI_REQUEST_NULL;
        MPI_Send(&in, 1, MPI_INT, 0, 5, MPI_COMM_SELF);
        do {
            rc = MPI_Request_get_status_all(2, q, &flag, MPI_STATUSES_IGNORE);
        } while (!flag);
        expect("get_status_all: no error", rc, MPI_SUCCESS);
        MPI_Wait(&q[0], MPI_STATUS_IGNORE);
    }
}

/* Generalized requests */

static int free_calls = 0;
static int handler_calls = 0, handler_code = MPI_SUCCESS;

static int query_ok(void *extra, MPI_Status *status)
{
    (void) extra;
    (void) status;
    return MPI_SUCCESS;
}

static int query_failing(void *extra, MPI_Status *status)
{
    (void) extra;
    (void) status;
    return MPI_ERR_TRUNCATE;
}

static int free_ok(void *extra)
{
    (void) extra;
    ++free_calls;
    return MPI_SUCCESS;
}

static int free_failing(void *extra)
{
    (void) extra;
    ++free_calls;
    return MPI_ERR_OTHER;
}

static int cancel_fn(void *extra, int complete)
{
    (void) extra;
    (void) complete;
    return MPI_SUCCESS;
}

static void count_errors(MPI_Comm *comm, int *code, ...)
{
    (void) comm;
    ++handler_calls;
    handler_code = *code;
}

/* A generalized request, completed */
static MPI_Request completed_greq(MPI_Grequest_query_function *query,
                                  MPI_Grequest_free_function *free_fn)
{
    MPI_Request request;

    MPI_Grequest_start(query, free_fn, cancel_fn, NULL, &request);
    MPI_Grequest_complete(request);
    return request;
}

/* [good, free_fn fails, good] */
static void greq_array(MPI_Request requests[MAX_REQS])
{
    free_calls = 0;
    requests[0] = completed_greq(query_ok, free_ok);
    requests[1] = completed_greq(query_ok, free_failing);
    requests[2] = completed_greq(query_ok, free_ok);
}

static void test_greq_free_failure(void)
{
    MPI_Request r, c[1], a[MAX_REQS];
    MPI_Status st[MAX_REQS];
    int rc, flag, count, index, indices[MAX_REQS], k;

    /* single completion: the error of free_fn, released, free_fn called once */
    free_calls = 0;
    r = completed_greq(query_ok, free_failing);
    rc = MPI_Wait(&r, MPI_STATUS_IGNORE);
    expect("greq Wait", rc, MPI_ERR_OTHER);
    expect("greq Wait: handle released", MPI_REQUEST_NULL == r, 1);
    expect("greq Wait: free_fn calls", free_calls, 1);

    free_calls = 0;
    r = completed_greq(query_ok, free_failing);
    rc = MPI_Test(&r, &flag, MPI_STATUS_IGNORE);
    expect("greq Test", rc, MPI_ERR_OTHER);
    expect("greq Test: handle released", MPI_REQUEST_NULL == r, 1);
    expect("greq Test: free_fn calls", free_calls, 1);

    free_calls = 0;
    c[0] = completed_greq(query_ok, free_failing);
    rc = MPI_Waitany(1, c, &index, MPI_STATUS_IGNORE);
    expect("greq Waitany", rc, MPI_ERR_OTHER);
    expect("greq Waitany: handle released", MPI_REQUEST_NULL == c[0], 1);

    free_calls = 0;
    c[0] = completed_greq(query_ok, free_failing);
    rc = MPI_Testany(1, c, &index, &flag, MPI_STATUS_IGNORE);
    expect("greq Testany", rc, MPI_ERR_OTHER);
    expect("greq Testany: handle released", MPI_REQUEST_NULL == c[0], 1);

    free_calls = 0;
    r = completed_greq(query_ok, free_failing);
    rc = MPI_Request_free(&r);
    expect("greq Request_free", rc, MPI_ERR_OTHER);
    expect("greq Request_free: handle released", MPI_REQUEST_NULL == r, 1);
    expect("greq Request_free: free_fn calls", free_calls, 1);

    /* freed before being completed: free_fn runs, and fails, in Grequest_complete */
    free_calls = 0;
    MPI_Grequest_start(query_ok, free_failing, cancel_fn, NULL, &r);
    c[0] = r;
    rc = MPI_Request_free(&r);
    expect("greq Request_free before complete", rc, MPI_SUCCESS);
    expect("greq Request_free before complete: free_fn calls", free_calls, 0);
    rc = MPI_Grequest_complete(c[0]);
    expect("Grequest_complete: free_fn error", rc, MPI_ERR_OTHER);
    expect("Grequest_complete: free_fn calls", free_calls, 1);

    /* query_fn fails (so does free_fn): the error of query_fn is reported */
    free_calls = 0;
    r = completed_greq(query_failing, free_failing);
    rc = MPI_Wait(&r, MPI_STATUS_IGNORE);
    expect("greq Wait, query_fn fails", rc, MPI_ERR_TRUNCATE);
    expect("greq Wait, query_fn fails: handle released", MPI_REQUEST_NULL == r, 1);
    expect("greq Wait, query_fn fails: free_fn calls", free_calls, 1);

    /* arrays: the error is in the status of the failing request, and the
       other requests are processed */
    greq_array(a);
    rc = MPI_Waitall(MAX_REQS, a, st);
    expect("greq Waitall/statuses", rc, MPI_ERR_IN_STATUS);
    expect("greq Waitall/statuses: failed status", st[1].MPI_ERROR, MPI_ERR_OTHER);
    expect("greq Waitall/statuses: good status 0", st[0].MPI_ERROR, MPI_SUCCESS);
    expect("greq Waitall/statuses: good status 2", st[2].MPI_ERROR, MPI_SUCCESS);
    expect("greq Waitall/statuses: handles released", all_null(MAX_REQS, a), 1);
    expect("greq Waitall/statuses: free_fn calls", free_calls, MAX_REQS);

    greq_array(a);
    rc = MPI_Waitall(MAX_REQS, a, MPI_STATUSES_IGNORE);
    expect("greq Waitall/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    expect("greq Waitall/STATUSES_IGNORE: handles released", all_null(MAX_REQS, a), 1);
    expect("greq Waitall/STATUSES_IGNORE: free_fn calls", free_calls, MAX_REQS);

    greq_array(a);
    rc = MPI_Testall(MAX_REQS, a, &flag, st);
    expect("greq Testall/statuses", rc, MPI_ERR_IN_STATUS);
    expect("greq Testall/statuses: failed status", st[1].MPI_ERROR, MPI_ERR_OTHER);
    expect("greq Testall/statuses: handles released", all_null(MAX_REQS, a), 1);
    expect("greq Testall/statuses: free_fn calls", free_calls, MAX_REQS);

    greq_array(a);
    rc = MPI_Testall(MAX_REQS, a, &flag, MPI_STATUSES_IGNORE);
    expect("greq Testall/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    expect("greq Testall/STATUSES_IGNORE: handles released", all_null(MAX_REQS, a), 1);
    expect("greq Testall/STATUSES_IGNORE: free_fn calls", free_calls, MAX_REQS);

    greq_array(a);
    rc = MPI_Waitsome(MAX_REQS, a, &count, indices, st);
    expect("greq Waitsome/statuses", rc, MPI_ERR_IN_STATUS);
    expect("greq Waitsome/statuses: outcount", count, MAX_REQS);
    k = index_of(count, indices, 1);
    expect("greq Waitsome/statuses: failed status", k >= 0 ? st[k].MPI_ERROR : -1, MPI_ERR_OTHER);
    expect("greq Waitsome/statuses: handles released", all_null(MAX_REQS, a), 1);
    expect("greq Waitsome/statuses: free_fn calls", free_calls, MAX_REQS);

    greq_array(a);
    rc = MPI_Waitsome(MAX_REQS, a, &count, indices, MPI_STATUSES_IGNORE);
    expect("greq Waitsome/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    expect("greq Waitsome/STATUSES_IGNORE: handles released", all_null(MAX_REQS, a), 1);
    expect("greq Waitsome/STATUSES_IGNORE: free_fn calls", free_calls, MAX_REQS);

    greq_array(a);
    rc = MPI_Testsome(MAX_REQS, a, &count, indices, st);
    expect("greq Testsome/statuses", rc, MPI_ERR_IN_STATUS);
    expect("greq Testsome/statuses: outcount", count, MAX_REQS);
    k = index_of(count, indices, 1);
    expect("greq Testsome/statuses: failed status", k >= 0 ? st[k].MPI_ERROR : -1, MPI_ERR_OTHER);
    expect("greq Testsome/statuses: handles released", all_null(MAX_REQS, a), 1);
    expect("greq Testsome/statuses: free_fn calls", free_calls, MAX_REQS);

    greq_array(a);
    rc = MPI_Testsome(MAX_REQS, a, &count, indices, MPI_STATUSES_IGNORE);
    expect("greq Testsome/STATUSES_IGNORE", rc, MPI_ERR_IN_STATUS);
    expect("greq Testsome/STATUSES_IGNORE: handles released", all_null(MAX_REQS, a), 1);
    expect("greq Testsome/STATUSES_IGNORE: free_fn calls", free_calls, MAX_REQS);

    /* the success path is unchanged */
    free_calls = 0;
    r = completed_greq(query_ok, free_ok);
    rc = MPI_Wait(&r, MPI_STATUS_IGNORE);
    expect("greq Wait, no error", rc, MPI_SUCCESS);
    expect("greq Wait, no error: handle released", MPI_REQUEST_NULL == r, 1);
    expect("greq Wait, no error: free_fn calls", free_calls, 1);
    free_calls = 0;
    r = completed_greq(query_ok, free_ok);
    rc = MPI_Request_free(&r);
    expect("greq Request_free, no error", rc, MPI_SUCCESS);
    expect("greq Request_free, no error: handle released", MPI_REQUEST_NULL == r, 1);
}

/* The error handler is invoked once, with the error of the callback */
static void test_greq_error_handler(void)
{
    MPI_Request r, a[MAX_REQS];
    MPI_Errhandler handler;
    int rc;

    MPI_Comm_create_errhandler(count_errors, &handler);
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, handler);
    MPI_Comm_set_errhandler(MPI_COMM_SELF, handler);

    handler_calls = 0;
    handler_code = MPI_SUCCESS;
    r = completed_greq(query_ok, free_failing);
    rc = MPI_Wait(&r, MPI_STATUS_IGNORE);
    expect("handler: Wait", rc, MPI_ERR_OTHER);
    expect("handler: calls", handler_calls, 1);
    expect("handler: error code", handler_code, MPI_ERR_OTHER);

    handler_calls = 0;
    greq_array(a);
    rc = MPI_Waitall(MAX_REQS, a, MPI_STATUSES_IGNORE);
    expect("handler: Waitall", rc, MPI_ERR_IN_STATUS);
    expect("handler: Waitall calls", handler_calls, 1);

    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_RETURN);
    MPI_Errhandler_free(&handler);
}

/* The error handler is invoked once with the error of the failing request,
   never with MPI_ERR_IN_STATUS, whichever function reports it */
#define HANDLER_CHECK(name, call, want)                                 \
    do {                                                                \
        handler_calls = 0;                                              \
        handler_code = MPI_SUCCESS;                                     \
        (void) (call);                                                  \
        expect(name ": handler calls", handler_calls, 1);               \
        expect(name ": handler code", handler_code, (want));            \
    } while (0)

#define TRUNCATED_CHECK(name, call)                                     \
    do {                                                                \
        post_truncated(r);                                              \
        HANDLER_CHECK(name, call, MPI_ERR_TRUNCATE);                    \
        drain(r);                                                       \
    } while (0)

#define GREQ_CHECK(name, call)                                          \
    do {                                                                \
        greq_array(a);                                                  \
        HANDLER_CHECK(name, call, MPI_ERR_OTHER);                       \
    } while (0)

static void test_handler_codes(void)
{
    MPI_Request r[2], a[MAX_REQS], single;
    MPI_Status st[MAX_REQS];
    MPI_Errhandler handler;
    int flag, count, index, indices[MAX_REQS];

    MPI_Comm_create_errhandler(count_errors, &handler);
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, handler);
    MPI_Comm_set_errhandler(MPI_COMM_SELF, handler);

    /* receives that fail with a truncation */
    TRUNCATED_CHECK("truncated Wait", MPI_Wait(&r[0], MPI_STATUS_IGNORE));
    TRUNCATED_CHECK("truncated Test", MPI_Test(&r[0], &flag, MPI_STATUS_IGNORE));
    TRUNCATED_CHECK("truncated Waitany", MPI_Waitany(2, r, &index, MPI_STATUS_IGNORE));
    TRUNCATED_CHECK("truncated Testany", MPI_Testany(2, r, &index, &flag, MPI_STATUS_IGNORE));
    TRUNCATED_CHECK("truncated Waitall/statuses", MPI_Waitall(2, r, st));
    TRUNCATED_CHECK("truncated Waitall/IGNORE", MPI_Waitall(2, r, MPI_STATUSES_IGNORE));
    TRUNCATED_CHECK("truncated Testall/statuses", MPI_Testall(2, r, &flag, st));
    TRUNCATED_CHECK("truncated Testall/IGNORE", MPI_Testall(2, r, &flag, MPI_STATUSES_IGNORE));
    TRUNCATED_CHECK("truncated Waitsome/statuses", MPI_Waitsome(2, r, &count, indices, st));
    TRUNCATED_CHECK("truncated Waitsome/IGNORE",
                    MPI_Waitsome(2, r, &count, indices, MPI_STATUSES_IGNORE));
    TRUNCATED_CHECK("truncated Testsome/statuses", MPI_Testsome(2, r, &count, indices, st));
    TRUNCATED_CHECK("truncated Testsome/IGNORE",
                    MPI_Testsome(2, r, &count, indices, MPI_STATUSES_IGNORE));
    TRUNCATED_CHECK("truncated get_status", MPI_Request_get_status(r[0], &flag, MPI_STATUS_IGNORE));
    TRUNCATED_CHECK("truncated get_status_any",
                    MPI_Request_get_status_any(2, r, &index, &flag, MPI_STATUS_IGNORE));
    TRUNCATED_CHECK("truncated get_status_some/statuses",
                    MPI_Request_get_status_some(2, r, &count, indices, st));
    TRUNCATED_CHECK("truncated get_status_some/IGNORE",
                    MPI_Request_get_status_some(2, r, &count, indices, MPI_STATUSES_IGNORE));
    TRUNCATED_CHECK("truncated get_status_all/statuses",
                    MPI_Request_get_status_all(2, r, &flag, st));
    TRUNCATED_CHECK("truncated get_status_all/IGNORE",
                    MPI_Request_get_status_all(2, r, &flag, MPI_STATUSES_IGNORE));

    /* generalized requests whose free_fn fails, which are released */
    single = completed_greq(query_ok, free_failing);
    HANDLER_CHECK("greq Wait", MPI_Wait(&single, MPI_STATUS_IGNORE), MPI_ERR_OTHER);
    single = completed_greq(query_ok, free_failing);
    HANDLER_CHECK("greq Test", MPI_Test(&single, &flag, MPI_STATUS_IGNORE), MPI_ERR_OTHER);
    single = completed_greq(query_ok, free_failing);
    HANDLER_CHECK("greq Waitany", MPI_Waitany(1, &single, &index, MPI_STATUS_IGNORE), MPI_ERR_OTHER);
    single = completed_greq(query_ok, free_failing);
    HANDLER_CHECK("greq Testany",
                  MPI_Testany(1, &single, &index, &flag, MPI_STATUS_IGNORE), MPI_ERR_OTHER);
    single = completed_greq(query_ok, free_failing);
    HANDLER_CHECK("greq Request_free", MPI_Request_free(&single), MPI_ERR_OTHER);
    GREQ_CHECK("greq Waitall/statuses", MPI_Waitall(MAX_REQS, a, st));
    GREQ_CHECK("greq Waitall/IGNORE", MPI_Waitall(MAX_REQS, a, MPI_STATUSES_IGNORE));
    GREQ_CHECK("greq Testall/statuses", MPI_Testall(MAX_REQS, a, &flag, st));
    GREQ_CHECK("greq Testall/IGNORE", MPI_Testall(MAX_REQS, a, &flag, MPI_STATUSES_IGNORE));
    GREQ_CHECK("greq Waitsome/statuses", MPI_Waitsome(MAX_REQS, a, &count, indices, st));
    GREQ_CHECK("greq Waitsome/IGNORE",
               MPI_Waitsome(MAX_REQS, a, &count, indices, MPI_STATUSES_IGNORE));
    GREQ_CHECK("greq Testsome/statuses", MPI_Testsome(MAX_REQS, a, &count, indices, st));
    GREQ_CHECK("greq Testsome/IGNORE",
               MPI_Testsome(MAX_REQS, a, &count, indices, MPI_STATUSES_IGNORE));

    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_RETURN);
    MPI_Errhandler_free(&handler);
}

int main(int argc, char *argv[])
{
    MPI_Init(&argc, &argv);
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_RETURN);

    test_truncated_completion();
    test_truncated_get_status();
    test_greq_free_failure();
    test_greq_error_handler();
    test_handler_codes();

    MPI_Finalize();
    if (0 != failures) {
        fprintf(stderr, "request_errors: %d check(s) failed\n", failures);
        return EXIT_FAILURE;
    }
    printf("request_errors: all checks passed\n");
    return EXIT_SUCCESS;
}
