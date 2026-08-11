/*
 * Copyright (c) 2026      Joseph Antony.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for notified RMA communication (MPI-5.1 section 12.6): all
 * eight notified communication operations, the notification counter
 * management calls, and the counter accessors.
 *
 * Single process on MPI_COMM_SELF, so the target of every operation is
 * this process itself.  That is enough to pin down the semantics this test
 * cares about -- that each notified operation moves the data its
 * non-notified counterpart would, and then increments exactly one
 * notification counter by exactly one -- without needing a launcher.
 *
 * osc/sm is forced because it is currently the only osc component that
 * implements the notified operations; on any other component the module's
 * notify function pointers are NULL.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "support.h"

#include "mpi.h"

#define WIN_COUNT 8
#define NUM_NOTIFY 4

/* Comfortably more than the osc_sm_num_notify_counters default of 16, so that
 * asking for this many forces the counters to be reallocated. */
#define NUM_NOTIFY_GROWN 100

static void test_counter_management(void);
static void test_blocking_ops(void);
static void test_request_ops(void);
static void test_notify_idx_errors(void);
static void test_counter_growth(void);
static void test_max_num_notify_assertion(void);
static void test_notify_attributes(void);

/* Read notification counter "idx" and check it against "expect". */
static void check_counter(MPI_Win win, int idx, MPI_Count expect,
                          const char *what)
{
    MPI_Count value = -1;
    int rc = MPI_Win_get_notify_value(win, idx, &value);
    test_verify(what, MPI_SUCCESS == rc && expect == value);
}

int main(int argc, char *argv[])
{
    /* Must be set before MPI_Init: component selection happens there. */
    setenv("OMPI_MCA_osc", "sm", 1);

    test_init("ompi win_notify");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_counter_management();
    test_blocking_ops();
    test_request_ops();
    test_notify_idx_errors();
    test_counter_growth();
    test_max_num_notify_assertion();
    test_notify_attributes();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

/* MPI-5.1 section 12.6.1: MPI_WIN_SET_NUM_NOTIFY / MPI_WIN_GET_NUM_NOTIFY,
 * and the reset-to-zero behavior of the former. */
static void test_counter_management(void)
{
    int *base = NULL;
    MPI_Win win = MPI_WIN_NULL;

    int rc = MPI_Win_allocate(WIN_COUNT * sizeof(int), sizeof(int),
                              MPI_INFO_NULL, MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate succeeds", MPI_SUCCESS == rc);
    if (MPI_SUCCESS != rc) {
        return;
    }

    /* MPI-5.1 section 12.6.1 does not state how many counters are attached
     * before the first MPI_WIN_SET_NUM_NOTIFY, and osc/sm and osc/ucx
     * currently disagree (osc/sm pre-attaches its full reserved capacity,
     * osc/ucx starts at zero -- see the note in osc_sm_component.c).  So
     * only require that the query works and reports something sane; a
     * portable program must call MPI_WIN_SET_NUM_NOTIFY first regardless. */
    int num = -1;
    rc = MPI_Win_get_num_notify(win, 0, &num);
    test_verify("Win_get_num_notify succeeds before set", MPI_SUCCESS == rc);
    test_verify("initial attached count is non-negative", num >= 0);

    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, NUM_NOTIFY);
    test_verify("Win_set_num_notify succeeds", MPI_SUCCESS == rc);

    /* "A subsequent call to MPI_WIN_GET_NUM_NOTIFY will return the value
     * given to MPI_WIN_SET_NUM_NOTIFY." */
    num = -1;
    rc = MPI_Win_get_num_notify(win, 0, &num);
    test_verify("Win_get_num_notify returns what was set",
                MPI_SUCCESS == rc && NUM_NOTIFY == num);

    /* All counters start at zero. */
    for (int i = 0; i < NUM_NOTIFY; ++i) {
        check_counter(win, i, 0, "counter is zero after set_num_notify");
    }

    /* Bump a counter, then check that set_num_notify resets it: "All
     * notification counters (both existing and newly attached) are reset to
     * zero by this call." */
    MPI_Win_lock_all(0, win);
    int src = 1;
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, 2, win);
    test_verify("Put_notify succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    check_counter(win, 2, 1, "counter advanced before reset");
    MPI_Win_unlock_all(win);

    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, NUM_NOTIFY);
    test_verify("Win_set_num_notify succeeds again", MPI_SUCCESS == rc);
    check_counter(win, 2, 0, "set_num_notify resets existing counters");

    /* MPI_WIN_RESET_NOTIFY_VALUE is an atomic fetch-and-zero. */
    MPI_Win_lock_all(0, win);
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, 1, win);
    test_verify("Put_notify succeeds for reset test", MPI_SUCCESS == rc);
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, 1, win);
    test_verify("second Put_notify succeeds for reset test", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    MPI_Win_unlock_all(win);

    MPI_Count value = -1;
    rc = MPI_Win_reset_notify_value(win, 1, &value);
    test_verify("Win_reset_notify_value returns the prior value",
                MPI_SUCCESS == rc && 2 == value);
    check_counter(win, 1, 0, "Win_reset_notify_value zeroes the counter");

    MPI_Win_free(&win);
}

/* The four blocking notified operations. */
static void test_blocking_ops(void)
{
    int *base = NULL;
    MPI_Win win = MPI_WIN_NULL;

    int rc = MPI_Win_allocate(WIN_COUNT * sizeof(int), sizeof(int),
                              MPI_INFO_NULL, MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate succeeds (blocking ops)", MPI_SUCCESS == rc);
    if (MPI_SUCCESS != rc) {
        return;
    }
    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, NUM_NOTIFY);
    test_verify("Win_set_num_notify succeeds (blocking ops)", MPI_SUCCESS == rc);

    memset(base, 0, WIN_COUNT * sizeof(int));

    /* Notified operations are permitted only during a passive target
     * epoch (MPI-5.1 section 12.3). */
    MPI_Win_lock_all(0, win);

    /* --- MPI_PUT_NOTIFY --- */
    int src = 42;
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, 0, win);
    test_verify("Put_notify succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Put_notify moved the data", 42 == base[0]);
    check_counter(win, 0, 1, "Put_notify incremented its counter by one");

    /* --- MPI_GET_NOTIFY --- */
    int dst = 0;
    rc = MPI_Get_notify(&dst, 1, MPI_INT, 0, 0, 1, MPI_INT, 0, win);
    test_verify("Get_notify succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Get_notify moved the data", 42 == dst);
    check_counter(win, 0, 2, "Get_notify incremented its counter by one");

    /* --- MPI_ACCUMULATE_NOTIFY --- */
    src = 8;
    rc = MPI_Accumulate_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, MPI_SUM,
                               1, win);
    test_verify("Accumulate_notify succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Accumulate_notify applied the op", 50 == base[0]);
    check_counter(win, 1, 1, "Accumulate_notify incremented its counter by one");
    check_counter(win, 0, 2, "Accumulate_notify left other counters alone");

    /* MPI_REPLACE takes the other branch in the osc/sm accumulate path. */
    src = 7;
    rc = MPI_Accumulate_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, MPI_REPLACE,
                               1, win);
    test_verify("Accumulate_notify with MPI_REPLACE succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Accumulate_notify applied MPI_REPLACE", 7 == base[0]);
    check_counter(win, 1, 2, "Accumulate_notify/REPLACE incremented its counter");

    /* --- MPI_GET_ACCUMULATE_NOTIFY --- */
    src = 3;
    int result = -1;
    rc = MPI_Get_accumulate_notify(&src, 1, MPI_INT, &result, 1, MPI_INT,
                                   0, 0, 1, MPI_INT, MPI_SUM, 2, win);
    test_verify("Get_accumulate_notify succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Get_accumulate_notify fetched the prior value", 7 == result);
    test_verify("Get_accumulate_notify applied the op", 10 == base[0]);
    check_counter(win, 2, 1,
                  "Get_accumulate_notify incremented its counter by one");

    /* MPI_NO_OP fetches without modifying, and still notifies: the window
     * was read, which is an access the notification covers. */
    result = -1;
    rc = MPI_Get_accumulate_notify(&src, 1, MPI_INT, &result, 1, MPI_INT,
                                   0, 0, 1, MPI_INT, MPI_NO_OP, 2, win);
    test_verify("Get_accumulate_notify with MPI_NO_OP succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Get_accumulate_notify/NO_OP fetched the value", 10 == result);
    test_verify("Get_accumulate_notify/NO_OP left the window alone", 10 == base[0]);
    check_counter(win, 2, 2,
                  "Get_accumulate_notify/NO_OP incremented its counter");

    MPI_Win_unlock_all(win);
    MPI_Win_free(&win);
}

/* The four request-based notified operations.  Completion of the request
 * indicates completion at the origin (MPI-5.1 section 12.6.4). */
static void test_request_ops(void)
{
    int *base = NULL;
    MPI_Win win = MPI_WIN_NULL;

    int rc = MPI_Win_allocate(WIN_COUNT * sizeof(int), sizeof(int),
                              MPI_INFO_NULL, MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate succeeds (request ops)", MPI_SUCCESS == rc);
    if (MPI_SUCCESS != rc) {
        return;
    }
    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, NUM_NOTIFY);
    test_verify("Win_set_num_notify succeeds (request ops)", MPI_SUCCESS == rc);

    memset(base, 0, WIN_COUNT * sizeof(int));

    MPI_Win_lock_all(0, win);

    MPI_Request req = MPI_REQUEST_NULL;

    /* --- MPI_RPUT_NOTIFY --- */
    int src = 42;
    rc = MPI_Rput_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, 0, win, &req);
    test_verify("Rput_notify succeeds", MPI_SUCCESS == rc);
    rc = MPI_Wait(&req, MPI_STATUS_IGNORE);
    test_verify("Wait on Rput_notify request succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Rput_notify moved the data", 42 == base[0]);
    check_counter(win, 0, 1, "Rput_notify incremented its counter by one");

    /* --- MPI_RGET_NOTIFY --- */
    int dst = 0;
    rc = MPI_Rget_notify(&dst, 1, MPI_INT, 0, 0, 1, MPI_INT, 0, win, &req);
    test_verify("Rget_notify succeeds", MPI_SUCCESS == rc);
    rc = MPI_Wait(&req, MPI_STATUS_IGNORE);
    test_verify("Wait on Rget_notify request succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Rget_notify moved the data", 42 == dst);
    check_counter(win, 0, 2, "Rget_notify incremented its counter by one");

    /* --- MPI_RACCUMULATE_NOTIFY --- */
    src = 8;
    rc = MPI_Raccumulate_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, MPI_SUM,
                                1, win, &req);
    test_verify("Raccumulate_notify succeeds", MPI_SUCCESS == rc);
    rc = MPI_Wait(&req, MPI_STATUS_IGNORE);
    test_verify("Wait on Raccumulate_notify request succeeds", MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Raccumulate_notify applied the op", 50 == base[0]);
    check_counter(win, 1, 1, "Raccumulate_notify incremented its counter by one");

    /* --- MPI_RGET_ACCUMULATE_NOTIFY --- */
    src = 3;
    int result = -1;
    rc = MPI_Rget_accumulate_notify(&src, 1, MPI_INT, &result, 1, MPI_INT,
                                    0, 0, 1, MPI_INT, MPI_SUM, 2, win, &req);
    test_verify("Rget_accumulate_notify succeeds", MPI_SUCCESS == rc);
    rc = MPI_Wait(&req, MPI_STATUS_IGNORE);
    test_verify("Wait on Rget_accumulate_notify request succeeds",
                MPI_SUCCESS == rc);
    MPI_Win_flush(0, win);
    test_verify("Rget_accumulate_notify fetched the prior value", 50 == result);
    test_verify("Rget_accumulate_notify applied the op", 53 == base[0]);
    check_counter(win, 2, 1,
                  "Rget_accumulate_notify incremented its counter by one");

    MPI_Win_unlock_all(win);
    MPI_Win_free(&win);
}

/* MPI-5.1 section 12.6: "Initiating a notified communication operation that
 * references a notification counter that is out of range at the target is
 * erroneous", reported as MPI_ERR_RMA_NOTIFICATION. */
static void test_notify_idx_errors(void)
{
    int *base = NULL;
    MPI_Win win = MPI_WIN_NULL;

    int rc = MPI_Win_allocate(WIN_COUNT * sizeof(int), sizeof(int),
                              MPI_INFO_NULL, MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate succeeds (error cases)", MPI_SUCCESS == rc);
    if (MPI_SUCCESS != rc) {
        return;
    }
    MPI_Win_set_errhandler(win, MPI_ERRORS_RETURN);

    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, NUM_NOTIFY);
    test_verify("Win_set_num_notify succeeds (error cases)", MPI_SUCCESS == rc);

    memset(base, 0, WIN_COUNT * sizeof(int));

    MPI_Win_lock_all(0, win);

    int src = 1;
    int result = 0;
    MPI_Request req = MPI_REQUEST_NULL;

    /* Negative index: rejected by the binding's parameter check. */
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, -1, win);
    test_verify("Put_notify rejects a negative index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Accumulate_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, MPI_SUM,
                               -1, win);
    test_verify("Accumulate_notify rejects a negative index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Get_accumulate_notify(&src, 1, MPI_INT, &result, 1, MPI_INT,
                                   0, 0, 1, MPI_INT, MPI_SUM, -1, win);
    test_verify("Get_accumulate_notify rejects a negative index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Rput_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, -1, win, &req);
    test_verify("Rput_notify rejects a negative index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Rget_notify(&result, 1, MPI_INT, 0, 0, 1, MPI_INT, -1, win, &req);
    test_verify("Rget_notify rejects a negative index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Raccumulate_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, MPI_SUM,
                                -1, win, &req);
    test_verify("Raccumulate_notify rejects a negative index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Rget_accumulate_notify(&src, 1, MPI_INT, &result, 1, MPI_INT,
                                    0, 0, 1, MPI_INT, MPI_SUM, -1, win, &req);
    test_verify("Rget_accumulate_notify rejects a negative index",
                MPI_ERR_RMA_NOTIFICATION == rc);

    /* Index at or past the target's attached count: rejected by the osc
     * module, which is the only layer that knows the target's count. */
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, NUM_NOTIFY, win);
    test_verify("Put_notify rejects an out-of-range index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Accumulate_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, MPI_SUM,
                               NUM_NOTIFY, win);
    test_verify("Accumulate_notify rejects an out-of-range index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Get_accumulate_notify(&src, 1, MPI_INT, &result, 1, MPI_INT,
                                   0, 0, 1, MPI_INT, MPI_SUM, NUM_NOTIFY, win);
    test_verify("Get_accumulate_notify rejects an out-of-range index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Raccumulate_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, MPI_SUM,
                                NUM_NOTIFY, win, &req);
    test_verify("Raccumulate_notify rejects an out-of-range index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Rget_accumulate_notify(&src, 1, MPI_INT, &result, 1, MPI_INT,
                                    0, 0, 1, MPI_INT, MPI_SUM, NUM_NOTIFY,
                                    win, &req);
    test_verify("Rget_accumulate_notify rejects an out-of-range index",
                MPI_ERR_RMA_NOTIFICATION == rc);

    /* An operation rejected for its index must not have touched the
     * window, and must not have notified. */
    test_verify("a rejected notified operation does not touch the window",
                0 == base[0]);
    for (int i = 0; i < NUM_NOTIFY; ++i) {
        check_counter(win, i, 0,
                      "a rejected notified operation does not notify");
    }

    /* The accessors validate their index too. */
    MPI_Count value = -1;
    rc = MPI_Win_get_notify_value(win, NUM_NOTIFY, &value);
    test_verify("Win_get_notify_value rejects an out-of-range index",
                MPI_ERR_RMA_NOTIFICATION == rc);
    rc = MPI_Win_reset_notify_value(win, NUM_NOTIFY, &value);
    test_verify("Win_reset_notify_value rejects an out-of-range index",
                MPI_ERR_RMA_NOTIFICATION == rc);

    MPI_Win_unlock_all(win);
    MPI_Win_free(&win);
}

/* ------------------------------------------------------------------ */

/* MPI-5.1 section 12.2 defines the mpi_assert_max_num_notify info key with a
 * default of 0, meaning "the implementation does not assume any limit on the
 * number of notification counters".  A window created without the key must
 * therefore honour a request for more counters than osc/sm reserves up front,
 * which it does by moving the counters to a larger allocation. */
static void test_counter_growth(void)
{
    int *base = NULL;
    MPI_Win win = MPI_WIN_NULL;
    int rc;

    rc = MPI_Win_allocate(WIN_COUNT * sizeof(int), sizeof(int), MPI_INFO_NULL,
                          MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate succeeds (growth)", MPI_SUCCESS == rc);
    MPI_Win_set_errhandler(win, MPI_ERRORS_RETURN);
    memset(base, 0, WIN_COUNT * sizeof(int));

    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, NUM_NOTIFY_GROWN);
    test_verify("Win_set_num_notify grows past the reserved capacity",
                MPI_SUCCESS == rc);

    int num = -1;
    rc = MPI_Win_get_num_notify(win, 0, &num);
    test_verify("Win_get_num_notify returns the grown count",
                MPI_SUCCESS == rc && NUM_NOTIFY_GROWN == num);

    /* Every counter in the grown range must exist and read as zero. */
    for (int i = 0; i < NUM_NOTIFY_GROWN; ++i) {
        check_counter(win, i, 0, "grown counter is zero");
    }

    MPI_Win_lock_all(0, win);

    /* An index only reachable after the growth must actually work end to end:
     * the operation moves data and lands on the right counter. */
    int src = 99;
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT,
                        NUM_NOTIFY_GROWN - 1, win);
    test_verify("Put_notify succeeds on a counter that only growth provided",
                MPI_SUCCESS == rc);
    test_verify("Put_notify moved the data after growth", 99 == base[0]);
    check_counter(win, NUM_NOTIFY_GROWN - 1, 1,
                  "the grown counter advanced by one");
    check_counter(win, 0, 0, "the grown counter did not disturb its neighbours");

    /* One past the grown range is still out of range. */
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT,
                        NUM_NOTIFY_GROWN, win);
    test_verify("Put_notify still rejects an index past the grown count",
                MPI_ERR_RMA_NOTIFICATION == rc);

    MPI_Win_unlock_all(win);
    MPI_Win_free(&win);
}

/* A non-zero mpi_assert_max_num_notify is the user promising not to ask for
 * more counters than that.  osc/sm reserves exactly that many and holds the
 * user to the promise rather than silently reallocating. */
static void test_max_num_notify_assertion(void)
{
    int *base = NULL;
    MPI_Win win = MPI_WIN_NULL;
    MPI_Info info = MPI_INFO_NULL;
    int rc;

    MPI_Info_create(&info);
    MPI_Info_set(info, "mpi_assert_max_num_notify", "8");

    rc = MPI_Win_allocate(WIN_COUNT * sizeof(int), sizeof(int), info,
                          MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate succeeds with mpi_assert_max_num_notify",
                MPI_SUCCESS == rc);
    MPI_Info_free(&info);
    MPI_Win_set_errhandler(win, MPI_ERRORS_RETURN);
    memset(base, 0, WIN_COUNT * sizeof(int));

    /* Up to the asserted bound is fine. */
    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, 8);
    test_verify("Win_set_num_notify accepts the asserted maximum",
                MPI_SUCCESS == rc);

    int num = -1;
    rc = MPI_Win_get_num_notify(win, 0, &num);
    test_verify("Win_get_num_notify returns the asserted maximum",
                MPI_SUCCESS == rc && 8 == num);

    /* Past it is an error rather than a reallocation: the window was sized on
     * the strength of the assertion. */
    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, 9);
    test_verify("Win_set_num_notify refuses to exceed the asserted maximum",
                MPI_ERR_ARG == rc);

    /* The refused call must not have disturbed the counters that do exist. */
    rc = MPI_Win_get_num_notify(win, 0, &num);
    test_verify("a refused Win_set_num_notify leaves the count alone",
                MPI_SUCCESS == rc && 8 == num);

    MPI_Win_lock_all(0, win);
    int src = 7;
    rc = MPI_Put_notify(&src, 1, MPI_INT, 0, 0, 1, MPI_INT, 7, win);
    test_verify("Put_notify works on the last asserted counter",
                MPI_SUCCESS == rc);
    check_counter(win, 7, 1, "the last asserted counter advanced");
    MPI_Win_unlock_all(win);

    MPI_Win_free(&win);
}

/* ------------------------------------------------------------------ */

/* MPI-5.1 section 12.2.6, Table 12.1: the three notification bounds are cached
 * on every window at creation.  NUM_SB and NUM_UB are int *, VALUE_UB is
 * MPI_Count *. */
static void test_notify_attributes(void)
{
    int *num_sb = NULL, *num_ub = NULL;
    MPI_Count *value_ub = NULL;
    int *base = NULL;
    MPI_Win win = MPI_WIN_NULL;
    MPI_Info info = MPI_INFO_NULL;
    int flag = 0, rc;

    /* Without an assertion the reservation bounds what is served without
     * reallocation, and nothing bounds what may be requested. */
    rc = MPI_Win_allocate(WIN_COUNT * sizeof(int), sizeof(int), MPI_INFO_NULL,
                          MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate succeeds (attributes)", MPI_SUCCESS == rc);
    MPI_Win_set_errhandler(win, MPI_ERRORS_RETURN);

    rc = MPI_Win_get_attr(win, MPI_WIN_NOTIFICATION_NUM_SB, &num_sb, &flag);
    test_verify("MPI_WIN_NOTIFICATION_NUM_SB is present",
                MPI_SUCCESS == rc && flag && NULL != num_sb);
    test_verify("MPI_WIN_NOTIFICATION_NUM_SB is positive", *num_sb > 0);

    rc = MPI_Win_get_attr(win, MPI_WIN_NOTIFICATION_NUM_UB, &num_ub, &flag);
    test_verify("MPI_WIN_NOTIFICATION_NUM_UB is present",
                MPI_SUCCESS == rc && flag && NULL != num_ub);
    test_verify("NUM_UB is unbounded when no assertion was given",
                INT_MAX == *num_ub);

    /* A suggested maximum above the hard maximum would be nonsense. */
    test_verify("NUM_SB does not exceed NUM_UB", *num_sb <= *num_ub);

    rc = MPI_Win_get_attr(win, MPI_WIN_NOTIFICATION_VALUE_UB, &value_ub, &flag);
    test_verify("MPI_WIN_NOTIFICATION_VALUE_UB is present",
                MPI_SUCCESS == rc && flag && NULL != value_ub);
    test_verify("VALUE_UB is the full range of the counter type",
                INT64_MAX == *value_ub);

    /* Asking for exactly NUM_SB counters must not need a reallocation, and
     * must be accepted. */
    int sb = *num_sb;
    rc = MPI_Win_set_num_notify(win, MPI_INFO_NULL, sb);
    test_verify("Win_set_num_notify accepts NUM_SB counters", MPI_SUCCESS == rc);

    MPI_Win_free(&win);

    /* With an assertion, both bounds collapse onto the asserted value. */
    MPI_Info_create(&info);
    MPI_Info_set(info, "mpi_assert_max_num_notify", "8");
    rc = MPI_Win_allocate(WIN_COUNT * sizeof(int), sizeof(int), info,
                          MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate succeeds (asserted attributes)", MPI_SUCCESS == rc);
    MPI_Info_free(&info);
    MPI_Win_set_errhandler(win, MPI_ERRORS_RETURN);

    rc = MPI_Win_get_attr(win, MPI_WIN_NOTIFICATION_NUM_UB, &num_ub, &flag);
    test_verify("NUM_UB reports the asserted maximum",
                MPI_SUCCESS == rc && flag && 8 == *num_ub);

    rc = MPI_Win_get_attr(win, MPI_WIN_NOTIFICATION_NUM_SB, &num_sb, &flag);
    test_verify("NUM_SB reports the asserted maximum",
                MPI_SUCCESS == rc && flag && 8 == *num_sb);

    MPI_Win_free(&win);
}
