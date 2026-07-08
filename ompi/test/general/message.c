/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/message layer (message.c): the MPI_Message
 * object produced by matched probe and consumed by matched receive.
 * Driven single-process by a self-send on MPI_COMM_SELF plus the
 * MPI_MESSAGE_NO_PROC special case.  (Matched probe/recv against other
 * ranks belongs to the np>1 effort.)
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include "support.h"

#include "mpi.h"

static void test_no_pending(void);
static void test_self_matched(void);
static void test_no_proc(void);

int main(int argc, char *argv[])
{
    test_init("ompi message");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_no_pending();
    test_self_matched();
    test_no_proc();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_no_pending(void)
{
    int flag = 1;
    MPI_Message msg = (MPI_Message) 0xdead;
    MPI_Status status;
    int rc = MPI_Improbe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_SELF,
                         &flag, &msg, &status);
    test_verify("Improbe with nothing pending succeeds", MPI_SUCCESS == rc);
    test_verify("Improbe reports no match", 0 == flag);
}

/* ------------------------------------------------------------------ */

static void test_self_matched(void)
{
    int sbuf = 42;
    MPI_Request sreq = MPI_REQUEST_NULL;
    int rc = MPI_Isend(&sbuf, 1, MPI_INT, 0, 7, MPI_COMM_SELF, &sreq);
    test_verify("self Isend succeeds", MPI_SUCCESS == rc);

    /* Bounded nonblocking matched probe drives progress without risking a
     * hang. */
    int flag = 0;
    MPI_Message msg = MPI_MESSAGE_NULL;
    MPI_Status status;
    for (int i = 0; i < 1000000 && !flag; ++i) {
        MPI_Improbe(0, 7, MPI_COMM_SELF, &flag, &msg, &status);
    }
    test_verify("Improbe matched the self-send", 1 == flag);
    test_verify("matched message is not MPI_MESSAGE_NULL", MPI_MESSAGE_NULL != msg);

    /* If the probe never matched (it always should for a COMM_SELF
     * self-send), the failure is already recorded above.  Bail out rather
     * than calling Mrecv with MPI_MESSAGE_NULL, which the default fatal
     * error handler would turn into a process abort that hides the real
     * failure. */
    if (!flag) {
        return;
    }

    int rbuf = -1;
    rc = MPI_Mrecv(&rbuf, 1, MPI_INT, &msg, &status);
    test_verify("Mrecv succeeds", MPI_SUCCESS == rc);
    test_verify("Mrecv received the sent value", 42 == rbuf);
    test_verify("Mrecv NULLs the message handle", MPI_MESSAGE_NULL == msg);

    MPI_Wait(&sreq, MPI_STATUS_IGNORE);
}

/* ------------------------------------------------------------------ */

static void test_no_proc(void)
{
    /* Matched receive from MPI_MESSAGE_NO_PROC is an empty receive. */
    MPI_Message msg = MPI_MESSAGE_NO_PROC;
    MPI_Status status;
    int rc = MPI_Mrecv(NULL, 0, MPI_INT, &msg, &status);
    test_verify("Mrecv(MPI_MESSAGE_NO_PROC) succeeds", MPI_SUCCESS == rc);
    test_verify("MESSAGE_NO_PROC source is MPI_PROC_NULL", MPI_PROC_NULL == status.MPI_SOURCE);

    int count = -1;
    MPI_Get_count(&status, MPI_INT, &count);
    test_verify("MESSAGE_NO_PROC receive count is 0", 0 == count);
}
