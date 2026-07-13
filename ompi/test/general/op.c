/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/op layer (op.c).  ompi_op_init() (which builds
 * the datatype->op map and all 15 intrinsic ops) runs during MPI_Init,
 * so a full singleton init plus the public MPI_Op API exercises the
 * bulk of op.c, including user-op creation (ompi_op_create_user) and the
 * reduction dispatch (MPI_Reduce_local).
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include "support.h"

#include "mpi.h"

static void test_predefined_commutative(void);
static void test_reduce_local_intrinsic(void);
static void test_user_op(void);

int main(int argc, char *argv[])
{
    test_init("ompi op");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_predefined_commutative();
    test_reduce_local_intrinsic();
    test_user_op();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_predefined_commutative(void)
{
    int flag = -1;
    MPI_Op_commutative(MPI_SUM, &flag);
    test_verify("MPI_SUM is commutative", 1 == flag);

    flag = -1;
    MPI_Op_commutative(MPI_MAX, &flag);
    test_verify("MPI_MAX is commutative", 1 == flag);

    flag = -1;
    MPI_Op_commutative(MPI_PROD, &flag);
    test_verify("MPI_PROD is commutative", 1 == flag);

    /* MPI_REPLACE and MPI_NO_OP exist and are usable handles */
    test_verify("MPI_REPLACE handle is non-NULL", MPI_OP_NULL != MPI_REPLACE);
    test_verify("MPI_NO_OP handle is non-NULL", MPI_OP_NULL != MPI_NO_OP);
}

/* ------------------------------------------------------------------ */

static void test_reduce_local_intrinsic(void)
{
    /* MPI_SUM */
    int in = 2, inout = 3;
    int rc = MPI_Reduce_local(&in, &inout, 1, MPI_INT, MPI_SUM);
    test_verify("Reduce_local MPI_SUM succeeds", MPI_SUCCESS == rc);
    test_verify("MPI_SUM: 2 + 3 == 5", 5 == inout);

    /* MPI_MAX */
    in = 2; inout = 3;
    MPI_Reduce_local(&in, &inout, 1, MPI_INT, MPI_MAX);
    test_verify("MPI_MAX: max(2,3) == 3", 3 == inout);
    in = 5; inout = 3;
    MPI_Reduce_local(&in, &inout, 1, MPI_INT, MPI_MAX);
    test_verify("MPI_MAX: max(5,3) == 5", 5 == inout);

    /* MPI_PROD over multiple elements */
    int va[3] = {1, 2, 3};
    int vb[3] = {4, 5, 6};
    MPI_Reduce_local(va, vb, 3, MPI_INT, MPI_PROD);
    test_verify("MPI_PROD elementwise", 4 == vb[0] && 10 == vb[1] && 18 == vb[2]);

    /* MPI_LAND on logical-ish ints */
    int la = 1, lb = 0;
    MPI_Reduce_local(&la, &lb, 1, MPI_INT, MPI_LAND);
    test_verify("MPI_LAND: 1 && 0 == 0", 0 == lb);
}

/* ------------------------------------------------------------------ */

static void user_sum(void *invec, void *inoutvec, int *len, MPI_Datatype *dt)
{
    (void) dt;
    int *in = (int *) invec;
    int *inout = (int *) inoutvec;
    for (int i = 0; i < *len; ++i) {
        inout[i] += in[i];
    }
}

static void test_user_op(void)
{
    /* commutative user op */
    MPI_Op op = MPI_OP_NULL;
    int rc = MPI_Op_create(user_sum, 1, &op);
    test_verify("Op_create (commutative) succeeds", MPI_SUCCESS == rc);
    test_verify("created op is non-NULL", MPI_OP_NULL != op);

    int flag = -1;
    MPI_Op_commutative(op, &flag);
    test_verify("user op reports commutative", 1 == flag);

    int in = 7, inout = 10;
    MPI_Reduce_local(&in, &inout, 1, MPI_INT, op);
    test_verify("user op reduce: 7 + 10 == 17", 17 == inout);

    rc = MPI_Op_free(&op);
    test_verify("Op_free succeeds", MPI_SUCCESS == rc);
    test_verify("Op_free sets handle to MPI_OP_NULL", MPI_OP_NULL == op);

    /* non-commutative user op */
    MPI_Op op2 = MPI_OP_NULL;
    MPI_Op_create(user_sum, 0, &op2);
    flag = -1;
    MPI_Op_commutative(op2, &flag);
    test_verify("non-commutative user op reports non-commutative", 0 == flag);
    MPI_Op_free(&op2);
}
