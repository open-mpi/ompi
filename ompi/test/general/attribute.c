/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/attribute engine (attribute.c,
 * attribute_predefined.c).  Exercised through the public MPI keyval /
 * attribute API on communicators, datatypes, and windows after a full
 * singleton MPI_Init, including copy/delete callbacks (driven via
 * dup) and the predefined attributes (MPI_TAG_UB, etc.).
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include "support.h"

#include "mpi.h"

/* callback bookkeeping */
static int comm_copy_calls = 0;
static int comm_delete_calls = 0;

static int comm_copy_fn(MPI_Comm comm, int keyval, void *extra,
                        void *attr_in, void *attr_out, int *flag)
{
    (void) comm; (void) keyval; (void) extra;
    ++comm_copy_calls;
    *(void **) attr_out = attr_in; /* propagate to the duplicate */
    *flag = 1;
    return MPI_SUCCESS;
}

static int comm_delete_fn(MPI_Comm comm, int keyval, void *attr, void *extra)
{
    (void) comm; (void) keyval; (void) attr; (void) extra;
    ++comm_delete_calls;
    return MPI_SUCCESS;
}

static void test_comm_attr(void);
static void test_comm_predefined(void);
static void test_type_attr(void);
static void test_win_attr(void);

int main(int argc, char *argv[])
{
    test_init("ompi attribute");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_comm_attr();
    test_comm_predefined();
    test_type_attr();
    test_win_attr();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_comm_attr(void)
{
    int keyval = MPI_KEYVAL_INVALID;
    int rc = MPI_Comm_create_keyval(comm_copy_fn, comm_delete_fn, &keyval, NULL);
    test_verify("Comm_create_keyval succeeds", MPI_SUCCESS == rc);
    test_verify("keyval is valid", MPI_KEYVAL_INVALID != keyval);

    static int value = 1234;
    MPI_Comm base = MPI_COMM_NULL;
    MPI_Comm_dup(MPI_COMM_WORLD, &base);

    rc = MPI_Comm_set_attr(base, keyval, &value);
    test_verify("Comm_set_attr succeeds", MPI_SUCCESS == rc);

    void *got = NULL;
    int flag = 0;
    MPI_Comm_get_attr(base, keyval, &got, &flag);
    test_verify("get_attr finds the attribute", 1 == flag);
    test_verify("attribute value round-trips", got == &value);

    /* dup must invoke the copy callback */
    comm_copy_calls = 0;
    MPI_Comm dup = MPI_COMM_NULL;
    MPI_Comm_dup(base, &dup);
    test_verify("copy callback fired during dup", comm_copy_calls >= 1);
    flag = 0; got = NULL;
    MPI_Comm_get_attr(dup, keyval, &got, &flag);
    test_verify("attribute copied to the duplicate", 1 == flag && got == &value);

    /* delete must invoke the delete callback */
    comm_delete_calls = 0;
    rc = MPI_Comm_delete_attr(base, keyval);
    test_verify("Comm_delete_attr succeeds", MPI_SUCCESS == rc);
    test_verify("delete callback fired", 1 == comm_delete_calls);
    flag = 1;
    MPI_Comm_get_attr(base, keyval, &got, &flag);
    test_verify("attribute gone after delete", 0 == flag);

    /* freeing a comm with a live attribute also fires delete */
    comm_delete_calls = 0;
    MPI_Comm_free(&dup);
    test_verify("delete callback fired on comm free", comm_delete_calls >= 1);

    MPI_Comm_free(&base);
    rc = MPI_Comm_free_keyval(&keyval);
    test_verify("Comm_free_keyval succeeds", MPI_SUCCESS == rc);
}

/* ------------------------------------------------------------------ */

static void test_comm_predefined(void)
{
    int *valp = NULL;
    int flag = 0;

    MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_TAG_UB, &valp, &flag);
    test_verify("MPI_TAG_UB is present", 1 == flag);
    test_verify("MPI_TAG_UB is at least 32767 (MPI minimum)",
                NULL != valp && *valp >= 32767);

    flag = 0;
    MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_WTIME_IS_GLOBAL, &valp, &flag);
    test_verify("MPI_WTIME_IS_GLOBAL is present", 1 == flag);

    flag = 0;
    MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_LASTUSEDCODE, &valp, &flag);
    test_verify("MPI_LASTUSEDCODE is present", 1 == flag);

    /* MPI_HOST and MPI_IO are required predefined attributes */
    flag = 0;
    MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_IO, &valp, &flag);
    test_verify("MPI_IO is present", 1 == flag);
}

/* ------------------------------------------------------------------ */

static int type_copy_calls = 0;

static int type_copy_fn(MPI_Datatype t, int keyval, void *extra,
                        void *attr_in, void *attr_out, int *flag)
{
    (void) t; (void) keyval; (void) extra;
    ++type_copy_calls;
    *(void **) attr_out = attr_in;
    *flag = 1;
    return MPI_SUCCESS;
}

static void test_type_attr(void)
{
    int keyval = MPI_KEYVAL_INVALID;
    int rc = MPI_Type_create_keyval(type_copy_fn, MPI_TYPE_NULL_DELETE_FN,
                                    &keyval, NULL);
    test_verify("Type_create_keyval succeeds", MPI_SUCCESS == rc);

    MPI_Datatype t = MPI_DATATYPE_NULL;
    MPI_Type_contiguous(2, MPI_INT, &t);
    MPI_Type_commit(&t);

    static int value = 99;
    rc = MPI_Type_set_attr(t, keyval, &value);
    test_verify("Type_set_attr succeeds", MPI_SUCCESS == rc);

    void *got = NULL;
    int flag = 0;
    MPI_Type_get_attr(t, keyval, &got, &flag);
    test_verify("Type_get_attr finds the attribute", 1 == flag && got == &value);

    /* dup invokes the copy callback */
    type_copy_calls = 0;
    MPI_Datatype dup = MPI_DATATYPE_NULL;
    MPI_Type_dup(t, &dup);
    test_verify("type copy callback fired on dup", type_copy_calls >= 1);

    rc = MPI_Type_delete_attr(t, keyval);
    test_verify("Type_delete_attr succeeds", MPI_SUCCESS == rc);

    MPI_Type_free(&dup);
    MPI_Type_free(&t);
    MPI_Type_free_keyval(&keyval);
}

/* ------------------------------------------------------------------ */

static void test_win_attr(void)
{
    MPI_Win win = MPI_WIN_NULL;
    static int buffer[16];
    int rc = MPI_Win_create(buffer, sizeof(buffer), sizeof(int),
                            MPI_INFO_NULL, MPI_COMM_SELF, &win);
    test_verify("Win_create succeeds", MPI_SUCCESS == rc);
    if (MPI_SUCCESS != rc || MPI_WIN_NULL == win) {
        return;
    }

    int keyval = MPI_KEYVAL_INVALID;
    rc = MPI_Win_create_keyval(MPI_WIN_NULL_COPY_FN, MPI_WIN_NULL_DELETE_FN,
                               &keyval, NULL);
    test_verify("Win_create_keyval succeeds", MPI_SUCCESS == rc);

    static int value = 7;
    rc = MPI_Win_set_attr(win, keyval, &value);
    test_verify("Win_set_attr succeeds", MPI_SUCCESS == rc);

    void *got = NULL;
    int flag = 0;
    MPI_Win_get_attr(win, keyval, &got, &flag);
    test_verify("Win_get_attr finds the attribute", 1 == flag && got == &value);

    /* predefined window attribute MPI_WIN_BASE */
    void *base = NULL;
    flag = 0;
    MPI_Win_get_attr(win, MPI_WIN_BASE, &base, &flag);
    test_verify("MPI_WIN_BASE is present", 1 == flag);

    rc = MPI_Win_delete_attr(win, keyval);
    test_verify("Win_delete_attr succeeds", MPI_SUCCESS == rc);

    MPI_Win_free_keyval(&keyval);
    MPI_Win_free(&win);
}
