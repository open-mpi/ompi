/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/communicator layer (comm.c, comm_init.c,
 * comm_cid.c, ...).  Exercised through the public MPI communicator API
 * after a full singleton MPI_Init.  All operations are valid for a
 * size-1 (singleton) communicator, so no launcher is required.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <string.h>

#include "support.h"

#include "mpi.h"

static void test_rank_size(void);
static void test_dup(void);
static void test_compare(void);
static void test_name(void);
static void test_group_roundtrip(void);
static void test_split(void);
static void test_split_type(void);
static void test_info(void);
static void test_attributes(void);
static void test_misc(void);

int main(int argc, char *argv[])
{
    test_init("ompi communicator");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_rank_size();
    test_dup();
    test_compare();
    test_name();
    test_group_roundtrip();
    test_split();
    test_split_type();
    test_info();
    test_attributes();
    test_misc();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_rank_size(void)
{
    int rank = -1, size = -1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    test_verify("WORLD rank is 0", 0 == rank);
    test_verify("WORLD size is 1", 1 == size);

    int srank = -1, ssize = -1;
    MPI_Comm_rank(MPI_COMM_SELF, &srank);
    MPI_Comm_size(MPI_COMM_SELF, &ssize);
    test_verify("SELF rank is 0", 0 == srank);
    test_verify("SELF size is 1", 1 == ssize);
}

/* ------------------------------------------------------------------ */

static void test_dup(void)
{
    MPI_Comm dup = MPI_COMM_NULL;
    int rc = MPI_Comm_dup(MPI_COMM_WORLD, &dup);
    test_verify("Comm_dup succeeds", MPI_SUCCESS == rc);
    test_verify("dup is not NULL", MPI_COMM_NULL != dup);

    int size = -1;
    MPI_Comm_size(dup, &size);
    test_verify("dup has same size as WORLD", 1 == size);

    MPI_Comm dwi = MPI_COMM_NULL;
    rc = MPI_Comm_dup_with_info(MPI_COMM_WORLD, MPI_INFO_NULL, &dwi);
    test_verify("Comm_dup_with_info succeeds", MPI_SUCCESS == rc);
    test_verify("dup_with_info is not NULL", MPI_COMM_NULL != dwi);

    MPI_Comm_free(&dup);
    test_verify("free sets comm to NULL", MPI_COMM_NULL == dup);
    MPI_Comm_free(&dwi);
}

/* ------------------------------------------------------------------ */

static void test_compare(void)
{
    int result = -1;
    MPI_Comm_compare(MPI_COMM_WORLD, MPI_COMM_WORLD, &result);
    test_verify("WORLD vs WORLD is MPI_IDENT", MPI_IDENT == result);

    MPI_Comm dup = MPI_COMM_NULL;
    MPI_Comm_dup(MPI_COMM_WORLD, &dup);
    result = -1;
    MPI_Comm_compare(MPI_COMM_WORLD, dup, &result);
    test_verify("WORLD vs its dup is MPI_CONGRUENT", MPI_CONGRUENT == result);
    MPI_Comm_free(&dup);

    result = -1;
    MPI_Comm_compare(MPI_COMM_WORLD, MPI_COMM_SELF, &result);
    test_verify("WORLD vs SELF is not IDENT", MPI_IDENT != result);
}

/* ------------------------------------------------------------------ */

static void test_name(void)
{
    MPI_Comm dup = MPI_COMM_NULL;
    MPI_Comm_dup(MPI_COMM_WORLD, &dup);

    int rc = MPI_Comm_set_name(dup, "my_comm");
    test_verify("Comm_set_name succeeds", MPI_SUCCESS == rc);

    char name[MPI_MAX_OBJECT_NAME];
    int len = -1;
    memset(name, 0, sizeof(name));
    rc = MPI_Comm_get_name(dup, name, &len);
    test_verify("Comm_get_name succeeds", MPI_SUCCESS == rc);
    test_verify("retrieved name matches", 0 == strcmp(name, "my_comm"));
    test_verify("retrieved name length matches", (int) strlen("my_comm") == len);

    MPI_Comm_free(&dup);
}

/* ------------------------------------------------------------------ */

static void test_group_roundtrip(void)
{
    MPI_Group grp = MPI_GROUP_NULL;
    int rc = MPI_Comm_group(MPI_COMM_WORLD, &grp);
    test_verify("Comm_group succeeds", MPI_SUCCESS == rc);

    int gsize = -1, grank = -1;
    MPI_Group_size(grp, &gsize);
    MPI_Group_rank(grp, &grank);
    test_verify("group size matches comm size", 1 == gsize);
    test_verify("group rank matches comm rank", 0 == grank);

    /* Build a communicator back from the full group. */
    MPI_Comm newcomm = MPI_COMM_NULL;
    rc = MPI_Comm_create(MPI_COMM_WORLD, grp, &newcomm);
    test_verify("Comm_create from full group succeeds", MPI_SUCCESS == rc);
    test_verify("created comm is not NULL", MPI_COMM_NULL != newcomm);
    if (MPI_COMM_NULL != newcomm) {
        int ns = -1;
        MPI_Comm_size(newcomm, &ns);
        test_verify("created comm has expected size", 1 == ns);
        MPI_Comm_free(&newcomm);
    }

    MPI_Group_free(&grp);
}

/* ------------------------------------------------------------------ */

static void test_split(void)
{
    MPI_Comm sub = MPI_COMM_NULL;
    int rc = MPI_Comm_split(MPI_COMM_WORLD, 0, 0, &sub);
    test_verify("Comm_split succeeds", MPI_SUCCESS == rc);
    test_verify("split produced a communicator", MPI_COMM_NULL != sub);
    if (MPI_COMM_NULL != sub) {
        int s = -1;
        MPI_Comm_size(sub, &s);
        test_verify("split comm has size 1 (singleton)", 1 == s);
        MPI_Comm_free(&sub);
    }

    /* color = MPI_UNDEFINED must yield MPI_COMM_NULL */
    MPI_Comm none = (MPI_Comm) 0xdeadbeef;
    rc = MPI_Comm_split(MPI_COMM_WORLD, MPI_UNDEFINED, 0, &none);
    test_verify("split with MPI_UNDEFINED color succeeds", MPI_SUCCESS == rc);
    test_verify("split with MPI_UNDEFINED yields MPI_COMM_NULL", MPI_COMM_NULL == none);
}

/* ------------------------------------------------------------------ */

static void test_split_type(void)
{
    MPI_Comm shared = MPI_COMM_NULL;
    int rc = MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0,
                                 MPI_INFO_NULL, &shared);
    test_verify("Comm_split_type(SHARED) succeeds", MPI_SUCCESS == rc);
    if (MPI_COMM_NULL != shared) {
        int ssize = -1;
        MPI_Comm_size(shared, &ssize);
        test_verify("shared-split comm has size 1 (singleton)", 1 == ssize);
        MPI_Comm_free(&shared);
    }
}

/* ------------------------------------------------------------------ */

static void test_info(void)
{
    MPI_Comm dup = MPI_COMM_NULL;
    MPI_Comm_dup(MPI_COMM_WORLD, &dup);

    MPI_Info info = MPI_INFO_NULL;
    MPI_Info_create(&info);
    MPI_Info_set(info, "mpi_assert_no_any_tag", "true");

    int rc = MPI_Comm_set_info(dup, info);
    test_verify("Comm_set_info succeeds", MPI_SUCCESS == rc);

    MPI_Info got = MPI_INFO_NULL;
    rc = MPI_Comm_get_info(dup, &got);
    test_verify("Comm_get_info succeeds", MPI_SUCCESS == rc);
    test_verify("Comm_get_info returns an info object", MPI_INFO_NULL != got);
    if (MPI_INFO_NULL != got) {
        MPI_Info_free(&got);
    }

    MPI_Info_free(&info);
    MPI_Comm_free(&dup);
}

/* ------------------------------------------------------------------ */

static int delete_called = 0;

static int copy_fn(MPI_Comm comm, int keyval, void *extra, void *attr_in,
                   void *attr_out, int *flag)
{
    (void) comm; (void) keyval; (void) extra; (void) attr_in;
    *(void **) attr_out = attr_in;
    *flag = 1;
    return MPI_SUCCESS;
}

static int delete_fn(MPI_Comm comm, int keyval, void *attr, void *extra)
{
    (void) comm; (void) keyval; (void) attr; (void) extra;
    delete_called = 1;
    return MPI_SUCCESS;
}

/* Light exercise of communicator-attached attributes (the attribute
 * engine itself gets a dedicated test). */
static void test_attributes(void)
{
    int keyval = MPI_KEYVAL_INVALID;
    int rc = MPI_Comm_create_keyval(copy_fn, delete_fn, &keyval, NULL);
    test_verify("Comm_create_keyval succeeds", MPI_SUCCESS == rc);

    int val = 42;
    rc = MPI_Comm_set_attr(MPI_COMM_SELF, keyval, &val);
    test_verify("Comm_set_attr succeeds", MPI_SUCCESS == rc);

    void *got = NULL;
    int flag = 0;
    rc = MPI_Comm_get_attr(MPI_COMM_SELF, keyval, &got, &flag);
    test_verify("Comm_get_attr succeeds", MPI_SUCCESS == rc);
    test_verify("attr flag set", 1 == flag);
    test_verify("attr value round-trips", got == &val);

    delete_called = 0;
    rc = MPI_Comm_delete_attr(MPI_COMM_SELF, keyval);
    test_verify("Comm_delete_attr succeeds", MPI_SUCCESS == rc);
    test_verify("delete callback fired", 1 == delete_called);

    rc = MPI_Comm_free_keyval(&keyval);
    test_verify("Comm_free_keyval succeeds", MPI_SUCCESS == rc);
}

/* ------------------------------------------------------------------ */

static void test_misc(void)
{
    int flag = -1;
    MPI_Comm_test_inter(MPI_COMM_WORLD, &flag);
    test_verify("WORLD is not an inter-communicator", 0 == flag);

    MPI_Comm parent = (MPI_Comm) 0xdeadbeef;
    int rc = MPI_Comm_get_parent(&parent);
    test_verify("Comm_get_parent succeeds", MPI_SUCCESS == rc);
    test_verify("singleton has no parent (MPI_COMM_NULL)", MPI_COMM_NULL == parent);
}
