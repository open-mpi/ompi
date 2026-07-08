/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/group layer (group.c, group_init.c).  Exercised
 * through the public MPI_Group API after a full singleton MPI_Init.  The
 * singleton has a single process, so groups are size 0 or 1, but the
 * include/exclude/range/set-operation code paths are still exercised.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include "support.h"

#include "mpi.h"

static MPI_Group world_group(void)
{
    MPI_Group g = MPI_GROUP_NULL;
    MPI_Comm_group(MPI_COMM_WORLD, &g);
    return g;
}

static int group_size(MPI_Group g)
{
    int s = -1;
    MPI_Group_size(g, &s);
    return s;
}

static void test_basic(void);
static void test_translate_compare(void);
static void test_incl_excl(void);
static void test_set_ops(void);
static void test_ranges(void);

int main(int argc, char *argv[])
{
    test_init("ompi group");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_basic();
    test_translate_compare();
    test_incl_excl();
    test_set_ops();
    test_ranges();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_basic(void)
{
    MPI_Group g = world_group();
    test_verify("Comm_group succeeds", MPI_GROUP_NULL != g);

    int size = -1, rank = -1;
    MPI_Group_size(g, &size);
    MPI_Group_rank(g, &rank);
    test_verify("world group size is 1", 1 == size);
    test_verify("world group rank is 0", 0 == rank);

    /* MPI_GROUP_EMPTY has size 0 */
    int esize = -1;
    MPI_Group_size(MPI_GROUP_EMPTY, &esize);
    test_verify("MPI_GROUP_EMPTY size is 0", 0 == esize);
    int erank = 0;
    MPI_Group_rank(MPI_GROUP_EMPTY, &erank);
    test_verify("rank in MPI_GROUP_EMPTY is MPI_UNDEFINED", MPI_UNDEFINED == erank);

    MPI_Group_free(&g);
    test_verify("Group_free sets handle to MPI_GROUP_NULL", MPI_GROUP_NULL == g);
}

/* ------------------------------------------------------------------ */

static void test_translate_compare(void)
{
    MPI_Group g = world_group();

    int result = -1;
    MPI_Group_compare(g, g, &result);
    test_verify("group compared with itself is MPI_IDENT", MPI_IDENT == result);

    result = -1;
    MPI_Group_compare(g, MPI_GROUP_EMPTY, &result);
    test_verify("non-equal groups are MPI_UNEQUAL", MPI_UNEQUAL == result);

    int ranks1[1] = {0};
    int ranks2[1] = {-1};
    MPI_Group_translate_ranks(g, 1, ranks1, g, ranks2);
    test_verify("translate_ranks maps rank 0 -> 0", 0 == ranks2[0]);

    MPI_Group_free(&g);
}

/* ------------------------------------------------------------------ */

static void test_incl_excl(void)
{
    MPI_Group g = world_group();

    int ranks[1] = {0};

    MPI_Group inc = MPI_GROUP_NULL;
    int rc = MPI_Group_incl(g, 1, ranks, &inc);
    test_verify("Group_incl succeeds", MPI_SUCCESS == rc);
    test_verify("incl([0]) has size 1", 1 == group_size(inc));

    MPI_Group exc = MPI_GROUP_NULL;
    rc = MPI_Group_excl(g, 1, ranks, &exc);
    test_verify("Group_excl succeeds", MPI_SUCCESS == rc);
    test_verify("excl([0]) has size 0", 0 == group_size(exc));

    /* incl with zero ranks yields the empty group */
    MPI_Group inc0 = MPI_GROUP_NULL;
    rc = MPI_Group_incl(g, 0, NULL, &inc0);
    test_verify("Group_incl(0) succeeds", MPI_SUCCESS == rc);
    test_verify("incl(0) has size 0", 0 == group_size(inc0));

    MPI_Group_free(&inc);
    MPI_Group_free(&exc);
    MPI_Group_free(&inc0);
    MPI_Group_free(&g);
}

/* ------------------------------------------------------------------ */

static void test_set_ops(void)
{
    MPI_Group g = world_group();

    MPI_Group u = MPI_GROUP_NULL;
    int rc = MPI_Group_union(g, MPI_GROUP_EMPTY, &u);
    test_verify("Group_union succeeds", MPI_SUCCESS == rc);
    test_verify("union(world, empty) has size 1", 1 == group_size(u));

    MPI_Group i = MPI_GROUP_NULL;
    rc = MPI_Group_intersection(g, g, &i);
    test_verify("Group_intersection succeeds", MPI_SUCCESS == rc);
    test_verify("intersection(world, world) has size 1", 1 == group_size(i));

    MPI_Group d = MPI_GROUP_NULL;
    rc = MPI_Group_difference(g, g, &d);
    test_verify("Group_difference succeeds", MPI_SUCCESS == rc);
    test_verify("difference(world, world) has size 0", 0 == group_size(d));

    MPI_Group_free(&u);
    MPI_Group_free(&i);
    MPI_Group_free(&d);
    MPI_Group_free(&g);
}

/* ------------------------------------------------------------------ */

static void test_ranges(void)
{
    MPI_Group g = world_group();

    int range[1][3] = {{0, 0, 1}}; /* first, last, stride */

    MPI_Group ri = MPI_GROUP_NULL;
    int rc = MPI_Group_range_incl(g, 1, range, &ri);
    test_verify("Group_range_incl succeeds", MPI_SUCCESS == rc);
    test_verify("range_incl([0:0:1]) has size 1", 1 == group_size(ri));

    MPI_Group re = MPI_GROUP_NULL;
    rc = MPI_Group_range_excl(g, 1, range, &re);
    test_verify("Group_range_excl succeeds", MPI_SUCCESS == rc);
    test_verify("range_excl([0:0:1]) has size 0", 0 == group_size(re));

    MPI_Group_free(&ri);
    MPI_Group_free(&re);
    MPI_Group_free(&g);
}
