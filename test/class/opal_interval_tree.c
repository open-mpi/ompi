/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for opal_interval_tree_t.  Exercises insert/find/delete,
 * traversal, size/depth, structural verification and DOT dump.  The
 * library is built with -DNDEBUG, so verification must use test_verify(),
 * never assert().
 */

#include "opal_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "support.h"

#include "opal/class/opal_interval_tree.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#define N 60

/* Traverse action: count how many nodes the traversal visited. */
static int count_action(uint64_t low, uint64_t high, void *data, void *ctx)
{
    (void) low;
    (void) high;
    (void) data;
    int *counter = (int *) ctx;
    (*counter)++;
    return OPAL_SUCCESS;
}

/* Traverse action that aborts the traversal by returning an error. */
static int abort_action(uint64_t low, uint64_t high, void *data, void *ctx)
{
    (void) low;
    (void) high;
    (void) data;
    (void) ctx;
    return OPAL_ERROR;
}

static void test_empty(void)
{
    opal_interval_tree_t tree;
    OBJ_CONSTRUCT(&tree, opal_interval_tree_t);
    test_verify("init empty tree", OPAL_SUCCESS == opal_interval_tree_init(&tree));

    test_verify("empty size is 0", 0 == opal_interval_tree_size(&tree));
    test_verify("empty find returns NULL", NULL == opal_interval_tree_find_overlapping(&tree, 5, 9));
    test_verify("empty tree verifies", opal_interval_tree_verify(&tree));
    test_verify("empty tree depth >= 1", opal_interval_tree_depth(&tree) >= 1);

    OBJ_DESTRUCT(&tree);
}

static void test_insert_find(void)
{
    opal_interval_tree_t tree;
    OBJ_CONSTRUCT(&tree, opal_interval_tree_t);
    opal_interval_tree_init(&tree);

    /* Insert N disjoint intervals [10*i, 10*i+5] with data = i+1. */
    for (int i = 0; i < N; ++i) {
        void *data = (void *) (intptr_t) (i + 1);
        test_verify("insert ok",
                    OPAL_SUCCESS == opal_interval_tree_insert(&tree, data, 10 * i, 10 * i + 5));
    }
    test_verify("size after inserts", N == (int) opal_interval_tree_size(&tree));
    test_verify("tree verifies after inserts", opal_interval_tree_verify(&tree));

    /* Exact-interval queries reliably locate their node. */
    int found_all = 1;
    for (int i = 0; i < N; ++i) {
        void *expect = (void *) (intptr_t) (i + 1);
        if (expect != opal_interval_tree_find_overlapping(&tree, 10 * i, 10 * i + 5)) {
            found_all = 0;
        }
    }
    test_verify("exact find returns inserted data", found_all);

    /* A range that falls in a gap (not contained by any interval) is not
       found. */
    test_verify("gap range not found", NULL == opal_interval_tree_find_overlapping(&tree, 7, 8));

    /* low > high is rejected. */
    test_verify("insert low>high rejected",
                OPAL_ERR_BAD_PARAM == opal_interval_tree_insert(&tree, (void *) 0x1, 50, 40));

    OBJ_DESTRUCT(&tree);
}

static void test_traverse(void)
{
    opal_interval_tree_t tree;
    int counter;
    OBJ_CONSTRUCT(&tree, opal_interval_tree_t);
    opal_interval_tree_init(&tree);

    for (int i = 0; i < 10; ++i) {
        opal_interval_tree_insert(&tree, (void *) (intptr_t) (i + 1), 10 * i, 10 * i + 5);
    }

    /* A traversal over the full range with partial_ok visits every node. */
    counter = 0;
    test_verify("traverse full range ok",
                OPAL_SUCCESS
                    == opal_interval_tree_traverse(&tree, 0, (uint64_t) -1, true, count_action,
                                                   &counter));
    test_verify("traverse visited all nodes", 10 == counter);

    /* NULL action is rejected. */
    test_verify("traverse NULL action rejected",
                OPAL_ERR_BAD_PARAM
                    == opal_interval_tree_traverse(&tree, 0, 100, true, NULL, NULL));

    /* An action that returns an error aborts the traversal and propagates
       the error code. */
    test_verify("traverse propagates action error",
                OPAL_ERROR
                    == opal_interval_tree_traverse(&tree, 0, (uint64_t) -1, true, abort_action,
                                                   NULL));

    OBJ_DESTRUCT(&tree);
}

static void test_delete(void)
{
    opal_interval_tree_t tree;
    OBJ_CONSTRUCT(&tree, opal_interval_tree_t);
    opal_interval_tree_init(&tree);

    for (int i = 0; i < N; ++i) {
        opal_interval_tree_insert(&tree, (void *) (intptr_t) (i + 1), 10 * i, 10 * i + 5);
    }

    /* Deleting a non-existent interval reports NOT_FOUND. */
    test_verify("delete missing -> NOT_FOUND",
                OPAL_ERR_NOT_FOUND == opal_interval_tree_delete(&tree, 9999, 10000, NULL));

    /* Delete every other interval by exact (low, high, data) match; this
       forces both leaf and interior deletions plus rebalancing. */
    int deleted = 0;
    for (int i = 0; i < N; i += 2) {
        void *data = (void *) (intptr_t) (i + 1);
        if (OPAL_SUCCESS == opal_interval_tree_delete(&tree, 10 * i, 10 * i + 5, data)) {
            deleted++;
        }
    }
    test_verify("deleted expected count", deleted == (N + 1) / 2);
    test_verify("size after deletes", (N - deleted) == (int) opal_interval_tree_size(&tree));
    test_verify("tree verifies after deletes", opal_interval_tree_verify(&tree));

    /* The remaining (odd-index) intervals are still findable; the deleted
       ones are gone. */
    int ok = 1;
    for (int i = 1; i < N; i += 2) {
        if ((void *) (intptr_t) (i + 1)
            != opal_interval_tree_find_overlapping(&tree, 10 * i, 10 * i + 5)) {
            ok = 0;
        }
    }
    test_verify("remaining intervals still found", ok);

    OBJ_DESTRUCT(&tree);
}

static void test_depth_and_dump(void)
{
    opal_interval_tree_t tree;
    OBJ_CONSTRUCT(&tree, opal_interval_tree_t);
    opal_interval_tree_init(&tree);

    /* Insert in ascending order to force a long run of rotations. */
    for (int i = 0; i < 63; ++i) {
        opal_interval_tree_insert(&tree, (void *) (intptr_t) (i + 1), 10 * i, 10 * i + 5);
    }

    /* A red-black tree of 63 nodes must stay O(log n) deep; allow a
       generous bound (2*log2(64) = 12, plus the root sentinel and slack). */
    size_t depth = opal_interval_tree_depth(&tree);
    test_verify("depth is O(log n)", depth >= 1 && depth <= 16);
    test_verify("tree of 63 verifies", opal_interval_tree_verify(&tree));

    /* DOT dump to a writable path succeeds; an unwritable path fails. */
    char path[] = "/tmp/opal_itree_dumpXXXXXX";
    int fd = mkstemp(path);
    if (fd >= 0) {
        close(fd);
        test_verify("dump to file ok", OPAL_SUCCESS == opal_interval_tree_dump(&tree, path));
        unlink(path);
    }
    test_verify("dump to bad path rejected",
                OPAL_ERR_BAD_PARAM
                    == opal_interval_tree_dump(&tree, "/nonexistent_dir_xyz/itree.dot"));

    OBJ_DESTRUCT(&tree);
}

int main(int argc, char *argv[])
{
    /* opal_interval_tree uses an opal_free_list, which relies on
       opal_cache_line_size and the MCA infrastructure being set up. */
    opal_init_util(&argc, &argv);

    test_init("opal_interval_tree_t");

    test_empty();
    test_insert_find();
    test_traverse();
    test_delete();
    test_depth_and_dump();

    int ret = test_finalize();
    opal_finalize_util();
    return ret;
}
