/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for ompi_seq_tracker_t (ompi/class/ompi_seq_tracker.c).  The
 * sequence tracker records which sequence numbers have been "seen",
 * compacting contiguous runs into a single [low,high] range entry.  This
 * test exercises insertion (including range extension, bidirectional
 * consolidation, and gap handling), duplicate detection (including the
 * forward/backward list navigation), and copy.
 *
 * Pure opal-level object: only opal_init_util() is required (the
 * OBJ_CLASS machinery + opal_list); no MPI/instance bring-up.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <stddef.h>

#include "support.h"

#include "opal/runtime/opal.h"
#include "opal/class/opal_list.h"

#include "ompi/class/ompi_seq_tracker.h"

/* Count the number of [low,high] range entries currently stored. */
static size_t n_ranges(ompi_seq_tracker_t *t)
{
    return opal_list_get_size(&t->seq_ids);
}

/* Fetch the nth range entry (in list order), or NULL if out of range. */
static ompi_seq_tracker_range_t *range_at(ompi_seq_tracker_t *t, size_t n)
{
    opal_list_item_t *item;
    size_t i = 0;
    for (item = opal_list_get_first(&t->seq_ids);
         item != opal_list_get_end(&t->seq_ids);
         item = opal_list_get_next(item), ++i) {
        if (i == n) {
            return (ompi_seq_tracker_range_t *) item;
        }
    }
    return NULL;
}

static void test_empty(void);
static void test_single(void);
static void test_duplicate(void);
static void test_ascending_run(void);
static void test_descending_run(void);
static void test_gap_then_fill(void);
static void test_two_ranges(void);
static void test_navigation(void);
static void test_copy(void);

int main(int argc, char *argv[])
{
    test_init("ompi_seq_tracker_t");

    opal_init_util(&argc, &argv);

    test_empty();
    test_single();
    test_duplicate();
    test_ascending_run();
    test_descending_run();
    test_gap_then_fill();
    test_two_ranges();
    test_navigation();
    test_copy();

    int r = test_finalize();
    opal_finalize_util();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_empty(void)
{
    ompi_seq_tracker_t *t = OBJ_NEW(ompi_seq_tracker_t);
    test_verify("OBJ_NEW(ompi_seq_tracker_t) succeeds", NULL != t);
    test_verify("fresh tracker has no ranges", 0 == n_ranges(t));
    test_verify("check_duplicate on empty tracker is false",
                false == ompi_seq_tracker_check_duplicate(t, 7));
    OBJ_RELEASE(t);
}

/* ------------------------------------------------------------------ */

static void test_single(void)
{
    ompi_seq_tracker_t *t = OBJ_NEW(ompi_seq_tracker_t);

    ompi_seq_tracker_insert(t, 5);
    test_verify("single insert creates one range", 1 == n_ranges(t));
    ompi_seq_tracker_range_t *r = range_at(t, 0);
    test_verify("range is [5,5]", NULL != r && 5 == r->seq_id_low && 5 == r->seq_id_high);

    test_verify("inserted id is a duplicate", true == ompi_seq_tracker_check_duplicate(t, 5));
    test_verify("neighbor below is not present", false == ompi_seq_tracker_check_duplicate(t, 4));
    test_verify("neighbor above is not present", false == ompi_seq_tracker_check_duplicate(t, 6));

    OBJ_RELEASE(t);
}

/* ------------------------------------------------------------------ */

static void test_duplicate(void)
{
    ompi_seq_tracker_t *t = OBJ_NEW(ompi_seq_tracker_t);

    ompi_seq_tracker_insert(t, 5);
    ompi_seq_tracker_insert(t, 5); /* inserting an existing id is a no-op */
    test_verify("re-inserting an id does not add a range", 1 == n_ranges(t));
    ompi_seq_tracker_range_t *r = range_at(t, 0);
    test_verify("range still [5,5] after duplicate insert",
                NULL != r && 5 == r->seq_id_low && 5 == r->seq_id_high);

    OBJ_RELEASE(t);
}

/* ------------------------------------------------------------------ */

static void test_ascending_run(void)
{
    ompi_seq_tracker_t *t = OBJ_NEW(ompi_seq_tracker_t);

    for (uint32_t i = 1; i <= 5; ++i) {
        ompi_seq_tracker_insert(t, i);
    }
    /* A contiguous ascending run must compact to a single [1,5] range. */
    test_verify("ascending run compacts to one range", 1 == n_ranges(t));
    ompi_seq_tracker_range_t *r = range_at(t, 0);
    test_verify("compacted range is [1,5]",
                NULL != r && 1 == r->seq_id_low && 5 == r->seq_id_high);

    for (uint32_t i = 1; i <= 5; ++i) {
        test_verify("each id in the run is a duplicate",
                    true == ompi_seq_tracker_check_duplicate(t, i));
    }
    test_verify("id past the run is not present", false == ompi_seq_tracker_check_duplicate(t, 6));

    OBJ_RELEASE(t);
}

/* ------------------------------------------------------------------ */

static void test_descending_run(void)
{
    ompi_seq_tracker_t *t = OBJ_NEW(ompi_seq_tracker_t);

    for (uint32_t i = 5; i >= 1; --i) {
        ompi_seq_tracker_insert(t, i); /* exercises the seq_id_low-1 path */
    }
    test_verify("descending run compacts to one range", 1 == n_ranges(t));
    ompi_seq_tracker_range_t *r = range_at(t, 0);
    test_verify("compacted descending range is [1,5]",
                NULL != r && 1 == r->seq_id_low && 5 == r->seq_id_high);

    OBJ_RELEASE(t);
}

/* ------------------------------------------------------------------ */

static void test_gap_then_fill(void)
{
    ompi_seq_tracker_t *t = OBJ_NEW(ompi_seq_tracker_t);

    ompi_seq_tracker_insert(t, 1);
    ompi_seq_tracker_insert(t, 3);
    test_verify("non-adjacent ids create two ranges", 2 == n_ranges(t));
    test_verify("the gap id is not present", false == ompi_seq_tracker_check_duplicate(t, 2));

    /* Inserting the gap value must merge the two ranges into one [1,3]. */
    ompi_seq_tracker_insert(t, 2);
    test_verify("filling the gap merges into one range", 1 == n_ranges(t));
    ompi_seq_tracker_range_t *r = range_at(t, 0);
    test_verify("merged range is [1,3]",
                NULL != r && 1 == r->seq_id_low && 3 == r->seq_id_high);
    test_verify("the filled id is now present", true == ompi_seq_tracker_check_duplicate(t, 2));

    OBJ_RELEASE(t);
}

/* ------------------------------------------------------------------ */

static void test_two_ranges(void)
{
    ompi_seq_tracker_t *t = OBJ_NEW(ompi_seq_tracker_t);

    ompi_seq_tracker_insert(t, 1);
    ompi_seq_tracker_insert(t, 2);
    ompi_seq_tracker_insert(t, 10);
    ompi_seq_tracker_insert(t, 11);
    test_verify("two disjoint runs yield two ranges", 2 == n_ranges(t));

    test_verify("1 present", true == ompi_seq_tracker_check_duplicate(t, 1));
    test_verify("2 present", true == ompi_seq_tracker_check_duplicate(t, 2));
    test_verify("10 present", true == ompi_seq_tracker_check_duplicate(t, 10));
    test_verify("11 present", true == ompi_seq_tracker_check_duplicate(t, 11));
    test_verify("5 (between runs) absent", false == ompi_seq_tracker_check_duplicate(t, 5));
    test_verify("12 (past runs) absent", false == ompi_seq_tracker_check_duplicate(t, 12));

    OBJ_RELEASE(t);
}

/* ------------------------------------------------------------------ */

/*
 * Build several disjoint ranges and then query ids that force the
 * duplicate-check scan to walk both forward and backward from the
 * "current" pointer.
 */
static void test_navigation(void)
{
    ompi_seq_tracker_t *t = OBJ_NEW(ompi_seq_tracker_t);

    /* ranges: [1,2], [10,11], [20,21] */
    ompi_seq_tracker_insert(t, 1);
    ompi_seq_tracker_insert(t, 2);
    ompi_seq_tracker_insert(t, 10);
    ompi_seq_tracker_insert(t, 11);
    ompi_seq_tracker_insert(t, 20);
    ompi_seq_tracker_insert(t, 21);
    test_verify("three disjoint runs yield three ranges", 3 == n_ranges(t));

    /* current is near the end after the last insert; query low ids
       (backward scan) then high ids (forward scan). */
    test_verify("backward scan finds 1", true == ompi_seq_tracker_check_duplicate(t, 1));
    test_verify("forward scan finds 21", true == ompi_seq_tracker_check_duplicate(t, 21));
    test_verify("middle range found", true == ompi_seq_tracker_check_duplicate(t, 10));
    test_verify("between-range id absent (15)", false == ompi_seq_tracker_check_duplicate(t, 15));
    test_verify("below-everything id absent (0)", false == ompi_seq_tracker_check_duplicate(t, 0));

    OBJ_RELEASE(t);
}

/* ------------------------------------------------------------------ */

static void test_copy(void)
{
    ompi_seq_tracker_t *src = OBJ_NEW(ompi_seq_tracker_t);
    ompi_seq_tracker_insert(src, 1);
    ompi_seq_tracker_insert(src, 2);
    ompi_seq_tracker_insert(src, 3);
    ompi_seq_tracker_insert(src, 10);

    ompi_seq_tracker_t *dst = OBJ_NEW(ompi_seq_tracker_t);
    ompi_seq_tracker_copy(dst, src);

    test_verify("copy reproduces the same range count",
                n_ranges(src) == n_ranges(dst));
    test_verify("copied tracker reports same membership (1)",
                true == ompi_seq_tracker_check_duplicate(dst, 1));
    test_verify("copied tracker reports same membership (3)",
                true == ompi_seq_tracker_check_duplicate(dst, 3));
    test_verify("copied tracker reports same membership (10)",
                true == ompi_seq_tracker_check_duplicate(dst, 10));
    test_verify("copied tracker absent id (5)",
                false == ompi_seq_tracker_check_duplicate(dst, 5));

    /* dst must be independent: extending dst must not change src. */
    ompi_seq_tracker_insert(dst, 5);
    test_verify("inserting into copy does not affect source",
                false == ompi_seq_tracker_check_duplicate(src, 5));

    OBJ_RELEASE(src);
    OBJ_RELEASE(dst);
}
