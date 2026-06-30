/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
 * opal_qsort is only compiled when OPAL_HAVE_BROKEN_QSORT is defined (non-zero).
 * On platforms where the system qsort is fine, opal_qsort does not exist and
 * including qsort.h directly emits a compile-time #error.  This file guards
 * all opal_qsort usage inside #if OPAL_HAVE_BROKEN_QSORT so it still
 * produces a valid (passing, no-op) test binary on those platforms.
 */

#include "opal_config.h"

#include "support.h"
#include "opal/constants.h"

#include <stdlib.h>
#include <string.h>

#if OPAL_HAVE_BROKEN_QSORT

#include "opal/util/qsort.h"

/* -----------------------------------------------------------------------
 * Comparator: ascending integer order (overflow-safe)
 * ----------------------------------------------------------------------- */

static int int_cmp(const void *a, const void *b)
{
    int ia = *(const int *) a;
    int ib = *(const int *) b;

    return (ia > ib) - (ia < ib);
}

/* -----------------------------------------------------------------------
 * Helper: verify array is sorted in non-decreasing order
 * ----------------------------------------------------------------------- */

static int is_sorted(const int *arr, size_t n)
{
    size_t i;

    for (i = 1; i < n; i++) {
        if (arr[i] < arr[i - 1]) {
            return 0;
        }
    }
    return 1;
}

/* -----------------------------------------------------------------------
 * Tests
 * ----------------------------------------------------------------------- */

static void test_empty_array(void)
{
    int arr[1]; /* unused memory; n=0 */

    opal_qsort(arr, 0, sizeof(int), int_cmp);
    test_verify("qsort empty array: no crash", 1);
}

static void test_single_element(void)
{
    int arr[1] = {42};

    opal_qsort(arr, 1, sizeof(int), int_cmp);
    test_verify("qsort single element: value preserved", 42 == arr[0]);
}

static void test_already_sorted(void)
{
    int arr[] = {1, 2, 3, 4, 5};
    size_t n = sizeof(arr) / sizeof(arr[0]);

    opal_qsort(arr, n, sizeof(int), int_cmp);
    test_verify("qsort already sorted", is_sorted(arr, n));
}

static void test_reverse_sorted(void)
{
    int arr[] = {5, 4, 3, 2, 1};
    size_t n = sizeof(arr) / sizeof(arr[0]);

    opal_qsort(arr, n, sizeof(int), int_cmp);
    test_verify("qsort reverse sorted: sorted", is_sorted(arr, n));
    test_verify("qsort reverse sorted: arr[0]==1", 1 == arr[0]);
    test_verify("qsort reverse sorted: arr[4]==5", 5 == arr[4]);
}

static void test_duplicates(void)
{
    int arr[] = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3};
    size_t n = sizeof(arr) / sizeof(arr[0]);

    opal_qsort(arr, n, sizeof(int), int_cmp);
    test_verify("qsort duplicates: sorted", is_sorted(arr, n));
    test_verify("qsort duplicates: arr[0]==1", 1 == arr[0]);
    test_verify("qsort duplicates: arr[9]==9", 9 == arr[9]);
}

static void test_all_equal(void)
{
    int arr[] = {7, 7, 7, 7, 7};
    size_t n = sizeof(arr) / sizeof(arr[0]);

    opal_qsort(arr, n, sizeof(int), int_cmp);
    test_verify("qsort all equal: sorted", is_sorted(arr, n));
    test_verify("qsort all equal: arr[0]==7", 7 == arr[0]);
}

static void test_two_elements_ordered(void)
{
    int arr[] = {1, 2};

    opal_qsort(arr, 2, sizeof(int), int_cmp);
    test_verify("qsort two elements ordered: arr[0]==1", 1 == arr[0]);
    test_verify("qsort two elements ordered: arr[1]==2", 2 == arr[1]);
}

static void test_two_elements_reversed(void)
{
    int arr[] = {2, 1};

    opal_qsort(arr, 2, sizeof(int), int_cmp);
    test_verify("qsort two elements reversed: arr[0]==1", 1 == arr[0]);
    test_verify("qsort two elements reversed: arr[1]==2", 2 == arr[1]);
}

static void test_negative_values(void)
{
    int arr[] = {0, -3, 5, -1, 2, -7};
    size_t n = sizeof(arr) / sizeof(arr[0]);

    opal_qsort(arr, n, sizeof(int), int_cmp);
    test_verify("qsort negative values: sorted", is_sorted(arr, n));
    test_verify("qsort negative values: arr[0]==-7", -7 == arr[0]);
    test_verify("qsort negative values: arr[5]==5", 5 == arr[5]);
}

/*
 * Array > 40 elements exercises the median-of-3 pivot selection and recursive
 * partition paths that are dead code for small inputs.
 */
static void test_large_array(void)
{
    /* 50 elements in reverse order */
    int arr[50];
    size_t n = sizeof(arr) / sizeof(arr[0]);
    size_t i;

    for (i = 0; i < n; i++) {
        arr[i] = (int)(n - i); /* 50, 49, ..., 1 */
    }

    opal_qsort(arr, n, sizeof(int), int_cmp);
    test_verify("qsort large array (>40): sorted", is_sorted(arr, n));
    test_verify("qsort large array: arr[0]==1", 1 == arr[0]);
    test_verify("qsort large array: arr[49]==50", 50 == arr[49]);
}

static void test_large_array_random_like(void)
{
    /* 60 elements with varied values to cover all partition branches */
    int arr[] = {
        34, 7,  23, 32, 5,  62,  32, 78, 1,  88,
        45, 21, 14, 57, 44, 99,  0,  -5, 33, 27,
        19, 84, 65, 11, 36, 100, 72, 48, 56, 13,
        93, 29, 8,  61, 41, 77,  52, 3,  66, 17,
        39, 85, 24, 71, 46, 58,  12, 90, 37, 2,
        55, 18, 43, 76, 31, 64,  9,  83, 50, 25
    };
    size_t n = sizeof(arr) / sizeof(arr[0]);

    opal_qsort(arr, n, sizeof(int), int_cmp);
    test_verify("qsort large random-like (60 elems): sorted", is_sorted(arr, n));
    test_verify("qsort large random-like: arr[0]==-5", -5 == arr[0]);
    test_verify("qsort large random-like: arr[59]==100", 100 == arr[59]);
}

#endif /* OPAL_HAVE_BROKEN_QSORT */

/* -----------------------------------------------------------------------
 * main
 * ----------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    test_init("opal_qsort");

#if OPAL_HAVE_BROKEN_QSORT
    test_empty_array();
    test_single_element();
    test_already_sorted();
    test_reverse_sorted();
    test_duplicates();
    test_all_equal();
    test_two_elements_ordered();
    test_two_elements_reversed();
    test_negative_values();
    test_large_array();
    test_large_array_random_like();
#else
    test_comment("opal_qsort is not compiled on this platform (OPAL_HAVE_BROKEN_QSORT==0); "
                 "tests skipped");
    test_success();
#endif

    return test_finalize();
}
