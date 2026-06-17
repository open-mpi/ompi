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

#include "opal_config.h"
#include "support.h"
#include "opal/util/argv.h"
#include "opal/constants.h"
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* opal_argv_append / opal_argv_append_nosize                         */
/* ------------------------------------------------------------------ */

static void test_append(void)
{
    char **argv = NULL;
    int argc = 42; /* intentionally bogus -- must be reset on first call */

    /* First append to NULL argv: argc is updated, array has 1 element + NULL */
    test_verify("append first: returns OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_append(&argc, &argv, "alpha"));
    test_verify("append first: argc == 1", 1 == argc);
    test_verify("append first: argv non-NULL", NULL != argv);
    test_verify("append first: argv[0] is 'alpha'",
                0 == strcmp("alpha", argv[0]));
    test_verify("append first: argv[1] is NULL", NULL == argv[1]);

    /* Second append */
    test_verify("append second: returns OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_append(&argc, &argv, "beta"));
    test_verify("append second: argc == 2", 2 == argc);
    test_verify("append second: argv[0] still 'alpha'",
                0 == strcmp("alpha", argv[0]));
    test_verify("append second: argv[1] is 'beta'",
                0 == strcmp("beta", argv[1]));
    test_verify("append second: argv[2] is NULL", NULL == argv[2]);

    /* Strings are copied by value -- mutate source and verify argv unchanged */
    {
        char buf[] = "gamma";
        test_verify("append copy-by-value setup",
                    OPAL_SUCCESS == opal_argv_append(&argc, &argv, buf));
        buf[0] = 'X';
        test_verify("append copy-by-value: stored string unchanged",
                    0 == strcmp("gamma", argv[2]));
    }

    opal_argv_free(argv);
}

static void test_append_nosize(void)
{
    char **argv = NULL;

    test_verify("append_nosize first: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_append_nosize(&argv, "one"));
    test_verify("append_nosize first: argv non-NULL", NULL != argv);
    test_verify("append_nosize first: argv[0] == 'one'",
                0 == strcmp("one", argv[0]));
    test_verify("append_nosize first: argv[1] == NULL", NULL == argv[1]);

    test_verify("append_nosize second: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_append_nosize(&argv, "two"));
    test_verify("append_nosize second: argv[1] == 'two'",
                0 == strcmp("two", argv[1]));
    test_verify("append_nosize second: argv[2] == NULL", NULL == argv[2]);

    opal_argv_free(argv);
}

/* ------------------------------------------------------------------ */
/* opal_argv_prepend_nosize                                            */
/* ------------------------------------------------------------------ */

static void test_prepend(void)
{
    char **argv = NULL;

    /* prepend to empty array */
    test_verify("prepend to NULL: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_prepend_nosize(&argv, "first"));
    test_verify("prepend to NULL: argv[0] == 'first'",
                0 == strcmp("first", argv[0]));
    test_verify("prepend to NULL: argv[1] == NULL", NULL == argv[1]);

    /* prepend a second element */
    test_verify("prepend second: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_prepend_nosize(&argv, "zero"));
    test_verify("prepend second: argv[0] == 'zero'",
                0 == strcmp("zero", argv[0]));
    test_verify("prepend second: argv[1] == 'first'",
                0 == strcmp("first", argv[1]));
    test_verify("prepend second: argv[2] == NULL", NULL == argv[2]);

    /* prepend a third */
    test_verify("prepend third: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_prepend_nosize(&argv, "neg1"));
    test_verify("prepend third: argv[0] == 'neg1'",
                0 == strcmp("neg1", argv[0]));
    test_verify("prepend third: argv[1] == 'zero'",
                0 == strcmp("zero", argv[1]));
    test_verify("prepend third: argv[2] == 'first'",
                0 == strcmp("first", argv[2]));
    test_verify("prepend third: argv[3] == NULL", NULL == argv[3]);

    opal_argv_free(argv);
}

/* ------------------------------------------------------------------ */
/* opal_argv_append_unique_nosize                                      */
/* ------------------------------------------------------------------ */

static void test_append_unique(void)
{
    char **argv = NULL;

    /* Fast path: NULL array -- appends unconditionally */
    test_verify("unique: NULL array appends",
                OPAL_SUCCESS == opal_argv_append_unique_nosize(&argv, "x", false));
    test_verify("unique: NULL array: argv[0] == 'x'",
                0 == strcmp("x", argv[0]));

    /* Adding a fresh element -- should append */
    test_verify("unique: new element appended",
                OPAL_SUCCESS == opal_argv_append_unique_nosize(&argv, "y", false));
    test_verify("unique: new element: count == 2",
                2 == opal_argv_count(argv));

    /* Duplicate with overwrite=false -- no change, still OPAL_SUCCESS */
    test_verify("unique: dup overwrite=false returns OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_append_unique_nosize(&argv, "x", false));
    test_verify("unique: dup overwrite=false: count stays 2",
                2 == opal_argv_count(argv));
    test_verify("unique: dup overwrite=false: argv[0] still 'x'",
                0 == strcmp("x", argv[0]));

    /* Duplicate with overwrite=true -- value replaced, count unchanged */
    test_verify("unique: dup overwrite=true returns OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_append_unique_nosize(&argv, "x", true));
    test_verify("unique: dup overwrite=true: count stays 2",
                2 == opal_argv_count(argv));
    test_verify("unique: dup overwrite=true: argv[0] still 'x'",
                0 == strcmp("x", argv[0]));

    opal_argv_free(argv);
}

/* ------------------------------------------------------------------ */
/* opal_argv_free                                                       */
/* ------------------------------------------------------------------ */

static void test_free(void)
{
    char **argv = NULL;
    int argc = 0;

    /* free(NULL) must be a no-op */
    opal_argv_free(NULL); /* would segfault if broken */
    test_verify("free(NULL) is harmless", 1);

    /* build and free a real array */
    opal_argv_append(&argc, &argv, "a");
    opal_argv_append(&argc, &argv, "b");
    opal_argv_free(argv); /* would segfault or fail asan/valgrind if broken */
    test_verify("free real array is harmless", 1);
}

/* ------------------------------------------------------------------ */
/* opal_argv_count                                                      */
/* ------------------------------------------------------------------ */

static void test_count(void)
{
    char *a[] = {"a", "b", "c", NULL};

    test_verify("count(NULL) == 0", 0 == opal_argv_count(NULL));
    test_verify("count(3-elem) == 3", 3 == opal_argv_count(a));

    /* empty array (only NULL terminator) */
    {
        char **empty = NULL;
        int argc = 0;
        opal_argv_append(&argc, &empty, "tmp");
        /* delete all */
        opal_argv_delete(&argc, &empty, 0, 99);
        test_verify("count after delete-all == 0", 0 == opal_argv_count(empty));
        opal_argv_free(empty);
    }
}

/* ------------------------------------------------------------------ */
/* opal_argv_split / opal_argv_split_with_empty                       */
/* ------------------------------------------------------------------ */

static void test_split(void)
{
    char **result;

    /* Basic split */
    result = opal_argv_split("a:b:c", ':');
    test_verify("split basic: non-NULL", NULL != result);
    test_verify("split basic: count == 3", 3 == opal_argv_count(result));
    test_verify("split basic: [0]=='a'", 0 == strcmp("a", result[0]));
    test_verify("split basic: [1]=='b'", 0 == strcmp("b", result[1]));
    test_verify("split basic: [2]=='c'", 0 == strcmp("c", result[2]));
    opal_argv_free(result);

    /* split skips consecutive delimiters (empty tokens) */
    result = opal_argv_split("a::b", ':');
    test_verify("split consecutive delimiters: count == 2",
                2 == opal_argv_count(result));
    test_verify("split consecutive: [0]=='a'", 0 == strcmp("a", result[0]));
    test_verify("split consecutive: [1]=='b'", 0 == strcmp("b", result[1]));
    opal_argv_free(result);

    /* split skips leading delimiter */
    result = opal_argv_split(":a:b", ':');
    test_verify("split leading delim: count == 2", 2 == opal_argv_count(result));
    test_verify("split leading: [0]=='a'", 0 == strcmp("a", result[0]));
    opal_argv_free(result);

    /* split skips trailing delimiter */
    result = opal_argv_split("a:b:", ':');
    test_verify("split trailing delim: count == 2", 2 == opal_argv_count(result));
    test_verify("split trailing: [0]=='a'", 0 == strcmp("a", result[0]));
    test_verify("split trailing: [1]=='b'", 0 == strcmp("b", result[1]));
    opal_argv_free(result);

    /* split empty string returns NULL (nothing to iterate) */
    result = opal_argv_split("", ':');
    test_verify("split empty string: returns NULL", NULL == result);
    /* (no free needed for NULL) */

    /* split NULL src_string returns NULL */
    result = opal_argv_split(NULL, ':');
    test_verify("split NULL src: returns NULL", NULL == result);
}

static void test_split_with_empty(void)
{
    char **result;

    /* Basic split_with_empty -- same as split for non-empty tokens */
    result = opal_argv_split_with_empty("a:b:c", ':');
    test_verify("split_with_empty basic: count == 3",
                3 == opal_argv_count(result));
    test_verify("split_with_empty basic: [0]=='a'",
                0 == strcmp("a", result[0]));
    opal_argv_free(result);

    /* Consecutive delimiters produce empty strings */
    result = opal_argv_split_with_empty("a::b", ':');
    test_verify("split_with_empty consecutive: count == 3",
                3 == opal_argv_count(result));
    test_verify("split_with_empty consecutive: [0]=='a'",
                0 == strcmp("a", result[0]));
    test_verify("split_with_empty consecutive: [1]==''",
                0 == strcmp("", result[1]));
    test_verify("split_with_empty consecutive: [2]=='b'",
                0 == strcmp("b", result[2]));
    opal_argv_free(result);

    /* Leading delimiter produces empty string at index 0 */
    result = opal_argv_split_with_empty(":a", ':');
    test_verify("split_with_empty leading: count == 2",
                2 == opal_argv_count(result));
    test_verify("split_with_empty leading: [0]==''",
                0 == strcmp("", result[0]));
    test_verify("split_with_empty leading: [1]=='a'",
                0 == strcmp("a", result[1]));
    opal_argv_free(result);

    /* An interior empty field is included.  (Whether a *trailing*
     * delimiter should also yield a trailing empty field is left
     * unspecified by the documentation -- which only says "include empty
     * strings" -- and the implementation does not produce one, so we do
     * not assert that ambiguous case here.) */
    result = opal_argv_split_with_empty("a::b", ':');
    test_verify("split_with_empty interior: count == 3", 3 == opal_argv_count(result));
    test_verify("split_with_empty interior: [0]=='a'", 0 == strcmp("a", result[0]));
    test_verify("split_with_empty interior: [1]==''", 0 == strcmp("", result[1]));
    test_verify("split_with_empty interior: [2]=='b'", 0 == strcmp("b", result[2]));
    opal_argv_free(result);

    /* Difference from split: split_with_empty keeps empty, split does not */
    {
        char **without_empty = opal_argv_split("a::b", ':');
        char **with_empty = opal_argv_split_with_empty("a::b", ':');
        test_verify("split vs split_with_empty differ on empty tokens",
                    opal_argv_count(without_empty) < opal_argv_count(with_empty));
        opal_argv_free(without_empty);
        opal_argv_free(with_empty);
    }

    /* Empty source string */
    result = opal_argv_split_with_empty("", ':');
    test_verify("split_with_empty empty string: NULL", NULL == result);

    /* NULL source string */
    result = opal_argv_split_with_empty(NULL, ':');
    test_verify("split_with_empty NULL src: NULL", NULL == result);
}

/* ------------------------------------------------------------------ */
/* opal_argv_join / opal_argv_join_range                              */
/* ------------------------------------------------------------------ */

static void test_join(void)
{
    char *joined;

    /* bozo cases */
    joined = opal_argv_join(NULL, ':');
    test_verify("join(NULL): returns non-NULL (empty string)",
                NULL != joined);
    test_verify("join(NULL): returns empty string",
                0 == strcmp("", joined));
    free(joined);

    {
        char *empty_argv[] = {NULL};
        joined = opal_argv_join(empty_argv, ':');
        test_verify("join(empty array): non-NULL", NULL != joined);
        test_verify("join(empty array): empty string",
                    0 == strcmp("", joined));
        free(joined);
    }

    /* basic join */
    {
        char *a[] = {"x", "y", "z", NULL};
        joined = opal_argv_join(a, ':');
        test_verify("join basic: non-NULL", NULL != joined);
        test_verify("join basic: 'x:y:z'",
                    0 == strcmp("x:y:z", joined));
        free(joined);
    }

    /* single element: no delimiter in output */
    {
        char *a[] = {"solo", NULL};
        joined = opal_argv_join(a, ',');
        test_verify("join single: 'solo'",
                    0 == strcmp("solo", joined));
        free(joined);
    }

    /* round-trip: split then join reproduces original */
    {
        char *orig = "the quick brown fox";
        char **parts = opal_argv_split(orig, ' ');
        joined = opal_argv_join(parts, ' ');
        test_verify("join round-trip",
                    0 == strcmp(orig, joined));
        free(joined);
        opal_argv_free(parts);
    }
}

static void test_join_range(void)
{
    char *joined;
    char *a[] = {"a", "b", "c", "d", NULL};

    /* bozo: NULL array */
    joined = opal_argv_join_range(NULL, 0, 2, ':');
    test_verify("join_range NULL array: non-NULL (empty string)",
                NULL != joined);
    test_verify("join_range NULL array: empty string",
                0 == strcmp("", joined));
    free(joined);

    /* bozo: start >= count */
    joined = opal_argv_join_range(a, 99, 100, ':');
    test_verify("join_range start>=count: empty string",
                0 == strcmp("", joined));
    free(joined);

    /* range [1,3) => "b:c" */
    joined = opal_argv_join_range(a, 1, 3, '-');
    test_verify("join_range [1,3): non-NULL", NULL != joined);
    test_verify("join_range [1,3): 'b-c'",
                0 == strcmp("b-c", joined));
    free(joined);

    /* range [0,4) => full array */
    joined = opal_argv_join_range(a, 0, 4, ':');
    test_verify("join_range [0,4): 'a:b:c:d'",
                0 == strcmp("a:b:c:d", joined));
    free(joined);

    /* range [2,4) => "c:d" */
    joined = opal_argv_join_range(a, 2, 4, ':');
    test_verify("join_range [2,4): 'c:d'",
                0 == strcmp("c:d", joined));
    free(joined);

    /* single element range [1,2) => "b" */
    joined = opal_argv_join_range(a, 1, 2, ':');
    test_verify("join_range single element: 'b'",
                0 == strcmp("b", joined));
    free(joined);

    /* end beyond array: stops at NULL */
    joined = opal_argv_join_range(a, 2, 999, ':');
    test_verify("join_range end>count: 'c:d'",
                0 == strcmp("c:d", joined));
    free(joined);
}

/* ------------------------------------------------------------------ */
/* opal_argv_len                                                        */
/* ------------------------------------------------------------------ */

static void test_len(void)
{
    /* NULL returns 0 */
    test_verify("len(NULL) == 0", (size_t) 0 == opal_argv_len(NULL));

    /* ["a","b","c"]: each entry: strlen + 1 (for '\0') + sizeof(char*),
       plus one trailing sizeof(char*) for the NULL sentinel pointer.
       = sizeof(char*) + 3*(1+1+sizeof(char*)) */
    {
        char *a[] = {"a", "b", "c", NULL};
        size_t expected = sizeof(char *) + 3 * (1 + 1 + sizeof(char *));
        test_verify("len(['a','b','c'])", expected == opal_argv_len(a));
    }
}

/* ------------------------------------------------------------------ */
/* opal_argv_copy                                                       */
/* ------------------------------------------------------------------ */

static void test_copy(void)
{
    /* NULL input returns NULL */
    test_verify("copy(NULL) == NULL", NULL == opal_argv_copy(NULL));

    /* empty array (just NULL sentinel) returns a valid [NULL] array */
    {
        char *empty[] = {NULL};
        char **dup = opal_argv_copy(empty);
        test_verify("copy([NULL]): non-NULL", NULL != dup);
        test_verify("copy([NULL]): count == 0", 0 == opal_argv_count(dup));
        opal_argv_free(dup);
    }

    /* normal array: deep copy */
    {
        char *a[] = {"foo", "bar", "baz", NULL};
        char **dup = opal_argv_copy(a);
        test_verify("copy: non-NULL", NULL != dup);
        test_verify("copy: count matches",
                    opal_argv_count(a) == opal_argv_count(dup));
        test_verify("copy: [0] matches", 0 == strcmp(a[0], dup[0]));
        test_verify("copy: [1] matches", 0 == strcmp(a[1], dup[1]));
        test_verify("copy: [2] matches", 0 == strcmp(a[2], dup[2]));
        /* deep copy: different pointers */
        test_verify("copy: [0] different pointer", a[0] != dup[0]);
        opal_argv_free(dup);
    }
}

/* ------------------------------------------------------------------ */
/* opal_argv_delete                                                     */
/* ------------------------------------------------------------------ */

static void test_delete(void)
{
    char **a;
    int argc;

    /* bozo: NULL argv/NULL *argv => OPAL_SUCCESS */
    test_verify("delete(NULL,NULL): OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(NULL, NULL, 0, 0));

    /* bozo: 0 num_to_delete => no-op */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "alpha");
    test_verify("delete 0 items: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(&argc, &a, 0, 0));
    test_verify("delete 0 items: count unchanged", 1 == opal_argv_count(a));
    opal_argv_free(a);

    /* bozo: start > count => no-op */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "alpha");
    test_verify("delete start>count: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(&argc, &a, 7, 1));
    test_verify("delete start>count: count unchanged", 1 == opal_argv_count(a));
    opal_argv_free(a);

    /* bad param: negative start */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "alpha");
    test_verify("delete negative start: OPAL_ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == opal_argv_delete(&argc, &a, -1, 1));
    opal_argv_free(a);

    /* delete first element */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    test_verify("delete first: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(&argc, &a, 0, 1));
    test_verify("delete first: count == 2", 2 == opal_argv_count(a));
    test_verify("delete first: [0]=='b'", 0 == strcmp("b", a[0]));
    test_verify("delete first: [1]=='c'", 0 == strcmp("c", a[1]));
    opal_argv_free(a);

    /* delete from middle */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    opal_argv_append(&argc, &a, "d");
    test_verify("delete middle: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(&argc, &a, 1, 2));
    test_verify("delete middle: count == 2", 2 == opal_argv_count(a));
    test_verify("delete middle: [0]=='a'", 0 == strcmp("a", a[0]));
    test_verify("delete middle: [1]=='d'", 0 == strcmp("d", a[1]));
    opal_argv_free(a);

    /* delete from end */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    test_verify("delete from end: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(&argc, &a, 2, 1));
    test_verify("delete from end: count == 2", 2 == opal_argv_count(a));
    test_verify("delete from end: [0]=='a'", 0 == strcmp("a", a[0]));
    test_verify("delete from end: [1]=='b'", 0 == strcmp("b", a[1]));
    opal_argv_free(a);

    /* over-delete: num_to_delete exceeds remaining elements */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    test_verify("delete over-delete: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(&argc, &a, 0, 99));
    test_verify("delete over-delete: count == 0", 0 == opal_argv_count(a));
    /* Over-deleting (asking to remove more elements than exist) must
     * still leave argc consistent with the actual array length. */
    test_verify("delete over-delete: argc == count", argc == opal_argv_count(a));
    opal_argv_free(a);

    /* delete all starting from middle */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    test_verify("delete from middle to end: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(&argc, &a, 1, 99));
    test_verify("delete from middle to end: count == 1", 1 == opal_argv_count(a));
    test_verify("delete from middle to end: [0]=='a'", 0 == strcmp("a", a[0]));
    opal_argv_free(a);

    /* delete last of 6 */
    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    opal_argv_append(&argc, &a, "d");
    opal_argv_append(&argc, &a, "e");
    opal_argv_append(&argc, &a, "f");
    test_verify("delete last of 6: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_delete(&argc, &a, 5, 1));
    test_verify("delete last of 6: count == 5", 5 == opal_argv_count(a));
    test_verify("delete last of 6: [4]=='e'", 0 == strcmp("e", a[4]));
    opal_argv_free(a);
}

/* ------------------------------------------------------------------ */
/* opal_argv_insert                                                     */
/* ------------------------------------------------------------------ */

static void test_insert(void)
{
    char **target;
    char **source;
    int t, s;

    /* bozo: NULL target => OPAL_ERR_BAD_PARAM */
    test_verify("insert NULL target: OPAL_ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == opal_argv_insert(NULL, 0, NULL));

    /* bozo: *target is NULL => OPAL_ERR_BAD_PARAM */
    {
        char **p = NULL;
        test_verify("insert *target==NULL: OPAL_ERR_BAD_PARAM",
                    OPAL_ERR_BAD_PARAM == opal_argv_insert(&p, 0, NULL));
    }

    /* bozo: negative start => OPAL_ERR_BAD_PARAM */
    target = NULL;
    t = 0;
    opal_argv_append(&t, &target, "orig a");
    test_verify("insert negative start: OPAL_ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == opal_argv_insert(&target, -1, NULL));
    opal_argv_free(target);

    /* NULL source => no-op, OPAL_SUCCESS */
    target = NULL;
    t = 0;
    opal_argv_append(&t, &target, "orig a");
    test_verify("insert NULL source: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert(&target, 0, NULL));
    test_verify("insert NULL source: count unchanged", 1 == opal_argv_count(target));
    opal_argv_free(target);

    /* append to end (start > count) */
    target = NULL;
    t = 0;
    source = NULL;
    s = 0;
    opal_argv_append(&t, &target, "orig a");
    opal_argv_append(&t, &target, "orig b");
    opal_argv_append(&s, &source, "ins a");
    opal_argv_append(&s, &source, "ins b");
    test_verify("insert beyond end: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert(&target, 99, source));
    test_verify("insert beyond end: count == 4", 4 == opal_argv_count(target));
    test_verify("insert beyond end: [0]=='orig a'",
                0 == strcmp("orig a", target[0]));
    test_verify("insert beyond end: [2]=='ins a'",
                0 == strcmp("ins a", target[2]));
    test_verify("insert beyond end: [3]=='ins b'",
                0 == strcmp("ins b", target[3]));
    opal_argv_free(target);
    opal_argv_free(source);

    /* insert at position 0 (beginning) */
    target = NULL;
    t = 0;
    source = NULL;
    s = 0;
    opal_argv_append(&t, &target, "orig a");
    opal_argv_append(&t, &target, "orig b");
    opal_argv_append(&t, &target, "orig c");
    opal_argv_append(&s, &source, "ins a");
    opal_argv_append(&s, &source, "ins b");
    opal_argv_append(&s, &source, "ins c");
    test_verify("insert at 0: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert(&target, 0, source));
    test_verify("insert at 0: count == 6", 6 == opal_argv_count(target));
    test_verify("insert at 0: [0]=='ins a'",
                0 == strcmp("ins a", target[0]));
    test_verify("insert at 0: [1]=='ins b'",
                0 == strcmp("ins b", target[1]));
    test_verify("insert at 0: [2]=='ins c'",
                0 == strcmp("ins c", target[2]));
    test_verify("insert at 0: [3]=='orig a'",
                0 == strcmp("orig a", target[3]));
    test_verify("insert at 0: [4]=='orig b'",
                0 == strcmp("orig b", target[4]));
    test_verify("insert at 0: [5]=='orig c'",
                0 == strcmp("orig c", target[5]));
    opal_argv_free(target);
    opal_argv_free(source);

    /* insert in the middle */
    target = NULL;
    t = 0;
    source = NULL;
    s = 0;
    opal_argv_append(&t, &target, "orig a");
    opal_argv_append(&t, &target, "orig b");
    opal_argv_append(&t, &target, "orig c");
    opal_argv_append(&s, &source, "ins a");
    opal_argv_append(&s, &source, "ins b");
    test_verify("insert middle: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert(&target, 1, source));
    test_verify("insert middle: count == 5", 5 == opal_argv_count(target));
    test_verify("insert middle: [0]=='orig a'",
                0 == strcmp("orig a", target[0]));
    test_verify("insert middle: [1]=='ins a'",
                0 == strcmp("ins a", target[1]));
    test_verify("insert middle: [2]=='ins b'",
                0 == strcmp("ins b", target[2]));
    test_verify("insert middle: [3]=='orig b'",
                0 == strcmp("orig b", target[3]));
    test_verify("insert middle: [4]=='orig c'",
                0 == strcmp("orig c", target[4]));
    opal_argv_free(target);
    opal_argv_free(source);
}

/* ------------------------------------------------------------------ */
/* opal_argv_insert_element                                            */
/* ------------------------------------------------------------------ */

static void test_insert_element(void)
{
    char **target;
    int t;
    char elem[] = "NEW";

    /* bozo: NULL target */
    test_verify("insert_element NULL target: OPAL_ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == opal_argv_insert_element(NULL, 0, elem));

    /* bozo: *target is NULL */
    {
        char **p = NULL;
        test_verify("insert_element *target==NULL: OPAL_ERR_BAD_PARAM",
                    OPAL_ERR_BAD_PARAM == opal_argv_insert_element(&p, 0, elem));
    }

    /* bozo: negative location */
    target = NULL;
    t = 0;
    opal_argv_append(&t, &target, "a");
    test_verify("insert_element negative loc: OPAL_ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == opal_argv_insert_element(&target, -1, elem));
    opal_argv_free(target);

    /* NULL source element: no-op */
    target = NULL;
    t = 0;
    opal_argv_append(&t, &target, "a");
    test_verify("insert_element NULL source: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert_element(&target, 0, NULL));
    test_verify("insert_element NULL source: count unchanged",
                1 == opal_argv_count(target));
    opal_argv_free(target);

    /* insert at position 0 (front) */
    target = NULL;
    t = 0;
    opal_argv_append(&t, &target, "a");
    opal_argv_append(&t, &target, "b");
    opal_argv_append(&t, &target, "c");
    test_verify("insert_element at 0: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert_element(&target, 0, elem));
    test_verify("insert_element at 0: count == 4", 4 == opal_argv_count(target));
    test_verify("insert_element at 0: [0]=='NEW'",
                0 == strcmp("NEW", target[0]));
    test_verify("insert_element at 0: [1]=='a'",
                0 == strcmp("a", target[1]));
    test_verify("insert_element at 0: [2]=='b'",
                0 == strcmp("b", target[2]));
    test_verify("insert_element at 0: [3]=='c'",
                0 == strcmp("c", target[3]));
    opal_argv_free(target);

    /* insert in the middle */
    target = NULL;
    t = 0;
    opal_argv_append(&t, &target, "a");
    opal_argv_append(&t, &target, "b");
    opal_argv_append(&t, &target, "c");
    test_verify("insert_element middle: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert_element(&target, 1, elem));
    test_verify("insert_element middle: count == 4", 4 == opal_argv_count(target));
    test_verify("insert_element middle: [0]=='a'",
                0 == strcmp("a", target[0]));
    test_verify("insert_element middle: [1]=='NEW'",
                0 == strcmp("NEW", target[1]));
    test_verify("insert_element middle: [2]=='b'",
                0 == strcmp("b", target[2]));
    test_verify("insert_element middle: [3]=='c'",
                0 == strcmp("c", target[3]));
    opal_argv_free(target);

    /* insert at end (location == count) */
    target = NULL;
    t = 0;
    opal_argv_append(&t, &target, "a");
    opal_argv_append(&t, &target, "b");
    test_verify("insert_element at count (end): OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert_element(&target, 2, elem));
    test_verify("insert_element at end: count == 3", 3 == opal_argv_count(target));
    test_verify("insert_element at end: [0]=='a'",
                0 == strcmp("a", target[0]));
    test_verify("insert_element at end: [1]=='b'",
                0 == strcmp("b", target[1]));
    test_verify("insert_element at end: [2]=='NEW'",
                0 == strcmp("NEW", target[2]));
    opal_argv_free(target);

    /* insert beyond end (location > count): appended */
    target = NULL;
    t = 0;
    opal_argv_append(&t, &target, "a");
    test_verify("insert_element beyond end: OPAL_SUCCESS",
                OPAL_SUCCESS == opal_argv_insert_element(&target, 99, elem));
    test_verify("insert_element beyond end: count == 2",
                2 == opal_argv_count(target));
    test_verify("insert_element beyond end: [1]=='NEW'",
                0 == strcmp("NEW", target[1]));
    opal_argv_free(target);
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    test_init("opal_argv");

    test_append();
    test_append_nosize();
    test_prepend();
    test_append_unique();
    test_free();
    test_count();
    test_split();
    test_split_with_empty();
    test_join();
    test_join_range();
    test_len();
    test_copy();
    test_delete();
    test_insert();
    test_insert_element();

    return test_finalize();
}
