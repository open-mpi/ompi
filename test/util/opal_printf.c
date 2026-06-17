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
#include "opal/util/printf.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* -----------------------------------------------------------------------
 * Helpers to exercise the va_list variants
 * ----------------------------------------------------------------------- */

static int call_vasprintf(char **ptr, const char *fmt, ...)
{
    va_list ap;
    int rc;

    va_start(ap, fmt);
    rc = opal_vasprintf(ptr, fmt, ap);
    va_end(ap);
    return rc;
}

static int call_vsnprintf(char *str, size_t size, const char *fmt, ...)
{
    va_list ap;
    int rc;

    va_start(ap, fmt);
    rc = opal_vsnprintf(str, size, fmt, ap);
    va_end(ap);
    return rc;
}

/* -----------------------------------------------------------------------
 * opal_asprintf tests
 * ----------------------------------------------------------------------- */

static void test_asprintf_string(void)
{
    char *s = NULL;
    int rc;

    rc = opal_asprintf(&s, "%s", "hello");
    test_verify("asprintf %s: rc == 5", 5 == rc);
    test_verify("asprintf %s: string matches", NULL != s && 0 == strcmp("hello", s));
    free(s);
}

static void test_asprintf_int(void)
{
    char *s = NULL;
    int rc;

    rc = opal_asprintf(&s, "%d", 42);
    test_verify("asprintf %d: rc == 2", 2 == rc);
    test_verify("asprintf %d: string matches", NULL != s && 0 == strcmp("42", s));
    free(s);
}

static void test_asprintf_negative_int(void)
{
    char *s = NULL;
    int rc;

    rc = opal_asprintf(&s, "%d", -7);
    test_verify("asprintf negative int: rc == 2", 2 == rc);
    test_verify("asprintf negative int: string matches", NULL != s && 0 == strcmp("-7", s));
    free(s);
}

static void test_asprintf_float(void)
{
    char *s = NULL;
    int rc;

    /* Use %.1f for predictable output */
    rc = opal_asprintf(&s, "%.1f", 3.5);
    test_verify("asprintf %.1f: rc > 0", rc > 0);
    test_verify("asprintf %.1f: string matches", NULL != s && 0 == strcmp("3.5", s));
    free(s);
}

static void test_asprintf_empty_format(void)
{
    char *s = NULL;
    int rc;
    /* Use a variable to avoid -Wformat-zero-length on a string literal "" */
    const char *fmt = "";

    rc = opal_asprintf(&s, fmt);
    test_verify("asprintf empty format: rc == 0", 0 == rc);
    test_verify("asprintf empty format: string is empty", NULL != s && 0 == strcmp("", s));
    free(s);
}

static void test_asprintf_mixed(void)
{
    char *s = NULL;
    int rc;

    rc = opal_asprintf(&s, "%s=%d", "x", 1);
    test_verify("asprintf mixed: rc == 3", 3 == rc);
    test_verify("asprintf mixed: string matches", NULL != s && 0 == strcmp("x=1", s));
    free(s);
}

/* -----------------------------------------------------------------------
 * opal_vasprintf tests
 * ----------------------------------------------------------------------- */

static void test_vasprintf_string(void)
{
    char *s = NULL;
    int rc;

    rc = call_vasprintf(&s, "%s", "world");
    test_verify("vasprintf %s: rc == 5", 5 == rc);
    test_verify("vasprintf %s: string matches", NULL != s && 0 == strcmp("world", s));
    free(s);
}

static void test_vasprintf_int(void)
{
    char *s = NULL;
    int rc;

    rc = call_vasprintf(&s, "%d", 100);
    test_verify("vasprintf %d: rc == 3", 3 == rc);
    test_verify("vasprintf %d: string matches", NULL != s && 0 == strcmp("100", s));
    free(s);
}

static void test_vasprintf_empty_format(void)
{
    char *s = NULL;
    int rc;
    const char *fmt = "";

    rc = call_vasprintf(&s, fmt);
    test_verify("vasprintf empty format: rc == 0", 0 == rc);
    test_verify("vasprintf empty format: empty string", NULL != s && 0 == strcmp("", s));
    free(s);
}

/* -----------------------------------------------------------------------
 * opal_snprintf tests
 * ----------------------------------------------------------------------- */

static void test_snprintf_fits(void)
{
    char buf[64];
    int rc;

    rc = opal_snprintf(buf, sizeof(buf), "%s=%d", "key", 99);
    test_verify("snprintf fits: rc == 6", 6 == rc);
    test_verify("snprintf fits: content", 0 == strcmp("key=99", buf));
}

static void test_snprintf_exact_fit(void)
{
    /* "hello" is 5 chars; buffer of 6 is exactly big enough (5 + NUL) */
    char buf[6];
    int rc;

    rc = opal_snprintf(buf, sizeof(buf), "%s", "hello");
    /* Returns full would-have-been length = 5 */
    test_verify("snprintf exact fit: rc == 5", 5 == rc);
    test_verify("snprintf exact fit: content", 0 == strcmp("hello", buf));
}

static void test_snprintf_truncation(void)
{
    /* Buffer of 5 forces truncation of "hello" (needs 6 bytes) */
    char buf[5];
    int rc;

    rc = opal_snprintf(buf, sizeof(buf), "%s", "hello");
    /* Returns the would-have-been length (5), not the truncated length */
    test_verify("snprintf truncation: rc == 5 (would-have-been)", 5 == rc);
    /* Output must be NUL-terminated */
    test_verify("snprintf truncation: NUL-terminated", '\0' == buf[4]);
    /* Truncated content: "hell\0" */
    test_verify("snprintf truncation: first 4 chars", 0 == strncmp("hell", buf, 4));
    test_verify("snprintf truncation: strlen == 4", 4 == (int) strlen(buf));
}

static void test_snprintf_null_buffer_zero_size(void)
{
    int rc;

    /* C99: NULL/0 returns the would-have-been length without writing */
    rc = opal_snprintf(NULL, 0, "%s %d", "test", 1004);
    test_verify("snprintf NULL/0: returns full length", rc > 0);
    /* "test 1004" is 9 chars */
    test_verify("snprintf NULL/0: rc == 9", 9 == rc);
}

static void test_snprintf_empty_format(void)
{
    char buf[8];
    int rc;
    const char *fmt = "";

    rc = opal_snprintf(buf, sizeof(buf), fmt);
    test_verify("snprintf empty format: rc == 0", 0 == rc);
    test_verify("snprintf empty format: empty string", 0 == strcmp("", buf));
}

static void test_snprintf_one_byte_buffer(void)
{
    /* A 1-byte buffer can only hold the NUL terminator */
    char buf[1];
    int rc;

    rc = opal_snprintf(buf, sizeof(buf), "%d", 42);
    test_verify("snprintf 1-byte buf: returns full length 2", 2 == rc);
    test_verify("snprintf 1-byte buf: buf[0] is NUL", '\0' == buf[0]);
}

/* -----------------------------------------------------------------------
 * opal_vsnprintf tests
 * ----------------------------------------------------------------------- */

static void test_vsnprintf_fits(void)
{
    char buf[64];
    int rc;

    rc = call_vsnprintf(buf, sizeof(buf), "%s", "vsnprintf");
    test_verify("vsnprintf fits: rc == 9", 9 == rc);
    test_verify("vsnprintf fits: content", 0 == strcmp("vsnprintf", buf));
}

static void test_vsnprintf_truncation(void)
{
    char buf[4];
    int rc;

    /* "abc" fits in 4 bytes ("abc\0"), "abcd" does not */
    rc = call_vsnprintf(buf, sizeof(buf), "%s", "abcd");
    /* Returns the full would-have-been length = 4 */
    test_verify("vsnprintf truncation: rc == 4", 4 == rc);
    test_verify("vsnprintf truncation: NUL-terminated", '\0' == buf[3]);
    test_verify("vsnprintf truncation: first 3 chars", 0 == strncmp("abc", buf, 3));
}

static void test_vsnprintf_length_equals_size_minus_one(void)
{
    /* content "abc" is 3 chars; buffer size=4 => fits exactly */
    char buf[4];
    int rc;

    rc = call_vsnprintf(buf, sizeof(buf), "%s", "abc");
    test_verify("vsnprintf length==size-1: rc == 3", 3 == rc);
    test_verify("vsnprintf length==size-1: content", 0 == strcmp("abc", buf));
}

static void test_vsnprintf_length_equals_size(void)
{
    /* "abcd" is 4 chars; buffer size=4 means last char is dropped */
    char buf[4];
    int rc;

    rc = call_vsnprintf(buf, sizeof(buf), "%s", "abcd");
    test_verify("vsnprintf length==size: rc == 4", 4 == rc);
    /* Only "abc" fits plus NUL */
    test_verify("vsnprintf length==size: content truncated", 0 == strcmp("abc", buf));
}

static void test_vsnprintf_null_zero(void)
{
    int rc;

    rc = call_vsnprintf(NULL, 0, "%d", 999);
    test_verify("vsnprintf NULL/0: returns 3", 3 == rc);
}

/* -----------------------------------------------------------------------
 * main
 * ----------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    test_init("opal_printf");

    /* opal_asprintf */
    test_asprintf_string();
    test_asprintf_int();
    test_asprintf_negative_int();
    test_asprintf_float();
    test_asprintf_empty_format();
    test_asprintf_mixed();

    /* opal_vasprintf */
    test_vasprintf_string();
    test_vasprintf_int();
    test_vasprintf_empty_format();

    /* opal_snprintf */
    test_snprintf_fits();
    test_snprintf_exact_fit();
    test_snprintf_truncation();
    test_snprintf_null_buffer_zero_size();
    test_snprintf_empty_format();
    test_snprintf_one_byte_buffer();

    /* opal_vsnprintf */
    test_vsnprintf_fits();
    test_vsnprintf_truncation();
    test_vsnprintf_length_equals_size_minus_one();
    test_vsnprintf_length_equals_size();
    test_vsnprintf_null_zero();

    return test_finalize();
}
