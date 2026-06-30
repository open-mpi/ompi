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
#include "opal/util/uri.h"
#include "opal/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* -----------------------------------------------------------------------
 * opal_uri_get_scheme
 * ----------------------------------------------------------------------- */

static void test_get_scheme_file(void)
{
    char *scheme;

    scheme = opal_uri_get_scheme("file:///tmp/foo");
    test_verify("get_scheme file:///tmp/foo returns 'file'",
                NULL != scheme && 0 == strcmp("file", scheme));
    free(scheme);
}

static void test_get_scheme_http(void)
{
    char *scheme;

    scheme = opal_uri_get_scheme("http://example.com/path");
    test_verify("get_scheme http://... returns 'http'",
                NULL != scheme && 0 == strcmp("http", scheme));
    free(scheme);
}

static void test_get_scheme_custom(void)
{
    char *scheme;

    scheme = opal_uri_get_scheme("myscheme://host/resource");
    test_verify("get_scheme myscheme://... returns 'myscheme'",
                NULL != scheme && 0 == strcmp("myscheme", scheme));
    free(scheme);
}

static void test_get_scheme_malformed(void)
{
    char *scheme;

    /* No colon -> documented to return NULL */
    scheme = opal_uri_get_scheme("nocolon");
    test_verify("get_scheme malformed (no colon) returns NULL", NULL == scheme);
    /* scheme is NULL; nothing to free */
}

/* -----------------------------------------------------------------------
 * opal_filename_to_uri  (NULL hostname = local file)
 * ----------------------------------------------------------------------- */

static void test_to_uri_local_simple(void)
{
    char *uri;

    uri = opal_filename_to_uri("/tmp/foo.txt", NULL);
    test_verify("to_uri /tmp/foo.txt local: not NULL", NULL != uri);
    test_verify("to_uri /tmp/foo.txt local: == 'file:///tmp/foo.txt'",
                NULL != uri && 0 == strcmp("file:///tmp/foo.txt", uri));
    free(uri);
}

static void test_to_uri_local_deep_path(void)
{
    char *uri;

    uri = opal_filename_to_uri("/a/b/c/d.dat", NULL);
    test_verify("to_uri /a/b/c/d.dat local: == 'file:///a/b/c/d.dat'",
                NULL != uri && 0 == strcmp("file:///a/b/c/d.dat", uri));
    free(uri);
}

static void test_to_uri_relative_path(void)
{
    char *uri;

    /* A relative path is not allowed; function must return NULL */
    uri = opal_filename_to_uri("relative/path", NULL);
    test_verify("to_uri relative path returns NULL", NULL == uri);
}

static void test_to_uri_with_hostname(void)
{
    char *uri;

    uri = opal_filename_to_uri("/data/file.txt", "myhost");
    test_verify("to_uri with hostname: not NULL", NULL != uri);
    /* Expected: file://myhost/data/file.txt */
    test_verify("to_uri with hostname: correct URI",
                NULL != uri && 0 == strcmp("file://myhost/data/file.txt", uri));
    free(uri);
}

/* -----------------------------------------------------------------------
 * opal_filename_from_uri
 * ----------------------------------------------------------------------- */

static void test_from_uri_local_triple_slash(void)
{
    char *fn;
    char *host = (char *) "sentinel"; /* should be set to NULL */

    fn = opal_filename_from_uri("file:///tmp/foo.txt", &host);
    test_verify("from_uri file:///tmp/foo.txt: filename not NULL", NULL != fn);
    test_verify("from_uri file:///tmp/foo.txt: == '/tmp/foo.txt'",
                NULL != fn && 0 == strcmp("/tmp/foo.txt", fn));
    test_verify("from_uri file:///tmp/foo.txt: host is NULL", NULL == host);
    free(fn);
    /* host is NULL; nothing to free */
}

static void test_from_uri_with_hostname(void)
{
    char *fn;
    char *host = NULL;

    fn = opal_filename_from_uri("file://myhost/data/file.txt", &host);
    test_verify("from_uri with hostname: filename not NULL", NULL != fn);
    test_verify("from_uri with hostname: == '/data/file.txt'",
                NULL != fn && 0 == strcmp("/data/file.txt", fn));
    test_verify("from_uri with hostname: host == 'myhost'",
                NULL != host && 0 == strcmp("myhost", host));
    free(fn);
    free(host);
}

static void test_from_uri_hostname_param_null(void)
{
    char *fn;

    /* Documented: passing NULL for hostname is valid */
    fn = opal_filename_from_uri("file:///tmp/bar", NULL);
    test_verify("from_uri NULL hostname param: filename not NULL", NULL != fn);
    test_verify("from_uri NULL hostname param: == '/tmp/bar'",
                NULL != fn && 0 == strcmp("/tmp/bar", fn));
    free(fn);
}

static void test_from_uri_malformed_no_colon(void)
{
    char *fn;

    fn = opal_filename_from_uri("nocolon", NULL);
    test_verify("from_uri malformed (no colon) returns NULL", NULL == fn);
}

static void test_from_uri_malformed_single_slash(void)
{
    char *fn;

    /* "file:/x" has only one slash after ':' -- not "//", not "///" */
    fn = opal_filename_from_uri("file:/x", NULL);
    test_verify("from_uri malformed (single slash) returns NULL", NULL == fn);
}

static void test_from_uri_malformed_no_path_slash(void)
{
    char *fn;

    /* "file://hostonly" has no '/' to separate host from path */
    fn = opal_filename_from_uri("file://hostonly", NULL);
    test_verify("from_uri malformed (no path slash) returns NULL", NULL == fn);
}

/* -----------------------------------------------------------------------
 * Round-trip: path -> URI -> path (local, no hostname)
 * ----------------------------------------------------------------------- */

static void test_roundtrip_local(void)
{
    const char *original = "/usr/local/bin/mpirun";
    char *uri;
    char *recovered;
    char *host = (char *) "sentinel";

    uri = opal_filename_to_uri(original, NULL);
    test_verify("roundtrip local: to_uri not NULL", NULL != uri);

    recovered = opal_filename_from_uri(uri, &host);
    test_verify("roundtrip local: from_uri not NULL", NULL != recovered);
    test_verify("roundtrip local: recovered == original",
                NULL != recovered && 0 == strcmp(original, recovered));
    test_verify("roundtrip local: host is NULL after from_uri", NULL == host);

    free(uri);
    free(recovered);
}

/* -----------------------------------------------------------------------
 * Round-trip: path -> URI -> path (with hostname)
 * ----------------------------------------------------------------------- */

static void test_roundtrip_with_hostname(void)
{
    const char *original = "/home/user/data.txt";
    const char *hostname_in = "compute01";
    char *uri;
    char *recovered;
    char *host = NULL;

    uri = opal_filename_to_uri(original, hostname_in);
    test_verify("roundtrip hostname: to_uri not NULL", NULL != uri);

    recovered = opal_filename_from_uri(uri, &host);
    test_verify("roundtrip hostname: from_uri not NULL", NULL != recovered);
    test_verify("roundtrip hostname: recovered == original",
                NULL != recovered && 0 == strcmp(original, recovered));
    test_verify("roundtrip hostname: host == hostname_in",
                NULL != host && 0 == strcmp(hostname_in, host));

    free(uri);
    free(recovered);
    free(host);
}

/* -----------------------------------------------------------------------
 * get_scheme on a file:// URI
 * ----------------------------------------------------------------------- */

static void test_get_scheme_on_file_uri(void)
{
    char *uri;
    char *scheme;

    uri = opal_filename_to_uri("/etc/passwd", NULL);
    test_verify("get_scheme on built URI: uri not NULL", NULL != uri);

    scheme = opal_uri_get_scheme(uri);
    test_verify("get_scheme on built URI: == 'file'",
                NULL != scheme && 0 == strcmp("file", scheme));

    free(uri);
    free(scheme);
}

/* -----------------------------------------------------------------------
 * reserved-char escaping preserves every filename character
 *
 * opal_filename_to_uri escapes reserved chars when a hostname is present
 * by prefixing each with a backslash.  A previous off-by-one in the
 * escaping loop (`k < strlen(filename) - 1`) dropped the last character
 * of the filename whenever escaping occurred; e.g. "/a;b/c" produced
 * "file://host/a\;b/" (the trailing 'c' was lost).  This test asserts the
 * full, correct result so the bound stays fixed.
 * ----------------------------------------------------------------------- */

static void test_to_uri_reserved_char_escaping(void)
{
    const char *original = "/a;b/c";
    const char *expected = "file://host/a\\;b/c";
    char *uri;

    uri = opal_filename_to_uri(original, "host");
    test_verify("to_uri with reserved char (';'): uri not NULL", NULL != uri);
    /* The reserved ';' is backslash-escaped and no characters are dropped. */
    test_verify("to_uri with reserved char (';'): escapes and preserves all chars",
                NULL != uri && 0 == strcmp(expected, uri));
    free(uri);

    /*
     * A reserved character that repeats is escaped at every occurrence, so
     * the output can grow to 2 bytes per input character.  This exercises
     * the buffer sizing (each ';' adds a backslash) and confirms no
     * characters are dropped.
     */
    original = "/a;b;c";
    expected = "file://host/a\\;b\\;c";
    uri = opal_filename_to_uri(original, "host");
    test_verify("to_uri with repeated reserved char: uri not NULL", NULL != uri);
    test_verify("to_uri with repeated reserved char: escapes each occurrence",
                NULL != uri && 0 == strcmp(expected, uri));
    free(uri);
}

/* -----------------------------------------------------------------------
 * main
 * ----------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    test_init("opal_uri");

    /* opal_uri_get_scheme */
    test_get_scheme_file();
    test_get_scheme_http();
    test_get_scheme_custom();
    test_get_scheme_malformed();

    /* opal_filename_to_uri */
    test_to_uri_local_simple();
    test_to_uri_local_deep_path();
    test_to_uri_relative_path();
    test_to_uri_with_hostname();

    /* opal_filename_from_uri */
    test_from_uri_local_triple_slash();
    test_from_uri_with_hostname();
    test_from_uri_hostname_param_null();
    test_from_uri_malformed_no_colon();
    test_from_uri_malformed_single_slash();
    test_from_uri_malformed_no_path_slash();

    /* Round-trips */
    test_roundtrip_local();
    test_roundtrip_with_hostname();
    test_get_scheme_on_file_uri();

    /* Suspected-bug coverage */
    test_to_uri_reserved_char_escaping();

    return test_finalize();
}
