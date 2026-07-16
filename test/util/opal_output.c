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
 * Unit tests for the opal_output subsystem.
 *
 * Functions exercised:
 *   opal_output_init      (called implicitly by opal_init_util)
 *   opal_output_open      (open a new stream with an opal_output_stream_t)
 *   opal_output_close     (close a stream)
 *   opal_output           (formatted output to a stream)
 *   opal_output_verbose   (macro -- output gated by verbosity level)
 *   opal_output_set_verbosity / opal_output_get_verbosity
 *   opal_output_check_verbosity
 *   opal_output_string    (return output as a malloc'd string)
 *   opal_output_reopen    (redirect an existing stream)
 *   opal_output_set_output_file_info (control file naming for file streams)
 *
 * File output strategy:
 *   We call opal_output_set_output_file_info() to set a known, process-
 *   unique directory (mkdtemp) and a fixed prefix so the filename is
 *   predictable.  The file is only created lazily on the first write, so
 *   we call opal_output() before attempting to fopen() it.  After each
 *   test we close the stream, unlink the file, and restore the old
 *   output dir/prefix.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/output.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* ------------------------------------------------------------------ */
/* helpers                                                             */
/* ------------------------------------------------------------------ */

/* Build a predictable filename: dir + "/" + prefix + suffix. */
static void build_filename(char *buf, size_t bufsz,
                           const char *dir, const char *prefix,
                           const char *suffix)
{
    snprintf(buf, bufsz, "%s/%s%s", dir, prefix, suffix);
}

/*
 * Read the entire content of a file into a malloc'd buffer.
 * Returns NULL on failure.  Caller must free().
 */
static char *slurp_file(const char *path)
{
    FILE *fp;
    long len;
    char *buf;
    size_t nread;

    fp = fopen(path, "r");
    if (NULL == fp) {
        return NULL;
    }
    if (0 != fseek(fp, 0, SEEK_END)) {
        fclose(fp);
        return NULL;
    }
    len = ftell(fp);
    if (len < 0) {
        fclose(fp);
        return NULL;
    }
    rewind(fp);
    buf = (char *) malloc((size_t) len + 1);
    if (NULL == buf) {
        fclose(fp);
        return NULL;
    }
    nread = fread(buf, 1, (size_t) len, fp);
    buf[nread] = '\0';
    fclose(fp);
    return buf;
}

/* ------------------------------------------------------------------ */
/* test_open_close_stderr                                              */
/*                                                                     */
/* Open a stream directed only to stderr; verify handle is >= 0;      */
/* then close it.                                                      */
/* ------------------------------------------------------------------ */
static void test_open_close_stderr(void)
{
    opal_output_stream_t lds;
    int h;

    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr = true;

    h = opal_output_open(&lds);
    test_verify("opal_output_open(stderr) returns valid handle", h >= 0);
    if (h >= 0) {
        opal_output_close(h);
    }
    OBJ_DESTRUCT(&lds);
}

/* ------------------------------------------------------------------ */
/* test_verbosity_get_set                                              */
/*                                                                     */
/* Open a stream, set its verbosity, read it back, and check the      */
/* gating predicate opal_output_check_verbosity.                      */
/* ------------------------------------------------------------------ */
static void test_verbosity_get_set(void)
{
    opal_output_stream_t lds;
    int h;

    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr  = true;
    lds.lds_verbose_level = 0;

    h = opal_output_open(&lds);
    test_verify("verbosity test: open succeeds", h >= 0);
    if (h < 0) {
        OBJ_DESTRUCT(&lds);
        return;
    }

    /* After open, verbosity should be 0 (set in lds). */
    test_verify("verbosity: initial get_verbosity == 0",
                0 == opal_output_get_verbosity(h));

    /* check_verbosity(0, h): level 0 <= stream verbosity 0 -> true */
    test_verify("verbosity: check_verbosity(0) true when level == 0",
                opal_output_check_verbosity(0, h));

    /* check_verbosity(1, h): level 1 > stream verbosity 0 -> false */
    test_verify("verbosity: check_verbosity(1) false when stream_level == 0",
                !opal_output_check_verbosity(1, h));

    opal_output_set_verbosity(h, 5);
    test_verify("verbosity: set_verbosity(5) -> get == 5",
                5 == opal_output_get_verbosity(h));

    /* Now level 5 should pass, level 6 should not. */
    test_verify("verbosity: check_verbosity(5) true when stream_level == 5",
                opal_output_check_verbosity(5, h));
    test_verify("verbosity: check_verbosity(6) false when stream_level == 5",
                !opal_output_check_verbosity(6, h));

    opal_output_close(h);
    OBJ_DESTRUCT(&lds);
}

/* ------------------------------------------------------------------ */
/* test_output_string                                                  */
/*                                                                     */
/* opal_output_string returns a malloc'd string when verbose_level    */
/* permits, and NULL when it does not.                                 */
/* ------------------------------------------------------------------ */
static void test_output_string(void)
{
    opal_output_stream_t lds;
    int h;
    char *s;

    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr   = false;
    lds.lds_verbose_level = 3;

    h = opal_output_open(&lds);
    test_verify("output_string: open succeeds", h >= 0);
    if (h < 0) {
        OBJ_DESTRUCT(&lds);
        return;
    }

    /* level 3 <= stream verbosity 3 -> should return a string */
    s = opal_output_string(3, h, "hello %s", "world");
    test_verify("output_string: returns non-NULL when level <= verbosity",
                NULL != s);
    if (NULL != s) {
        test_verify("output_string: payload present in returned string",
                    NULL != strstr(s, "hello world"));
        free(s);
    }

    /* level 4 > stream verbosity 3 -> should return NULL */
    s = opal_output_string(4, h, "should not appear");
    test_verify("output_string: returns NULL when level > verbosity",
                NULL == s);

    opal_output_close(h);
    OBJ_DESTRUCT(&lds);
}

/* ------------------------------------------------------------------ */
/* test_output_verbose_macro                                           */
/*                                                                     */
/* opal_output_verbose is a macro gated by opal_output_check_verbosity*/
/* We can only verify it compiles and doesn't crash; side effects are  */
/* to stderr so we can't capture them here.                            */
/* ------------------------------------------------------------------ */
static void test_output_verbose_macro(void)
{
    opal_output_stream_t lds;
    int h;

    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr   = true;
    lds.lds_verbose_level = 2;

    h = opal_output_open(&lds);
    test_verify("output_verbose: open succeeds", h >= 0);
    if (h < 0) {
        OBJ_DESTRUCT(&lds);
        return;
    }

    /* These calls must not crash (level 2 passes; level 3 is suppressed). */
    opal_output_verbose(2, h, "verbose message at level 2");
    opal_output_verbose(3, h, "this should be suppressed");

    test_verify("output_verbose: survived both calls without crash", 1);

    opal_output_close(h);
    OBJ_DESTRUCT(&lds);
}

/* ------------------------------------------------------------------ */
/* test_file_output                                                    */
/*                                                                     */
/* Direct output to a file in a temp directory, then verify the file  */
/* received the payload.                                               */
/*                                                                     */
/* File naming: output_dir + "/" + output_prefix + suffix             */
/*   We set output_dir = our tmpdir                                   */
/*      and output_prefix = "opaltest-"                               */
/*      suffix = "output-test.txt"                                    */
/* ------------------------------------------------------------------ */
static void test_file_output(void)
{
    char tmpdir[] = "/tmp/opaltest.XXXXXX";
    char *dir;
    char *olddir  = NULL;
    char *oldpfx  = NULL;
    opal_output_stream_t lds;
    int h;
    char filename[1024];
    char *content;
    const char *prefix = "opaltest-";
    const char *suffix = "output-test.txt";
    const char *payload = "TESTPAYLOAD_12345";

    dir = mkdtemp(tmpdir);
    test_verify("test_file_output: mkdtemp succeeded", NULL != dir);
    if (NULL == dir) {
        return;
    }

    /* Redirect output infrastructure to our temp directory. */
    opal_output_set_output_file_info(dir, prefix, &olddir, &oldpfx);

    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr      = false;
    lds.lds_want_file        = true;
    lds.lds_want_file_append = false;
    lds.lds_file_suffix      = strdup(suffix); /* owned + freed by OBJ_DESTRUCT */

    h = opal_output_open(&lds);
    test_verify("test_file_output: open returns valid handle", h >= 0);
    if (h < 0) {
        OBJ_DESTRUCT(&lds);
        opal_output_set_output_file_info(olddir, oldpfx, NULL, NULL);
        free(olddir);
        free(oldpfx);
        rmdir(dir);
        return;
    }

    /* The file is created lazily on first write. */
    opal_output(h, "%s", payload);

    opal_output_close(h);
    OBJ_DESTRUCT(&lds);

    /* Check content. */
    build_filename(filename, sizeof(filename), dir, prefix, suffix);
    content = slurp_file(filename);
    test_verify("test_file_output: output file exists and is readable",
                NULL != content);
    if (NULL != content) {
        test_verify("test_file_output: payload found in file content",
                    NULL != strstr(content, payload));
        free(content);
    }

    /* Cleanup. */
    unlink(filename);
    rmdir(dir);

    /* Restore previous output dir/prefix. */
    opal_output_set_output_file_info(olddir, oldpfx, NULL, NULL);
    free(olddir);
    free(oldpfx);
}

/* ------------------------------------------------------------------ */
/* test_reopen                                                         */
/*                                                                     */
/* opal_output_reopen() redirects an existing stream to a new set of  */
/* destinations.  We open a stream to stderr, reopen it with a        */
/* different verbosity level, and verify the new level took effect.   */
/* ------------------------------------------------------------------ */
static void test_reopen(void)
{
    opal_output_stream_t lds;
    int h;
    int h2;

    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr   = true;
    lds.lds_verbose_level = 0;

    h = opal_output_open(&lds);
    test_verify("reopen: initial open succeeds", h >= 0);
    if (h < 0) {
        OBJ_DESTRUCT(&lds);
        return;
    }

    /* Reopen with a new verbosity level. */
    lds.lds_verbose_level = 7;
    h2 = opal_output_reopen(h, &lds);

    /*
     * opal_output_reopen returns the stream id (same as h when reopening an
     * existing id).
     */
    test_verify("reopen: returns the same stream id", h == h2);
    test_verify("reopen: new verbosity level is visible",
                7 == opal_output_get_verbosity(h));

    opal_output_close(h);
    OBJ_DESTRUCT(&lds);
}

/* ------------------------------------------------------------------ */
/* test_get_verbosity_invalid_handle                                   */
/*                                                                     */
/* opal_output_get_verbosity with an unused/invalid id must return -1.*/
/* ------------------------------------------------------------------ */
static void test_get_verbosity_invalid_handle(void)
{
    /* Stream id 63 is within OPAL_OUTPUT_MAX_STREAMS (64) but we never
     * open it, so ldi_used should be false and get_verbosity must return -1. */
    test_verify("get_verbosity(unused id) returns -1",
                -1 == opal_output_get_verbosity(63));

    /* Negative id: also returns -1. */
    test_verify("get_verbosity(-1) returns -1",
                -1 == opal_output_get_verbosity(-1));
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    test_init("opal_output");

    opal_init_util(&argc, &argv);

    test_open_close_stderr();
    test_verbosity_get_set();
    test_output_string();
    test_output_verbose_macro();
    test_file_output();
    test_reopen();
    test_get_verbosity_invalid_handle();

    r = test_finalize();
    opal_finalize_util();
    return r;
}
