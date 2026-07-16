/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
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
 * Unit tests for opal/util/keyval_parse.c.
 *
 * Tests: opal_util_keyval_parse (with various file formats).
 *
 * Note: assert() in the library may be a no-op (when built with -DNDEBUG)
 * or active (otherwise), so this test's verification must go through
 * test_verify() rather than relying on assert().
 *
 * Callback note: the keyval_parse.h header states that key and value
 * are "pointers into static buffers" that "may be overwritten
 * immediately after the callback returns."  Therefore the callback
 * MUST copy the strings before returning.
 *
 * Lexer token notes (from keyval_lex.l):
 *   CHAR = [A-Za-z0-9_\-\.]
 *   A line "key = value\n" produces SINGLE_WORD → parse_line() →
 *     callback(key, value) with key and value as lexed (no prefix
 *     stripping).  So "mca_foo = bar" → callback("mca_foo", "bar").
 *
 *   A line "-mca foo bar\n" produces MCAVAR → parse_line_new() with
 *     trim_name(key_buffer, "-mca", NULL) / trim_name(key_buffer,
 *     "--mca", NULL).  The lex token text is the whole "-mca foo" span
 *     (lexer rule: "-"?"-mca"{WHITE}+{CHAR}+{WHITE}+); after trimming
 *     "-mca" and leading/trailing whitespace the key becomes "foo".
 *
 *   Comment lines ("#..." or "//...") return NEWLINE → zero callbacks.
 *
 * Do NOT call opal_util_keyval_parse_init() from this test:
 * opal_init_util() (via opal_init_core.c) already calls it exactly once.
 * A second call re-registers the cleanup and re-constructs keyval_mutex,
 * so opal_finalize_util() then OBJ_DESTRUCTs the mutex twice -- which
 * aborts on the OBJ magic-id assertion in builds with assertions enabled
 * (i.e. without -DNDEBUG, as on some CI platforms).
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/keyval_parse.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* ------------------------------------------------------------------ */
/* callback recording infrastructure                                   */
/* ------------------------------------------------------------------ */

#define MAX_PAIRS 32

typedef struct {
    char *key;
    char *value; /* NULL when the parser provided NULL value */
} kv_pair_t;

static kv_pair_t g_pairs[MAX_PAIRS];
static int       g_npairs;

static void reset_pairs(void)
{
    int i;
    for (i = 0; i < g_npairs; ++i) {
        free(g_pairs[i].key);
        free(g_pairs[i].value);
        g_pairs[i].key   = NULL;
        g_pairs[i].value = NULL;
    }
    g_npairs = 0;
}

static void record_callback(const char *key, const char *value)
{
    if (g_npairs >= MAX_PAIRS) {
        return;
    }
    /* The buffers are static inside the parser -- must strdup. */
    g_pairs[g_npairs].key   = (NULL != key)   ? strdup(key)   : NULL;
    g_pairs[g_npairs].value = (NULL != value)  ? strdup(value) : NULL;
    ++g_npairs;
}

/*
 * Write 'content' to a temp file.  Fills 'tmppath' (must be >= 64 bytes)
 * with the path on success.  Returns 0 on success, -1 on failure.
 */
static int write_temp_file(char *tmppath, size_t bufsz, const char *content)
{
    int fd;
    FILE *fp;

    snprintf(tmppath, bufsz, "/tmp/opal_keyval_test_XXXXXX");
    fd = mkstemp(tmppath);
    if (-1 == fd) {
        return -1;
    }
    fp = fdopen(fd, "w");
    if (NULL == fp) {
        close(fd);
        return -1;
    }
    fputs(content, fp);
    fclose(fp);
    return 0;
}

/* ------------------------------------------------------------------ */
/* opal_util_keyval_parse: plain key = value lines                     */
/* ------------------------------------------------------------------ */

static void test_parse_basic(void)
{
    char tmppath[64];
    int rc;

    /* File with two plain key = value lines and a blank line. */
    const char *content =
        "key1 = value1\n"
        "\n"
        "key2 = value2\n";

    if (0 != write_temp_file(tmppath, sizeof(tmppath), content)) {
        test_failure("test_parse_basic: could not create temp file");
        return;
    }

    reset_pairs();
    rc = opal_util_keyval_parse(tmppath, record_callback);
    unlink(tmppath);

    test_verify("parse_basic: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("parse_basic: 2 pairs recorded", 2 == g_npairs);

    if (2 == g_npairs) {
        test_verify("parse_basic: pair[0].key == 'key1'",
                    NULL != g_pairs[0].key &&
                    0 == strcmp("key1", g_pairs[0].key));
        test_verify("parse_basic: pair[0].value == 'value1'",
                    NULL != g_pairs[0].value &&
                    0 == strcmp("value1", g_pairs[0].value));

        test_verify("parse_basic: pair[1].key == 'key2'",
                    NULL != g_pairs[1].key &&
                    0 == strcmp("key2", g_pairs[1].key));
        test_verify("parse_basic: pair[1].value == 'value2'",
                    NULL != g_pairs[1].value &&
                    0 == strcmp("value2", g_pairs[1].value));
    }

    reset_pairs();
}

/* ------------------------------------------------------------------ */
/* opal_util_keyval_parse: mca-prefixed key = value                   */
/* ------------------------------------------------------------------ */

static void test_parse_mca_prefix(void)
{
    char tmppath[64];
    int rc;

    /*
     * A line "mca_foo = bar" uses CHAR tokens and the plain "=" path,
     * so it goes through parse_line().  The key received by the callback
     * is the literal lexed token "mca_foo" (no prefix stripping occurs
     * in parse_line).
     */
    const char *content =
        "mca_foo = bar\n"
        "mca_btl = tcp\n";

    if (0 != write_temp_file(tmppath, sizeof(tmppath), content)) {
        test_failure("test_parse_mca_prefix: could not create temp file");
        return;
    }

    reset_pairs();
    rc = opal_util_keyval_parse(tmppath, record_callback);
    unlink(tmppath);

    test_verify("parse_mca_prefix: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("parse_mca_prefix: 2 pairs recorded", 2 == g_npairs);

    if (2 == g_npairs) {
        test_verify("parse_mca_prefix: pair[0].key == 'mca_foo'",
                    NULL != g_pairs[0].key &&
                    0 == strcmp("mca_foo", g_pairs[0].key));
        test_verify("parse_mca_prefix: pair[0].value == 'bar'",
                    NULL != g_pairs[0].value &&
                    0 == strcmp("bar", g_pairs[0].value));

        test_verify("parse_mca_prefix: pair[1].key == 'mca_btl'",
                    NULL != g_pairs[1].key &&
                    0 == strcmp("mca_btl", g_pairs[1].key));
        test_verify("parse_mca_prefix: pair[1].value == 'tcp'",
                    NULL != g_pairs[1].value &&
                    0 == strcmp("tcp", g_pairs[1].value));
    }

    reset_pairs();
}

/* ------------------------------------------------------------------ */
/* opal_util_keyval_parse: comment lines produce zero callbacks        */
/* ------------------------------------------------------------------ */

static void test_parse_comments(void)
{
    char tmppath[64];
    int rc;

    /* Only comment and blank lines: no callbacks expected. */
    const char *content =
        "# This is a comment\n"
        "// Another comment\n"
        "\n"
        "# More comments\n";

    if (0 != write_temp_file(tmppath, sizeof(tmppath), content)) {
        test_failure("test_parse_comments: could not create temp file");
        return;
    }

    reset_pairs();
    rc = opal_util_keyval_parse(tmppath, record_callback);
    unlink(tmppath);

    test_verify("parse_comments: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("parse_comments: 0 pairs recorded", 0 == g_npairs);

    reset_pairs();
}

/* ------------------------------------------------------------------ */
/* opal_util_keyval_parse: mixed content                               */
/* ------------------------------------------------------------------ */

static void test_parse_mixed(void)
{
    char tmppath[64];
    int rc;

    /* Mix of comments, blank lines, and real key=value pairs. */
    const char *content =
        "# top comment\n"
        "\n"
        "alpha = one\n"
        "# middle comment\n"
        "beta = two\n"
        "\n"
        "gamma = three\n";

    if (0 != write_temp_file(tmppath, sizeof(tmppath), content)) {
        test_failure("test_parse_mixed: could not create temp file");
        return;
    }

    reset_pairs();
    rc = opal_util_keyval_parse(tmppath, record_callback);
    unlink(tmppath);

    test_verify("parse_mixed: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("parse_mixed: 3 pairs recorded", 3 == g_npairs);

    if (3 == g_npairs) {
        test_verify("parse_mixed: pair[0].key == 'alpha'",
                    NULL != g_pairs[0].key &&
                    0 == strcmp("alpha", g_pairs[0].key));
        test_verify("parse_mixed: pair[0].value == 'one'",
                    NULL != g_pairs[0].value &&
                    0 == strcmp("one", g_pairs[0].value));

        test_verify("parse_mixed: pair[1].key == 'beta'",
                    NULL != g_pairs[1].key &&
                    0 == strcmp("beta", g_pairs[1].key));
        test_verify("parse_mixed: pair[1].value == 'two'",
                    NULL != g_pairs[1].value &&
                    0 == strcmp("two", g_pairs[1].value));

        test_verify("parse_mixed: pair[2].key == 'gamma'",
                    NULL != g_pairs[2].key &&
                    0 == strcmp("gamma", g_pairs[2].key));
        test_verify("parse_mixed: pair[2].value == 'three'",
                    NULL != g_pairs[2].value &&
                    0 == strcmp("three", g_pairs[2].value));
    }

    reset_pairs();
}

/* ------------------------------------------------------------------ */
/* opal_util_keyval_parse: nonexistent file                            */
/* ------------------------------------------------------------------ */

static void test_parse_nonexistent(void)
{
    int rc;
    const char *nonexistent = "/tmp/opal_keyval_test_file_that_does_not_exist_xyzzy";

    reset_pairs();
    rc = opal_util_keyval_parse(nonexistent, record_callback);

    test_verify("parse_nonexistent: returns OPAL_ERR_NOT_FOUND",
                OPAL_ERR_NOT_FOUND == rc);
    test_verify("parse_nonexistent: 0 callbacks", 0 == g_npairs);

    reset_pairs();
}

/* ------------------------------------------------------------------ */
/* opal_util_keyval_parse: key with no value (empty after =)          */
/* ------------------------------------------------------------------ */

static void test_parse_empty_value(void)
{
    char tmppath[64];
    int rc;

    /*
     * A line "key =" with nothing after the '=' (only newline) causes
     * the VALUE start condition to see {WHITE}*\n → NEWLINE.
     * parse_line() then calls keyval_callback(key_buffer, NULL).
     */
    const char *content = "mykey =\n";

    if (0 != write_temp_file(tmppath, sizeof(tmppath), content)) {
        test_failure("test_parse_empty_value: could not create temp file");
        return;
    }

    reset_pairs();
    rc = opal_util_keyval_parse(tmppath, record_callback);
    unlink(tmppath);

    test_verify("parse_empty_value: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("parse_empty_value: 1 pair recorded", 1 == g_npairs);

    if (1 == g_npairs) {
        test_verify("parse_empty_value: key == 'mykey'",
                    NULL != g_pairs[0].key &&
                    0 == strcmp("mykey", g_pairs[0].key));
        /* Value is NULL for empty right-hand side. */
        test_verify("parse_empty_value: value is NULL",
                    NULL == g_pairs[0].value);
    }

    reset_pairs();
}

/* ------------------------------------------------------------------ */
/* opal_util_keyval_parse: whitespace-padded values                   */
/* ------------------------------------------------------------------ */

static void test_parse_whitespace_value(void)
{
    char tmppath[64];
    int rc;

    /*
     * The VALUE lex rule is:
     *   <VALUE>[^\n]*[^\t \n]/[\t ]*  -> OPAL_UTIL_KEYVAL_PARSE_VALUE
     * This captures everything up to (but not including) trailing
     * whitespace.  So "key = hello world" produces value "hello world"
     * (internal spaces preserved, trailing spaces stripped).
     */
    const char *content = "key = hello world\n";

    if (0 != write_temp_file(tmppath, sizeof(tmppath), content)) {
        test_failure("test_parse_whitespace_value: could not create temp file");
        return;
    }

    reset_pairs();
    rc = opal_util_keyval_parse(tmppath, record_callback);
    unlink(tmppath);

    test_verify("parse_whitespace_value: returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
    test_verify("parse_whitespace_value: 1 pair recorded", 1 == g_npairs);

    if (1 == g_npairs) {
        test_verify("parse_whitespace_value: key == 'key'",
                    NULL != g_pairs[0].key &&
                    0 == strcmp("key", g_pairs[0].key));
        test_verify("parse_whitespace_value: value == 'hello world'",
                    NULL != g_pairs[0].value &&
                    0 == strcmp("hello world", g_pairs[0].value));
    }

    reset_pairs();
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    opal_init_util(&argc, &argv);
    test_init("opal_keyval_parse");

    test_parse_basic();
    test_parse_mca_prefix();
    test_parse_comments();
    test_parse_mixed();
    test_parse_nonexistent();
    test_parse_empty_value();
    test_parse_whitespace_value();

    r = test_finalize();
    opal_finalize_util();
    return r;
}
