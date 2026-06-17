/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Unit test for opal_cmd_line_t and its full public API.
 *
 * Coverage targets (opal/util/cmd_line.c, 539 lines, 0% prior coverage):
 *   opal_cmd_line_create, opal_cmd_line_add, opal_cmd_line_make_opt_mca,
 *   opal_cmd_line_make_opt3, opal_cmd_line_parse, opal_cmd_line_get_usage_msg,
 *   opal_cmd_line_is_taken, opal_cmd_line_get_ninsts, opal_cmd_line_get_param,
 *   opal_cmd_line_get_argc, opal_cmd_line_get_argv, opal_cmd_line_get_tail.
 *   Also exercises internally: make_opt, find_option, split_shorts, set_dest,
 *   free_parse_results, fill, qsort_callback, get_help_otype, build_parsable.
 *
 * NOTE: -DNDEBUG is set; assert() is a no-op. All verification uses
 * test_verify().  Do NOT use assert().
 *
 * NOTE: The cmd_line module uses opal_output / MCA / opal classes, so
 * opal_init_util() is required before any cmd_line calls.
 */

#include "opal_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "support.h"

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

/* -----------------------------------------------------------------------
 * Forward declarations
 * ----------------------------------------------------------------------- */
static void test_make_opt3_errors(void);
static void test_create_and_add(void);
static void test_make_opt_mca(void);
static void test_parse_flags(void);
static void test_parse_option_with_params(void);
static void test_get_param_documented_example(void);
static void test_parse_multiple_instances(void);
static void test_parse_single_dash_name(void);
static void test_combined_shorts(void);
static void test_parse_double_dash_separator(void);
static void test_parse_unknown_option_error(void);
static void test_parse_missing_param_error(void);
static void test_parse_ignore_unknown(void);
static void test_get_argc_and_argv(void);
static void test_get_tail(void);
static void test_get_ninsts_unregistered(void);
static void test_get_usage_msg(void);
static void test_set_dest_variable(void);
static void test_reparse(void);
static void test_set_dest_int_invalid(void);

/* -----------------------------------------------------------------------
 * main
 * ----------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    int rc;

    test_init("opal_cmd_line");

    rc = opal_init_util(&argc, &argv);
    test_verify("opal_init_util succeeds", OPAL_SUCCESS == rc);
    if (OPAL_SUCCESS != rc) {
        return test_finalize();
    }

    test_make_opt3_errors();
    test_create_and_add();
    test_make_opt_mca();
    test_parse_flags();
    test_parse_option_with_params();
    test_get_param_documented_example();
    test_parse_multiple_instances();
    test_parse_single_dash_name();
    test_combined_shorts();
    test_parse_double_dash_separator();
    test_parse_unknown_option_error();
    test_parse_missing_param_error();
    test_parse_ignore_unknown();
    test_get_argc_and_argv();
    test_get_tail();
    test_get_ninsts_unregistered();
    test_get_usage_msg();
    test_set_dest_variable();
    test_set_dest_int_invalid();
    test_reparse();

    opal_finalize_util();

    return test_finalize();
}

/* -----------------------------------------------------------------------
 * test_make_opt3_errors -- error paths in make_opt / make_opt3
 * ----------------------------------------------------------------------- */
static void test_make_opt3_errors(void)
{
    opal_cmd_line_t cmd;
    int rc;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);

    /* All three name arguments NULL/'\0' must fail */
    rc = opal_cmd_line_make_opt3(&cmd, '\0', NULL, NULL, 0, "no names");
    test_verify("make_opt3 all-empty names returns ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == rc);

    /* num_params < 0 must fail */
    rc = opal_cmd_line_make_opt3(&cmd, 'x', NULL, NULL, -1, "neg params");
    test_verify("make_opt3 num_params<0 returns ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == rc);

    /* Valid option must succeed */
    rc = opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose mode");
    test_verify("make_opt3 valid short+long succeeds",
                OPAL_SUCCESS == rc);

    /* Duplicate long name must fail */
    rc = opal_cmd_line_make_opt3(&cmd, '\0', NULL, "verbose", 0, "Dup");
    test_verify("make_opt3 duplicate long name returns ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == rc);

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_create_and_add -- opal_cmd_line_create + opal_cmd_line_add
 * ----------------------------------------------------------------------- */
static void test_create_and_add(void)
{
    opal_cmd_line_t cmd;
    int rc;

    /* Table-driven create */
    opal_cmd_line_init_t table[] = {
        { NULL, 'h', NULL, "help", 0,
          NULL, OPAL_CMD_LINE_TYPE_NULL,
          "This help message", OPAL_CMD_LINE_OTYPE_GENERAL },

        { NULL, 'n', NULL, "numprocs", 1,
          NULL, OPAL_CMD_LINE_TYPE_NULL,
          "Number of processes", OPAL_CMD_LINE_OTYPE_GENERAL },

        /* Sentinel */
        { NULL, '\0', NULL, NULL, 0,
          NULL, OPAL_CMD_LINE_TYPE_NULL, NULL, OPAL_CMD_LINE_OTYPE_NULL }
    };

    rc = opal_cmd_line_create(&cmd, table);
    test_verify("opal_cmd_line_create returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* Extend with an additional option via opal_cmd_line_add */
    opal_cmd_line_init_t extra[] = {
        { NULL, 'd', NULL, "debug", 0,
          NULL, OPAL_CMD_LINE_TYPE_NULL,
          "Enable debug", OPAL_CMD_LINE_OTYPE_DEBUG },

        { NULL, '\0', NULL, NULL, 0,
          NULL, OPAL_CMD_LINE_TYPE_NULL, NULL, OPAL_CMD_LINE_OTYPE_NULL }
    };
    rc = opal_cmd_line_add(&cmd, extra);
    test_verify("opal_cmd_line_add returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* NULL table is legal (no-op) */
    rc = opal_cmd_line_add(&cmd, NULL);
    test_verify("opal_cmd_line_add(NULL table) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* Before parsing: nothing is taken */
    test_verify("is_taken('help') before parse == false",
                !opal_cmd_line_is_taken(&cmd, "help"));
    test_verify("get_ninsts('numprocs') before parse == 0",
                0 == opal_cmd_line_get_ninsts(&cmd, "numprocs"));

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_make_opt_mca -- opal_cmd_line_make_opt_mca
 * ----------------------------------------------------------------------- */
static void test_make_opt_mca(void)
{
    opal_cmd_line_t cmd;
    int rc;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);

    opal_cmd_line_init_t entry = {
        NULL,           /* ocl_mca_param_name */
        'q',            /* ocl_cmd_short_name */
        NULL,           /* ocl_cmd_single_dash_name */
        "quiet",        /* ocl_cmd_long_name */
        0,              /* ocl_num_params */
        NULL,           /* ocl_variable_dest */
        OPAL_CMD_LINE_TYPE_NULL,
        "Quiet mode",   /* ocl_description */
        OPAL_CMD_LINE_OTYPE_GENERAL
    };
    rc = opal_cmd_line_make_opt_mca(&cmd, entry);
    test_verify("make_opt_mca valid entry returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* All-empty sentinel is a no-op, not an error */
    opal_cmd_line_init_t sentinel = {
        NULL, '\0', NULL, NULL, 0,
        NULL, OPAL_CMD_LINE_TYPE_NULL, NULL, OPAL_CMD_LINE_OTYPE_NULL
    };
    rc = opal_cmd_line_make_opt_mca(&cmd, sentinel);
    test_verify("make_opt_mca sentinel returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_parse_flags -- zero-param options (flags)
 * ----------------------------------------------------------------------- */
static void test_parse_flags(void)
{
    opal_cmd_line_t cmd;
    int rc;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");
    opal_cmd_line_make_opt3(&cmd, 'd', NULL, "debug",   0, "Debug");
    opal_cmd_line_make_opt3(&cmd, 'q', NULL, "quiet",   0, "Quiet");

    /* Parse: program verbose debug; quiet absent */
    char *args[] = { "prog", "--verbose", "--debug", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 3, args);
    test_verify("parse flags returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    test_verify("is_taken('verbose') == true",
                opal_cmd_line_is_taken(&cmd, "verbose"));
    test_verify("is_taken('v') == true (short alias)",
                opal_cmd_line_is_taken(&cmd, "v"));
    test_verify("is_taken('debug') == true",
                opal_cmd_line_is_taken(&cmd, "debug"));
    test_verify("is_taken('quiet') == false (not passed)",
                !opal_cmd_line_is_taken(&cmd, "quiet"));

    test_verify("get_ninsts('verbose') == 1", 1 == opal_cmd_line_get_ninsts(&cmd, "verbose"));
    test_verify("get_ninsts('debug') == 1",   1 == opal_cmd_line_get_ninsts(&cmd, "debug"));
    test_verify("get_ninsts('quiet') == 0",   0 == opal_cmd_line_get_ninsts(&cmd, "quiet"));

    /* Zero-param option: get_param returns NULL (no parameter tokens recorded) */
    test_verify("get_param(verbose,0,0) == NULL for zero-param option",
                NULL == opal_cmd_line_get_param(&cmd, "verbose", 0, 0));

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_parse_option_with_params -- options that take string parameters
 * ----------------------------------------------------------------------- */
static void test_parse_option_with_params(void)
{
    opal_cmd_line_t cmd;
    int rc;
    char *p;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'o', NULL, "output",  1, "Output file");
    opal_cmd_line_make_opt3(&cmd, 'n', NULL, "numprocs",1, "Num procs");

    char *args[] = { "prog", "--output", "results.txt", "-n", "8", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 5, args);
    test_verify("parse with params returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    test_verify("is_taken('output') == true",
                opal_cmd_line_is_taken(&cmd, "output"));
    test_verify("is_taken('o') == true (short)",
                opal_cmd_line_is_taken(&cmd, "o"));
    test_verify("is_taken('numprocs') == true",
                opal_cmd_line_is_taken(&cmd, "numprocs"));

    p = opal_cmd_line_get_param(&cmd, "output", 0, 0);
    test_verify("get_param(output,0,0) non-NULL", NULL != p);
    test_verify("get_param(output,0,0) == 'results.txt'",
                NULL != p && 0 == strcmp("results.txt", p));

    p = opal_cmd_line_get_param(&cmd, "numprocs", 0, 0);
    test_verify("get_param(numprocs,0,0) non-NULL", NULL != p);
    test_verify("get_param(numprocs,0,0) == '8'",
                NULL != p && 0 == strcmp("8", p));

    /* Out-of-range instance and param index both return NULL */
    test_verify("get_param(output,1,0) == NULL (instance out of range)",
                NULL == opal_cmd_line_get_param(&cmd, "output", 1, 0));
    test_verify("get_param(output,0,1) == NULL (param index out of range)",
                NULL == opal_cmd_line_get_param(&cmd, "output", 0, 1));

    /* Unknown option name returns NULL */
    test_verify("get_param on unknown opt == NULL",
                NULL == opal_cmd_line_get_param(&cmd, "nonexistent", 0, 0));

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_get_param_documented_example -- exact example from cmd_line.h
 *
 * From header: executable --foo bar1 bar2 --foo bar3 bar4
 *   get_param(cmd, "foo", 1, 1) == "bar4"
 *   get_param(cmd, "bar", 0, 0) == NULL   (unregistered)
 *   get_param(cmd, "foo", 2, 2) == NULL   (inst 2 doesn't exist)
 * ----------------------------------------------------------------------- */
static void test_get_param_documented_example(void)
{
    opal_cmd_line_t cmd;
    int rc;
    char *p;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    /* "foo" takes 2 parameters */
    opal_cmd_line_make_opt3(&cmd, '\0', NULL, "foo", 2, "Foo option");

    char *args[] = {
        "executable",
        "--foo", "bar1", "bar2",
        "--foo", "bar3", "bar4",
        NULL
    };
    rc = opal_cmd_line_parse(&cmd, false, false, 7, args);
    test_verify("documented example parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    test_verify("get_ninsts('foo') == 2",
                2 == opal_cmd_line_get_ninsts(&cmd, "foo"));

    /* Instance 0, param 0 -> "bar1" */
    p = opal_cmd_line_get_param(&cmd, "foo", 0, 0);
    test_verify("get_param(foo,0,0) == 'bar1'",
                NULL != p && 0 == strcmp("bar1", p));

    /* Instance 0, param 1 -> "bar2" */
    p = opal_cmd_line_get_param(&cmd, "foo", 0, 1);
    test_verify("get_param(foo,0,1) == 'bar2'",
                NULL != p && 0 == strcmp("bar2", p));

    /* Instance 1, param 0 -> "bar3" */
    p = opal_cmd_line_get_param(&cmd, "foo", 1, 0);
    test_verify("get_param(foo,1,0) == 'bar3'",
                NULL != p && 0 == strcmp("bar3", p));

    /* Header documented example: instance 1, param 1 -> "bar4" */
    p = opal_cmd_line_get_param(&cmd, "foo", 1, 1);
    test_verify("get_param(foo,1,1) == 'bar4' [documented example]",
                NULL != p && 0 == strcmp("bar4", p));

    /* Header documented example: unregistered option -> NULL */
    p = opal_cmd_line_get_param(&cmd, "bar", 0, 0);
    test_verify("get_param(bar,0,0) == NULL [documented: unregistered]",
                NULL == p);

    /* Header documented example: instance 2 doesn't exist -> NULL */
    p = opal_cmd_line_get_param(&cmd, "foo", 2, 2);
    test_verify("get_param(foo,2,2) == NULL [documented: no third instance]",
                NULL == p);

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_parse_multiple_instances -- same option repeated on command line
 * ----------------------------------------------------------------------- */
static void test_parse_multiple_instances(void)
{
    opal_cmd_line_t cmd;
    int rc;
    char *p;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'I', NULL, "include", 1, "Include path");

    char *args[] = {
        "prog",
        "--include", "/usr/include",
        "--include", "/opt/include",
        "--include", "/home/user/include",
        NULL
    };
    rc = opal_cmd_line_parse(&cmd, false, false, 7, args);
    test_verify("multiple instances parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    test_verify("is_taken('include') == true",
                opal_cmd_line_is_taken(&cmd, "include"));
    test_verify("get_ninsts('include') == 3",
                3 == opal_cmd_line_get_ninsts(&cmd, "include"));

    p = opal_cmd_line_get_param(&cmd, "include", 0, 0);
    test_verify("include instance 0 == '/usr/include'",
                NULL != p && 0 == strcmp("/usr/include", p));

    p = opal_cmd_line_get_param(&cmd, "include", 1, 0);
    test_verify("include instance 1 == '/opt/include'",
                NULL != p && 0 == strcmp("/opt/include", p));

    p = opal_cmd_line_get_param(&cmd, "include", 2, 0);
    test_verify("include instance 2 == '/home/user/include'",
                NULL != p && 0 == strcmp("/home/user/include", p));

    /* Instance 3 (zero-indexed 4th) does not exist */
    p = opal_cmd_line_get_param(&cmd, "include", 3, 0);
    test_verify("include instance 3 == NULL (doesn't exist)",
                NULL == p);

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_parse_single_dash_name -- single-dash multi-char name (e.g., -np)
 * ----------------------------------------------------------------------- */
static void test_parse_single_dash_name(void)
{
    opal_cmd_line_t cmd;
    int rc;
    char *p;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);

    /* Register a single-dash name "np" (no short, no long) */
    opal_cmd_line_make_opt3(&cmd, '\0', "np", NULL, 1, "Num procs (compat)");
    /* Also register a normal long option to keep things interesting */
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");

    char *args[] = { "mpirun", "-np", "4", "--verbose", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 4, args);
    test_verify("single-dash name parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    test_verify("is_taken('np') == true",
                opal_cmd_line_is_taken(&cmd, "np"));
    test_verify("is_taken('verbose') == true",
                opal_cmd_line_is_taken(&cmd, "verbose"));

    p = opal_cmd_line_get_param(&cmd, "np", 0, 0);
    test_verify("get_param('np',0,0) == '4'",
                NULL != p && 0 == strcmp("4", p));

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_combined_shorts -- "-abc" expands to "-a -b -c" via split_shorts
 * ----------------------------------------------------------------------- */
static void test_combined_shorts(void)
{
    opal_cmd_line_t cmd;
    int rc;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'a', NULL, "all",     0, "Option A");
    opal_cmd_line_make_opt3(&cmd, 'b', NULL, "batch",   0, "Option B");
    opal_cmd_line_make_opt3(&cmd, 'c', NULL, "check",   0, "Option C");
    opal_cmd_line_make_opt3(&cmd, 'd', NULL, "debug",   0, "Debug");

    /* "-abc" should expand to "-a", "-b", "-c" */
    char *args[] = { "prog", "-abc", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 2, args);
    test_verify("combined shorts parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    test_verify("is_taken('a') == true after -abc",
                opal_cmd_line_is_taken(&cmd, "a"));
    test_verify("is_taken('b') == true after -abc",
                opal_cmd_line_is_taken(&cmd, "b"));
    test_verify("is_taken('c') == true after -abc",
                opal_cmd_line_is_taken(&cmd, "c"));
    test_verify("is_taken('d') == false (not in -abc)",
                !opal_cmd_line_is_taken(&cmd, "d"));

    /* Each flag appears once */
    test_verify("get_ninsts('a') == 1", 1 == opal_cmd_line_get_ninsts(&cmd, "a"));
    test_verify("get_ninsts('b') == 1", 1 == opal_cmd_line_get_ninsts(&cmd, "b"));
    test_verify("get_ninsts('c') == 1", 1 == opal_cmd_line_get_ninsts(&cmd, "c"));

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_parse_double_dash_separator -- "--" stops parsing, rest goes to tail
 * ----------------------------------------------------------------------- */
static void test_parse_double_dash_separator(void)
{
    opal_cmd_line_t cmd;
    int rc;
    int tailc;
    char **tailv;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");
    opal_cmd_line_make_opt3(&cmd, 'n', NULL, "numprocs",1, "Num procs");

    /* "--" separates known options from the "tail" arguments */
    char *args[] = {
        "prog", "--verbose", "--", "tail1", "tail2", "tail3", NULL
    };
    rc = opal_cmd_line_parse(&cmd, false, false, 6, args);
    test_verify("double-dash parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    test_verify("is_taken('verbose') == true (before --)",
                opal_cmd_line_is_taken(&cmd, "verbose"));
    test_verify("is_taken('numprocs') == false (not in argv)",
                !opal_cmd_line_is_taken(&cmd, "numprocs"));

    tailc = 0;
    tailv = NULL;
    rc = opal_cmd_line_get_tail(&cmd, &tailc, &tailv);
    test_verify("get_tail returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* tailc = number of real tail tokens (NOT including the NULL sentinel,
     * despite what the header says about "length including the final NULL
     * entry" -- the code uses opal_argv_append which counts real tokens
     * only, so tailc == 3 here.
     * SUSPECTED BUG: header documents tailc as "length including the final
     * NULL entry", but the implementation sets tailc = opal_argv_append
     * count, which excludes the NULL terminator.  We assert the code's
     * actual behavior (tailc == 3) and verify tailv[tailc] == NULL. */
    test_verify("tailc == 3 (code counts real tokens, not the NULL sentinel)",
                3 == tailc);
    test_verify("tailv is non-NULL", NULL != tailv);
    if (NULL != tailv) {
        test_verify("tailv[0] == 'tail1'",
                    0 == strcmp("tail1", tailv[0]));
        test_verify("tailv[1] == 'tail2'",
                    0 == strcmp("tail2", tailv[1]));
        test_verify("tailv[2] == 'tail3'",
                    0 == strcmp("tail3", tailv[2]));
        test_verify("tailv[tailc] == NULL (null-terminated)",
                    NULL == tailv[tailc]);
        opal_argv_free(tailv);
    }

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_parse_unknown_option_error -- unrecognized "-foo" triggers error
 * ----------------------------------------------------------------------- */
static void test_parse_unknown_option_error(void)
{
    opal_cmd_line_t cmd;
    int rc;
    int tailc;
    char **tailv;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");

    /* "--unknown" is not registered; parse should fail with OPAL_ERR_SILENT */
    char *args[] = { "prog", "--verbose", "--unknown", "after", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 4, args);
    test_verify("unknown option returns OPAL_ERR_SILENT",
                OPAL_ERR_SILENT == rc);

    /* verbose was seen before the unknown; it should still be recorded */
    test_verify("is_taken('verbose') == true (seen before unknown)",
                opal_cmd_line_is_taken(&cmd, "verbose"));

    /* Everything from the unknown token onward goes to the tail */
    tailc = 0;
    tailv = NULL;
    opal_cmd_line_get_tail(&cmd, &tailc, &tailv);
    test_verify("tail has items after unknown option", tailc >= 1);
    if (NULL != tailv) {
        test_verify("first tail token is the unknown option",
                    0 == strcmp("--unknown", tailv[0]));
        opal_argv_free(tailv);
    }

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_parse_missing_param_error -- option that needs N params but gets <N
 * ----------------------------------------------------------------------- */
static void test_parse_missing_param_error(void)
{
    opal_cmd_line_t cmd;
    int rc;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, '\0', NULL, "pair", 2, "Needs two args");
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");

    /* "--pair" needs 2 args but only 1 follows */
    char *args[] = { "prog", "--pair", "only_one", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 3, args);
    test_verify("missing param always returns OPAL_ERR_SILENT",
                OPAL_ERR_SILENT == rc);

    /* The incomplete option should NOT be recorded as taken */
    test_verify("is_taken('pair') == false (incomplete, error path)",
                !opal_cmd_line_is_taken(&cmd, "pair"));
    test_verify("get_ninsts('pair') == 0", 0 == opal_cmd_line_get_ninsts(&cmd, "pair"));

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_parse_ignore_unknown -- ignore_unknown=true, unknown becomes tail
 * ----------------------------------------------------------------------- */
static void test_parse_ignore_unknown(void)
{
    opal_cmd_line_t cmd;
    int rc;
    int tailc;
    char **tailv;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");

    /* Unknown plain token (no leading dash) stops parsing when
     * ignore_unknown=false.  With ignore_unknown=true, it becomes tail. */
    char *args[] = { "prog", "--verbose", "plain_arg", NULL };
    rc = opal_cmd_line_parse(&cmd, true, true, 3, args);
    test_verify("ignore_unknown=true returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    test_verify("is_taken('verbose') == true", opal_cmd_line_is_taken(&cmd, "verbose"));

    tailc = 0;
    tailv = NULL;
    opal_cmd_line_get_tail(&cmd, &tailc, &tailv);
    /* "plain_arg" has no leading dash, so it's an unknown token; it goes to tail */
    test_verify("plain token is captured in tail", tailc >= 1);
    if (NULL != tailv) {
        test_verify("tailv[0] == 'plain_arg'",
                    0 == strcmp("plain_arg", tailv[0]));
        opal_argv_free(tailv);
    }

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_get_argc_and_argv -- opal_cmd_line_get_argc / get_argv
 * ----------------------------------------------------------------------- */
static void test_get_argc_and_argv(void)
{
    opal_cmd_line_t cmd;
    int rc;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");

    /* Before any parse, argc should be 0 (no argv stored yet) */
    test_verify("get_argc before parse == 0", 0 == opal_cmd_line_get_argc(&cmd));
    test_verify("get_argv before parse == NULL", NULL == opal_cmd_line_get_argv(&cmd, 0));

    char *args[] = { "prog", "--verbose", "extra", NULL };
    rc = opal_cmd_line_parse(&cmd, true, true, 3, args);
    test_verify("argc/argv parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* After parsing, argc == original argc passed to parse */
    test_verify("get_argc after parse == 3", 3 == opal_cmd_line_get_argc(&cmd));

    /* get_argv returns the stored copy of the original tokens */
    test_verify("get_argv(0) == 'prog'",
                0 == strcmp("prog", opal_cmd_line_get_argv(&cmd, 0)));
    test_verify("get_argv(1) == '--verbose'",
                0 == strcmp("--verbose", opal_cmd_line_get_argv(&cmd, 1)));
    test_verify("get_argv(2) == 'extra'",
                0 == strcmp("extra", opal_cmd_line_get_argv(&cmd, 2)));

    /* Out-of-range index */
    test_verify("get_argv(-1) == NULL", NULL == opal_cmd_line_get_argv(&cmd, -1));
    test_verify("get_argv(3) == NULL (one past end)", NULL == opal_cmd_line_get_argv(&cmd, 3));
    test_verify("get_argv(999) == NULL", NULL == opal_cmd_line_get_argv(&cmd, 999));

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_get_tail -- detailed tail coverage
 * ----------------------------------------------------------------------- */
static void test_get_tail(void)
{
    opal_cmd_line_t cmd;
    int rc;
    int tailc;
    char **tailv;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");

    /* No tail expected on a clean parse */
    char *args1[] = { "prog", "--verbose", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 2, args1);
    test_verify("clean parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    tailc = 99;
    tailv = NULL;
    rc = opal_cmd_line_get_tail(&cmd, &tailc, &tailv);
    test_verify("get_tail returns OPAL_SUCCESS on clean parse", OPAL_SUCCESS == rc);
    test_verify("tailc == 0 on clean parse", 0 == tailc);
    /* opal_argv_copy(NULL) may return NULL; either way free is safe */
    if (NULL != tailv) {
        opal_argv_free(tailv);
    }

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_get_ninsts_unregistered -- get_ninsts on an option not in the handle
 *
 * Header prose says: "OPAL_ERR if the command line option was not found
 * during opal_cmd_line_parse(), or opal_cmd_line_parse() was not invoked
 * on this handle."
 *
 * Code reality: get_ninsts initializes ret=0 and returns 0 when
 * find_option returns NULL for an unregistered name.  Returning 0 is
 * internally consistent with the "count including 0" semantics documented
 * elsewhere in the same function, and is_taken (which calls get_ninsts)
 * produces the correct false result.  We therefore treat the header prose
 * as a documentation inaccuracy rather than a code bug, and assert 0.
 *
 * DOCUMENTATION DEFECT: header says OPAL_ERR(-1) for an unregistered
 * option; the implementation returns 0.  Asserting 0 (the sensible value
 * matching "zero instances found") is correct; the prose should be
 * updated to match.
 * ----------------------------------------------------------------------- */
static void test_get_ninsts_unregistered(void)
{
    opal_cmd_line_t cmd;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");

    char *args[] = { "prog", "--verbose", NULL };
    opal_cmd_line_parse(&cmd, false, false, 2, args);

    /* Returns 0 (not OPAL_ERROR/-1 as the header prose implies).
     * DOCUMENTATION DEFECT: see block comment above. */
    test_verify("get_ninsts on unregistered option returns 0 "
                "[header prose says OPAL_ERR -- documentation defect, "
                "0 is the sensible value]",
                0 == opal_cmd_line_get_ninsts(&cmd, "notregistered"));

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_get_usage_msg -- opal_cmd_line_get_usage_msg returns non-NULL string
 * ----------------------------------------------------------------------- */
static void test_get_usage_msg(void)
{
    opal_cmd_line_t cmd;
    char *msg;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose",  0, "Enable verbose output");
    opal_cmd_line_make_opt3(&cmd, 'n', NULL, "numprocs", 1, "Number of processes");
    opal_cmd_line_make_opt3(&cmd, '\0', NULL, "hidden", 0, NULL); /* no description */

    msg = opal_cmd_line_get_usage_msg(&cmd);
    test_verify("get_usage_msg returns non-NULL", NULL != msg);
    if (NULL != msg) {
        /* The message should mention at least one of the documented options */
        test_verify("usage msg contains 'verbose'",
                    NULL != strstr(msg, "verbose"));
        test_verify("usage msg contains 'numprocs'",
                    NULL != strstr(msg, "numprocs"));
        free(msg);
    }

    /* Empty handle (no options) should return a non-NULL (possibly empty) string */
    opal_cmd_line_t empty;
    OBJ_CONSTRUCT(&empty, opal_cmd_line_t);
    msg = opal_cmd_line_get_usage_msg(&empty);
    test_verify("get_usage_msg on empty handle returns non-NULL", NULL != msg);
    if (NULL != msg) {
        free(msg);
    }
    OBJ_DESTRUCT(&empty);

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_set_dest_variable -- ocl_variable_dest auto-fill via create table
 * ----------------------------------------------------------------------- */
static void test_set_dest_variable(void)
{
    opal_cmd_line_t cmd;
    int rc;

    /* Destinations that get filled by the parser */
    bool    flag_val  = false;
    char   *str_val   = NULL;
    int     int_val   = 0;

    opal_cmd_line_init_t table[] = {
        /* Boolean flag */
        { NULL, 'f', NULL, "flag", 0,
          &flag_val, OPAL_CMD_LINE_TYPE_BOOL,
          "A boolean flag", OPAL_CMD_LINE_OTYPE_GENERAL },

        /* String option */
        { NULL, 's', NULL, "str", 1,
          &str_val, OPAL_CMD_LINE_TYPE_STRING,
          "A string option", OPAL_CMD_LINE_OTYPE_GENERAL },

        /* Int option */
        { NULL, 'i', NULL, "ival", 1,
          &int_val, OPAL_CMD_LINE_TYPE_INT,
          "An int option", OPAL_CMD_LINE_OTYPE_GENERAL },

        /* Sentinel */
        { NULL, '\0', NULL, NULL, 0,
          NULL, OPAL_CMD_LINE_TYPE_NULL, NULL, OPAL_CMD_LINE_OTYPE_NULL }
    };

    rc = opal_cmd_line_create(&cmd, table);
    test_verify("create with dest table succeeds", OPAL_SUCCESS == rc);

    char *args[] = { "prog", "--flag", "--str", "hello", "--ival", "42", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 6, args);
    test_verify("parse with dest variables returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* Bool flag dest set to true (1) */
    test_verify("bool dest set to true after --flag", flag_val);

    /* String dest set to a copy of the argument */
    test_verify("str dest non-NULL after --str hello", NULL != str_val);
    if (NULL != str_val) {
        test_verify("str dest == 'hello'", 0 == strcmp("hello", str_val));
        /* set_dest strdup's the value; we free it to avoid a leak */
        free(str_val);
        str_val = NULL;
    }

    /* Int dest set to 42 */
    test_verify("int dest == 42 after --ival 42", 42 == int_val);

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_set_dest_int_invalid -- set_dest with non-numeric value for INT type
 *
 * When an option is registered with OPAL_CMD_LINE_TYPE_INT and the value
 * supplied on the command line is not a valid integer (e.g. "abc"), set_dest
 * prints an error message to stderr and returns OPAL_ERR_SILENT, which
 * propagates out of opal_cmd_line_parse.
 *
 * SUSPECTED BUG: In the OPAL_CMD_LINE_TYPE_INT validation error path
 * (cmd_line.c set_dest), the function prints to stderr and returns
 * OPAL_ERR_SILENT but does NOT call OBJ_RELEASE(param) before returning —
 * the param_t allocated during parsing is leaked.  This is an early-return
 * path that bypasses the normal cleanup.  We cannot assert the leak here
 * (no portable way to observe it), but we note it for future investigation.
 * ----------------------------------------------------------------------- */
static void test_set_dest_int_invalid(void)
{
    opal_cmd_line_t cmd;
    int rc;
    int int_val = 0;

    opal_cmd_line_init_t table[] = {
        { NULL, 'i', NULL, "ival", 1,
          &int_val, OPAL_CMD_LINE_TYPE_INT,
          "An int option", OPAL_CMD_LINE_OTYPE_GENERAL },
        { NULL, '\0', NULL, NULL, 0,
          NULL, OPAL_CMD_LINE_TYPE_NULL, NULL, OPAL_CMD_LINE_OTYPE_NULL }
    };

    rc = opal_cmd_line_create(&cmd, table);
    test_verify("create for int-invalid test succeeds", OPAL_SUCCESS == rc);

    /* "abc" is not a valid integer; set_dest returns OPAL_ERR_SILENT */
    char *args[] = { "prog", "--ival", "abc", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 3, args);
    test_verify("parse with non-numeric INT value returns OPAL_ERR_SILENT",
                OPAL_ERR_SILENT == rc);

    /* int_val must remain at its pre-parse sentinel (0) since set_dest errored */
    test_verify("int dest unchanged after invalid parse", 0 == int_val);

    OBJ_DESTRUCT(&cmd);
}

/* -----------------------------------------------------------------------
 * test_reparse -- calling opal_cmd_line_parse twice on the same handle
 *                 must erase previous results
 * ----------------------------------------------------------------------- */
static void test_reparse(void)
{
    opal_cmd_line_t cmd;
    int rc;

    OBJ_CONSTRUCT(&cmd, opal_cmd_line_t);
    opal_cmd_line_make_opt3(&cmd, 'v', NULL, "verbose", 0, "Verbose");
    opal_cmd_line_make_opt3(&cmd, 'd', NULL, "debug",   0, "Debug");

    /* First parse: only verbose */
    char *args1[] = { "prog", "--verbose", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 2, args1);
    test_verify("first parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("verbose taken after first parse",
                opal_cmd_line_is_taken(&cmd, "verbose"));
    test_verify("debug not taken after first parse",
                !opal_cmd_line_is_taken(&cmd, "debug"));

    /* Second parse: only debug — previous results must be cleared */
    char *args2[] = { "prog", "--debug", NULL };
    rc = opal_cmd_line_parse(&cmd, false, false, 2, args2);
    test_verify("second parse returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("debug taken after second parse",
                opal_cmd_line_is_taken(&cmd, "debug"));
    test_verify("verbose NOT taken after second parse (results erased)",
                !opal_cmd_line_is_taken(&cmd, "verbose"));
    test_verify("get_ninsts('verbose') == 0 after second parse",
                0 == opal_cmd_line_get_ninsts(&cmd, "verbose"));

    OBJ_DESTRUCT(&cmd);
}
