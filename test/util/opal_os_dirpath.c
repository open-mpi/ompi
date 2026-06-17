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
 * Unit tests for opal/util/os_dirpath.c
 *
 * Covers: opal_os_dirpath_create, opal_os_dirpath_is_empty,
 *         opal_os_dirpath_access, opal_os_dirpath_destroy.
 *
 * All temp directories are created under a mkdtemp base and cleaned
 * up before the process exits.  We use S_IRWXU throughout to avoid
 * umask stripping group/other bits and making mode assertions flaky.
 *
 * Library is compiled with -DNDEBUG, so assert() is a no-op.
 * All verification must go through test_verify().
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/os_dirpath.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

/* ------------------------------------------------------------------ */
/* Helpers                                                             */
/* ------------------------------------------------------------------ */

/*
 * Create a regular file at the given path.  Returns 0 on success.
 */
static int create_file(const char *path)
{
    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
    if (-1 == fd) {
        return -1;
    }
    close(fd);
    return 0;
}

/*
 * Build a path string from two components without using opal_os_path().
 * Result is malloc'd; caller must free().
 */
static char *path_join(const char *base, const char *sub)
{
    size_t len = strlen(base) + 1 + strlen(sub) + 1;
    char *p = malloc(len);
    if (NULL != p) {
        snprintf(p, len, "%s/%s", base, sub);
    }
    return p;
}

/*
 * Build a path string from three components.  Result is malloc'd.
 */
static char *path_join3(const char *a, const char *b, const char *c)
{
    size_t len = strlen(a) + 1 + strlen(b) + 1 + strlen(c) + 1;
    char *p = malloc(len);
    if (NULL != p) {
        snprintf(p, len, "%s/%s/%s", a, b, c);
    }
    return p;
}

/* ------------------------------------------------------------------ */
/* Forward declarations                                                */
/* ------------------------------------------------------------------ */

static void test_create_null_path(void);
static void test_create_existing_dir(void);
static void test_create_nested_dirs(void);
static void test_is_empty(void);
static void test_access_nonexistent(void);
static void test_access_exists_match(void);
static void test_access_exists_no_match(void);
static void test_destroy_recursive_no_callback(void);
static void test_destroy_recursive_with_callback(void);
static void test_destroy_nonexistent(void);

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    test_init("opal_os_dirpath");

    opal_init_util(&argc, &argv);

    test_create_null_path();
    test_create_existing_dir();
    test_create_nested_dirs();
    test_is_empty();
    test_access_nonexistent();
    test_access_exists_match();
    test_access_exists_no_match();
    test_destroy_recursive_no_callback();
    test_destroy_recursive_with_callback();
    test_destroy_nonexistent();

    int r = test_finalize();
    opal_finalize_util();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_create_null_path(void)
{
    int rc = opal_os_dirpath_create(NULL, S_IRWXU);
    test_verify("create(NULL) returns error", OPAL_SUCCESS != rc);
}

/* ------------------------------------------------------------------ */

static void test_create_existing_dir(void)
{
    /* Create a temp dir with mkdtemp, then call opal_os_dirpath_create
     * on it.  It already exists with the right permissions, so the
     * function should succeed immediately. */
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_create_existing_dir: mkdtemp failed");
        return;
    }

    int rc = opal_os_dirpath_create(base, S_IRWXU);
    test_verify("create on existing dir returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    rmdir(base);
}

/* ------------------------------------------------------------------ */

/*
 * Force the "build our way down the tree" branch by requesting a
 * two-level path that does not yet exist under the temp root.
 */
static void test_create_nested_dirs(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_create_nested_dirs: mkdtemp failed");
        return;
    }

    char *leaf = path_join3(base, "level1", "level2");
    if (NULL == leaf) {
        test_failure("test_create_nested_dirs: OOM building path");
        rmdir(base);
        return;
    }

    int rc = opal_os_dirpath_create(leaf, S_IRWXU);
    test_verify("create nested dirs returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* The leaf must exist and be a directory */
    struct stat buf;
    int sr = stat(leaf, &buf);
    test_verify("nested leaf directory exists after create", 0 == sr);
    test_verify("nested leaf is a directory", 0 == sr && S_ISDIR(buf.st_mode));

    /* The required mode bits must be set */
    test_verify("nested leaf has S_IRWXU permission bits",
                0 == sr && (S_IRWXU == (buf.st_mode & S_IRWXU)));

    /* Clean up */
    rmdir(leaf);
    char *mid = path_join(base, "level1");
    if (NULL != mid) {
        rmdir(mid);
        free(mid);
    }
    rmdir(base);
    free(leaf);
}

/* ------------------------------------------------------------------ */

static void test_is_empty(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_is_empty: mkdtemp failed");
        return;
    }

    /* Fresh directory must be empty */
    test_verify("fresh dir is empty", true == opal_os_dirpath_is_empty(base));

    /* Create a file inside */
    char *fpath = path_join(base, "afile");
    if (NULL == fpath) {
        test_failure("test_is_empty: OOM");
        rmdir(base);
        return;
    }

    int cr = create_file(fpath);
    if (0 != cr) {
        test_failure("test_is_empty: could not create file");
        free(fpath);
        rmdir(base);
        return;
    }

    test_verify("dir with file is not empty",
                false == opal_os_dirpath_is_empty(base));

    unlink(fpath);
    free(fpath);

    test_verify("dir is empty again after file removed",
                true == opal_os_dirpath_is_empty(base));

    rmdir(base);
}

/* ------------------------------------------------------------------ */

static void test_access_nonexistent(void)
{
    /* A path that certainly does not exist */
    int rc = opal_os_dirpath_access("/tmp/opal_nonexistent_dir_xyz987", S_IRWXU);
    test_verify("access on non-existent path returns OPAL_ERR_NOT_FOUND",
                OPAL_ERR_NOT_FOUND == rc);
}

/* ------------------------------------------------------------------ */

static void test_access_exists_match(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_access_exists_match: mkdtemp failed");
        return;
    }
    /* mkdtemp creates with 0700 = S_IRWXU; requesting S_IRWXU must succeed */
    int rc = opal_os_dirpath_access(base, S_IRWXU);
    test_verify("access on dir with matching mode returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);

    rmdir(base);
}

/* ------------------------------------------------------------------ */

static void test_access_exists_no_match(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_access_exists_no_match: mkdtemp failed");
        return;
    }
    /* Strip all permissions so S_IRWXU won't match */
    chmod(base, 0);

    int rc = opal_os_dirpath_access(base, S_IRWXU);
    test_verify("access on dir with wrong mode returns OPAL_ERROR",
                OPAL_ERROR == rc);

    /* Restore so rmdir can proceed */
    chmod(base, S_IRWXU);
    rmdir(base);
}

/* ------------------------------------------------------------------ */

static void test_destroy_recursive_no_callback(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_recursive_no_callback: mkdtemp failed");
        return;
    }

    /* Populate: base/sub/ and base/sub/file */
    char *sub = path_join(base, "sub");
    if (NULL == sub) {
        test_failure("test_destroy_recursive_no_callback: OOM");
        rmdir(base);
        return;
    }
    mkdir(sub, S_IRWXU);

    char *fpath = path_join(sub, "file");
    if (NULL == fpath) {
        test_failure("test_destroy_recursive_no_callback: OOM fpath");
        rmdir(sub);
        rmdir(base);
        free(sub);
        return;
    }
    create_file(fpath);

    int rc = opal_os_dirpath_destroy(base, true /*recursive*/, NULL /*no cb*/);
    test_verify("recursive destroy (no cb) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* base itself must be gone (destroy rmdir's it when empty) */
    struct stat buf;
    test_verify("top dir removed after recursive destroy", 0 != stat(base, &buf));

    free(fpath);
    free(sub);
}

/* ------------------------------------------------------------------ */

/*
 * Callback that vetoes removal of files named "protected".
 */
static bool cb_protect(const char *root, const char *name)
{
    (void) root;
    if (0 == strcmp(name, "protected")) {
        return false; /* do NOT remove */
    }
    return true; /* allow removal */
}

static void test_destroy_recursive_with_callback(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_recursive_with_callback: mkdtemp failed");
        return;
    }

    /* Create base/protected and base/removable */
    char *prot = path_join(base, "protected");
    char *remo = path_join(base, "removable");
    if (NULL == prot || NULL == remo) {
        test_failure("test_destroy_recursive_with_callback: OOM");
        if (NULL != prot) { free(prot); }
        if (NULL != remo) { free(remo); }
        rmdir(base);
        return;
    }
    create_file(prot);
    create_file(remo);

    /*
     * destroy with the protecting callback.  The callback returns
     * false for "protected", so that file stays.  "removable" gets
     * unlinked.  Because the directory is not empty (protected is
     * still there), the top dir itself is NOT removed by the
     * cleanup block.
     */
    int rc = opal_os_dirpath_destroy(base, true, cb_protect);
    test_verify("destroy with protect callback returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* "protected" must still exist */
    struct stat buf;
    test_verify("protected file survives callback veto", 0 == stat(prot, &buf));

    /* "removable" must be gone */
    test_verify("removable file was removed", 0 != stat(remo, &buf));

    /* base dir must still exist (not empty) */
    test_verify("base dir survives (not empty)", 0 == stat(base, &buf));

    /* Manual cleanup */
    unlink(prot);
    rmdir(base);

    free(prot);
    free(remo);
}

/* ------------------------------------------------------------------ */

static void test_destroy_nonexistent(void)
{
    int rc = opal_os_dirpath_destroy("/tmp/opal_nonexistent_dir_xyz987",
                                    true, NULL);
    test_verify("destroy on non-existent path returns OPAL_ERR_NOT_FOUND",
                OPAL_ERR_NOT_FOUND == rc);
}
