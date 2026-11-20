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
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
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
static void test_create_on_file(void);
static void test_create_on_symlink(void);
static void test_create_on_symlink_trailing_separator(void);
static void test_destroy_does_not_follow_symlink(void);
static void test_destroy_symlink_base(void);
static void test_destroy_nonrecursive_with_subdir(void);
static void test_destroy_callback_veto_in_subdir(void);
static void test_create_on_unreadable_dir(void);
static void test_destroy_unsearchable_dir(void);
static void test_destroy_subdir_rmdir_failure(void);
static void test_destroy_base_rmdir_failure(void);
static void test_destroy_trailing_separator(void);
static void test_destroy_unreadable_parent(void);

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
    test_create_on_file();
    test_create_on_symlink();
    test_create_on_symlink_trailing_separator();
    test_destroy_does_not_follow_symlink();
    test_destroy_symlink_base();
    test_destroy_nonrecursive_with_subdir();
    test_destroy_callback_veto_in_subdir();
    test_create_on_unreadable_dir();
    test_destroy_unsearchable_dir();
    test_destroy_subdir_rmdir_failure();
    test_destroy_base_rmdir_failure();
    test_destroy_trailing_separator();
    test_destroy_unreadable_parent();

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

/* ------------------------------------------------------------------ */

/*
 * A pre-existing regular file at the requested path must be an
 * error: it cannot be used as a directory (previously it was
 * silently chmod'ed and reported as success).
 */
static void test_create_on_file(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_create_on_file: mkdtemp failed");
        return;
    }
    char *file = path_join(base, "iamafile");

    if (0 != create_file(file)) {
        test_failure("test_create_on_file: create_file failed");
        goto out;
    }

    int rc = opal_os_dirpath_create(file, S_IRWXU);
    test_verify("create on existing regular file returns error", OPAL_SUCCESS != rc);

out:
    unlink(file);
    rmdir(base);
    free(file);
}

/* ------------------------------------------------------------------ */

/*
 * A symlink planted at the requested path must be refused, even when
 * it points at a directory the caller owns: following it would make
 * the ownership/mode checks inspect the link's target, and the
 * adopted path would later be recursively destroyed.
 */
static void test_create_on_symlink(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_create_on_symlink: mkdtemp failed");
        return;
    }
    char *target = path_join(base, "target");
    char *link = path_join(base, "link");

    if (0 != mkdir(target, S_IRWXU) || 0 != symlink(target, link)) {
        test_failure("test_create_on_symlink: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_create(link, S_IRWXU);
    test_verify("create on symlink-to-own-dir returns error", OPAL_SUCCESS != rc);

    struct stat buf;
    test_verify("symlink target still exists", 0 == stat(target, &buf));

out:
    unlink(link);
    rmdir(target);
    rmdir(base);
    free(target);
    free(link);
}

/*
 * A symlink planted at the requested path must still be refused when the
 * name is given with a trailing separator.  The separator makes the kernel
 * resolve the final component as a directory, following the link to do it,
 * so open(..., O_DIRECTORY | O_NOFOLLOW) opens the link's target and the
 * fchmod() that follows re-permissions whatever the link names.  The target
 * is a directory here (rather than a file as in test_create_on_symlink)
 * because O_DIRECTORY has to succeed for the resolution to land at all.
 */
static void test_create_on_symlink_trailing_separator(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_create_on_symlink_trailing_separator: mkdtemp failed");
        return;
    }
    char *target = path_join(base, "target");
    char *link = path_join(base, "link");
    char *link_sep = path_join(base, "link/");
    struct stat before, after;

    if (0 != mkdir(target, S_IRWXU) || 0 != symlink(target, link)
        || 0 != stat(target, &before)) {
        test_failure("test_create_on_symlink_trailing_separator: setup failed");
        goto out;
    }

    (void) opal_os_dirpath_create(link_sep, S_IRWXU | S_IRGRP | S_IXGRP);

    test_verify("symlink target mode unchanged through a trailing separator",
                0 == stat(target, &after)
                    && (before.st_mode & 07777) == (after.st_mode & 07777));

out:
    unlink(link);
    rmdir(target);
    rmdir(base);
    free(target);
    free(link);
    free(link_sep);
}

/* ------------------------------------------------------------------ */

/*
 * Recursive destroy must not follow a symlink inside the tree: the
 * link itself is removed, but the directory it points to (and that
 * directory's contents) must survive.
 */
static void test_destroy_does_not_follow_symlink(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_does_not_follow_symlink: mkdtemp failed");
        return;
    }
    char *victim = path_join(base, "victim");
    char *vfile = path_join(victim, "precious");
    char *doomed = path_join(base, "doomed");
    char *link = path_join(doomed, "escape");

    if (0 != mkdir(victim, S_IRWXU) || 0 != create_file(vfile)
        || 0 != mkdir(doomed, S_IRWXU) || 0 != symlink(victim, link)) {
        test_failure("test_destroy_does_not_follow_symlink: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(doomed, true, NULL);
    test_verify("destroy of dir containing symlink returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);

    struct stat buf;
    test_verify("symlink was removed", 0 != lstat(link, &buf));
    test_verify("destroyed dir is gone", 0 != stat(doomed, &buf));
    test_verify("symlink target dir survives", 0 == stat(victim, &buf));
    test_verify("file inside symlink target survives", 0 == stat(vfile, &buf));

out:
    unlink(vfile);
    rmdir(victim);
    unlink(link);
    rmdir(doomed);
    rmdir(base);
    free(victim);
    free(vfile);
    free(doomed);
    free(link);
}

/* ------------------------------------------------------------------ */

/*
 * Destroy must refuse a symlink as its base path: the link's target
 * (and the target's contents) must be untouched, and the link itself
 * must remain (destroy errored out, it did not "destroy the link").
 */
static void test_destroy_symlink_base(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_symlink_base: mkdtemp failed");
        return;
    }
    char *victim = path_join(base, "victim");
    char *vfile = path_join(victim, "precious");
    char *link = path_join(base, "link");

    if (0 != mkdir(victim, S_IRWXU) || 0 != create_file(vfile)
        || 0 != symlink(victim, link)) {
        test_failure("test_destroy_symlink_base: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(link, true, NULL);
    test_verify("destroy on symlink base returns error", OPAL_SUCCESS != rc);

    struct stat buf;
    test_verify("symlink target dir untouched", 0 == stat(victim, &buf));
    test_verify("file inside target untouched", 0 == stat(vfile, &buf));

out:
    unlink(link);
    unlink(vfile);
    rmdir(victim);
    rmdir(base);
    free(victim);
    free(vfile);
    free(link);
}

/* ------------------------------------------------------------------ */

/*
 * Non-recursive destroy of a directory that contains a subdirectory:
 * files are removed, the subdirectory survives, and OPAL_ERROR is
 * returned (we found a directory but were not told to remove it).
 * The top directory survives because it is not empty.
 */
static void test_destroy_nonrecursive_with_subdir(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_nonrecursive_with_subdir: mkdtemp failed");
        return;
    }
    char *sub = path_join(base, "sub");
    char *file = path_join(base, "afile");

    if (0 != mkdir(sub, S_IRWXU) || 0 != create_file(file)) {
        test_failure("test_destroy_nonrecursive_with_subdir: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(base, false /* not recursive */, NULL);
    test_verify("non-recursive destroy with subdir returns error",
                OPAL_SUCCESS != rc);

    struct stat buf;
    test_verify("file was still removed", 0 != stat(file, &buf));
    test_verify("subdir survives non-recursive destroy", 0 == stat(sub, &buf));
    test_verify("top dir survives (not empty)", 0 == stat(base, &buf));

out:
    unlink(file);
    rmdir(sub);
    rmdir(base);
    free(sub);
    free(file);
}

/* ------------------------------------------------------------------ */

/*
 * Callback veto on a file inside a subdirectory: the subdirectory
 * cannot be removed (it still holds the protected file), which must
 * be harmless -- overall destroy still succeeds, the protected file
 * and its parent dir survive, and unprotected siblings are removed.
 */
static void test_destroy_callback_veto_in_subdir(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_callback_veto_in_subdir: mkdtemp failed");
        return;
    }
    char *sub = path_join(base, "sub");
    char *prot = path_join3(base, "sub", "protected");
    char *remo = path_join3(base, "sub", "removable");

    if (0 != mkdir(sub, S_IRWXU) || 0 != create_file(prot)
        || 0 != create_file(remo)) {
        test_failure("test_destroy_callback_veto_in_subdir: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(base, true, cb_protect);
    test_verify("destroy with nested veto returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);

    struct stat buf;
    test_verify("nested protected file survives", 0 == stat(prot, &buf));
    test_verify("nested removable file was removed", 0 != stat(remo, &buf));
    test_verify("subdir holding protected file survives", 0 == stat(sub, &buf));

out:
    unlink(prot);
    unlink(remo);
    rmdir(sub);
    rmdir(base);
    free(sub);
    free(prot);
    free(remo);
}

/* ------------------------------------------------------------------ */

/*
 * The remaining tests rely on permission bits denying access, which
 * root bypasses.
 */
static bool running_as_root(const char *test_name)
{
    if (0 == geteuid()) {
        char msg[256];
        snprintf(msg, sizeof(msg), "%s: skipped (running as root)", test_name);
        test_comment(msg);
        return true;
    }
    return false;
}

/*
 * An existing directory we own but cannot read cannot be opened, so
 * its mode cannot be adjusted through a descriptor.  That must be
 * reported as a permission error -- not misclassified as a missing
 * directory and sent down the tree-building path.
 */
static void test_create_on_unreadable_dir(void)
{
    if (running_as_root("test_create_on_unreadable_dir")) {
        return;
    }
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_create_on_unreadable_dir: mkdtemp failed");
        return;
    }
    char *dir = path_join(base, "unreadable");

    if (0 != mkdir(dir, S_IRWXU) || 0 != chmod(dir, S_IWUSR | S_IXUSR)) {
        test_failure("test_create_on_unreadable_dir: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_create(dir, S_IRWXU);
    test_verify("create on unreadable existing dir returns OPAL_ERR_PERM",
                OPAL_ERR_PERM == rc);

out:
    chmod(dir, S_IRWXU);
    rmdir(dir);
    rmdir(base);
    free(dir);
}

/*
 * A directory that is readable but not searchable can be listed, but
 * its entries cannot be inspected or removed.  Destroy must report
 * that, not silently skip every entry.  (A callback that vetoes
 * nothing is used so that the leftover entries are not the only
 * thing flagging the failure.)
 */
static void test_destroy_unsearchable_dir(void)
{
    if (running_as_root("test_destroy_unsearchable_dir")) {
        return;
    }
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_unsearchable_dir: mkdtemp failed");
        return;
    }
    char *file = path_join(base, "afile");

    if (0 != create_file(file) || 0 != chmod(base, S_IRUSR | S_IWUSR)) {
        test_failure("test_destroy_unsearchable_dir: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(base, true, cb_protect);
    test_verify("destroy of unsearchable dir returns error", OPAL_SUCCESS != rc);

out:
    chmod(base, S_IRWXU);
    unlink(file);
    rmdir(base);
    free(file);
}

/*
 * Failing to remove an emptied subdirectory (here: its parent is not
 * writable) is a real failure, unlike the subdirectory still holding
 * something a callback preserved.
 */
static void test_destroy_subdir_rmdir_failure(void)
{
    if (running_as_root("test_destroy_subdir_rmdir_failure")) {
        return;
    }
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_subdir_rmdir_failure: mkdtemp failed");
        return;
    }
    char *sub = path_join(base, "sub");
    char *file = path_join(sub, "afile");

    if (0 != mkdir(sub, S_IRWXU) || 0 != create_file(file)
        || 0 != chmod(base, S_IRUSR | S_IXUSR)) {
        test_failure("test_destroy_subdir_rmdir_failure: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(base, true, cb_protect);
    test_verify("destroy with unremovable subdir returns error", OPAL_SUCCESS != rc);

out:
    chmod(base, S_IRWXU);
    unlink(file);
    rmdir(sub);
    rmdir(base);
    free(sub);
    free(file);
}

/*
 * Likewise for the base directory itself: if it is emptied but cannot
 * be removed (its parent is not writable), destroy has not done what
 * was asked.
 */
static void test_destroy_base_rmdir_failure(void)
{
    if (running_as_root("test_destroy_base_rmdir_failure")) {
        return;
    }
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_base_rmdir_failure: mkdtemp failed");
        return;
    }
    char *doomed = path_join(base, "doomed");

    if (0 != mkdir(doomed, S_IRWXU) || 0 != chmod(base, S_IRUSR | S_IXUSR)) {
        test_failure("test_destroy_base_rmdir_failure: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(doomed, true, NULL);
    test_verify("destroy of unremovable base dir returns error", OPAL_SUCCESS != rc);

out:
    chmod(base, S_IRWXU);
    rmdir(doomed);
    rmdir(base);
    free(doomed);
}

/*
 * Trailing path separators name the same directory, which must still
 * be emptied and removed.
 */
static void test_destroy_trailing_separator(void)
{
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_trailing_separator: mkdtemp failed");
        return;
    }
    char *doomed = path_join(base, "doomed//");
    char *file = path_join(base, "doomed/afile");

    if (0 != mkdir(doomed, S_IRWXU) || 0 != create_file(file)) {
        test_failure("test_destroy_trailing_separator: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(doomed, true, NULL);
    test_verify("destroy with trailing separators returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    struct stat buf;
    test_verify("dir named with trailing separators is gone", 0 != stat(doomed, &buf));

out:
    unlink(file);
    rmdir(doomed);
    rmdir(base);
    free(doomed);
    free(file);
}

/*
 * The directory being destroyed is removed relative to a descriptor
 * on its parent.  Removing it needs only write and search permission
 * on the parent -- not read permission -- and that must still work.
 */
static void test_destroy_unreadable_parent(void)
{
    if (running_as_root("test_destroy_unreadable_parent")) {
        return;
    }
    char tmpl[] = "/tmp/opal_test_XXXXXX";
    char *base = mkdtemp(tmpl);
    if (NULL == base) {
        test_failure("test_destroy_unreadable_parent: mkdtemp failed");
        return;
    }
    char *doomed = path_join(base, "doomed");
    char *file = path_join(doomed, "afile");

    if (0 != mkdir(doomed, S_IRWXU) || 0 != create_file(file)
        || 0 != chmod(base, S_IWUSR | S_IXUSR)) {
        test_failure("test_destroy_unreadable_parent: setup failed");
        goto out;
    }

    int rc = opal_os_dirpath_destroy(doomed, true, NULL);
    test_verify("destroy under write+search-only parent returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);

    struct stat buf;
    test_verify("dir under write+search-only parent is gone", 0 != stat(doomed, &buf));

out:
    chmod(base, S_IRWXU);
    unlink(file);
    rmdir(doomed);
    rmdir(base);
    free(doomed);
    free(file);
}
