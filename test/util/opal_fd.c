/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2008-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "support.h"
#include "opal/util/fd.h"
#include "opal/constants.h"

#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <stdint.h>

/* -----------------------------------------------------------------------
 * Tests: opal_fd_write / opal_fd_read round-trip via pipe
 * ---------------------------------------------------------------------- */

static void test_write_read_roundtrip(void)
{
    int fds[2];
    const char src[] = "Hello, OPAL fd test!";
    const int len = (int) (sizeof(src) - 1);   /* exclude NUL */
    char dst[64];
    int rc;

    memset(dst, 0, sizeof(dst));

    if (0 != pipe(fds)) {
        test_verify("fd_write/read: pipe() succeeded", 0 == 1);
        return;
    }

    rc = opal_fd_write(fds[1], len, src);
    test_verify("fd_write: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* Close write end so read end does not block forever */
    close(fds[1]);

    rc = opal_fd_read(fds[0], len, dst);
    test_verify("fd_read: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("fd_read: data matches what was written", 0 == memcmp(src, dst, (size_t) len));

    close(fds[0]);
}

static void test_write_read_large_buffer(void)
{
    /*
     * Exercise the multi-iteration write/read loops in opal_fd_write and
     * opal_fd_read with a buffer larger than any platform's pipe capacity.
     * opal_fd_write blocks until every byte is written, so the pipe must be
     * drained concurrently -- writing a buffer bigger than the pipe with no
     * reader would deadlock.  Fork a child to read while the parent writes;
     * the child verifies the data (against its inherited copy of wbuf) and
     * reports the result via its exit status.
     */
    const int len = 256 * 1024;   /* exceeds typical pipe capacities (16-64 KB) */
    char *wbuf = malloc((size_t) len);
    int fds[2];
    int i;
    pid_t pid;

    if (NULL == wbuf) {
        test_verify("fd_write/read large: malloc succeeded", 0 == 1);
        return;
    }

    for (i = 0; i < len; i++) {
        wbuf[i] = (char) (i & 0xff);
    }

    if (0 != pipe(fds)) {
        test_verify("fd_write/read large: pipe() succeeded", 0 == 1);
        free(wbuf);
        return;
    }

    pid = fork();
    if (-1 == pid) {
        test_verify("fd_write/read large: fork() succeeded", 0 == 1);
        close(fds[0]);
        close(fds[1]);
        free(wbuf);
        return;
    }

    if (0 == pid) {
        /* Child: drain the pipe and verify the bytes byte-for-byte against
           the inherited copy of wbuf.  Exit 0 on success, non-zero on any
           error or mismatch.  Do not call test_verify() here -- the child
           has its own (separate) test counters. */
        char *rbuf = malloc((size_t) len);
        int child_status = 0;

        close(fds[1]);   /* read end only */
        if (NULL == rbuf) {
            _exit(2);
        }
        if (OPAL_SUCCESS != opal_fd_read(fds[0], len, rbuf)
            || 0 != memcmp(wbuf, rbuf, (size_t) len)) {
            child_status = 1;
        }
        free(rbuf);
        close(fds[0]);
        _exit(child_status);
    } else {
        /* Parent: write the whole buffer, then reap the child. */
        int rc;
        int wstatus = 0;

        close(fds[0]);   /* write end only */
        rc = opal_fd_write(fds[1], len, wbuf);
        test_verify("fd_write large: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
        close(fds[1]);

        waitpid(pid, &wstatus, 0);
        test_verify("fd_read large (child): data matches what was written",
                    WIFEXITED(wstatus) && 0 == WEXITSTATUS(wstatus));
        free(wbuf);
    }
}

static void test_read_timeout_on_closed_pipe(void)
{
    /*
     * Reading from a pipe whose write end has been closed with no data
     * available should return OPAL_ERR_TIMEOUT (EOF before len bytes read).
     */
    int fds[2];
    char buf[4];
    int rc;

    if (0 != pipe(fds)) {
        test_verify("fd_read timeout: pipe() succeeded", 0 == 1);
        return;
    }

    /* Close write end immediately -- no data written */
    close(fds[1]);

    rc = opal_fd_read(fds[0], (int) sizeof(buf), buf);
    test_verify("fd_read: closed pipe with no data -> OPAL_ERR_TIMEOUT",
                OPAL_ERR_TIMEOUT == rc);

    close(fds[0]);
}

static void test_write_bad_fd(void)
{
    /*
     * Writing to an invalid fd (-1) must return OPAL_ERR_IN_ERRNO.
     */
    const char buf[] = "data";
    int rc = opal_fd_write(-1, (int) sizeof(buf), buf);
    test_verify("fd_write to bad fd -> OPAL_ERR_IN_ERRNO", OPAL_ERR_IN_ERRNO == rc);
}

static void test_read_bad_fd(void)
{
    /*
     * Reading from an invalid fd (-1) must return OPAL_ERR_IN_ERRNO.
     */
    char buf[4];
    int rc = opal_fd_read(-1, (int) sizeof(buf), buf);
    test_verify("fd_read from bad fd -> OPAL_ERR_IN_ERRNO", OPAL_ERR_IN_ERRNO == rc);
}

/* -----------------------------------------------------------------------
 * Tests: opal_fd_set_cloexec
 * ---------------------------------------------------------------------- */

static void test_set_cloexec(void)
{
    int fds[2];
    int flags;
    int rc;

    if (0 != pipe(fds)) {
        test_verify("fd_set_cloexec: pipe() succeeded", 0 == 1);
        return;
    }

    rc = opal_fd_set_cloexec(fds[0]);
    test_verify("fd_set_cloexec: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    flags = fcntl(fds[0], F_GETFD, 0);
    test_verify("fd_set_cloexec: FD_CLOEXEC bit is set in fd flags",
                (flags & FD_CLOEXEC) != 0);

    close(fds[0]);
    close(fds[1]);
}

static void test_set_cloexec_bad_fd(void)
{
    /*
     * opal_fd_set_cloexec on an invalid fd: fcntl(F_GETFD) will fail
     * so the function should return OPAL_ERR_IN_ERRNO.
     * On platforms where FD_CLOEXEC is not defined the function is a no-op
     * that returns OPAL_SUCCESS; we allow both outcomes.
     */
    int rc = opal_fd_set_cloexec(-1);
    test_verify("fd_set_cloexec on bad fd: fails or is no-op (no crash)",
                OPAL_ERR_IN_ERRNO == rc || OPAL_SUCCESS == rc);
}

/* -----------------------------------------------------------------------
 * Tests: opal_fd_is_regular
 * ---------------------------------------------------------------------- */

static void test_is_regular_tmpfile(void)
{
    char tmpfile[] = "/tmp/opal_fd_test_XXXXXX";
    int fd = mkstemp(tmpfile);

    if (-1 == fd) {
        test_verify("fd_is_regular: mkstemp succeeded", 0 == 1);
        return;
    }

    test_verify("fd_is_regular: mkstemp file is a regular file",
                opal_fd_is_regular(fd));
    test_verify("fd_is_regular: mkstemp file is NOT a char device",
                !opal_fd_is_chardev(fd));
    test_verify("fd_is_regular: mkstemp file is NOT a block device",
                !opal_fd_is_blkdev(fd));

    close(fd);
    unlink(tmpfile);
}

static void test_is_regular_pipe(void)
{
    /*
     * A pipe fd is NOT a regular file.
     */
    int fds[2];

    if (0 != pipe(fds)) {
        test_verify("fd_is_regular pipe: pipe() succeeded", 0 == 1);
        return;
    }

    test_verify("fd_is_regular: pipe fd is NOT a regular file",
                !opal_fd_is_regular(fds[0]));

    close(fds[0]);
    close(fds[1]);
}

static void test_is_regular_bad_fd(void)
{
    /* Invalid fd: fstat fails, so all three predicates return false */
    test_verify("fd_is_regular bad fd: returns false", !opal_fd_is_regular(-1));
    test_verify("fd_is_chardev bad fd: returns false", !opal_fd_is_chardev(-1));
    test_verify("fd_is_blkdev bad fd: returns false",  !opal_fd_is_blkdev(-1));
}

/* -----------------------------------------------------------------------
 * Tests: opal_fd_is_chardev
 * ---------------------------------------------------------------------- */

static void test_is_chardev_dev_null(void)
{
    /*
     * /dev/null is always a character device on POSIX systems.
     */
    int fd = open("/dev/null", O_RDONLY);

    if (-1 == fd) {
        test_verify("fd_is_chardev: open /dev/null succeeded", 0 == 1);
        return;
    }

    test_verify("fd_is_chardev: /dev/null is a character device",
                opal_fd_is_chardev(fd));
    test_verify("fd_is_regular: /dev/null is NOT a regular file",
                !opal_fd_is_regular(fd));
    test_verify("fd_is_blkdev: /dev/null is NOT a block device",
                !opal_fd_is_blkdev(fd));

    close(fd);
}

/* -----------------------------------------------------------------------
 * Tests: opal_fd_is_blkdev
 * ---------------------------------------------------------------------- */

static void test_is_blkdev_tmpfile(void)
{
    /*
     * A regular temp file is NOT a block device.
     */
    char tmpfile[] = "/tmp/opal_fd_blkdev_XXXXXX";
    int fd = mkstemp(tmpfile);

    if (-1 == fd) {
        test_verify("fd_is_blkdev tmpfile: mkstemp succeeded", 0 == 1);
        return;
    }

    test_verify("fd_is_blkdev: regular tmp file is NOT a block device",
                !opal_fd_is_blkdev(fd));

    close(fd);
    unlink(tmpfile);
}

/* -----------------------------------------------------------------------
 * main
 * ---------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    test_init("opal_fd");

    /* opal_fd_write / opal_fd_read */
    test_write_read_roundtrip();
    test_write_read_large_buffer();
    test_read_timeout_on_closed_pipe();
    test_write_bad_fd();
    test_read_bad_fd();

    /* opal_fd_set_cloexec */
    test_set_cloexec();
    test_set_cloexec_bad_fd();

    /* opal_fd_is_regular */
    test_is_regular_tmpfile();
    test_is_regular_pipe();
    test_is_regular_bad_fd();

    /* opal_fd_is_chardev */
    test_is_chardev_dev_null();

    /* opal_fd_is_blkdev */
    test_is_blkdev_tmpfile();

    return test_finalize();
}
