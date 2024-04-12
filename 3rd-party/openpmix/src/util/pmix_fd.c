/*
 * Copyright (c) 2008-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "pmix_common.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_PWD_H
#    include <pwd.h>
#endif
#ifdef HAVE_DIRENT_H
#    include <dirent.h>
#endif
#ifdef HAVE_STRINGS_H
#    include <strings.h>
#endif
#include <ctype.h>

#include "src/util/pmix_error.h"
#include "src/util/pmix_fd.h"
#include "src/util/pmix_string_copy.h"
#include "src/runtime/pmix_rte.h"

/*
 * Simple loop over reading from a fd
 */
pmix_status_t pmix_fd_read(int fd, int len, void *buffer)
{
    int rc;
    char *b = buffer;

    while (len > 0) {
        rc = read(fd, b, len);
        if (rc < 0 && (EAGAIN == errno || EINTR == errno)) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else if (0 == rc) {
            return PMIX_ERR_TIMEOUT;
        } else {
            return PMIX_ERR_IN_ERRNO;
        }
    }
    return PMIX_SUCCESS;
}

/*
 * Simple loop over writing to an fd
 */
pmix_status_t pmix_fd_write(int fd, int len, const void *buffer)
{
    int rc;
    const char *b = buffer;

    while (len > 0) {
        rc = write(fd, b, len);
        if (rc < 0 && (EAGAIN == errno || EINTR == errno)) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else {
            return PMIX_ERR_IN_ERRNO;
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_fd_set_cloexec(int fd)
{
#ifdef FD_CLOEXEC
    int flags;

    /* Stevens says that we should get the fd's flags before we set
       them.  So say we all. */
    flags = fcntl(fd, F_GETFD, 0);
    if (-1 == flags) {
        return PMIX_ERR_IN_ERRNO;
    }

    if (fcntl(fd, F_SETFD, FD_CLOEXEC | flags) == -1) {
        return PMIX_ERR_IN_ERRNO;
    }
#endif

    return PMIX_SUCCESS;
}

bool pmix_fd_is_regular(int fd)
{
    struct stat buf;
    /* coverity[TOCTOU] */
    if (fstat(fd, &buf)) {
        return false;
    }
    return S_ISREG(buf.st_mode);
}

bool pmix_fd_is_chardev(int fd)
{
    struct stat buf;
    /* coverity[TOCTOU] */
    if (fstat(fd, &buf)) {
        return false;
    }
    return S_ISCHR(buf.st_mode);
}

bool pmix_fd_is_blkdev(int fd)
{
    struct stat buf;
    /* coverity[TOCTOU] */
    if (fstat(fd, &buf)) {
        return false;
    }
    return S_ISBLK(buf.st_mode);
}

#if PMIX_ENABLE_IPV6
static char str[INET6_ADDRSTRLEN];
#else
static char str[INET_ADDRSTRLEN];
#endif

const char *pmix_fd_get_peer_name(int fd)
{
    const char *ret = NULL;
    struct sockaddr sa;
    socklen_t slt = (socklen_t) sizeof(sa);
    int rc;

    memset(str, 0, sizeof(str));

    rc = getpeername(fd, &sa, &slt);
    if (0 != rc) {
        pmix_string_copy(str, "Unknown", sizeof(str) - 1);
        ret = str;
        return ret;
    }

    if (sa.sa_family == AF_INET) {
        struct sockaddr_in *si;
        si = (struct sockaddr_in *) &sa;
        ret = inet_ntop(AF_INET, &(si->sin_addr), str, INET_ADDRSTRLEN);
    }
#if PMIX_ENABLE_IPV6
    else if (sa.sa_family == AF_INET6) {
        struct sockaddr_in6 *si6;
        si6 = (struct sockaddr_in6 *) &sa;
        ret = inet_ntop(AF_INET6, &(si6->sin6_addr), str, INET6_ADDRSTRLEN);
    }
#endif
    else {
        // This string is guaranteed to be <= INET_ADDRSTRLEN
        memset(str, 0, sizeof(str));
        pmix_string_copy(str, "Unknown", sizeof(str) - 1);
        ret = str;
    }

    return ret;
}

static int fdmax = -1;

/* close all open file descriptors w/ exception of stdin/stdout/stderr
 and the pipe up to the parent. */
void pmix_close_open_file_descriptors(int protected_fd)
{
#if defined(__OSX__)
    DIR *dir = opendir("/dev/fd");
#else  /* Linux */
    DIR *dir = opendir("/proc/self/fd");
#endif  /* defined(__OSX__) */
    struct dirent *files;
    int dir_scan_fd = -1;

    if (NULL == dir) {
        goto slow;
    }

    /* grab the fd of the opendir above so we don't close in the
     * middle of the scan. */
    dir_scan_fd = dirfd(dir);
    if (dir_scan_fd < 0) {
        goto slow;
    }

    while (NULL != (files = readdir(dir))) {
        if (!isdigit(files->d_name[0])) {
            continue;
        }
        int fd = strtol(files->d_name, NULL, 10);
        if (errno == EINVAL || errno == ERANGE) {
            closedir(dir);
            goto slow;
        }
        if (fd >= 3 && (-1 == protected_fd || fd != protected_fd) && fd != dir_scan_fd) {
            close(fd);
        }
    }
    closedir(dir);
    return;

slow:
    // close *all* file descriptors -- slow
    if (0 > fdmax) {
        fdmax = sysconf(_SC_OPEN_MAX);
    }
    // On some OS's (e.g., macOS), the value returned by
    // sysconf(_SC_OPEN_MAX) can be set by the user via "ulimit -n X",
    // where X can be -1 (unlimited) or a positive integer. On macOS
    // in particular, if the user does not set this value, it's
    // unclear how the default value is chosen.  Some users have
    // reported seeing arbitrarily large default values (in the
    // billions), resulting in a very large loop over close() that can
    // take minutes/hours to complete, leading the user to think that
    // the app has hung.  To avoid this, ensure that we cap the max FD
    // that we'll try to close.  This is not a perfect scheme, and
    // there's uncertainty on how the macOS default value works, so we
    // provide the pmix_maxfd MCA var to allow the user to set the max
    // FD value if needed.
    if (-1 == fdmax || pmix_maxfd < fdmax) {
        fdmax = pmix_maxfd;
    }
    for (int fd = 3; fd < fdmax; fd++) {
        if (fd != protected_fd) {
            close(fd);
        }
    }
}
