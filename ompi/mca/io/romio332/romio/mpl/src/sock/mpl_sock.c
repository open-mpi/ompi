/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

#if defined MPL_HAVE_SYS_UIO_H
/* Some platforms, such as Mac OSX (at least as of 10.9.1) hang when
 * attempting to send more than 2GB data, even though the writev
 * function is supposed to be able to handle large data.  This
 * function is a simple workaround for this case by attempting to send
 * lesser data, and having the upper layer retry later if needed.
 * This adds a small amount of bookkeeping overhead, but it should be
 * negligible compared to the system call overhead for small messages
 * and compared to the data transmission overhead for large
 * messages. */
ssize_t MPL_large_writev(int fd, const struct iovec *iov, int iovcnt)
{
    ssize_t total_size, tmp;
    struct iovec dummy;
    int i;

    /* If the total data fits into INT_MAX, directly use writev */
    total_size = 0;
    for (i = 0; i < iovcnt; i++)
        total_size += iov[i].iov_len;

    if (total_size <= INT_MAX) {
        do {
            tmp = writev(fd, iov, iovcnt);
        } while (tmp == -1 && errno == EINTR);
        return tmp;
    }

    /* Total data is larger than INT_MAX.  Issue writev with fewer
     * elements, so as to not exceed INT_MAX.  In this case, doing
     * multiple write calls, one for each iov segment is not a big
     * deal with respect to performance. */

    total_size = 0;
    for (i = 0; i < iovcnt; i++) {
        if (iov[i].iov_len <= INT_MAX) {
            do {
                tmp = writev(fd, &iov[i], 1);
            } while (tmp == -1 && errno == EINTR);
        } else {
            dummy.iov_base = iov[i].iov_base;
            dummy.iov_len = INT_MAX;
            do {
                tmp = writev(fd, &dummy, 1);
            } while (tmp == -1 && errno == EINTR);
        }

        if (tmp < 0)
            return tmp;
        else if (tmp < iov[i].iov_len) {
            total_size += tmp;
            return total_size;
        } else
            total_size += tmp;
    }

    return total_size;
}


ssize_t MPL_large_readv(int fd, const struct iovec * iov, int iovcnt)
{
    ssize_t total_size, tmp;
    struct iovec dummy;
    int i;

    /* If the total data fits into INT_MAX, directly use readv */
    total_size = 0;
    for (i = 0; i < iovcnt; i++)
        total_size += iov[i].iov_len;

    if (total_size <= INT_MAX) {
        do {
            tmp = readv(fd, iov, iovcnt);
        } while (tmp == -1 && errno == EINTR);
        return tmp;
    }

    /* Total data is larger than INT_MAX.  Issue readv with fewer
     * elements, so as to not exceed INT_MAX.  In this case, doing
     * multiple read calls, one for each iov segment is not a big
     * deal with respect to performance. */

    total_size = 0;
    for (i = 0; i < iovcnt; i++) {
        if (iov[i].iov_len <= INT_MAX) {
            do {
                tmp = readv(fd, &iov[i], 1);
            } while (tmp == -1 && errno == EINTR);
        } else {
            dummy.iov_base = iov[i].iov_base;
            dummy.iov_len = INT_MAX;
            do {
                tmp = readv(fd, &dummy, 1);
            } while (tmp == -1 && errno == EINTR);
        }

        if (tmp < 0)
            return tmp;
        else if (tmp < iov[i].iov_len) {
            total_size += tmp;
            return total_size;
        } else
            total_size += tmp;
    }

    return total_size;
}
#endif /* MPL_HAVE_SYS_UIO_H */
