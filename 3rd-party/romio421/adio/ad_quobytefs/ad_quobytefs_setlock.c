/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "adio.h"
#include "lock_internal.h"

#include "ad_quobytefs.h"

int ADIOI_QUOBYTEFS_SetLock(ADIO_File fd, int cmd, int type, ADIO_Offset offset, int whence,
                            ADIO_Offset len)
{
    struct flock lock;
    lock.l_type = type;
    lock.l_whence = whence;
    lock.l_start = offset;
    lock.l_len = len;

    int err, save_errno, err_count;     /* save previous errno in case we recover from retryable errors */
    errno = 0;
    err_count = 0;
    save_errno = errno;

    do {
        err = quobyte_lock(fd->file_handle, cmd, &lock);
    } while (err && ((errno == EINTR) || ((errno == EINPROGRESS) && (++err_count < 10000))));

    if (!err)   /* report fcntl failure errno's (EBADF), otherwise */
        errno = save_errno;     /* restore previous errno in case we recovered from retryable errors */

    if (err && (errno != EBADF)) {
        FPRINTF(stderr,
                "File locking failed in ADIOI_QUOBYTEFS_SetLock(fd %X,cmd %s/%X,type %s/%X,whence "
                "%X) with return value %X and errno %X.\n",
                fd->fd_sys, ADIOI_GEN_flock_cmd_to_string(cmd), cmd,
                ADIOI_GEN_flock_type_to_string(type), type, whence, err, errno);
        perror("ADIOI_QUOBYTEFS_SetLock:");
        FPRINTF(stderr, "ADIOI_QUOBYTEFS_SetLock:offset %llu, length %llu\n",
                (unsigned long long) offset, (unsigned long long) len);
        MPI_Abort(MPI_COMM_WORLD, 1);
        return MPI_ERR_IO;
    }
    return MPI_SUCCESS;
}
