/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *   Copyright (C) 1997 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

static const char *flock_cmd_to_string(int cmd)
{
    switch (cmd) {
#ifdef F_GETLK64
        case F_GETLK64:
            return "F_GETLK64";
#else
        case F_GETLK:
            return "F_GETLK";
#endif
#ifdef F_SETLK64
        case F_SETLK64:
            return "F_SETLK64";
#else
        case F_SETLK:
            return "F_SETLK";
#endif
#ifdef F_SETLKW64
        case F_SETLKW64:
            return "F_SETLKW64";
#else
        case F_SETLKW:
            return "F_SETLKW";
#endif
        default:
            return "UNEXPECTED";
    }
}

static const char *flock_type_to_string(int type)
{
    switch (type) {
        case F_RDLCK:
            return "F_RDLCK";
        case F_WRLCK:
            return "F_WRLCK";
        case F_UNLCK:
            return "F_UNLOCK";
        default:
            return "UNEXPECTED";
    }
}

#ifdef ROMIO_NTFS
/* This assumes that lock will always remain in the common directory and
 * that the ntfs directory will always be called ad_ntfs. */
#include "..\ad_ntfs\ad_ntfs.h"
int ADIOI_Set_lock(FDTYPE fd, int cmd, int type, ADIO_Offset offset, int whence, ADIO_Offset len)
{
    static char myname[] = "ADIOI_Set_lock";
    int ret_val, error_code = MPI_SUCCESS;
    OVERLAPPED Overlapped;
    DWORD dwFlags;

    MPL_UNREFERENCED_ARG(whence);

    if (len == 0)
        return MPI_SUCCESS;

    dwFlags = type;

    Overlapped.hEvent = /*0; */ CreateEvent(NULL, TRUE, FALSE, NULL);
#ifdef HAVE_INT64
    Overlapped.Offset = ((DWORD) (offset & (__int64) 0xFFFFFFFF));
    Overlapped.OffsetHigh = ((DWORD) ((offset >> 32) & (__int64) 0xFFFFFFFF));

    if (cmd == ADIOI_LOCK_CMD) {
        /*printf("locking %d\n", (int)fd);fflush(stdout); */
        ret_val = LockFileEx(fd, dwFlags, 0,
                             ((DWORD) (len & (__int64) 0xFFFFFFFF)),
                             ((DWORD) ((len >> 32) & (__int64) 0xFFFFFFFF)), &Overlapped);
    } else {
        /*printf("unlocking %d\n", (int)fd);fflush(stdout); */
        ret_val = UnlockFileEx(fd, 0,
                               ((DWORD) (len & (__int64) 0xFFFFFFFF)),
                               ((DWORD) ((len >> 32) & (__int64) 0xFFFFFFFF)), &Overlapped);
    }
#else
    Overlapped.Offset = offset;
    Overlapped.OffsetHigh = 0;

    if (cmd == ADIOI_LOCK_CMD) {
        /*printf("locking %d\n", (int)fd);fflush(stdout); */
        ret_val = LockFileEx(fd, dwFlags, 0, len, 0, &Overlapped);
    } else {
        /*printf("unlocking %d\n", (int)fd);fflush(stdout); */
        ret_val = UnlockFileEx(fd, 0, len, 0, &Overlapped);
    }
#endif

    if (!ret_val) {
        char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
        /*
         * FPRINTF(stderr, "File locking failed in ADIOI_Set_lock.\n");
         * MPI_Abort(MPI_COMM_WORLD, 1);
         */
        ret_val = GetLastError();
        if (ret_val == ERROR_IO_PENDING) {
            DWORD dummy;
            ret_val = GetOverlappedResult(fd, &Overlapped, &dummy, TRUE);
            if (ret_val) {
                CloseHandle(Overlapped.hEvent);
                return MPI_SUCCESS;
            }
            ret_val = GetLastError();
        }
        ADIOI_NTFS_Strerror(ret_val, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
        error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE, myname, __LINE__,
                                          MPI_ERR_IO, "**io", "**io %s", errMsg);
    }
    CloseHandle(Overlapped.hEvent);

    return error_code;
}
#else
int ADIOI_Set_lock(FDTYPE fd, int cmd, int type, ADIO_Offset offset, int whence, ADIO_Offset len)
{
    int err, error_code, err_count = 0, sav_errno;
    struct flock lock;

    if (len == 0)
        return MPI_SUCCESS;


    /* Depending on the compiler flags and options, struct flock
     * may not be defined with types that are the same size as
     * ADIO_Offsets.  */
/* FIXME: This is a temporary hack until we use flock64 where
   available. It also doesn't fix the broken Solaris header sys/types.h
   header file, which declars off_t as a UNION ! Configure tests to
   see if the off64_t is a union if large file support is requested;
   if so, it does not select large file support.
*/
#ifdef NEEDS_INT_CAST_WITH_FLOCK
    lock.l_type = type;
    lock.l_start = (int) offset;
    lock.l_whence = whence;
    lock.l_len = (int) len;
#else
    lock.l_type = type;
    lock.l_whence = whence;
    lock.l_start = offset;
    lock.l_len = len;
#endif

    sav_errno = errno;  /* save previous errno in case we recover from retryable errors */
    errno = 0;
    do {
        err = fcntl(fd, cmd, &lock);
#ifdef MPL_USE_DBG_LOGGING
/*      if (MPL_DBG_SELECTED(ROMIO,TERSE)) */
        {
            if (err && ((errno == EINTR) || (errno == EINPROGRESS))) {
                if ((err_count < 5) || (err_count > 9995)) {
                    fprintf(stderr,
                            "File locking failed in ADIOI_Set_lock(fd %#X,cmd %s/%#X,type %s/%#X,whence %#X) with return value %#X and errno %#X.  Retry (%d).\n",
                            fd, flock_cmd_to_string(cmd), cmd, flock_type_to_string(type), type,
                            whence, err, errno, err_count);
                    perror("ADIOI_Set_lock:");
                    fprintf(stderr, "ADIOI_Set_lock:offset %#llx, length %#llx\n",
                            (unsigned long long) offset, (unsigned long long) len);
                }
            }
        }
#endif
    } while (err && ((errno == EINTR) || ((errno == EINPROGRESS) && (++err_count < 10000))));

    if (err && (errno != EBADF)) {
        /* FIXME: This should use the error message system,
         * especially for MPICH */
        FPRINTF(stderr,
                "This requires fcntl(2) to be implemented. As of 8/25/2011 it is not. Generic MPICH Message: File locking failed in ADIOI_Set_lock(fd %X,cmd %s/%X,type %s/%X,whence %X) with return value %X and errno %X.\n"
                "- If the file system is NFS, you need to use NFS version 3, ensure that the lockd daemon is running on all the machines, and mount the directory with the 'noac' option (no attribute caching).\n"
                "- If the file system is LUSTRE, ensure that the directory is mounted with the 'flock' option.\n",
                fd, flock_cmd_to_string(cmd), cmd, flock_type_to_string(type), type, whence, err,
                errno);
        perror("ADIOI_Set_lock:");
        FPRINTF(stderr, "ADIOI_Set_lock:offset %llu, length %llu\n", (unsigned long long) offset,
                (unsigned long long) len);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (!err)   /* report fcntl failure errno's (EBADF), otherwise */
        errno = sav_errno;      /* restore previous errno in case we recovered from retryable errors */

    error_code = (err == 0) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
    return error_code;
}
#endif

int ADIOI_Set_lock64(FDTYPE fd, int cmd, int type, ADIO_Offset offset, int whence, ADIO_Offset len)
{
    int err, error_code;
#ifdef _LARGEFILE64_SOURCE
    struct flock64 lock;
#else
    struct flock lock;
#endif

    if (len == 0)
        return MPI_SUCCESS;

    lock.l_type = type;
    lock.l_start = offset;
    lock.l_whence = whence;
    lock.l_len = len;

    do {
        err = fcntl(fd, cmd, &lock);
    } while (err && (errno == EINTR));

    if (err && (errno != EBADF)) {
        FPRINTF(stderr,
                "File locking failed in ADIOI_Set_lock64(fd %X,cmd %s/%X,type %s/%X,whence %X) with return value %X and errno %X.\n"
                "If the file system is NFS, you need to use NFS version 3, ensure that the lockd daemon is running on all the machines, and mount the directory with the 'noac' option (no attribute caching).\n",
                fd, flock_cmd_to_string(cmd), cmd, flock_type_to_string(type), type, whence, err,
                errno);
        perror("ADIOI_Set_lock64:");
        FPRINTF(stderr, "ADIOI_Set_lock:offset %llu, length %llu\n", (unsigned long long) offset,
                (unsigned long long) len);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    error_code = (err == 0) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
    return error_code;
}
