/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: lock.c,v 1.9 2002/10/24 17:01:14 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

#ifdef ROMIO_NTFS
int ADIOI_Set_lock(FDTYPE fd, int cmd, int type, ADIO_Offset offset, int whence,
	     ADIO_Offset len) 
{
    int ret_val, error_code;
	OVERLAPPED Overlapped;
	DWORD dwFlags;
	
	dwFlags = type;

	Overlapped.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
#ifdef HAVE_INT64
	Overlapped.Offset = ( (DWORD) ( offset & (__int64) 0xFFFFFFFF ) );
	Overlapped.OffsetHigh = ( (DWORD) ( (offset >> 32) & (__int64) 0xFFFFFFFF ) );

	if (cmd == ADIOI_LOCK_CMD)
		ret_val = LockFileEx(fd, dwFlags, 0, 
			( (DWORD) ( len & (__int64) 0xFFFFFFFF ) ), 
			( (DWORD) ( (len >> 32) & (__int64) 0xFFFFFFFF ) ), 
			&Overlapped);
	else
		ret_val = UnlockFileEx(fd, 0, 
			( (DWORD) ( len & (__int64) 0xFFFFFFFF ) ), 
			( (DWORD) ( (len >> 32) & (__int64) 0xFFFFFFFF ) ), 
			&Overlapped);
#else
	Overlapped.Offset = offset;
	Overlapped.OffsetHigh = 0;

	if (cmd == ADIOI_LOCK_CMD)
		ret_val = LockFileEx(fd, dwFlags, 0, len, 0, &Overlapped);
	else
		ret_val = UnlockFileEx(fd, 0, len, 0, &Overlapped);
#endif

    if (!ret_val) {
	FPRINTF(stderr, "File locking failed in ADIOI_Set_lock.\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    error_code = (ret_val) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
    return error_code;
}
#else
int ADIOI_Set_lock(FDTYPE fd, int cmd, int type, ADIO_Offset offset, int whence,
	     ADIO_Offset len) 
{
    int err, error_code;
    struct flock lock;

    lock.l_type = type;
    lock.l_start = offset;
    lock.l_whence = whence;
    lock.l_len = len;

    do {
	err = fcntl(fd, cmd, &lock);
    } while (err && (errno == EINTR));

    if (err && (errno != EBADF)) {
	FPRINTF(stderr, "File locking failed in ADIOI_Set_lock. If the file system is NFS, you need to use NFS version 3 and mount the directory with the 'noac' option (no attribute caching).\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    error_code = (err == 0) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
    return error_code;
}
#endif

#if (defined(HFS) || defined(XFS))
int ADIOI_Set_lock64(FDTYPE fd, int cmd, int type, ADIO_Offset offset, int whence,
	     ADIO_Offset len) 
{
    int err, error_code;
    struct flock64 lock;

    lock.l_type = type;
    lock.l_start = offset;
    lock.l_whence = whence;
    lock.l_len = len;

    do {
	err = fcntl(fd, cmd, &lock);
    } while (err && (errno == EINTR));

    if (err && (errno != EBADF)) {
	FPRINTF(stderr, "File locking failed in ADIOI_Set_lock64\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    error_code = (err == 0) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
    return error_code;
}
#endif
