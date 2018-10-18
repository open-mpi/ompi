/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif
/* The following function selects the name of the file to be used to 
   store the shared file pointer. The shared-file-pointer file is a 
   hidden file in the same directory as the real file being accessed.
   If the real file is /tmp/thakur/testfile, the shared-file-pointer
   file will be /tmp/thakur/.testfile.shfp.yyy.xxxx, where yyy
   is rank 0's process id and xxxx is a random number. If the
   underlying file system supports shared file pointers
   (PVFS does not, for example), the file name is always
   constructed. This file is created only if the shared
   file pointer functions are used and is deleted when the real
   file is closed. */

void ADIOI_Shfp_fname(ADIO_File fd, int rank, int *error_code)
{
    int i;
    int len;
    char *slash, *ptr, tmp[128];
    int pid = 0;

    fd->shared_fp_fname = (char *) ADIOI_Malloc(PATH_MAX);

    if (!rank) {
        srand(time(NULL));
        i = rand();
	pid = (int)getpid();
	
	if (ADIOI_Strncpy(fd->shared_fp_fname, fd->filename, PATH_MAX)) {
	    *error_code = ADIOI_Err_create_code("ADIOI_Shfp_fname",
		    fd->filename, ENAMETOOLONG);
	    return;
	}
	
#ifdef ROMIO_NTFS
	slash = strrchr(fd->filename, '\\');
#else
	slash = strrchr(fd->filename, '/');
#endif
	if (!slash) {
	    if (ADIOI_Strncpy(fd->shared_fp_fname, ".", 2)) {
		*error_code = ADIOI_Err_create_code("ADIOI_Shfp_fname",
			fd->filename, ENAMETOOLONG);
		return;
	    }
	    if (ADIOI_Strncpy(fd->shared_fp_fname + 1, fd->filename, PATH_MAX-1)) {
		*error_code = ADIOI_Err_create_code("ADIOI_Shfp_fname",
			fd->filename, ENAMETOOLONG);
		return;
	    }
	}
	else {
	    ptr = slash;
#ifdef ROMIO_NTFS
		slash = strrchr(fd->shared_fp_fname, '\\');
#else
	    slash = strrchr(fd->shared_fp_fname, '/');
#endif
	    if (ADIOI_Strncpy(slash + 1, ".", 2))  {
		*error_code = ADIOI_Err_create_code("ADIOI_Shfp_fname",
			fd->filename, ENAMETOOLONG);
		return;
	    }
	    /* ok to cast: file names bounded by PATH_MAX and NAME_MAX */
	    len = (int) (PATH_MAX - (slash+2 - fd->shared_fp_fname));
	    if (ADIOI_Strncpy(slash + 2, ptr + 1, len)) {
		*error_code = ADIOI_Err_create_code("ADIOI_Shfp_fname",
			ptr + 1, ENAMETOOLONG);
		return;
	    }
	}
	    
	ADIOI_Snprintf(tmp, 128, ".shfp.%d.%d", pid, i);
	/* ADIOI_Strnapp will return non-zero if truncated.  That's ok */
	ADIOI_Strnapp(fd->shared_fp_fname, tmp, PATH_MAX);
	
	len = (int)strlen(fd->shared_fp_fname);
	MPI_Bcast(&len, 1, MPI_INT, 0, fd->comm);
	MPI_Bcast(fd->shared_fp_fname, len+1, MPI_CHAR, 0, fd->comm);
    }
    else {
	MPI_Bcast(&len, 1, MPI_INT, 0, fd->comm);
	MPI_Bcast(fd->shared_fp_fname, len+1, MPI_CHAR, 0, fd->comm);
    }
}
