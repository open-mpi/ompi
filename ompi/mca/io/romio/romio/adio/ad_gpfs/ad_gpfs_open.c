/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_gpfs_open.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_gpfs.h"
#include "ad_gpfs_tuning.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>


#ifdef HAVE_GPFS_H
#include <gpfs.h>
#endif
#ifdef HAVE_GPFS_FCNTL_H
#include <gpfs_fcntl.h>
#endif

#ifdef HAVE_GPFS_FCNTL_H
static void gpfs_free_all_locks(int fd)
{
    int rc;
    struct {
	gpfsFcntlHeader_t header;
	gpfsFreeRange_t release;
    } release_all;

    release_all.header.totalLength = sizeof(release_all);
    release_all.header.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
    release_all.header.fcntlReserved = 0;

    release_all.release.structLen = sizeof(release_all.release);
    release_all.release.structType = GPFS_FREE_RANGE;
    release_all.release.start = 0;
    release_all.release.length = 0;

    rc = gpfs_fcntl(fd, &release_all);
    if (rc != 0) {
	DBGV_FPRINTF(stderr,"GPFS fcntl release failed with rc=%d, errno=%d\n",
		rc,errno);
    }
}
#endif


void ADIOI_GPFS_Open(ADIO_File fd, int *error_code)
{
  int perm, old_mask, amode, rank, rc;
  static char myname[] = "ADIOI_GPFS_OPEN";

  /* set internal variables for tuning environment variables */
  ad_gpfs_get_env_vars();

  if (fd->perm == ADIO_PERM_NULL)  {
    old_mask = umask(022);
    umask(old_mask);
    perm = old_mask ^ 0666;
  }
  else perm = fd->perm;

    amode = 0;
    if (fd->access_mode & ADIO_CREATE)
	amode = amode | O_CREAT;
    if (fd->access_mode & ADIO_RDONLY)
	amode = amode | O_RDONLY;
    if (fd->access_mode & ADIO_WRONLY)
	amode = amode | O_WRONLY;
    if (fd->access_mode & ADIO_RDWR)
	amode = amode | O_RDWR;
    if (fd->access_mode & ADIO_EXCL)
	amode = amode | O_EXCL;
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_open_a, 0, NULL);
#endif
    fd->fd_sys = open(fd->filename, amode, perm);
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_open_b, 0, NULL);
#endif
  DBG_FPRINTF(stderr,"open('%s',%#X,%#X) rc=%d, errno=%d\n",fd->filename,amode,perm,fd->fd_sys,errno);
  fd->fd_direct = -1;

  if (gpfsmpio_devnullio == 1) {
      fd->null_fd = open("/dev/null", O_RDWR);
  } else {
      fd->null_fd = -1;
  }

  if ((fd->fd_sys != -1) && (fd->access_mode & ADIO_APPEND))
    fd->fp_ind = fd->fp_sys_posn = lseek(fd->fd_sys, 0, SEEK_END);

    if(fd->fd_sys != -1)
    {

        fd->blksize = 1048576; /* default to 1M */

#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_stat_a, 0, NULL);
#endif
	/* in this fs-specific routine, we might not be called over entire
	 * communicator (deferred open).  Collect statistics on one process.
	 * ADIOI_GEN_Opencoll (common-code caller) will take care of the
	 * broadcast */

	MPI_Comm_rank(fd->comm, &rank);
	if ((rank == fd->hints->ranklist[0]) || (fd->comm == MPI_COMM_SELF)) {
	    struct stat64 gpfs_statbuf;
	    /* Get the (real) underlying file system block size */
	    rc = stat64(fd->filename, &gpfs_statbuf);
	    if (rc >= 0)
	    {
		fd->blksize = gpfs_statbuf.st_blksize;
		DBGV_FPRINTF(stderr,"Successful stat '%s'.  Blocksize=%ld\n",
			fd->filename,gpfs_statbuf.st_blksize);
	    }
	    else
	    {
		DBGV_FPRINTF(stderr,"Stat '%s' failed with rc=%d, errno=%d\n",
			fd->filename,rc,errno);
	    }
	}
	/* all other ranks have incorrect fd->blocksize, but ADIOI_GEN_Opencoll
	 * will take care of that in both standard and deferred-open case */

#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_stat_b, 0, NULL);
#endif

#ifdef HAVE_GPFS_FCNTL_H
	/* in parallel workload, might be helpful to immediately release block
	 * tokens.  Or, system call overhead will outweigh any benefits... */
	if (getenv("ROMIO_GPFS_FREE_LOCKS")!=NULL)
	    gpfs_free_all_locks(fd->fd_sys);

#endif
    }

  if (fd->fd_sys == -1)  {
      *error_code = ADIOI_Err_create_code(myname, fd->filename, errno);
  }
  else *error_code = MPI_SUCCESS;
}
/* 
 *vim: ts=8 sts=4 sw=4 noexpandtab 
 */
