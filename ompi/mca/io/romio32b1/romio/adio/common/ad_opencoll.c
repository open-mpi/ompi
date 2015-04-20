/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2007 UChicago/Argonne LLC
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

/* Generic version of a "collective open".  Assumes a "real" underlying
 * file system (meaning no wonky consistency semantics like NFS).
 *
 * optimization: by having just one process create a file, close it,
 * then have all N processes open it, we can possibly avoid contention
 * for write locks on a directory for some file systems.  
 *
 * Happy side-effect: exclusive create (error if file already exists)
 * just falls out 
 *
 * Note: this is not a "scalable open" (c.f. "The impact of file systems
 * on MPI-IO scalability").  
 */
     
void ADIOI_GEN_OpenColl(ADIO_File fd, int rank, 
	int access_mode, int *error_code)
{
    int orig_amode_excl, orig_amode_wronly;
    MPI_Comm tmp_comm;

    orig_amode_excl = access_mode;

    if (access_mode & ADIO_CREATE ){
       if(rank == fd->hints->ranklist[0]) {
	   /* remove delete_on_close flag if set */
	   if (access_mode & ADIO_DELETE_ON_CLOSE)
	       fd->access_mode = access_mode ^ ADIO_DELETE_ON_CLOSE;
	   else 
	       fd->access_mode = access_mode;
	       
	   tmp_comm = fd->comm;
	   fd->comm = MPI_COMM_SELF;
	   (*(fd->fns->ADIOI_xxx_Open))(fd, error_code);
	   fd->comm = tmp_comm;
	   MPI_Bcast(error_code, 1, MPI_INT, \
		     fd->hints->ranklist[0], fd->comm);
	   /* if no error, close the file and reopen normally below */
	   if (*error_code == MPI_SUCCESS) 
	       (*(fd->fns->ADIOI_xxx_Close))(fd, error_code);

	   fd->access_mode = access_mode; /* back to original */
       }
       else MPI_Bcast(error_code, 1, MPI_INT, fd->hints->ranklist[0], fd->comm);

       if (*error_code != MPI_SUCCESS) {
	   return;
       } 
       else {
           /* turn off CREAT (and EXCL if set) for real multi-processor open */
           access_mode ^= ADIO_CREATE; 
	   if (access_mode & ADIO_EXCL)
		   access_mode ^= ADIO_EXCL;
       }
    }
    fd->blksize = 1024*1024*4; /* this large default value should be good for
				 most file systems.  any ROMIO driver is free
				 to stat the file and find an optimial value */

    /* if we are doing deferred open, non-aggregators should return now */
    if (fd->hints->deferred_open ) {
        if (!(fd->is_agg)) {
            /* we might have turned off EXCL for the aggregators.
             * restore access_mode that non-aggregators get the right
             * value from get_amode */
            fd->access_mode = orig_amode_excl;
	    /* In file-system specific open, a driver might collect some
	     * information via stat().  Deferred open means not every process
	     * participates in fs-specific open, but they all participate in
	     * this open call.  Broadcast a bit of information in case
	     * lower-level file system driver (e.g. 'bluegene') collected it
	     * (not all do)*/
	    MPI_Bcast(&(fd->blksize), 1, MPI_LONG, fd->hints->ranklist[0], fd->comm);
	    *error_code = MPI_SUCCESS;
	    ADIOI_Assert(fd->blksize > 0);
	    return;
	}
    }

/* For writing with data sieving, a read-modify-write is needed. If 
   the file is opened for write_only, the read will fail. Therefore,
   if write_only, open the file as read_write, but record it as write_only
   in fd, so that get_amode returns the right answer. */

    /* observation from David Knaak: file systems that do not support data
     * sieving do not need to change the mode */

    orig_amode_wronly = access_mode;
    if ( (access_mode & ADIO_WRONLY) &&
	    ADIO_Feature(fd, ADIO_DATA_SIEVING_WRITES) ) {
	access_mode = access_mode ^ ADIO_WRONLY;
	access_mode = access_mode | ADIO_RDWR;
    }
    fd->access_mode = access_mode;

    (*(fd->fns->ADIOI_xxx_Open))(fd, error_code);

    /* if error, may be it was due to the change in amode above. 
       therefore, reopen with access mode provided by the user.*/ 
    fd->access_mode = orig_amode_wronly;  
    if (*error_code != MPI_SUCCESS) 
        (*(fd->fns->ADIOI_xxx_Open))(fd, error_code);

    /* if we turned off EXCL earlier, then we should turn it back on */
    if (fd->access_mode != orig_amode_excl) fd->access_mode = orig_amode_excl;

    /* broadcast a bit of information (blocksize for now) to all proceses in
     * communicator, not just those who participated in open */
    MPI_Bcast(&(fd->blksize), 1, MPI_LONG, fd->hints->ranklist[0], fd->comm);
    /* file domain code will get terribly confused in a hard-to-debug way if
     * gpfs blocksize not sensible */
    ADIOI_Assert( fd->blksize > 0);
    /* for deferred open: this process has opened the file (because if we are
     * not an aggregaor and we are doing deferred open, we returned earlier)*/
    fd->is_open = 1;

}

/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
