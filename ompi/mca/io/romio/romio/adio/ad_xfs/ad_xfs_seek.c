/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_xfs_seek.c,v 1.6 2002/10/24 17:01:10 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_xfs.h"
#include "adio_extern.h"

ADIO_Offset ADIOI_XFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
		      int whence, int *error_code)
{
/* implemented for whence=SEEK_SET only. SEEK_CUR and SEEK_END must
   be converted to the equivalent with SEEK_SET before calling this 
   routine. */
/* offset is in units of etype relative to the filetype */

    ADIO_Offset abs_off_in_filetype=0, off;
    ADIOI_Flatlist_node *flat_file;

    int i, n_etypes_in_filetype, n_filetypes, etype_in_filetype;
    int size_in_filetype, sum;
    int filetype_size, etype_size, filetype_is_contig;
    MPI_Aint filetype_extent;

    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);
    etype_size = fd->etype_size;

    if (filetype_is_contig) off = fd->disp + etype_size * offset;
    else {
        flat_file = ADIOI_Flatlist;
        while (flat_file->type != fd->filetype) flat_file = flat_file->next;

	MPI_Type_extent(fd->filetype, &filetype_extent);
	MPI_Type_size(fd->filetype, &filetype_size);
	if ( ! filetype_size ) {
	    /* Since offset relative to the filetype size, we can't
	       do compute the offset when that result is zero.
	       Return zero for the offset for now */
	    *error_code = MPI_SUCCESS; 
	    return 0;
	}

	n_etypes_in_filetype = filetype_size/etype_size;
	n_filetypes = (int) (offset / n_etypes_in_filetype);
	etype_in_filetype = (int) (offset % n_etypes_in_filetype);
	size_in_filetype = etype_in_filetype * etype_size;
 
	sum = 0;
	for (i=0; i<flat_file->count; i++) {
	    sum += flat_file->blocklens[i];
	    if (sum > size_in_filetype) {
		abs_off_in_filetype = flat_file->indices[i] +
		    size_in_filetype - (sum - flat_file->blocklens[i]);
		break;
	    }
	}

	/* abs. offset in bytes in the file */
	off = fd->disp + (ADIO_Offset) n_filetypes * filetype_extent +
                abs_off_in_filetype;
    }

    /* no need to lseek, since we use pread/pwrite */
    /* err = lseek64(fd->fd_sys, off, SEEK_SET); */

    fd->fp_ind = off;
    fd->fp_sys_posn = -1;  /* null because we use pread/pwrite */

    *error_code = MPI_SUCCESS;
    return off;
}
