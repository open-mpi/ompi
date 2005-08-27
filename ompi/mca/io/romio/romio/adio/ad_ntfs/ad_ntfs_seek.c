/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ntfs_seek.c,v 1.5 2002/11/13 13:30:36 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* Set the style to c++ since this code will only be compiled with the
   Windows C/C++ compiler that accepts C++ style comments and other 
   constructions */
/* style:c++ header */

#include "ad_ntfs.h"
//#include "adio.h"
#include "adio_extern.h"
#ifdef PROFILE
#include "mpe.h"
#endif

ADIO_Offset ADIOI_NTFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
		      int whence, int *error_code)
{
    //return ADIOI_GEN_SeekIndividual(fd, offset, whence, error_code);

/* implemented for whence=SEEK_SET only. SEEK_CUR and SEEK_END must
   be converted to the equivalent with SEEK_SET before calling this 
   routine. */
/* offset is in units of etype relative to the filetype */

#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_GEN_SEEKINDIVIDUAL";
#endif
    ADIO_Offset off, err;
    ADIOI_Flatlist_node *flat_file;

    int i, n_etypes_in_filetype, n_filetypes, etype_in_filetype;
    ADIO_Offset abs_off_in_filetype=0;
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

#ifdef PROFILE
    MPE_Log_event(11, 0, "start seek");
#endif
#ifdef ROMIO_NTFS
	{
#ifdef HAVE_INT64
		DWORD dwTemp;
		dwTemp = ( (DWORD) ( (off >> 32) & (__int64) 0xFFFFFFFF ) );
		err = SetFilePointer(fd->fd_sys, 
			( (DWORD) ( off & (__int64) 0xFFFFFFFF ) ), &dwTemp, 
			FILE_BEGIN);
#else
		err = SetFilePointer(fd->fd_sys, off, NULL, FILE_BEGIN);
#endif
	}
#else
    err = lseek(fd->fd_sys, off, SEEK_SET);
#endif
#ifdef PROFILE
    MPE_Log_event(12, 0, "end seek");
#endif
    fd->fp_ind = off;
    fd->fp_sys_posn = off;

#ifdef PRINT_ERR_MSG
    *error_code = (err == -1) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
    if (err == -1) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(MPI_FILE_NULL, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif

    return off;
}
