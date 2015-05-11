/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

/* Returns the absolute byte position in the file corresponding to 
   "offset" etypes relative to the current view. */

void ADIOI_Get_byte_offset(ADIO_File fd, ADIO_Offset offset, ADIO_Offset *disp)
{
    ADIOI_Flatlist_node *flat_file;
    int i;
    ADIO_Offset n_filetypes, etype_in_filetype, sum, abs_off_in_filetype=0, size_in_filetype;
    MPI_Count n_etypes_in_filetype, filetype_size, etype_size;
    int filetype_is_contig;
    MPI_Aint filetype_extent;

    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);
    etype_size = fd->etype_size;

    if (filetype_is_contig) *disp = fd->disp + etype_size * offset;
    else {
/* filetype already flattened in ADIO_Open */
        flat_file = ADIOI_Flatlist;
        while (flat_file->type != fd->filetype) flat_file = flat_file->next;

	MPI_Type_size_x(fd->filetype, &filetype_size);
	n_etypes_in_filetype = filetype_size/etype_size;
	n_filetypes = offset / n_etypes_in_filetype;
	etype_in_filetype = offset % n_etypes_in_filetype;
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
	MPI_Type_extent(fd->filetype, &filetype_extent);
	*disp = fd->disp + n_filetypes * ADIOI_AINT_CAST_TO_OFFSET filetype_extent + abs_off_in_filetype;
    }
}
