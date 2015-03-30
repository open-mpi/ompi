/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void ADIO_Close(ADIO_File fd, int *error_code)
{
    int i, j, k, combiner, myrank, err, is_contig;
    static char myname[] = "ADIO_CLOSE";

    if (fd->async_count) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO, "**io",
					   "**io %s", strerror(errno));
	return;
    }

    /* because of deferred open, this warants a bit of explaining.  First, if
     * we've done aggregation,
     * then close the file.  Then, if any process left has done independent
     * i/o, close the file.  Otherwise, we'll skip the fs-specific close and
     * just say everything is a-ok.
     *
     * XXX: is it ok for those processes with a "real" communicator and those
     * with "MPI_COMM_SELF" to both call ADIOI_xxx_Close at the same time ?
     * everyone who ever opened the file will close it. Is order important? Is
     * timing important?
     */
    if (fd->hints->deferred_open && fd->is_agg) {
	    (*(fd->fns->ADIOI_xxx_Close))(fd, error_code);
    } else {
	    if(fd->is_open)  {
		    (*(fd->fns->ADIOI_xxx_Close))(fd, error_code);
	    } else {
		    *error_code = MPI_SUCCESS;
	    }
	    
    }

    if (fd->access_mode & ADIO_DELETE_ON_CLOSE) {
	/* if we are doing aggregation and deferred open, then it's possible
	 * that rank 0 does not have access to the file. make sure only an
	 * aggregator deletes the file.*/
	MPI_Comm_rank(fd->comm, &myrank);
	if (myrank == fd->hints->ranklist[0]) {
		ADIO_Delete(fd->filename, &err);
	}
	MPI_Barrier(fd->comm);
    }

    if (fd->fortran_handle != -1) {
	ADIOI_Ftable[fd->fortran_handle] = MPI_FILE_NULL;
    }

    if (fd->hints && fd->hints->ranklist) ADIOI_Free(fd->hints->ranklist);
    if (fd->hints && fd->hints->cb_config_list) ADIOI_Free(fd->hints->cb_config_list);

    /* This BlueGene platform-specific free must be done in the common code
     * because the malloc's for these hint data structures are done at the
     * scope of ADIO_Open within the SetInfo call (ADIOI_GPFS_SetInfo which
     * calls ADIOI_BG_gen_agg_ranklist).  They cannot be done in the
     * ADIOI_GPFS_Close because of the file creation case where the
     * ADIOI_GPFS_Close and re-open via ADIOI_GPFS_Open are done which results
     * in a double-free - ADIOI_GPFS_Open does not redo the SetInfo...  */
#ifdef BGQPLATFORM
    if (fd->hints && fd->hints->fs_hints.bg.bridgelist)
      ADIOI_Free(fd->hints->fs_hints.bg.bridgelist);
    if (fd->hints && fd->hints->fs_hints.bg.bridgelistnum)
      ADIOI_Free(fd->hints->fs_hints.bg.bridgelistnum);
#endif

    /* Persistent File Realms */
    if (fd->hints->cb_pfr == ADIOI_HINT_ENABLE) {
	/* AAR, FSIZE, and User provided uniform File realms */
	if (1) {
	    ADIOI_Delete_flattened (fd->file_realm_types[0]);
	    MPI_Type_free (&fd->file_realm_types[0]);
	}
	else {
	    for (i=0; i<fd->hints->cb_nodes; i++) {
		ADIOI_Datatype_iscontig(fd->file_realm_types[i], &is_contig);
		if (!is_contig)
		    ADIOI_Delete_flattened(fd->file_realm_types[i]);
		MPI_Type_free (&fd->file_realm_types[i]);
	    }
	}
	ADIOI_Free(fd->file_realm_st_offs);
	ADIOI_Free(fd->file_realm_types);
    }
    if (fd->hints) ADIOI_Free(fd->hints);



    MPI_Comm_free(&(fd->comm));
    ADIOI_Free(fd->filename); 

    MPI_Type_get_envelope(fd->etype, &i, &j, &k, &combiner);
    if (combiner != MPI_COMBINER_NAMED) MPI_Type_free(&(fd->etype));

    ADIOI_Datatype_iscontig(fd->filetype, &is_contig);
    if (!is_contig) ADIOI_Delete_flattened(fd->filetype);

    MPI_Type_get_envelope(fd->filetype, &i, &j, &k, &combiner);
    if (combiner != MPI_COMBINER_NAMED) MPI_Type_free(&(fd->filetype));

    MPI_Info_free(&(fd->info));

    if (fd->io_buf != NULL) ADIOI_Free(fd->io_buf);

    /* memory for fd is freed in MPI_File_close */
}
