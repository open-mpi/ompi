/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_close.c,v 1.5 2002/10/24 17:01:11 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#ifdef MPISGI
#include "mpisgi2.h"
#endif

void ADIO_Close(ADIO_File fd, int *error_code)
{
    int i, j, k, combiner, myrank, err, is_contig;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIO_CLOSE";
#endif

    if (fd->async_count) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_Close: Error! There are outstanding nonblocking I/O operations on this file.\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_ASYNC_OUTSTANDING,
				     myname, (char *) 0, (char *) 0);
	ADIOI_Error(fd, *error_code, myname);
        return;
#endif
    }

    (*(fd->fns->ADIOI_xxx_Close))(fd, error_code);

    if (fd->access_mode & ADIO_DELETE_ON_CLOSE) {
	MPI_Comm_rank(fd->comm, &myrank);
	MPI_Barrier(fd->comm);
	if (!myrank) ADIO_Delete(fd->filename, &err);
    }

    ADIOI_Free(fd->fns);
    MPI_Comm_free(&(fd->comm));
    free(fd->filename);  /* should not use ADIOI_Free here, because
                            it was strdup'ed */

    MPI_Type_get_envelope(fd->etype, &i, &j, &k, &combiner);
    if (combiner != MPI_COMBINER_NAMED) MPI_Type_free(&(fd->etype));

    ADIOI_Datatype_iscontig(fd->filetype, &is_contig);
    if (!is_contig) ADIOI_Delete_flattened(fd->filetype);

    MPI_Type_get_envelope(fd->filetype, &i, &j, &k, &combiner);
    if (combiner != MPI_COMBINER_NAMED) MPI_Type_free(&(fd->filetype));

    MPI_Info_free(&(fd->info));

    ADIOI_Free(fd);
}
