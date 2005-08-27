/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: seek.c,v 1.8 2002/10/24 15:54:43 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_seek = PMPI_File_seek
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_seek MPI_File_seek
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_seek as PMPI_File_seek
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_seek - Updates the individual file pointer

Input Parameters:
. fh - file handle (handle)
. offset - file offset (integer)
. whence - update mode (state)

.N fortran
@*/
int MPI_File_seek(MPI_File fh, MPI_Offset offset, int whence)
{
    int error_code;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_SEEK";
#endif
    MPI_Offset curr_offset, eof_offset;
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILESEEK, TRDTBLOCK, fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_seek: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (fh->access_mode & MPI_MODE_SEQUENTIAL) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_seek: Can't use this function because file was opened with MPI_MODE_SEQUENTIAL\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_UNSUPPORTED_OPERATION, 
                        MPIR_ERR_AMODE_SEQ, myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    switch(whence) {
    case MPI_SEEK_SET:
	if (offset < 0) {
#ifdef PRINT_ERR_MSG
	    FPRINTF(stderr, "MPI_File_seek: Invalid offset argument\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_OFFSET_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
	}
	break;
    case MPI_SEEK_CUR:
	/* find offset corr. to current location of file pointer */
	ADIOI_Get_position(fh, &curr_offset);
	offset += curr_offset;
	if (offset < 0) {
#ifdef PRINT_ERR_MSG
	    FPRINTF(stderr, "MPI_File_seek: offset points to a negative location in the file\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_OFFSET_ARG_NEG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
	}
	break;
    case MPI_SEEK_END:
	/* find offset corr. to end of file */
	ADIOI_Get_eof_offset(fh, &eof_offset);
	offset += eof_offset;
	if (offset < 0) {
#ifdef PRINT_ERR_MSG
	    FPRINTF(stderr, "MPI_File_seek: offset points to a negative location in the file\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_OFFSET_ARG_NEG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
	}
	break;
    default:
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_seek: Invalid whence argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_WHENCE_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    ADIO_SeekIndividual(fh, offset, ADIO_SEEK_SET, &error_code);

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */
    return error_code;
}
