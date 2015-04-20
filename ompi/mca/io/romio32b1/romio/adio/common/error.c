/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

/* NOTE: THIS FUNCTION IS DEPRECATED AND ONLY EXISTS HERE BECAUSE
 * SOME DEPRECATED ADIO IMPLEMENTATIONS STILL CALL IT (SFS, HFS, PIOFS).
 */
int ADIOI_Error(ADIO_File fd, int error_code, char *string)
{
    char buf[MPI_MAX_ERROR_STRING];
    int myrank, result_len; 
    MPI_Errhandler err_handler;

    if (fd == ADIO_FILE_NULL) err_handler = ADIOI_DFLT_ERR_HANDLER;
    else err_handler = fd->err_handler;

    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    if (err_handler == MPI_ERRORS_ARE_FATAL) {
	MPI_Error_string(error_code, buf, &result_len);
	FPRINTF(stderr, "[%d] - %s : %s\n", myrank, string, buf);
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    else if (err_handler != MPI_ERRORS_RETURN) {
	/* MPI_File_call_errorhandler(fd, error_code); */

	FPRINTF(stderr, "Only MPI_ERRORS_RETURN and MPI_ERRORS_ARE_FATAL are currently supported as error handlers for files\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    return error_code;
}
/* Check for special error codes for those MPI error
   classes that relate to particular problems.
   Returns an MPI error code corresponding to "my_errno", for function "myname"
 * and the given file, "filename".  */
int ADIOI_Err_create_code(const char *myname, const char *filename, int my_errno)
{
    int error_code = MPI_SUCCESS;
    if(!my_errno) return MPI_SUCCESS;

    /* --BEGIN ERROR HANDLING-- */
    switch(my_errno) {
        case EACCES:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_ACCESS,
                                              "**fileaccess",
                                              "**fileaccess %s",
                                              filename );
            break;
        case ENAMETOOLONG:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_BAD_FILE,
                                              "**filenamelong",
                                              "**filenamelong %s %d",
                                              filename,
                                              strlen(filename));
            break;
        case ENOENT:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_NO_SUCH_FILE,
                                              "**filenoexist",
                                              "**filenoexist %s",
                                              filename);
            break;
        case EISDIR:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE,
                                              myname, __LINE__,
                                              MPI_ERR_BAD_FILE,
                                              "**filenamedir",
                                              "**filenamedir %s",
                                              filename);
            break;
        case EROFS:
	    /* Read only file or file system and write access requested */
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_READ_ONLY,
                                              "**ioneedrd", 0 );
            break;
        case EEXIST:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_FILE_EXISTS,
                                              "**fileexist", 0);
            break;
	case ENOTDIR:
	case ELOOP:
	    error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE,
					       myname, __LINE__,
					       MPI_ERR_BAD_FILE,
					       "**filenamedir",
					       "**filenamedir %s",
					       filename);
            break;
	case ENOSPC:
	    error_code = MPIO_Err_create_code(MPI_SUCCESS,
		    MPIR_ERR_RECOVERABLE, myname, __LINE__,
		    MPI_ERR_NO_SPACE,
		    "**filenospace", 0);
	    break;
	case EDQUOT:
	    error_code = MPIO_Err_create_code(MPI_SUCCESS,
		    MPIR_ERR_RECOVERABLE, myname, __LINE__,
		    MPI_ERR_QUOTA,
		    "**filequota", 0);

        default:
            error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                              myname, __LINE__, MPI_ERR_IO, "**io",
                                              "**io %s", strerror(my_errno));
            break;
    }
    /* --END ERROR HANDLING-- */

    return error_code;
}
