/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: iowait.c,v 1.8 2002/10/24 15:54:41 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIO_Wait = PMPIO_Wait
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIO_Wait MPIO_Wait
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIO_Wait as PMPIO_Wait
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/* status object not filled currently */

/*@
    MPIO_Wait - Waits for the completion of a nonblocking read or write

Input Parameters:
. request - request object (handle)

Output Parameters:
. status - status object (Status)

.N fortran
@*/
int MPIO_Wait(MPIO_Request *request, MPI_Status *status)
{
    int error_code;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPIO_WAIT";
#endif
#ifdef MPI_hpux
    int fl_xmpi;

    if (*request != MPIO_REQUEST_NULL) {
	HPMP_IO_WSTART(fl_xmpi, BLKMPIOWAIT, TRDTBLOCK, (*request)->fd);
    }
#endif /* MPI_hpux */

    if (*request == MPIO_REQUEST_NULL) return MPI_SUCCESS;

    if ((*request < (MPIO_Request) 0) || 
	     ((*request)->cookie != ADIOI_REQ_COOKIE)) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPIO_Wait: Invalid request object\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_REQUEST, MPIR_ERR_REQUEST_NULL,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(MPI_FILE_NULL, error_code, myname);
#endif
    }

    switch ((*request)->optype) {
    case ADIOI_READ:
        ADIO_ReadComplete(request, status, &error_code);
        break;
    case ADIOI_WRITE:
        ADIO_WriteComplete(request, status, &error_code);
        break;
    }

#ifdef MPI_hpux
    HPMP_IO_WEND(fl_xmpi);
#endif /* MPI_hpux */
    return error_code;
}
