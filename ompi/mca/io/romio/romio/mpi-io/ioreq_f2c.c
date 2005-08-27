/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ioreq_f2c.c,v 1.8 2002/10/24 15:54:40 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIO_Request_f2c = PMPIO_Request_f2c
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIO_Request_f2c MPIO_Request_f2c
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIO_Request_f2c as PMPIO_Request_f2c
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif
#include "adio_extern.h"

/*@
    MPIO_Request_f2c - Translates a Fortran I/O-request handle to 
                       a C I/O-request handle

Input Parameters:
. request - Fortran I/O-request handle (integer)

Return Value:
  C I/O-request handle (handle)
@*/
MPIO_Request MPIO_Request_f2c(MPI_Fint request)
{

#ifndef INT_LT_POINTER
    return (MPIO_Request) request;
#else
    if (!request) return MPIO_REQUEST_NULL;
    if ((request < 0) || (request > ADIOI_Reqtable_ptr)) {
	FPRINTF(stderr, "MPIO_Request_f2c: Invalid request\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    return ADIOI_Reqtable[request];
#endif
}
