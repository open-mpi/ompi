/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_Info_f2c = PMPI_Info_f2c
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_Info_f2c MPI_Info_f2c
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_Info_f2c as PMPI_Info_f2c
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif
#include "adio_extern.h"

/*@
    MPI_Info_f2c - Translates a Fortran info handle to a C info handle

Input Parameters:
. info - Fortran info handle (integer)

Return Value:
  C info handle (handle)
@*/
MPI_Info MPI_Info_f2c(MPI_Fint info)
{

#ifndef INT_LT_POINTER
    return (MPI_Info) info;
#else
    if (!info) return MPI_INFO_NULL;
    if ((info < 0) || (info > MPIR_Infotable_ptr)) {
	FPRINTF(stderr, "MPI_Info_f2c: Invalid info handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    return MPIR_Infotable[info];
#endif
}
