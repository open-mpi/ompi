/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: iowaitf.c,v 1.13 2002/10/24 17:01:21 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "mpio.h"

#if defined(MPIO_BUILD_PROFILING) || defined(HAVE_WEAK_SYMBOLS)

#if defined(HAVE_WEAK_SYMBOLS)
#if defined(HAVE_PRAGMA_WEAK)
#if defined(FORTRANCAPS)
#pragma weak MPIO_WAIT = PMPIO_WAIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma weak mpio_wait__ = pmpio_wait__
#elif !defined(FORTRANUNDERSCORE)
#pragma weak mpio_wait = pmpio_wait
#else
#pragma weak mpio_wait_ = pmpio_wait_
#endif

#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#if defined(FORTRANCAPS)
#pragma _HP_SECONDARY_DEF PMPIO_WAIT MPIO_WAIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma _HP_SECONDARY_DEF pmpio_wait__ mpio_wait__
#elif !defined(FORTRANUNDERSCORE)
#pragma _HP_SECONDARY_DEF pmpio_wait mpio_wait
#else
#pragma _HP_SECONDARY_DEF pmpio_wait_ mpio_wait_
#endif

#elif defined(HAVE_PRAGMA_CRI_DUP)
#if defined(FORTRANCAPS)
#pragma _CRI duplicate MPIO_WAIT as PMPIO_WAIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma _CRI duplicate mpio_wait__ as pmpio_wait__
#elif !defined(FORTRANUNDERSCORE)
#pragma _CRI duplicate mpio_wait as pmpio_wait
#else
#pragma _CRI duplicate mpio_wait_ as pmpio_wait_
#endif

/* end of weak pragmas */
#endif
/* Include mapping from MPI->PMPI */
#include "mpioprof.h"
#endif

#ifdef FORTRANCAPS
#define mpio_wait_ PMPIO_WAIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define mpio_wait_ pmpio_wait__
#elif !defined(FORTRANUNDERSCORE)
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF pmpio_wait pmpio_wait_
#endif
#define mpio_wait_ pmpio_wait
#else
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF pmpio_wait_ pmpio_wait
#endif
#define mpio_wait_ pmpio_wait_
#endif

#else

#ifdef FORTRANCAPS
#define mpio_wait_ MPIO_WAIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define mpio_wait_ mpio_wait__
#elif !defined(FORTRANUNDERSCORE)
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF mpio_wait mpio_wait_
#endif
#define mpio_wait_ mpio_wait
#else
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF mpio_wait_ mpio_wait
#endif
#endif
#endif

/* Prototype to keep compiler happy */
FORTRAN_API void FORT_CALL mpio_wait_(MPI_Fint *request,MPI_Status *status, int *ierr );

FORTRAN_API void FORT_CALL mpio_wait_(MPI_Fint *request,MPI_Status *status, int *ierr )
{
    MPIO_Request req_c;
    
    req_c = MPIO_Request_f2c(*request);
    *ierr = MPIO_Wait(&req_c, status);
    *request = MPIO_Request_c2f(req_c);
}
