/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2003 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIO_Testany = PMPIO_Testany
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIO_Testany MPIO_Testany
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIO_Testany as PMPIO_Testany
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*
  This is a temporary function until we switch to using MPI-2's generalized
  requests.
*/

int MPIO_Testany(int count, MPIO_Request requests[], int *index, 
		 int *flag, MPI_Status *status)
{
    int i, err; 
    MPIU_THREADPRIV_DECL;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    if (count == 1) {
	err = MPIO_Test( requests, flag, status );
	if (!err) *index = 0;
	goto fn_exit;
    }

    /* Check for no active requests */
    for (i=0; i<count; i++) {
	if (requests[i] != MPIO_REQUEST_NULL) {
	    break;
	}
    }
    if (i == count) {
	*index = MPI_UNDEFINED;
#ifdef MPICH
	/* need to set empty status */
	if (status != MPI_STATUS_IGNORE) {
	    status->MPI_SOURCE = MPI_ANY_SOURCE;
	    status->MPI_TAG    = MPI_ANY_TAG;
	    status->count      = 0;
	    status->cancelled  = 0;
	}
#endif
	err = MPI_SUCCESS;
	goto fn_exit;
    }

    err = MPI_SUCCESS;
    for (i=0; i<count; i++) {
      if (requests[i] != MPIO_REQUEST_NULL) {
	err = MPIO_Test( &requests[i], flag, status );
	if (*flag) {
	  if (!err) *index = i;
	  break;
	}
      }
    }


fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);
    return err;
}
