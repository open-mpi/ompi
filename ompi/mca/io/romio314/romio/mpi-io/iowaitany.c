/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2003 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIO_Waitany = PMPIO_Waitany
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIO_Waitany MPIO_Waitany
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIO_Waitany as PMPIO_Waitany
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

int MPIO_Waitany(int count, MPIO_Request requests[], int *index, 
		 MPI_Status *status)
{
    int i, flag, err; 
    MPIU_THREADPRIV_DECL;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    if (count == 1) {
	err = MPIO_Wait( requests, status );
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
            MPIR_STATUS_SET_COUNT(*status, 0);
            MPIR_STATUS_SET_CANCEL_BIT(*status, 0);
	}
#endif
	err = MPI_SUCCESS;
	goto fn_exit;
    }

    err = MPI_SUCCESS;
    do {
	flag = 0;
	for (i=0; i<count; i++) {
	    if (requests[i] != MPIO_REQUEST_NULL) {
		err = MPIO_Test( &requests[i], &flag, status );
		if (flag) {
		    if (!err) *index = i;
		    break;
		}
	    }
	}
    } while (flag == 0);

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return err;
}
