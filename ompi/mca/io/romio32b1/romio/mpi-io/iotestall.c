/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2003 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIO_Testall = PMPIO_Testall
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIO_Testall MPIO_Testall
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIO_Testall as PMPIO_Testall
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

int MPIO_Testall(int count, MPIO_Request requests[], int *flag,
		 MPI_Status statuses[])
{
    int done, i, err; 
    MPIU_THREADPRIV_DECL;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);
    if (count == 1)  {
	    err = MPIO_Test( requests, flag, statuses );
	    goto fn_exit;
    }

    /* This is actually very difficult to do.  We can't use MPIO_Test, 
       since we must change the requests only if *ALL* requests are complete
    */
    /* FIXME: THIS IS NOT CORRECT (see above).  But most applications won't 
     care */
    done = 1;
    for (i=0; i<count; i++) {
      if (requests[i] != MPIO_REQUEST_NULL) {
	err = MPIO_Test( &requests[i], flag, &statuses[i] );
	if (!*flag) done = 0;
	if (err) goto fn_exit;
      }
      else {
#ifdef MPICH
	  /* need to set empty status */
	  if (statuses != MPI_STATUSES_IGNORE) {
	      statuses[i].MPI_SOURCE = MPI_ANY_SOURCE;
	      statuses[i].MPI_TAG    = MPI_ANY_TAG;
              MPIR_STATUS_SET_COUNT(statuses[i], 0);
              MPIR_STATUS_SET_CANCEL_BIT(statuses[i], 0);
	  }
#else
	  ;
#endif
      }
    }
    
    *flag = done;

    err = MPI_SUCCESS;
fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);
    return err;
}

