/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIO_Testsome = PMPIO_Testsome
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIO_Testsome MPIO_Testsome
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIO_Testsome as PMPIO_Testsome
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

int MPIO_Testsome(int count, MPIO_Request requests[], int *outcount,
                  int indices[], MPI_Status * statuses)
{
    int i, err;
    int flag;

    ROMIO_THREAD_CS_ENTER();

    if (count == 1) {
        err = MPIO_Test(requests, &flag, statuses);
        if (!err) {
            if (flag) {
                indices[0] = 0;
                *outcount = 1;
            } else {
                *outcount = 0;
            }
        }
        goto fn_exit;
    }

    /* Check for no active requests */
    for (i = 0; i < count; i++) {
        if (requests[i] != MPIO_REQUEST_NULL) {
            break;
        }
    }
    if (i == count) {
        *outcount = MPI_UNDEFINED;
        err = MPI_SUCCESS;
        goto fn_exit;
    }

    err = MPI_SUCCESS;
    *outcount = 0;
    for (i = 0; i < count; i++) {
        if (requests[i] != MPIO_REQUEST_NULL) {
            err = MPIO_Test(&requests[i], &flag, statuses);
            if (flag) {
                if (!err) {
                    indices[0] = i;
                    indices++;
                    statuses++;
                    *outcount = *outcount + 1;
                }
            }
        }
    }

  fn_exit:

    ROMIO_THREAD_CS_EXIT();
    return err;
}
