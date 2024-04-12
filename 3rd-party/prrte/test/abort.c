/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of applications
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "pmix.h"

int main(int argc, char *argv[])
{
    int size;
    int errcode;
    pmix_proc_t myproc;
    pmix_status_t rc;
    pmix_proc_t proc;
    pmix_value_t *val = NULL;

    if (1 < argc) {
        errcode = strtol(argv[1], NULL, 10);
    } else {
        errcode = 2;
    }

    rc = PMIx_Init(&myproc, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client: PMIx_Init failed: %s\n", PMIx_Error_string(rc));
        exit(errcode);
    }

    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    PMIX_VALUE_GET_NUMBER(rc, val, size, int);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: get size number failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    PMIX_VALUE_RELEASE(val);

    printf("Hello, World, I am %d of %d\n", myproc.rank, size);

    if (1 == size) {
        PMIx_Abort(errcode, "Aborting", NULL, 0);
    } else {
        if (1 == myproc.rank) {
            PMIx_Abort(errcode, "Aborting", NULL, 0);
        } else {
            errcode = 0;
            sleep(99999999);
        }
    }

done:
    PMIx_Finalize(NULL, 0);
    return errcode;
}
