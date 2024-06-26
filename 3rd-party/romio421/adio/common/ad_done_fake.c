/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"

/* Generic implementation of ReadDone/WriteDone simply sets the
 * bytes field in the status structure and frees the request.
 *
 * Same function is used for both reads and writes.
 */
int ADIOI_FAKE_IODone(ADIO_Request * request, ADIO_Status * status, int *error_code)
{
    /* should not ever get called now */
    return 1;
}
