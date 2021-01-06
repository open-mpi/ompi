/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

#if (MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE) && !defined(MPL_TLS)

/* This routine is called when a thread exits; it is passed the value
 * associated with the key.  In our case, this is simply storage
 * allocated with MPL_calloc */
void MPLI_cleanup_tls(void *a)
{
    MPL_free(a);
}

#endif
