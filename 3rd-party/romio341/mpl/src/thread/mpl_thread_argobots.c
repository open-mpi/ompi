/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/* common header includes */
#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if defined(MPL_THREAD_PACKAGE_NAME) && (MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_ARGOBOTS)
/* begin argobots impl */

void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * idp, int *errp)
{
    /* not used */
    return;
}

/* end argobots impl */
#endif
