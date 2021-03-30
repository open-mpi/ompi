/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

/* This file currently implements these as a preprocessor if/elif/else sequence.
 * This has the upside of not doing #includes for .c files or (poorly
 * named) .i files.  It has the downside of making this file large-ish
 * and a little harder to read in some cases.  If this becomes
 * unmanagable at some point these should be separated back out into
 * header files and included as needed. [goodell@ 2009-06-24] */

/* Implementation specific function definitions (usually in the form of macros) */

#if MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_SOLARIS

/*
 * struct MPLI_thread_info
 *
 * Structure used to pass the user function and data to the intermediate function, MPLI_thread_start.  See comment in
 * MPLI_thread_start() header for more information.
 */
struct MPLI_thread_info {
    MPL_thread_func_t func;
    void *data;
};


void *MPLI_thread_start(void *arg);


/*
 * MPL_thread_create()
 */
void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * idp, int *errp)
{
    struct MPLI_thread_info *thread_info;
    int err = MPL_SUCCESS;

    /* FIXME: faster allocation, or avoid it all together? */
    thread_info =
        (struct MPLI_thread_info *) MPL_malloc(sizeof(struct MPLI_thread_info), MPL_MEM_THREAD);
    if (thread_info != NULL) {
        thread_info->func = func;
        thread_info->data = data;

        err = thr_create(NULL, 0, MPLI_thread_start, thread_info, THR_DETACHED, idp);
        /* FIXME: convert error to an MPL_THREAD_ERR value */
    } else {
        err = 1000000000;
    }

    if (errp != NULL) {
        *errp = err;
    }
}


/*
 * MPLI_thread_start()
 *
 * Start functions in Solaris threads are expected to return a void pointer.  Since our start functions do not return a value we
 * must use an intermediate function to perform call to the user's start function and then return a value of NULL.
 */
void *MPLI_thread_start(void *arg)
{
    struct MPLI_thread_info *thread_info = (struct MPLI_thread_info *) arg;
    MPL_thread_func_t func = thread_info->func;
    void *data = thread_info->data;

    MPL_free(arg);

    func(data);

    return NULL;
}

#endif
