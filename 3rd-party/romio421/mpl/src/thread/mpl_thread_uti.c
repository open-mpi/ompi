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
#if MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_UTI

#include "uti.h"

/*
 * struct MPLI_thread_info
 *
 * Structure used to pass the user function and data to the intermediate
 * function, MPLI_thread_start.  See comment in
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

    if (thread_info == NULL) {
        err = 1000000000;
        goto uti_exit;
    }

    thread_info->func = func;
    thread_info->data = data;

    uti_attr_t uti_attr;
    err = uti_attr_init(&uti_attr);
    if (err) {
        goto uti_exit;
    }

    /* Give a hint that it's beneficial to put the thread
     * on the same NUMA-node as the creator */
    err = UTI_ATTR_SAME_NUMA_DOMAIN(&uti_attr);
    if (err) {
        goto uti_destroy_and_exit;
    }

    /* Give a hint that the thread repeatedly monitors a device
     * using CPU. */
    err = UTI_ATTR_CPU_INTENSIVE(&uti_attr);
    if (err) {
        goto uti_destroy_and_exit;
    }

    err = uti_pthread_create(idp, NULL, MPLI_thread_start, thread_info, &uti_attr);
    if (err) {
        goto uti_destroy_and_exit;
    }

  uti_exit:
    if (errp != NULL) {
        *errp = err;
    }
    return;
  uti_destroy_and_exit:
    err = uti_attr_destroy(&uti_attr);
    goto uti_exit;
}


/*
 * MPLI_thread_start()
 *
 * Start functions in pthreads are expected to return a void pointer.  Since
 * our start functions do not return a value we must
 * use an intermediate function to perform call to the user's start function
 * and then return a value of NULL.
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
