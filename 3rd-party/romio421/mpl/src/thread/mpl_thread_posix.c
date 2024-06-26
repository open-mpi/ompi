/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"
#include <sched.h>

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

/* This file currently implements these as a preprocessor if/elif/else sequence.
 * This has the upside of not doing #includes for .c files or (poorly
 * named) .i files.  It has the downside of making this file large-ish
 * and a little harder to read in some cases.  If this becomes
 * unmanagable at some point these should be separated back out into
 * header files and included as needed. [goodell@ 2009-06-24] */

/* Implementation specific function definitions (usually in the form of macros) */
#if MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_POSIX

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
    if (thread_info != NULL) {

        thread_info->func = func;
        thread_info->data = data;

        err = pthread_create(idp, NULL, MPLI_thread_start, thread_info);
        /* FIXME: convert error to an MPL_ERR_THREAD value */
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

void MPL_thread_set_affinity(MPL_thread_id_t thread, int *affinity_arr, int affinity_size,
                             int *errp)
{
#if defined(MPL_HAVE_PTHREAD_SETAFFINITY_NP) && defined(MPL_HAVE_CPU_SET_MACROS) && defined(__linux__)
    /* FIXME: this implementation uses Linux-specific types and macros */
    int err = MPL_SUCCESS;
    int proc_idx, set_size = 0;
    cpu_set_t cpuset;
    CPU_ZERO_S(sizeof(cpu_set_t), &cpuset);

    for (proc_idx = 0; proc_idx < affinity_size; proc_idx++)
        CPU_SET_S(affinity_arr[proc_idx], sizeof(cpu_set_t), &cpuset);

    if (pthread_setaffinity_np(thread, sizeof(cpu_set_t), &cpuset) != 0) {
        err = MPL_ERR_THREAD;
        goto fn_exit;
    }

    if (pthread_getaffinity_np(thread, sizeof(cpu_set_t), &cpuset) != 0) {
        err = MPL_ERR_THREAD;
        goto fn_exit;
    }

    for (proc_idx = 0; proc_idx < affinity_size; proc_idx++) {
        if (CPU_ISSET_S(affinity_arr[proc_idx], sizeof(cpu_set_t), &cpuset))
            set_size++;
    }
    if (set_size != affinity_size) {
        err = MPL_ERR_THREAD;
        goto fn_exit;
    }

  fn_exit:
    if (errp != NULL)
        *errp = err;
#else
    /* stub implementation */
    if (errp)
        *errp = MPL_ERR_THREAD;
#endif
}

#endif
