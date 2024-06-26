/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/* common header includes */
#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if defined(MPL_THREAD_PACKAGE_NAME) && (MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_ARGOBOTS)

typedef struct {
    MPL_thread_func_t func;
    void *data;
} thread_info;

static void thread_start(void *arg)
{
    thread_info *info = (thread_info *) arg;
    MPL_thread_func_t func = info->func;
    void *data = info->data;

    MPL_free(arg);

    func(data);
}

static int thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * idp)
{
    if (ABT_initialized() != ABT_SUCCESS) {
        /* Not initialized yet. */
        return MPL_ERR_THREAD;
    }

    ABT_xstream xstream;
    if (ABT_xstream_self(&xstream) != ABT_SUCCESS) {
        /* Not executed on an execution stream. */
        return MPL_ERR_THREAD;
    }

    ABT_pool pool;
    if (ABT_xstream_get_main_pools(xstream, 1, &pool) != ABT_SUCCESS) {
        return MPL_ERR_THREAD;
    }

    thread_info *info = (thread_info *) MPL_malloc(sizeof(thread_info), MPL_MEM_THREAD);
    if (info == NULL) {
        return MPL_ERR_THREAD;
    }

    info->func = func;
    info->data = data;
    /* Push this thread to the current ES's first pool. */
    int err = ABT_thread_create(pool, thread_start, info, ABT_THREAD_ATTR_NULL, (ABT_thread *) idp);
    /* We assume that the last bit of MPL_thread_id_t is 0 if it points to an
     * Argobots thread.  Let's check it. */
    assert(!(*idp & (MPL_thread_id_t) 0x1));
    return (err == ABT_SUCCESS) ? MPL_SUCCESS : MPL_ERR_THREAD;
}

/*
 * MPL_thread_create()
 */
void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * idp, int *errp)
{
    int err = thread_create(func, data, idp);

    if (errp != NULL) {
        *errp = err;
    }
}

void MPL_thread_set_affinity(MPL_thread_id_t thread, int *affinity_arr, int affinity_size, int *err)
{
    /* stub implementation */
    if (err)
        *err = MPL_ERR_THREAD;
}

#endif
