/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* needed for pthread_mutexattr_settype */
#define _GNU_SOURCE

#include <unistd.h>
#include <pthread.h>

#include "opal/constants.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/tsd.h"
#include "opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/util/sys_limits.h"

int opal_tsd_key_create(opal_tsd_key_t *key, opal_tsd_destructor_t destructor)
{
    int rc;
    rc = pthread_key_create(key, destructor);
    return 0 == rc ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
}


int opal_thread_internal_mutex_init_recursive(opal_thread_internal_mutex_t *p_mutex)
{
    int ret;
#if OPAL_ENABLE_DEBUG
    pthread_mutexattr_t mutex_attr;
    ret = pthread_mutexattr_init(&mutex_attr);
    if (0 != ret)
        return OPAL_ERR_IN_ERRNO;
    ret = pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_RECURSIVE);
    if (0 != ret) {
        ret = pthread_mutexattr_destroy(&mutex_attr);
        assert(0 == ret);
        return OPAL_ERR_IN_ERRNO;
    }
    ret = pthread_mutex_init(p_mutex, &mutex_attr);
    if (0 != ret) {
        ret = pthread_mutexattr_destroy(&mutex_attr);
        assert(0 == ret);
        return OPAL_ERR_IN_ERRNO;
    }
    ret = pthread_mutexattr_destroy(&mutex_attr);
    assert(0 == ret);
#else
    pthread_mutexattr_t mutex_attr;
    ret = pthread_mutexattr_init(&mutex_attr);
    if (0 != ret) {
        return OPAL_ERR_IN_ERRNO;
    }
    pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_RECURSIVE);
    ret = pthread_mutex_init(p_mutex, &mutex_attr);
    pthread_mutexattr_destroy(&mutex_attr);
#endif
    return 0 == ret ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
}
