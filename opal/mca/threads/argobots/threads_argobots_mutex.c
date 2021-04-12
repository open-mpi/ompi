/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2021      Argonne National Laboratory.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/mca/threads/argobots/threads_argobots.h"

#include <errno.h>
#include <string.h>

#include "opal/constants.h"
#include "opal/mca/threads/argobots/threads_argobots_mutex.h"
#include "opal/mca/threads/mutex.h"

/*
 * Wait and see if some upper layer wants to use threads, if support
 * exists.
 */
bool opal_uses_threads = false;

static void mca_threads_argobots_mutex_constructor(opal_mutex_t *p_mutex)
{
    opal_threads_argobots_ensure_init();
    const ABT_mutex_memory init_mutex = ABT_MUTEX_INITIALIZER;
    memcpy(&p_mutex->m_lock_argobots, &init_mutex, sizeof(ABT_mutex_memory));
    p_mutex->m_recursive = 0;
#if OPAL_ENABLE_DEBUG
    p_mutex->m_lock_debug = 0;
    p_mutex->m_lock_file = NULL;
    p_mutex->m_lock_line = 0;
#endif
    opal_atomic_lock_init(&p_mutex->m_lock_atomic, 0);
}

static void mca_threads_argobots_recursive_mutex_constructor(opal_recursive_mutex_t *p_mutex)
{
    opal_threads_argobots_ensure_init();
    const ABT_mutex_memory init_mutex = ABT_RECURSIVE_MUTEX_INITIALIZER;
    memcpy(&p_mutex->m_lock_argobots, &init_mutex, sizeof(ABT_mutex_memory));
    p_mutex->m_recursive = 1;
#if OPAL_ENABLE_DEBUG
    p_mutex->m_lock_debug = 0;
    p_mutex->m_lock_file = NULL;
    p_mutex->m_lock_line = 0;
#endif
    opal_atomic_lock_init(&p_mutex->m_lock_atomic, 0);
}

OBJ_CLASS_INSTANCE(opal_mutex_t, opal_object_t, mca_threads_argobots_mutex_constructor, NULL);
OBJ_CLASS_INSTANCE(opal_recursive_mutex_t, opal_object_t,
                   mca_threads_argobots_recursive_mutex_constructor, NULL);

int opal_cond_init(opal_cond_t *cond)
{
    const ABT_cond_memory init_cond = ABT_COND_INITIALIZER;
    memcpy(cond, &init_cond, sizeof(ABT_cond_memory));
    return OPAL_SUCCESS;
}

int opal_cond_wait(opal_cond_t *cond, opal_mutex_t *lock)
{
    ABT_mutex abt_mutex = ABT_MUTEX_MEMORY_GET_HANDLE(&lock->m_lock_argobots);
    ABT_cond abt_cond = ABT_COND_MEMORY_GET_HANDLE(cond);
    int ret = ABT_cond_wait(abt_cond, abt_mutex);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

int opal_cond_broadcast(opal_cond_t *cond)
{
    ABT_cond abt_cond = ABT_COND_MEMORY_GET_HANDLE(cond);
    int ret = ABT_cond_broadcast(abt_cond);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

int opal_cond_signal(opal_cond_t *cond)
{
    ABT_cond abt_cond = ABT_COND_MEMORY_GET_HANDLE(cond);
    int ret = ABT_cond_signal(abt_cond);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

int opal_cond_destroy(opal_cond_t *cond)
{
    return OPAL_SUCCESS;
}
