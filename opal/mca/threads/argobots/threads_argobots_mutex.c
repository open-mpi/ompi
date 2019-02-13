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
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal/mca/threads/argobots/threads_argobots.h"
#include "opal_config.h"

#include <errno.h>
#include <pthread.h>

#include "opal/mca/threads/mutex.h"
#include "opal/mca/threads/argobots/threads_argobots_mutex.h"

/*
 * Wait and see if some upper layer wants to use threads, if support
 * exists.
 */
bool opal_uses_threads = false;

static void mca_threads_argobots_mutex_constructor(opal_mutex_t *p_mutex) {
    ensure_init_argobots();
    p_mutex->m_lock_argobots = OPAL_ABT_MUTEX_NULL;
    p_mutex->m_recursive = 0;
#if OPAL_ENABLE_DEBUG
    p_mutex->m_lock_debug = 0;
    p_mutex->m_lock_file = NULL;
    p_mutex->m_lock_line = 0;
#endif
    opal_atomic_lock_init(&p_mutex->m_lock_atomic, 0);
}

static void mca_threads_argobots_mutex_desctructor(opal_mutex_t *p_mutex) {
    ensure_init_argobots();
    if (p_mutex->m_lock_argobots != OPAL_ABT_MUTEX_NULL)
        ABT_mutex_free(&p_mutex->m_lock_argobots);
}

static void mca_threads_argobots_recursive_mutex_constructor
        (opal_recursive_mutex_t *p_mutex) {
    ensure_init_argobots();
    p_mutex->m_lock_argobots = OPAL_ABT_MUTEX_NULL;
    p_mutex->m_recursive = 1;
#if OPAL_ENABLE_DEBUG
    p_mutex->m_lock_debug = 0;
    p_mutex->m_lock_file = NULL;
    p_mutex->m_lock_line = 0;
#endif
    opal_atomic_lock_init(&p_mutex->m_lock_atomic, 0);
}

static void mca_threads_argobots_recursive_mutex_desctructor
        (opal_recursive_mutex_t *p_mutex) {
    ensure_init_argobots();
    if (p_mutex->m_lock_argobots != OPAL_ABT_MUTEX_NULL)
        ABT_mutex_free(&p_mutex->m_lock_argobots);
}

OBJ_CLASS_INSTANCE(opal_mutex_t,
                   opal_object_t,
                   mca_threads_argobots_mutex_constructor,
                   mca_threads_argobots_mutex_desctructor);
OBJ_CLASS_INSTANCE(opal_recursive_mutex_t,
                   opal_object_t,
                   mca_threads_argobots_recursive_mutex_constructor,
                   mca_threads_argobots_recursive_mutex_desctructor);
