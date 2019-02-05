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

#include "opal_config.h"

#include "opal/mca/threads/mutex.h"
#include "opal/constants.h"

/*
 * Wait and see if some upper layer wants to use threads, if support
 * exists.
 */
bool opal_uses_threads = false;

static void opal_mutex_construct(opal_mutex_t *m)
{
}

static void opal_mutex_destruct(opal_mutex_t *m)
{
}

OBJ_CLASS_INSTANCE(opal_mutex_t,
                   opal_object_t,
                   opal_mutex_construct,
                   opal_mutex_destruct);

static void opal_recursive_mutex_construct(opal_recursive_mutex_t *m)
{
}

OBJ_CLASS_INSTANCE(opal_recursive_mutex_t,
                   opal_object_t,
                   opal_recursive_mutex_construct,
                   opal_mutex_destruct);

static void opal_cond_create(opal_cond_t *cond)
{
    ensure_init_qthreads();
}

int opal_cond_init(opal_cond_t *cond)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

int opal_cond_wait(opal_cond_t *cond, opal_mutex_t *lock)
{
    ensure_init_qthreads();
    return 0 == qthread_readFE(*cond, &lock->m_lock_qthreads) ? OPAL_SUCCESS
                                                              : OPAL_ERROR;
}

int opal_cond_broadcast(opal_cond_t *cond)
{
    ensure_init_qthreads();
    return 0 == qthread_fill(*cond) ? OPAL_SUCCESS : OPAL_ERROR;
}

int opal_cond_signal(opal_cond_t *cond)
{
    ensure_init_qthreads();
    return 0 == qthread_fill(*cond) ? OPAL_SUCCESS : OPAL_ERROR;
}

int opal_cond_destroy(opal_cond_t *cond)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}
