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

#include <errno.h>
#include <pthread.h>

#include "opal/mca/threads/mutex.h"
//#include "opal/mca/threads/pthreads/mutex_unix.h"


OBJ_CLASS_INSTANCE(opal_mutex_t,
                   opal_object_t,
                   NULL,
                   NULL);
OBJ_CLASS_INSTANCE(opal_recursive_mutex_t,
                   opal_object_t,
                   NULL,
                   NULL);

/*
 * Wait and see if some upper layer wants to use threads, if support
 * exists.
 */
bool opal_uses_threads = false;

struct opal_pthread_mutex_t {
    opal_object_t super;

    pthread_mutex_t m_lock_pthread;

#if OPAL_ENABLE_DEBUG
    int m_lock_debug;
    const char *m_lock_file;
    int m_lock_line;
#endif

    opal_atomic_lock_t m_lock_atomic;
};
typedef struct opal_pthread_mutex_t opal_pthread_mutex_t;
typedef struct opal_pthread_mutex_t opal_pthread_recursive_mutex_t;

