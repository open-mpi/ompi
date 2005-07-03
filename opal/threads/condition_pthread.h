/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OMPI_CONDITION_PTHREAD_H
#define OMPI_CONDITION_PTHREAD_H

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include "opal/threads/mutex.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct opal_condition_t {
    opal_object_t super;
    pthread_cond_t c_cond;
};
typedef struct opal_condition_t opal_condition_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(opal_condition_t);


static inline int opal_condition_wait(opal_condition_t *c, opal_mutex_t *m)
{
    return pthread_cond_wait(&c->c_cond, &m->m_lock_pthread);
}

static inline int opal_condition_timedwait(opal_condition_t *c,
                                           opal_mutex_t *m,
                                           const struct timespec *abstime)
{
    return pthread_cond_timedwait(&c->c_cond, &m->m_lock_pthread, abstime);
}

static inline int opal_condition_signal(opal_condition_t *c)
{
    return pthread_cond_signal(&c->c_cond);
}

static inline int opal_condition_broadcast(opal_condition_t *c)
{
    return pthread_cond_broadcast(&c->c_cond);
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

