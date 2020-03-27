/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include <unistd.h>

#include "opal/constants.h"
#include "opal/util/sys_limits.h"
#include "opal/util/output.h"
#include "opal/prefetch.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/tsd.h"

struct opal_tsd_key_value {
    opal_tsd_key_t key;
    opal_tsd_destructor_t destructor;
};

static opal_mutex_t opal_tsd_lock = OPAL_MUTEX_STATIC_INIT;
static struct opal_tsd_key_value *opal_tsd_key_values = NULL;
static int opal_tsd_key_values_count = 0;
static int opal_tsd_key_values_size = 0;

/*
 * Constructor
 */
static void opal_thread_construct(opal_thread_t *t)
{
    t->t_run = 0;
    t->t_handle = (pthread_t)-1;
}

OBJ_CLASS_INSTANCE(opal_thread_t,
                   opal_object_t,
                   opal_thread_construct, NULL);

int opal_thread_start(opal_thread_t *t)
{
    int rc;

    if (OPAL_ENABLE_DEBUG) {
        if (NULL == t->t_run || (pthread_t)-1 != t->t_handle) {
            return OPAL_ERR_BAD_PARAM;
        }
    }

    rc = pthread_create(&t->t_handle, NULL, (void *(*)(void *))t->t_run, t);

    return 0 == rc ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
}

int opal_thread_join(opal_thread_t *t, void **thr_return)
{
    int rc = pthread_join(t->t_handle, thr_return);
    t->t_handle = (pthread_t)-1;
    return 0 == rc ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
}

bool opal_thread_self_compare(opal_thread_t *t)
{
    return pthread_self() == t->t_handle;
}

opal_thread_t *opal_thread_get_self(void)
{
    opal_thread_t *t = OBJ_NEW(opal_thread_t);
    t->t_handle = pthread_self();
    return t;
}

int opal_tsd_key_create(opal_tsd_key_t *key, opal_tsd_destructor_t destructor)
{
    int rc;
    rc = pthread_key_create(key, destructor);
    if (0 == rc) {
        opal_mutex_lock(&opal_tsd_lock);
        if (opal_tsd_key_values_size <= opal_tsd_key_values_count) {
            opal_tsd_key_values_size = opal_tsd_key_values_size == 0
                                       ? 1 : opal_tsd_key_values_size * 2;
            opal_tsd_key_values = (struct opal_tsd_key_value *)
                realloc(opal_tsd_key_values, opal_tsd_key_values_size *
                                             sizeof(struct opal_tsd_key_value));
        }
        opal_tsd_key_values[opal_tsd_key_values_count].key = *key;
        opal_tsd_key_values[opal_tsd_key_values_count].destructor = destructor;
        opal_tsd_key_values_count++;
        opal_mutex_unlock(&opal_tsd_lock);
    }
    return 0 == rc ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
}

int opal_tsd_keys_destruct(void)
{
    int i;
    void *ptr;
    opal_mutex_lock(&opal_tsd_lock);
    for (i = 0; i < opal_tsd_key_values_count; i++) {
        if (OPAL_SUCCESS ==
            opal_tsd_getspecific(opal_tsd_key_values[i].key, &ptr)) {
            if (NULL != opal_tsd_key_values[i].destructor) {
                opal_tsd_key_values[i].destructor(ptr);
                opal_tsd_setspecific(opal_tsd_key_values[i].key, NULL);
            }
        }
    }
    if (0 < opal_tsd_key_values_count) {
        free(opal_tsd_key_values);
        opal_tsd_key_values = NULL;
        opal_tsd_key_values_count = 0;
        opal_tsd_key_values_size = 0;
    }
    opal_mutex_unlock(&opal_tsd_lock);
    return OPAL_SUCCESS;
}

void opal_thread_set_main(void)
{
}
