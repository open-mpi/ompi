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

#ifndef OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_TSD_H
#define OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_TSD_H

#include "opal/mca/threads/argobots/threads_argobots.h"

typedef ABT_key opal_tsd_key_t;

static inline int opal_tsd_key_delete(opal_tsd_key_t key)
{
    int ret = ABT_key_free(&key);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

static inline int opal_tsd_set(opal_tsd_key_t key, void *value)
{
    opal_threads_argobots_ensure_init();
    int ret = ABT_key_set(key, value);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

static inline int opal_tsd_get(opal_tsd_key_t key, void **valuep)
{
    int ret = ABT_key_get(key, valuep);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

#endif /* OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_TSD_H */
