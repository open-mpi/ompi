/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_H
#define OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_H

#include "opal/mca/threads/thread.h"
#include <stdint.h>
#include <time.h>

typedef void(opal_threads_pthreads_yield_fn_t)(void);

OPAL_DECLSPEC int opal_threads_pthreads_yield_init(const mca_base_component_t *component);

OPAL_DECLSPEC extern opal_threads_pthreads_yield_fn_t *opal_threads_pthreads_yield_fn;

#endif /* OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_H */
