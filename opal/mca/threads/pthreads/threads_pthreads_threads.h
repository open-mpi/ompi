/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2020 High Performance Computing Center Stuttgart,
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

#ifndef OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_THREADS_H
#define OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_THREADS_H

#include <pthread.h>
#include <signal.h>

#include "opal/mca/threads/pthreads/threads_pthreads.h"
#include "opal/mca/threads/threads.h"

/* Pthreads do not need to yield when idle */
#define OPAL_THREAD_YIELD_WHEN_IDLE_DEFAULT false

static inline void opal_thread_yield(void)
{
    opal_threads_pthreads_yield_fn();
}

#endif /* OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_THREADS_H */
