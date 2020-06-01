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

#ifndef OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_THREADS_H
#define OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_THREADS_H

#include <signal.h>
#include "opal/mca/threads/argobots/threads_argobots.h"

struct opal_thread_t {
    opal_object_t super;
    opal_thread_fn_t t_run;
    void *t_arg;
    ABT_thread t_handle;
    void *t_ret;
};

#endif /* OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_THREADS_H */
