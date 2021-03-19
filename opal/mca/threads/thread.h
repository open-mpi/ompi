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
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_THREADS_THREAD_H
#define OPAL_MCA_THREADS_THREAD_H

#include "opal_config.h"

#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"

/**
 * Structure for threads components.
 */
struct opal_threads_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t threadsc_version;
    /** MCA base data */
    mca_base_component_data_t threadsc_data;
};
/**
 * Convenience typedef
 */
typedef struct opal_threads_base_component_1_0_0_t opal_threads_base_component_1_0_0_t;

/*
 * Macro for use in components that are of type threads
 */
#define OPAL_THREADS_BASE_VERSION_1_0_0 OPAL_MCA_BASE_VERSION_2_1_0("threads", 1, 0, 0)

#endif /* OPAL_MCA_THREADS_THREAD_H */
