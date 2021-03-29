/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef OPAL_MCA_THREADS_BASE_BASE_H
#define OPAL_MCA_THREADS_BASE_BASE_H

#include "opal_config.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/threads/threads.h"

/*
 * Global functions for MCA overall threads open and close
 */

BEGIN_C_DECLS

/**
 * Framework structure declaration
 */
OPAL_DECLSPEC extern mca_base_framework_t opal_threads_base_framework;

END_C_DECLS

/* include implementation to call */
#include MCA_threads_base_include_HEADER

#endif /* OPAL_MCA_THREADS_BASE_BASE_H */
