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
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/threads/base/base.h"

#if OPAL_ENABLE_DEBUG
bool opal_debug_threads = false;
#endif

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/threads/base/static-components.h"

static int mca_threads_base_register(mca_base_register_flag_t flags)
{
    return OPAL_SUCCESS;
}

/*
 * Globals
 */
/* Use default register/open/close functions */
MCA_BASE_FRAMEWORK_DECLARE(opal, threads, "OPAL threads", mca_threads_base_register, NULL, NULL,
                           mca_threads_base_static_components, 0);
