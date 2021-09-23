/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2021      Argonne National Laboratory.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <unistd.h>

#include "opal/constants.h"
#include "opal/mca/threads/argobots/threads_argobots.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/tsd.h"
#include "opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/util/sys_limits.h"

int opal_tsd_key_create(opal_tsd_key_t *key, opal_tsd_destructor_t destructor)
{
    opal_threads_argobots_ensure_init();
    int rc;
    rc = ABT_key_create(destructor, key);
    return (ABT_SUCCESS == rc) ? OPAL_SUCCESS : OPAL_ERROR;
}
