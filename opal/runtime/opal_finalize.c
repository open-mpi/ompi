/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "opal_config.h"

#include "opal/class/opal_object.h"
#include "opal/constants.h"
#include "opal/mca/threads/tsd.h"
#include "opal/memoryhooks/memory.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

extern int opal_initialized;

int opal_finalize(void)
{
    if (--opal_initialized != 0) {
        if (opal_initialized < 0) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }

    opal_finalize_cleanup_domain(&opal_init_domain);
    OBJ_DESTRUCT(&opal_init_domain);

    /* finalize libevent code */
    opal_event_finalize();

    /* finalize util code */
    opal_finalize_util();

    return OPAL_SUCCESS;
}

static bool fork_warning_issued = false;
static bool atfork_called = false;

static void warn_fork_cb(void)
{
    if (opal_initialized && !fork_warning_issued) {
        opal_show_help("help-opal-runtime.txt", "opal_init:warn-fork", true,
                       OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), getpid());
        fork_warning_issued = true;
    }
}

void opal_warn_fork(void)
{
    if (opal_warn_on_fork && !atfork_called) {
        pthread_atfork(warn_fork_cb, NULL, NULL);
        atfork_called = true;
    }
}
