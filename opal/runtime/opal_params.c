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
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <signal.h>
#include <time.h>

#include "opal/constants.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/threads/mutex.h"
#include "opal/mca/threads/threads.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_params.h"
#include "opal/util/opal_environ.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"
#include "opal/util/timings.h"


static bool opal_register_done = false;

static void opal_deregister_params(void)
{
    opal_register_done = false;
}

int opal_register_params(void)
{
    if (opal_register_done) {
        return OPAL_SUCCESS;
    }

    opal_register_done = true;

#if defined(HAVE_SCHED_YIELD)
    opal_progress_yield_when_idle = false;
    int ret1;
    ret1 = mca_base_var_register("opal", "opal", "progress", "yield_when_idle",
                                "Yield the processor when waiting on progress",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                OPAL_INFO_LVL_8, MCA_BASE_VAR_SCOPE_LOCAL,
                                &opal_progress_yield_when_idle);
    if (ret1 < 0) {
        return ret1;
    }
#endif

#if OPAL_ENABLE_DEBUG
    opal_progress_debug = false;
    int ret;
    ret = mca_base_var_register("opal", "opal", "progress", "debug",
                                "Set to non-zero to debug progress engine features",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                OPAL_INFO_LVL_8, MCA_BASE_VAR_SCOPE_LOCAL, &opal_progress_debug);
    if (0 > ret) {
        return ret;
    }
#endif

    opal_finalize_register_cleanup(opal_deregister_params);

    return OPAL_SUCCESS;
}
