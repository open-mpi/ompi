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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
 *                         All Rights reserved.
 * Copyright (c) 2018      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018-2019 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "opal/include/opal_config.h"

#include "opal/datatype/opal_datatype.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/if/base/base.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/memcpy/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/patcher/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/mca/reachable/base/base.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/smsc/base/base.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/tsd.h"
#include "opal/mca/timer/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "opal/runtime/opal.h"
#include "opal/util/arch.h"
#include "opal/util/malloc.h"
#include "opal/util/net.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include "opal/mca/backtrace/base/base.h"
#include "opal/mca/threads/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/util/opal_environ.h"

#include "opal/constants.h"
#include "opal/util/error.h"
#include "opal/util/event.h"
#include "opal/util/keyval_parse.h"
#include "opal/util/stacktrace.h"
#include "opal/util/sys_limits.h"
#include "opal/util/timings.h"

const char opal_version_string[] = OPAL_IDENT_STRING;

int opal_initialized = 0;
/* We have to put a guess in here in case hwloc is not available.  If
   hwloc is available, this value will be overwritten when the
   hwloc data is loaded. */
int opal_cache_line_size = 128;

/* Defined in opal_util_init.c and part of the opal_util.la library */
int opal_init_error(const char *error, int ret);

int opal_init_psm(void)
{
    /* Very early in the init sequence -- before *ANY* MCA components
       are opened -- we need to disable some behavior from the PSM and
       PSM2 libraries (by default): at least some old versions of
       these libraries hijack signal handlers during their library
       constructors and then do not un-hijack them when the libraries
       are unloaded.

       It is a bit of an abstraction break that we have to put
       vendor/transport-specific code in the OPAL core, but we're
       out of options, unfortunately.

       NOTE: We only disable this behavior if the corresponding
       environment variables are not already set (i.e., if the
       user/environment has indicated a preference for this behavior,
       we won't override it). */
    if (NULL == getenv("IPATH_NO_BACKTRACE")) {
        opal_setenv("IPATH_NO_BACKTRACE", "1", true, &environ);
    }
    if (NULL == getenv("HFI_NO_BACKTRACE")) {
        opal_setenv("HFI_NO_BACKTRACE", "1", true, &environ);
    }

    return OPAL_SUCCESS;
}

/* the memcpy component should be one of the first who get
 * loaded in order to make sure we have all the available
 * versions of memcpy correctly configured.
 */
static mca_base_framework_t *opal_init_frameworks[] = {
    &opal_if_base_framework,
    &opal_threads_base_framework, &opal_hwloc_base_framework,
    &opal_memcpy_base_framework, &opal_memchecker_base_framework,
    &opal_backtrace_base_framework, &opal_timer_base_framework,
    &opal_shmem_base_framework, &opal_reachable_base_framework,
    &opal_pmix_base_framework,
    NULL,
};

int opal_init(int *pargc, char ***pargv)
{
    int ret;

    OPAL_TIMING_ENV_INIT(otmng);

    if (opal_initialized != 0) {
        if (opal_initialized < 0) {
            return OPAL_ERROR;
        }
        ++opal_initialized;
        return OPAL_SUCCESS;
    }

    /* initialize util code */
    if (OPAL_SUCCESS != (ret = opal_init_util(pargc, pargv))) {
        return ret;
    }

    /* initialize if framework */
    if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_if_base_framework, 0))) {
        fprintf(stderr,
                "opal_if_base_open() failed -- process will likely abort (%s:%d, returned %d "
                "instead of OPAL_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    OPAL_TIMING_ENV_NEXT(otmng, "opal_if_init");

    /* register PMIx cleanup function for output streams */
    opal_output_register_pmix_cleanup_fn(&opal_pmix_register_cleanup);

    /* register non-core MCA parameters for opal */
    if (OPAL_SUCCESS != (ret = opal_register_params())) {
        return opal_init_error("opal_register_params", ret);
    }

    // Disable PSM signal hijacking (see comment in function for more
    // details)
    opal_init_psm();

    OPAL_TIMING_ENV_NEXT(otmng, "opal_init_psm");

    if (OPAL_SUCCESS != (ret = opal_net_init())) {
        return opal_init_error("opal_net_init", ret);
    }

    OPAL_TIMING_ENV_NEXT(otmng, "opal_net_init");

    OBJ_CONSTRUCT(&opal_init_domain, opal_finalize_domain_t);
    (void) opal_finalize_domain_init(&opal_init_domain, "opal_init");
    opal_finalize_set_domain(&opal_init_domain);

    /* register for cleanup */
    opal_finalize_register_cleanup_arg(mca_base_framework_close_list, opal_init_frameworks);

    ret = mca_base_framework_open_list(opal_init_frameworks, 0);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        return opal_init_error("opal_init framework open", ret);
    }

    /* Intitialize Accelerator framework
     * The datatype convertor code has a dependency on the accelerator framework
     * being initialized. */
    ret = mca_base_framework_open(&opal_accelerator_base_framework, 0);
    if (OPAL_SUCCESS == ret && OPAL_SUCCESS != (ret = opal_accelerator_base_select())) {
        return opal_init_error("opal_accelerator_base_select", ret);
    }
    opal_finalize_register_cleanup(opal_accelerator_base_selected_component.accelerator_finalize);

    /* initialize the datatype engine */
    if (OPAL_SUCCESS != (ret = opal_datatype_init())) {
        return opal_init_error("opal_datatype_init", ret);
    }
    /* The ddt engine has a few parameters */
    ret = opal_datatype_register_params();
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    ret = opal_event_register_params();
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    OPAL_TIMING_ENV_NEXT(otmng, "opal_datatype_init");

    /* initialize libevent */
    if (OPAL_SUCCESS != (ret = opal_event_init())) {
        return opal_init_error("opal_event_init", ret);
    }

    /* initialize the memory manager / tracker */
    if (OPAL_SUCCESS != (ret = opal_mem_hooks_init())) {
        return opal_init_error("opal_mem_hooks_init", ret);
    }

    /* select the memory checker */
    if (OPAL_SUCCESS != (ret = opal_memchecker_base_select())) {
        return opal_init_error("opal_memchecker_base_select", ret);
    }

    /*
     * Initialize the general progress engine
     */
    if (OPAL_SUCCESS != (ret = opal_progress_init())) {
        return opal_init_error("opal_progress_init", ret);
    }
    /* we want to tick the event library whenever possible */
    opal_progress_event_users_increment();

    /* setup the shmem framework */
    if (OPAL_SUCCESS != (ret = opal_shmem_base_select())) {
        return opal_init_error("opal_shmem_base_select", ret);
    }

    /* Initialize reachable framework */
    if (OPAL_SUCCESS != (ret = opal_reachable_base_select())) {
        return opal_init_error("opal_reachable_base_select", ret);
    }

    ++opal_initialized;

    return OPAL_SUCCESS;
}
