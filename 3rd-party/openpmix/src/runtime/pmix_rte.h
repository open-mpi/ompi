/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#ifndef PMIX_RTE_H
#define PMIX_RTE_H

#include "src/include/pmix_config.h"
#include "pmix_common.h"
#include "src/class/pmix_object.h"

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <event.h>

#include "src/include/pmix_globals.h"
#include "src/mca/ptl/ptl_types.h"

BEGIN_C_DECLS

#if PMIX_ENABLE_TIMING
PMIX_EXPORT extern char *pmix_timing_sync_file;
PMIX_EXPORT extern char *pmix_timing_output;
PMIX_EXPORT extern bool pmix_timing_overhead;
#endif

PMIX_EXPORT extern char *pmix_net_private_ipv4;
PMIX_EXPORT extern int pmix_event_caching_window;
PMIX_EXPORT extern bool pmix_suppress_missing_data_warning;
PMIX_EXPORT extern char *pmix_progress_thread_cpus;
PMIX_EXPORT extern bool pmix_bind_progress_thread_reqd;
PMIX_EXPORT extern int pmix_maxfd;

/** version string of pmix */
extern const char pmix_version_string[];

/**
 * Initialize the PMIX layer, including the MCA system.
 *
 * @retval PMIX_SUCCESS Upon success.
 * @retval PMIX_ERROR Upon failure.
 *
 */
PMIX_EXPORT pmix_status_t pmix_rte_init(uint32_t type, pmix_info_t info[], size_t ninfo,
                                        pmix_ptl_cbfunc_t cbfunc);

/**
 * Finalize the PMIX layer, including the MCA system.
 *
 */
PMIX_EXPORT void pmix_rte_finalize(void);

/**
 * Internal function.  Do not call.
 */
PMIX_EXPORT pmix_status_t pmix_register_params(void);
PMIX_EXPORT pmix_status_t pmix_deregister_params(void);

END_C_DECLS

#endif /* PMIX_RTE_H */
