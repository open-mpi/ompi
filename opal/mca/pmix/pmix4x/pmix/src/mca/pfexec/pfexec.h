/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * The PMIx Fork/Exec Subsystem
 *
 */

#ifndef PMIX_MCA_PFEXEC_H
#define PMIX_MCA_PFEXEC_H

#include "pmix_config.h"
#include "pmix_common.h"
#include "src/include/types.h"

#include "src/mca/mca.h"

BEGIN_C_DECLS

/*
 * pfexec module functions
 */

/**
 * Locally fork/exec the provided process
 */
typedef pmix_status_t (*pmix_pfexec_base_module_spawn_process_fn_t)(const pmix_info_t job_info[], size_t ninfo,
                                                                    const pmix_app_t apps[], size_t napps);

/**
 * Kill the local process we started
 */
typedef pmix_status_t (*pmix_pfexec_base_module_kill_process_fn_t)(pmix_rank_t rank);

/**
 * Signal local process we started
 */
typedef pmix_status_t (*pmix_pfexec_base_module_signal_process_fn_t)(pmix_rank_t rank, int signum);

/**
 * pfexec module version
 */
typedef struct {
    pmix_pfexec_base_module_spawn_process_fn_t       spawn_proc;
    pmix_pfexec_base_module_kill_process_fn_t        kill_proc;
    pmix_pfexec_base_module_signal_process_fn_t      signal_proc;
} pmix_pfexec_base_module_t;

/**
 * pfexec component
 */
typedef struct {
    /** component version */
    pmix_mca_base_component_t version;
    /** component data */
    pmix_mca_base_component_data_t base_data;
} pmix_pfexec_base_component_t;


/**
 * Macro for use in modules that are of type pfexec
 */
#define PMIX_PFEXEC_BASE_VERSION_1_0_0 \
    PMIX_MCA_BASE_VERSION_1_0_0("pfexec", 1, 0, 0)

/* Global structure for accessing PFEXEC functions
*/
PMIX_EXPORT extern pmix_pfexec_base_module_t pmix_pfexec;  /* holds selected module's function pointers */

END_C_DECLS

#endif /* MCA_PFEXEC_H */
