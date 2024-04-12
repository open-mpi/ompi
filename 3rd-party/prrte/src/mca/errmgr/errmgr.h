/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The PRTE Error and Recovery Manager (ErrMgr)
 *
 * This framework is the logically central clearing house for process/daemon
 * state updates. In particular when a process fails and another process detects
 * it, then that information is reported through this framework. This framework
 * then (depending on the active component) decides how to handle the failure.
 *
 * For example, if a process fails this may activate an automatic recovery
 * of the process from a previous checkpoint, or initial state. Conversely,
 * the active component could decide not to continue the job, and request that
 * it be terminated. The error and recovery policy is determined by individual
 * components within this framework.
 *
 */

#ifndef PRTE_MCA_ERRMGR_H
#define PRTE_MCA_ERRMGR_H

/*
 * includes
 */

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include "src/pmix/pmix-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

#include "src/class/pmix_object.h"
#include "src/class/pmix_pointer_array.h"
#include "src/util/error.h"
#include "src/util/pmix_output.h"

#include "src/mca/plm/plm_types.h"
#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

/*
 * Macro definitions
 */
/*
 * Thess macros and associated error name array are used to output intelligible error
 * messages.
 */

#define PRTE_ERROR_NAME(n) prte_strerror(n)

/*
 * Framework Interfaces
 */
/**
 * Module initialization function.
 *
 * @retval PRTE_SUCCESS The operation completed successfully
 * @retval PRTE_ERROR   An unspecifed error occurred
 */
typedef int (*prte_errmgr_base_module_init_fn_t)(void);

/**
 * Module finalization function.
 *
 * @retval PRTE_SUCCESS The operation completed successfully
 * @retval PRTE_ERROR   An unspecifed error occurred
 */
typedef int (*prte_errmgr_base_module_finalize_fn_t)(void);

/**
 * This is not part of any module so it can be used at any time!
 */
typedef void (*prte_errmgr_base_module_log_fn_t)(int error_code, char *filename, int line);

/*
 * Module Structure
 */
struct prte_errmgr_base_module_2_3_0_t {
    /** Initialization Function */
    prte_errmgr_base_module_init_fn_t init;
    /** Finalization Function */
    prte_errmgr_base_module_finalize_fn_t finalize;

    prte_errmgr_base_module_log_fn_t logfn;
};
typedef struct prte_errmgr_base_module_2_3_0_t prte_errmgr_base_module_2_3_0_t;
typedef prte_errmgr_base_module_2_3_0_t prte_errmgr_base_module_t;
PRTE_EXPORT extern prte_errmgr_base_module_t prte_errmgr;

/*
 * ErrMgr Component
 */
struct prte_errmgr_base_component_3_0_0_t {
    /** MCA base component */
    pmix_mca_base_component_t base_version;

    /** Verbosity Level */
    int verbose;
    /** Output Handle for pmix_output */
    int output_handle;
    /** Default Priority */
    int priority;
};
typedef struct prte_errmgr_base_component_3_0_0_t prte_errmgr_base_component_3_0_0_t;
typedef prte_errmgr_base_component_3_0_0_t prte_errmgr_base_component_t;

/*
 * Macro for use in components that are of type errmgr
 */
#define PRTE_ERRMGR_BASE_VERSION_3_0_0 PRTE_MCA_BASE_VERSION_3_0_0("errmgr", 3, 0, 0)

END_C_DECLS

#endif
