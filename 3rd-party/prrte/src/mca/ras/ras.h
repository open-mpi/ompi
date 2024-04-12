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
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The PRTE Resource Allocation Subsystem (RAS)
 *
 * The resource allocation subsystem is responsible for determining
 * what (if any) resources have been allocated to the specified job
 * (via some prior action), and to obtain an allocation (if possible)
 * if resources have NOT been previously allocated. It is anticipated
 * that PRTE users will execute an "mpirun" or other command that
 * invokes PRTE through one of two channels:
 *
 * 1. the user will login to the computing resource they intend
 * to use, request a resource allocation from that system, and then
 * execute the mpirun or other command. Thus, the allocation has
 * already been obtained prior to PRTE's initialization.  In most
 * cases, systems pass allocation information via environmental
 * parameters.  Thus, the RAS components must know the correct
 * environmental parameter to look for within the environment they
 * seek to support (e.g., an LSF component should know that LSF passes
 * allocation parameters as a specific LSF-named entity).
 *
 * 2. the user issues an mpirun command or an application that uses
 * PRTE without obtaining an allocation in advance. Thus, the associated
 * RAS component must know how to request an allocation from the
 * designated resource. If it doesn't, or it cannot obtain the allocation,
 * then it shall indicate this by setting the system to an appropriate
 * state.
 */

#ifndef PRTE_MCA_RAS_H
#define PRTE_MCA_RAS_H

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"
#include "src/mca/mca.h"
#include "src/pmix/pmix-internal.h"

#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

/* allocation event - the event one activates to schedule resource
 * allocation for pending jobs
 */
PRTE_EXPORT extern prte_event_t prte_allocate_event;

/*
 * ras module functions - these are not accessible to the outside world,
 * but are defined here by convention
 */

/* init the module */
typedef int (*prte_ras_base_module_init_fn_t)(void);

/**
 * Allocate resources to a job.
 */
typedef int (*prte_ras_base_module_allocate_fn_t)(prte_job_t *jdata, pmix_list_t *nodes);

/* deallocate resources */
typedef void (*prte_ras_base_module_dealloc_fn_t)(prte_job_t *jdata, prte_app_context_t *app);

/**
 * Cleanup module resources.
 */
typedef int (*prte_ras_base_module_finalize_fn_t)(void);

/**
 * ras module
 */
struct prte_ras_base_module_2_0_0_t {
    /** init */
    prte_ras_base_module_init_fn_t init;
    /** Allocation function pointer */
    prte_ras_base_module_allocate_fn_t allocate;
    prte_ras_base_module_dealloc_fn_t deallocate;
    /** Finalization function pointer */
    prte_ras_base_module_finalize_fn_t finalize;
};
/** Convenience typedef */
typedef struct prte_ras_base_module_2_0_0_t prte_ras_base_module_2_0_0_t;
/** Convenience typedef */
typedef prte_ras_base_module_2_0_0_t prte_ras_base_module_t;

/*
 * ras component
 */

/** Convenience typedef */
typedef pmix_mca_base_component_t prte_ras_base_component_t;

/**
 * Macro for use in components that are of type ras
 */
#define PRTE_RAS_BASE_VERSION_2_0_0 PRTE_MCA_BASE_VERSION_3_0_0("ras", 2, 0, 0)

END_C_DECLS

#endif
