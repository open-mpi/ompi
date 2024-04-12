/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
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
 * The Process Lifecycle Management (PLM) subsystem serves as the central
 * switchyard for all process management activities, including
 * resource allocation, process mapping, process launch, and process
 * monitoring.
 */

#ifndef PRTE_PLM_H
#define PRTE_PLM_H

/*
 * includes
 */

#include "prte_config.h"
#include "types.h"

#include "src/class/pmix_pointer_array.h"
#include "src/mca/mca.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"

#include "plm_types.h"

BEGIN_C_DECLS

/*
 * Component functions - all MUST be provided
 */

/*
 * allow the selected module to initialize
 */
typedef int (*prte_plm_base_module_init_fn_t)(void);

/*
 * Spawn a job - this is a non-blocking function!
 */
typedef int (*prte_plm_base_module_spawn_fn_t)(prte_job_t *jdata);

/*
 * Remote spawn - spawn called by a daemon to launch a process on its own
 */
typedef int (*prte_plm_base_module_remote_spawn_fn_t)(void);

/*
 * Entry point to set the HNP name
 */
typedef int (*prte_plm_base_module_set_hnp_name_fn_t)(void);

/**
 * Cleanup resources held by module.
 */

typedef int (*prte_plm_base_module_finalize_fn_t)(void);

/**
 * Terminate any processes launched for the respective jobid by
 * this component.
 */
typedef int (*prte_plm_base_module_terminate_job_fn_t)(pmix_nspace_t);

/**
 * Terminate the daemons
 */
typedef int (*prte_plm_base_module_terminate_orteds_fn_t)(void);

/**
 * Terminate an array of specific procs
 */
typedef int (*prte_plm_base_module_terminate_procs_fn_t)(pmix_pointer_array_t *procs);

/**
 * Signal any processes launched for the respective jobid by
 * this component.
 */
typedef int (*prte_plm_base_module_signal_job_fn_t)(pmix_nspace_t, int32_t);

/**
 * plm module version 1.0.0
 */
struct prte_plm_base_module_1_0_0_t {
    prte_plm_base_module_init_fn_t init;
    prte_plm_base_module_set_hnp_name_fn_t set_hnp_name;
    prte_plm_base_module_spawn_fn_t spawn;
    prte_plm_base_module_remote_spawn_fn_t remote_spawn;
    prte_plm_base_module_terminate_job_fn_t terminate_job;
    prte_plm_base_module_terminate_orteds_fn_t terminate_orteds;
    prte_plm_base_module_terminate_procs_fn_t terminate_procs;
    prte_plm_base_module_signal_job_fn_t signal_job;
    prte_plm_base_module_finalize_fn_t finalize;
};

/** shprten prte_plm_base_module_1_0_0_t declaration */
typedef struct prte_plm_base_module_1_0_0_t prte_plm_base_module_1_0_0_t;
/** shprten prte_plm_base_module_t declaration */
typedef struct prte_plm_base_module_1_0_0_t prte_plm_base_module_t;

/**
 * plm component
 */
typedef pmix_mca_base_component_t prte_plm_base_component_t;

/**
 * Macro for use in modules that are of type plm
 */
#define PRTE_PLM_BASE_VERSION_2_0_0 PRTE_MCA_BASE_VERSION_3_0_0("plm", 2, 0, 0)

/* Global structure for accessing PLM functions */
PRTE_EXPORT extern prte_plm_base_module_t prte_plm; /* holds selected module's function pointers */

END_C_DECLS

#endif
