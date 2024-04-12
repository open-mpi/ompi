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
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * The PRTE Daemon's Local Launch Subsystem
 *
 */

#ifndef PRTE_MCA_ODLS_H
#define PRTE_MCA_ODLS_H

#include "prte_config.h"
#include "types.h"

#include "src/class/pmix_pointer_array.h"
#include "src/mca/mca.h"
#include "src/pmix/pmix-internal.h"
#include "src/rml/rml_types.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"

#include "src/mca/odls/odls_types.h"

BEGIN_C_DECLS

/*
 * odls module functions
 */

/*
 * Construct a buffer for use in adding local processes
 * In order to reuse daemons, we need a way for the HNP to construct a buffer that
 * contains the data needed by the active ODLS component to launch a local process. Since the
 * only one that knows what a particular ODLS component needs is that component, we require an
 * entry point that the HNP can call to get the required buffer. This is constructed
 * for *all* nodes - the individual orteds then parse that data to find the specific launch info
 * for procs on their node
 */
typedef int (*prte_odls_base_module_get_add_procs_data_fn_t)(pmix_data_buffer_t *data,
                                                             pmix_nspace_t job);

/**
 * Locally launch the provided processes
 */
typedef int (*prte_odls_base_module_launch_local_processes_fn_t)(pmix_data_buffer_t *data);

/**
 * Kill the local processes on this node
 */
typedef int (*prte_odls_base_module_kill_local_processes_fn_t)(pmix_pointer_array_t *procs);

/**
 * Signal local processes
 */
typedef int (*prte_odls_base_module_signal_local_process_fn_t)(const pmix_proc_t *proc,
                                                               int32_t signal);

/**
 * Restart a local process
 */
typedef int (*prte_odls_base_module_restart_proc_fn_t)(prte_proc_t *child);

/**
 * pls module version
 */
struct prte_odls_base_module_1_3_0_t {
    prte_odls_base_module_get_add_procs_data_fn_t get_add_procs_data;
    prte_odls_base_module_launch_local_processes_fn_t launch_local_procs;
    prte_odls_base_module_kill_local_processes_fn_t kill_local_procs;
    prte_odls_base_module_signal_local_process_fn_t signal_local_procs;
    prte_odls_base_module_restart_proc_fn_t restart_proc;
};

/** shprten prte_odls_base_module_1_3_0_t declaration */
typedef struct prte_odls_base_module_1_3_0_t prte_odls_base_module_1_3_0_t;
/** shprten prte_odls_base_module_t declaration */
typedef struct prte_odls_base_module_1_3_0_t prte_odls_base_module_t;

/**
 * odls component
 */
typedef pmix_mca_base_component_t prte_odls_base_component_t;

/**
 * Macro for use in modules that are of type odls
 */
#define PRTE_ODLS_BASE_VERSION_2_0_0 PRTE_MCA_BASE_VERSION_3_0_0("odls", 2, 0, 0)

/* Global structure for accessing ODLS functions
 */
PRTE_EXPORT extern prte_odls_base_module_t
    prte_odls; /* holds selected module's function pointers */

END_C_DECLS

#endif /* MCA_ODLS_H */
