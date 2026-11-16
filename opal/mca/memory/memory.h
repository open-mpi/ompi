/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/* NOTE: This framework accommodates two kinds of memory hooks systems:

   1. Those that only require being setup once and then will
      automatically call back to internal component/module functions
      as required (e.g., the ptmalloc2 component).  It is expected
      that such components will have a NULL value for memoryc_process
      and empty (base) implementations of memoryc_register and
      memoryc_deregister.

   2. Those that must be queried periodically to find out if something
      has happened to the registered memory mappings (e.g., the
      ummunot component).  It is expected that such components will
      have complete implementations for memoryc_process,
      memoryc_register, and memoryc_deregister.

   Every component that configure can build is built.  At run time,
   the base opens the components in priority order and uses the first
   one that opens.  If none of them opens, there is no memory manager
   support in this process.

   This framework used to build only one component, chosen at
   configure time, because some components (e.g., the ptmalloc2
   wrapper) intercepted memory functions at link time and so could not
   be loaded or selected at run time.  That is why there is no separate
   module structure: everything is in the component.  The structure
   was kept, rather than refactored, when run-time selection was added.

   rcache calls memoryc_process() (through opal_memory_base_process())
   before it looks up or inserts cached registrations.  A component of
   the second kind reports the changes it has seen from there, on the
   calling thread, through opal_mem_hooks_release_hook().  Such a
   component may learn about a change some time after it happened, so
   it also provides memoryc_valid(), which rcache calls before it
   reuses a registration.  It does not set OPAL_MEMORY_FREE_SUPPORT or
   OPAL_MEMORY_MUNMAP_SUPPORT, because it does not see every release;
   rcache recognizes it by memoryc_valid().
*/

#ifndef OPAL_MCA_MEMORY_MEMORY_H
#define OPAL_MCA_MEMORY_MEMORY_H

#include "opal_config.h"

#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"

BEGIN_C_DECLS

/**
 * Prototype for reporting changes to the memory mappings of the
 * regions passed to memoryc_register.  This function is assumed to do
 * whatever is necessary to a) figure out what has changed, and b)
 * adjust OMPI as relevant (e.g., call opal_mem_hooks_release_hook).
 * It is called often, so it must be cheap when nothing changed.
 */
typedef int (*opal_memory_base_component_process_fn_t)(void);

/**
 * Prototype for a function that is invoked when the memory base is
 * trying to select a component. This functionality is required.
 */
typedef int (*opal_memory_base_component_query_fn_t)(int *priority);

/**
 * Prototype for a function that is invoked when Open MPI starts to
 * "care" about a specific memory region.  That is, Open MPI declares
 * that it wants to be notified if the memory mapping for this region
 * changes.
 *
 * If a component does not want/need to provide this functionality, it
 * can use the value opal_memory_base_register_empty (an empty
 * implementation of this function).
 */
typedef int (*opal_memory_base_component_register_fn_t)(void *base, size_t len, uint64_t cookie);

/**
 * Prototype for a function that is the opposite of
 * opal_memory_base_component_register_fn_t: this function is invoked
 * when Open MPI stops to "caring" about a specific memory region.
 * That is, Open MPI declares that it no longer wants to be notified
 * if the memory mapping for this region changes.
 *
 * The parameters of this function will exactly match parameters that
 * were previously passed to the call to
 * opal_memory_base_component_register_fn_t.
 *
 * If a component does not want/need to provide this functionality, it
 * can use the value opal_memory_base_deregister_empty (an empty
 * implementation of this function).
 */
typedef int (*opal_memory_base_component_deregister_fn_t)(void *base, size_t len, uint64_t cookie);

/**
 * Prototype for a function that checks whether the memory mapping of
 * a region has changed since the region was passed to
 * memoryc_register.  The parameters match the ones passed to
 * memoryc_register.  Returns true if the mapping is unchanged, and
 * false if it changed or the component cannot tell.
 */
typedef bool (*opal_memory_base_component_valid_fn_t)(void *base, size_t len, uint64_t cookie);

/**
 * Prototype for a function that set the memory alignment
 */
typedef void (*opal_memory_base_component_set_alignment_fn_t)(int use_memalign,
                                                              size_t memalign_threshold);

/**
 * Function to be called when initializing malloc hooks
 */
typedef void (*opal_memory_base_component_init_hook_fn_t)(void);

/**
 * Structure for memory components.
 */
typedef struct opal_memory_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t memoryc_version;
    /** MCA base data */
    mca_base_component_data_t memoryc_data;

    opal_memory_base_component_query_fn_t memoryc_query;

    /** This function will be called when the malloc hooks are
     * initialized. It may be NULL if no hooks are needed. */
    opal_memory_base_component_init_hook_fn_t memoryc_init_hook;

    /** Function to call to report changes to the regions passed to
        memoryc_register.  May be NULL. */
    opal_memory_base_component_process_fn_t memoryc_process;

    /** Function invoked when Open MPI starts "caring" about a
        specific memory region */
    opal_memory_base_component_register_fn_t memoryc_register;
    /** Function invoked when Open MPI stops "caring" about a
        specific memory region */
    opal_memory_base_component_deregister_fn_t memoryc_deregister;
    /** Function invoked in order to set malloc'ed memory alignment */
    opal_memory_base_component_set_alignment_fn_t memoryc_set_alignment;
    /** Function invoked to check that a region passed to
        memoryc_register has not changed.  May be NULL if the component
        reports every change before it returns from the call that made
        it. */
    opal_memory_base_component_valid_fn_t memoryc_valid;
} opal_memory_base_component_2_0_0_t;

OPAL_DECLSPEC extern opal_memory_base_component_2_0_0_t *opal_memory;

END_C_DECLS

/*
 * Macro for use in components that are of type memory
 */
#define OPAL_MEMORY_BASE_VERSION_2_0_0 OPAL_MCA_BASE_VERSION_2_1_0("memory", 2, 0, 0)

#endif /* OPAL_MCA_MEMORY_MEMORY_H */
