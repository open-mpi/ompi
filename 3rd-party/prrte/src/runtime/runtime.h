/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
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
 * Interface into the PRTE Run Time Environment
 */
#ifndef PRTE_RUNTIME_H
#define PRTE_RUNTIME_H

#include "prte_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

#include "src/util/proc_info.h"

BEGIN_C_DECLS

/** version string of ompi */
PRTE_EXPORT extern const char prte_version_string[];

/**
 * Whether PRTE is initialized or we are in prte_finalize
 */
PRTE_EXPORT extern bool prte_initialized;
PRTE_EXPORT extern bool prte_finalizing;
PRTE_EXPORT extern int prte_debug_output;
PRTE_EXPORT extern bool prte_debug_flag;
PRTE_EXPORT extern int prte_cache_line_size;

/**
 * Initialize the Open Run Time Environment
 *
 * Initlize the Open Run Time Environment, including process
 * control, malloc debugging and threads, and out of band messaging.
 * This function should be called exactly once.  This function should
 * be called by every application using the RTE interface, including
 * MPI applications and mpirun.
 *
 * @param pargc  Pointer to the number of arguments in the pargv array
 * @param pargv  The list of arguments.
 * @param flags  Whether we are PRTE tool or not
 */
PRTE_EXPORT int prte_init(int *pargc, char ***pargv, prte_proc_type_t flags);
PRTE_EXPORT int prte_init_util(prte_proc_type_t flags);
PRTE_EXPORT int prte_init_minimum(void);

/**
 * Initialize parameters for PRTE.
 *
 * @retval PRTE_SUCCESS Upon success.
 * @retval PRTE_ERROR Upon failure.
 */
PRTE_EXPORT int prte_register_params(void);

/**
 * Finalize the Open run time environment. Any function calling \code
 * prte_init should call \code prte_finalize.
 *
 */
PRTE_EXPORT int prte_finalize(void);

END_C_DECLS

#endif /* RUNTIME_H */
