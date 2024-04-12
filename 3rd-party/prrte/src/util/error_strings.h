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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights reserved
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file:
 *
 */

#ifndef _PRTE_ERROR_STRINGS_H_
#define _PRTE_ERROR_STRINGS_H_

#include "prte_config.h"

#include "src/mca/plm/plm_types.h"
#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

PRTE_EXPORT const char *prte_job_state_to_str(prte_job_state_t state);

PRTE_EXPORT const char *prte_app_ctx_state_to_str(prte_app_state_t state);

PRTE_EXPORT const char *prte_proc_state_to_str(prte_proc_state_t state);

PRTE_EXPORT const char *prte_node_state_to_str(prte_node_state_t state);

END_C_DECLS
#endif
