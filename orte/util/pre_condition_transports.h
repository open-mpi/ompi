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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file:
 *
 * Pre-condition transports
 *
 *
 */

#ifndef _ORTE_PRE_CONDITION_TRANSPORTS_H_
#define _ORTE_PRE_CONDITION_TRANSPORTS_H_

#include "orte_config.h"

#include "orte/mca/rmgr/rmgr_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

ORTE_DECLSPEC int orte_pre_condition_transports(orte_app_context_t **app_context, size_t num_context);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
