/* -*- C -*-
 *
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
 *
 */
/** @file
 */

#ifndef ORTE_GPR_NULL_H
#define ORTE_GPR_NULL_H

#include "orte_config.h"

#include "orte/mca/ns/ns_types.h"

#include "orte/mca/gpr/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

ORTE_MODULE_DECLSPEC extern mca_gpr_base_component_t mca_gpr_null_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
