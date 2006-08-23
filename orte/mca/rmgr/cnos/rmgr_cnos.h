/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Universal Resource Manager (URM)
 */
#ifndef ORTE_RMGR_URM_H
#define ORTE_RMGR_URM_H

#include "orte/mca/rmgr/rmgr.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/** Global URM component */
ORTE_DECLSPEC extern orte_rmgr_base_component_t mca_rmgr_cnos_component;
/** Global URM module */
extern orte_rmgr_base_module_t orte_rmgr_cnos_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
