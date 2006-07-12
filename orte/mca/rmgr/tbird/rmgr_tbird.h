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
/**
 * @file
 *
 * Universal Resource Manager (tbird)
 */
#ifndef ORTE_RMGR_tbird_H
#define ORTE_RMGR_tbird_H

#include "orte/mca/rmgr/rmgr.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
* tbird component structure -- add some stuff beyond what is in the
* normal rmgr component.
*/
struct orte_rmgr_tbird_component_t {
    /** Base rmgr component */
    orte_rmgr_base_component_t super;
    /** Has RDS query been called */
    bool tbird_rds;
    /** Selected ras module */
    orte_ras_base_module_t *tbird_ras;
    /** Selected rmaps module */
    orte_rmaps_base_module_t *tbird_rmaps;
    /** Selected pls module */
    orte_pls_base_module_t *tbird_pls;
};
/** Convenience typedef */
typedef struct orte_rmgr_tbird_component_t orte_rmgr_tbird_component_t;

/** Global tbird component */
OMPI_COMP_EXPORT extern orte_rmgr_tbird_component_t mca_rmgr_tbird_component;
/** Global tbird module */
OMPI_COMP_EXPORT extern orte_rmgr_base_module_t orte_rmgr_tbird_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
