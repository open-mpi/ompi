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

#include "opal/threads/condition.h"

#include "orte/mca/rmgr/rmgr.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
* URM component structure -- add some stuff beyond what is in the
* normal rmgr component.
*/
struct orte_rmgr_urm_component_t {
    /** Base rmgr component */
    orte_rmgr_base_component_t super;
    /** Has RDS query been called */
    bool urm_rds;
    /* timing tests requested */
    bool timing;
    /* mutex lock */
    opal_mutex_t lock;
    /* conditioned wait variable */
    opal_condition_t cond;
    /* flag for tracking completion of app launch */
    bool done;
    /* return status code */
    int rc;
};
/** Convenience typedef */
typedef struct orte_rmgr_urm_component_t orte_rmgr_urm_component_t;

/** Global URM component */
ORTE_MODULE_DECLSPEC extern orte_rmgr_urm_component_t mca_rmgr_urm_component;
/** Global URM module */
extern orte_rmgr_base_module_t orte_rmgr_urm_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
