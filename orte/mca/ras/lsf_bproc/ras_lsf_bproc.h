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
 * Resource Allocation (LSF over BPROC)
 */
#ifndef ORTE_RAS_LSF_BPROC_H
#define ORTE_RAS_LSF_BPROC_H

#include "orte/mca/ras/ras.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * RAS Component 
 */
struct orte_ras_lsf_bproc_component_t {
    orte_ras_base_component_t super;
    int debug;
    int priority;
};
typedef struct orte_ras_lsf_bproc_component_t orte_ras_lsf_bproc_component_t;

ORTE_DECLSPEC extern orte_ras_lsf_bproc_component_t mca_ras_lsf_bproc_component;
ORTE_DECLSPEC extern orte_ras_base_module_t orte_ras_lsf_bproc_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
