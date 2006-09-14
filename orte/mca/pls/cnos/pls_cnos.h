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
 */
#ifndef ORTE_PLS_CNOS_H
#define ORTE_PLS_CNOS_H

#include "orte/mca/pls/pls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/** Global PLS component */
ORTE_DECLSPEC extern orte_pls_base_component_t mca_pls_cnos_component;
/** Global PLS module */
extern orte_pls_base_module_t orte_pls_cnos_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
