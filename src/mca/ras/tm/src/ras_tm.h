/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
 * Resource Allocation (TM)
 */
#ifndef ORTE_RAS_TM_H
#define ORTE_RAS_TM_H

#include "mca/ras/ras.h"
#include "mca/ras/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    OMPI_COMP_EXPORT extern orte_ras_base_component_1_0_0_t mca_ras_tm_component;
    OMPI_COMP_EXPORT extern orte_ras_base_module_t orte_ras_tm_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
