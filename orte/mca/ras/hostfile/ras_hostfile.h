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
 * Resource Allocation (hostfile)
 */
#ifndef ORTE_RAS_HOSTFILE_H
#define ORTE_RAS_HOSTFILE_H

#include "orte/mca/ras/ras.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * hostfile-specific RAS component struct
     */
    struct orte_ras_hostfile_component_t {
        /** Base RAS component */
        orte_ras_base_component_t super;
        /** What's the priority of this component */
        int priority;
    };
    /**
     * Convenience typedef
     */
    typedef struct orte_ras_hostfile_component_t orte_ras_hostfile_component_t;

    /**
     * Component export structure
     */
    ORTE_MODULE_DECLSPEC extern orte_ras_hostfile_component_t mca_ras_hostfile_component;
    
    /**
     * Module init function
     */
    orte_ras_base_module_t *orte_ras_hostfile_init(int* priority);
    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
