/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
 * Resource Allocation (Host)
 */
#ifndef ORTE_RAS_LOCALHOST_H
#define ORTE_RAS_LOCALHOST_H

#include "mca/ras/ras.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Localhost-specific RAS component struct
     */
    struct orte_ras_localhost_component_t {
        /** Base RAS component */
        orte_ras_base_component_t super;
        /** What's the priority of this component */
        int priority;
    };
    /**
     * Convenience typedef
     */
    typedef struct orte_ras_localhost_component_t orte_ras_localhost_component_t;

    /**
     * Component export structure
     */
    OMPI_COMP_EXPORT extern orte_ras_localhost_component_t mca_ras_localhost_component;
    
    /**
     * Module init function
     */
    orte_ras_base_module_t *orte_ras_localhost_init(int* priority);
    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
