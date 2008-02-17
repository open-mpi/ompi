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
 * Copyright (c) 2008      UT-Battelle, LLC
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_PLS_ALPS_EXPORT_H
#define ORTE_PLS_ALPS_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/pls/pls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    struct orte_pls_alps_component_t {
        orte_pls_base_component_t super;
        int priority;
        int debug;
        bool timing;
        char *orted;
        char *custom_args;
    };
    typedef struct orte_pls_alps_component_t orte_pls_alps_component_t;

    /*
     * Globally exported variable
     */
    
    ORTE_MODULE_DECLSPEC extern orte_pls_alps_component_t 
        mca_pls_alps_component;
    ORTE_DECLSPEC extern orte_pls_base_module_t
        orte_pls_alps_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_ALPS_EXPORT_H */
