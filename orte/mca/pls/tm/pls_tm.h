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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_PLS_TM_EXPORT_H
#define ORTE_PLS_TM_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/pls/pls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    struct orte_pls_tm_component_t {
        orte_pls_base_component_t super;
        int priority;
        int debug;
        int verbose;
        bool want_path_check;
        char *orted;
        char **checked_paths;
        bool timing;
    };
    typedef struct orte_pls_tm_component_t orte_pls_tm_component_t;

    /* Globally exported variables */
    ORTE_DECLSPEC extern orte_pls_tm_component_t mca_pls_tm_component;
    extern orte_pls_base_module_t orte_pls_tm_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_PLS_TM_EXPORT_H */
