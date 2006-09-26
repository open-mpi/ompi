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

/*
 * NOTE:  This header is an Objective-C file.  It might not do what
 * you intend with a C/C++ compiler
 */

#import "orte/mca/pls/pls.h"
#import "pls_xgrid_client.h"

/**
 * PLS Component
 */
struct orte_pls_xgrid_component_t {
    orte_pls_base_component_t super;
    PlsXGridClient *client;
    NSAutoreleasePool *pool;
};
typedef struct orte_pls_xgrid_component_t orte_pls_xgrid_component_t;
extern orte_pls_xgrid_component_t mca_pls_xgrid_component;
extern orte_pls_base_module_1_3_0_t orte_pls_xgrid_module;

int orte_pls_xgrid_progress(void);
