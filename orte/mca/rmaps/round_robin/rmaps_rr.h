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
 * Resource Mapping 
 */
#ifndef ORTE_RMAPS_RR_H
#define ORTE_RMAPS_RR_H

#include "orte/mca/rmaps/rmaps.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * RMGR Component 
 */
struct orte_rmaps_round_robin_component_t {
    orte_rmaps_base_component_t super;
    int debug;
    int priority;
    bool bynode;
    bool per_node;
    bool n_per_node;
    bool no_use_local;
    bool oversubscribe;
    bool no_allocate_range;
};
typedef struct orte_rmaps_round_robin_component_t orte_rmaps_round_robin_component_t;

ORTE_MODULE_DECLSPEC extern orte_rmaps_round_robin_component_t mca_rmaps_round_robin_component;
extern orte_rmaps_base_module_t orte_rmaps_round_robin_module;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
