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
 * Copyright (c) 2008      Voltaire. All rights reserved
 * 
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


#include "orte_config.h"
#include "opal_config.h"
#include "opal/util/argv.h"
#include "opal/mca/paffinity/paffinity.h"

#ifndef ORTE_RMAPS_RF_H
#define ORTE_RMAPS_RF_H

#include "orte/mca/rmaps/rmaps.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * RMGR Component 
 */
struct orte_rmaps_rank_file_component_t {
    orte_rmaps_base_component_t super;
    int debug;
    int priority;
};
typedef struct orte_rmaps_rank_file_component_t orte_rmaps_rank_file_component_t;

ORTE_MODULE_DECLSPEC extern orte_rmaps_rank_file_component_t mca_rmaps_rank_file_component;
extern orte_rmaps_base_module_t orte_rmaps_rank_file_module;
extern char *orte_mca_rmaps_rank_file_slot_list;


extern char *orte_rmaps_rank_file_path;

typedef struct cpu_socket_t cpu_socket_t;

struct orte_rmaps_rank_file_map_t {
    int rank;
    char* node_name;
    char* slot_list;
};
typedef struct orte_rmaps_rank_file_map_t orte_rmaps_rank_file_map_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_rmaps_rank_file_map_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
