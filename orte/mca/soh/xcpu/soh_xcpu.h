/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef ORTE_SOH_XCPU_H
#define ORTE_SOH_XCPU_H

#include "orte/mca/soh/soh.h"
#include "opal/event/event.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Bproc node registry keys
 */
#define ORTE_SOH_XCPU_NODE_STATUS      "orte-node-xcpu-status"
#define ORTE_SOH_XCPU_NODE_MODE        "orte-node-xcpu-mode"
#define ORTE_SOH_XCPU_NODE_USER        "orte-node-xcpu-user"
#define ORTE_SOH_XCPU_NODE_GROUP       "orte-node-xcpu-group"


/**
 * Module init/fini
 */
int orte_soh_xcpu_module_init(void);
int orte_soh_xcpu_module_finalize(void);

struct orte_soh_xcpu_component_t {
    orte_soh_base_component_t super;
    /* not sure which of the following variabels are 
     * needed
     * */
    int debug;
    int priority;
    opal_event_t notify_event;
    int notify_fd;
    orte_cellid_t cellid;
    /*struct xcpu_node_set_t node_set;*/
};
typedef struct orte_soh_xcpu_component_t orte_soh_xcpu_component_t;

OMPI_COMP_EXPORT extern orte_soh_base_module_t orte_soh_xcpu_module;
OMPI_COMP_EXPORT extern orte_soh_xcpu_component_t mca_soh_xcpu_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
