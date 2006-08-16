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
/**
 * @file
 */
#ifndef ORTE_SMR_BPROC_H
#define ORTE_SMR_BPROC_H

#include <sys/bproc.h>

#include "orte/mca/smr/smr.h"
#include "opal/event/event.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Bproc node registry keys
 */
#define ORTE_SMR_BPROC_NODE_STATUS      "orte-node-bproc-status"
#define ORTE_SMR_BPROC_NODE_MODE        "orte-node-bproc-mode"
#define ORTE_SMR_BPROC_NODE_USER        "orte-node-bproc-user"
#define ORTE_SMR_BPROC_NODE_GROUP       "orte-node-bproc-group"


/**
 * Module init/fini
 */
int orte_smr_bproc_module_init(void);
int orte_smr_bproc_module_finalize(void);

struct orte_smr_bproc_component_t {
    orte_smr_base_component_t super;
    int debug;
    int priority;
    opal_event_t notify_event;
    int notify_fd;
    orte_cellid_t cellid;
    struct bproc_node_set_t node_set;
};
typedef struct orte_smr_bproc_component_t orte_smr_bproc_component_t;

OMPI_COMP_EXPORT extern orte_smr_base_module_t orte_smr_bproc_module;
OMPI_COMP_EXPORT extern orte_smr_bproc_component_t mca_smr_bproc_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

