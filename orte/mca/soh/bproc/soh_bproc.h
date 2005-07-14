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
 */
#ifndef ORTE_SOH_BPROC_H
#define ORTE_SOH_BPROC_H

#include <sys/bproc.h>

#include "mca/soh/soh.h"
#include "opal/event/event.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Module init/fini
 */
int orte_soh_bproc_module_init(void);
int orte_soh_bproc_module_finalize(void);

struct orte_soh_bproc_component_t {
    orte_soh_base_component_t super;
    int debug;
    int priority;
    opal_event_t notify_event;
    int notify_fd;
    orte_cellid_t cellid;
    struct bproc_node_set_t node_set;
};
typedef struct orte_soh_bproc_component_t orte_soh_bproc_component_t;

OMPI_COMP_EXPORT extern orte_soh_base_module_t orte_soh_bproc_module;
OMPI_COMP_EXPORT extern orte_soh_bproc_component_t mca_soh_bproc_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

