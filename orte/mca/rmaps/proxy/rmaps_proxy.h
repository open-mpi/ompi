/* -*- C -*-
 *
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
 *
 */
#ifndef ORTE_RMAPS_PROXY_H
#define ORTE_RMAPS_PROXY_H


#include "orte_config.h"
#include "orte/orte_types.h"

#include "orte/mca/ns/ns_types.h"

#include "orte/mca/rmaps/rmaps.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_rmaps_proxy_open(void);
int orte_rmaps_proxy_close(void);


/*
 * Startup / Shutdown
 */
orte_rmaps_base_module_t*
orte_rmaps_proxy_component_init(int *priority);

int orte_rmaps_proxy_finalize(void);

/*
 * globals used within the component
 */
typedef struct {
    int debug;
    orte_process_name_t *replica;
} orte_rmaps_proxy_globals_t;


extern orte_rmaps_proxy_globals_t orte_rmaps_proxy_globals;

/*
 * Component API functions
 */
int orte_rmaps_proxy_map(orte_jobid_t job, char *desired_mapper);

/*
 * Global component.
 */
ORTE_MODULE_DECLSPEC extern orte_rmaps_base_component_t mca_rmaps_proxy_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
