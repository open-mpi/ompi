/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_SOH_BASE_H
#define MCA_SOH_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
/* #include "mca/ns/ns_types.h" */
#include "mca/soh/soh.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OMPI_DECLSPEC    int orte_soh_base_open(void);
OMPI_DECLSPEC    int orte_soh_base_select(void);
OMPI_DECLSPEC    int orte_soh_base_close(void);

int orte_soh_base_get_proc_soh(orte_proc_state_t *state,
                               int *status,
                               orte_process_name_t *proc);
                                                              
int orte_soh_base_set_proc_soh(orte_process_name_t *proc,
                               orte_proc_state_t state,
                               int status);

int orte_soh_base_get_node_soh_not_available(orte_node_state_t *state,
                                             orte_cellid_t cell,
                                             char *nodename);

int orte_soh_base_set_node_soh_not_available(orte_cellid_t cell,
                                             char *nodename,
                                             orte_node_state_t state);

int orte_soh_base_begin_monitoring_not_available(orte_jobid_t job);


int orte_soh_base_module_finalize_not_available (void);

/*
 * globals that might be needed
 */

OMPI_DECLSPEC extern int orte_soh_base_output;
OMPI_DECLSPEC extern orte_soh_base_module_t orte_soh;  /* holds selected module's function pointers */
OMPI_DECLSPEC extern bool orte_soh_base_selected;

typedef struct orte_soh_base_t {
    int soh_output;
    ompi_list_t soh_components;
} orte_soh_base_t;

OMPI_DECLSPEC extern orte_soh_base_t orte_soh_base;


/*
 * external API functions will be documented in the mca/soh/soh.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
