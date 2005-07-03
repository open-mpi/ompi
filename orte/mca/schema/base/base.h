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
/** @file:
 *
 * The OpenRTE Schema.
 */

#ifndef ORTE_SCHEMA_BASE_H_
#define ORTE_SCHEMA_BASE_H_

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "opal/class/opal_list.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"

#include "mca/ns/ns_types.h"

#include "mca/schema/schema.h"

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    OMPI_DECLSPEC int orte_schema_base_open(void);
    OMPI_DECLSPEC int orte_schema_base_select(void);
    OMPI_DECLSPEC int orte_schema_base_close(void);

    /* general usage functions */
int orte_schema_base_get_proc_tokens(char ***tokens, size_t* num_tokens, orte_process_name_t *proc);
int orte_schema_base_get_node_tokens(char ***tokens, size_t* num_tokens, orte_cellid_t cellid, char *nodename);
int orte_schema_base_get_cell_tokens(char ***tokens, size_t* num_tokens, orte_cellid_t cellid);
int orte_schema_base_get_job_segment_name(char **name, orte_jobid_t jobid);
int orte_schema_base_extract_jobid_from_segment_name(orte_jobid_t *jobid, char *name);
int orte_schema_base_store_my_info(void);
int orte_schema_base_get_std_trigger_name(char **name,
                                          char *trigger,
                                          orte_jobid_t jobid);
int orte_schema_base_get_std_subscription_name(char **name,
                                               char *subscription,
                                               orte_jobid_t jobid);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * globals that might be needed inside the gpr
 */
extern int orte_schema_base_output;
extern bool orte_schema_base_selected;
extern opal_list_t orte_schema_base_components_available;
extern mca_schema_base_component_t orte_schema_base_selected_component;
extern bool orte_schema_initialized;

#endif
