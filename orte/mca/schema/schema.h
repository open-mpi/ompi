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

#ifndef ORTE_SCHEMA_H
#define ORTE_SCHEMA_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/ns/ns_types.h"

#include "orte/mca/schema/schema_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Convenience functions for accessing ORTE data
 */
typedef int (*orte_schema_get_proc_tokens_fn_t)(
    char ***tokens, 
    orte_std_cntr_t* num_tokens, 
    orte_process_name_t *proc);

typedef int (*orte_schema_get_job_tokens_fn_t)(
    char ***tokens, 
    orte_std_cntr_t* num_tokens, 
    orte_jobid_t jobid);

typedef int (*orte_schema_get_node_tokens_fn_t)(
    char ***tokens, 
    orte_std_cntr_t* num_tokens, 
    orte_cellid_t cellid, 
    char *nodename);

typedef int (*orte_schema_get_cell_tokens_fn_t)(
    char ***tokens, 
    orte_std_cntr_t* num_tokens, 
    orte_cellid_t cellid);

typedef int (*orte_schema_get_job_segment_name_fn_t)(char **name, orte_jobid_t jobid);

typedef int (*orte_schema_extract_jobid_from_segment_name_fn_t)(orte_jobid_t *jobid, char *name);

typedef int (*orte_schema_get_std_trigger_name_fn_t)(char **name,
                    char *trigger,
                    orte_jobid_t jobid);

typedef int (*orte_schema_extract_jobid_from_std_trigger_name_fn_t)(orte_jobid_t *jobid,
                    char *trigger);

typedef bool (*orte_schema_check_std_trigger_name_fn_t)(char *name, char *trigger);

typedef int (*orte_schema_get_std_subscription_name_fn_t)(char **name,
                    char *subscription,
                    orte_jobid_t jobid);


/*
 * Ver 1.0.0
 */
struct orte_schema_base_module_1_0_0_t {
    orte_schema_get_proc_tokens_fn_t get_proc_tokens;
    orte_schema_get_node_tokens_fn_t get_node_tokens;
    orte_schema_get_job_tokens_fn_t get_job_tokens;
    orte_schema_get_cell_tokens_fn_t get_cell_tokens;
    orte_schema_get_job_segment_name_fn_t get_job_segment_name;
    orte_schema_extract_jobid_from_segment_name_fn_t extract_jobid_from_segment_name;
    orte_schema_get_std_trigger_name_fn_t get_std_trigger_name;
    orte_schema_check_std_trigger_name_fn_t check_std_trigger_name;
    orte_schema_extract_jobid_from_std_trigger_name_fn_t extract_jobid_from_std_trigger_name;
    orte_schema_get_std_subscription_name_fn_t get_std_subscription_name;
};


typedef struct orte_schema_base_module_1_0_0_t orte_schema_base_module_1_0_0_t;
typedef orte_schema_base_module_1_0_0_t orte_schema_base_module_t;

/*
 * SCHEMA Component
 */

typedef orte_schema_base_module_t* (*orte_schema_base_component_init_fn_t)(
    bool *allow_multi_user_threads,
    bool *have_hidden_threads,
    int *priority);

typedef int (*orte_schema_base_component_finalize_fn_t)(void);
 
/*
 * the standard component data structure
 */

struct mca_schema_base_component_1_0_0_t {
    mca_base_component_t schema_version;
    mca_base_component_data_1_0_0_t schema_data;

    orte_schema_base_component_init_fn_t schema_init;
    orte_schema_base_component_finalize_fn_t schema_finalize;
};
typedef struct mca_schema_base_component_1_0_0_t mca_schema_base_component_1_0_0_t;
typedef mca_schema_base_component_1_0_0_t mca_schema_base_component_t;



/*
 * Macro for use in components that are of type schema v1.0.0
 */
#define ORTE_SCHEMA_BASE_VERSION_1_0_0 \
  /* schema v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* schema v1.0 */ \
  "schema", 1, 0, 0

ORTE_DECLSPEC extern orte_schema_base_module_t orte_schema;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
