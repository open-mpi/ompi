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
/** @file:
 */

#ifndef MCA_NS_PRIVATE_H
#define MCA_NS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml_types.h"

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* default limits */
#define ORTE_NS_ARRAY_MAX_SIZE  INT_MAX
#define ORTE_NS_ARRAY_BLOCK_SIZE    512
/*
 * Internal definitions
 */
typedef uint8_t orte_ns_cmd_bitmask_t;
typedef uint8_t orte_ns_cmd_flag_t;

/*
 * packing type definitions
 */
/* CAUTION - any changes here must also change corresponding
 * typedefs above and in ns_types.h
 */
#define ORTE_NS_CMD      ORTE_INT8
#define ORTE_CELLID_T    ORTE_INT32
#define ORTE_NODEID_T    ORTE_INT32
#define ORTE_JOBID_T     ORTE_INT32
#define ORTE_VPID_T      ORTE_INT32

/*
 * define flag values for remote commands - only used internally
 */
#define ORTE_NS_CREATE_CELLID_CMD       (int8_t)  1
#define ORTE_NS_GET_CELL_INFO_CMD       (int8_t)  2
#define ORTE_NS_CREATE_NODEID_CMD       (int8_t)  3
#define ORTE_NS_GET_NODE_INFO_CMD       (int8_t)  4
#define ORTE_NS_CREATE_JOBID_CMD        (int8_t)  5
#define ORTE_NS_GET_JOB_DESC_CMD        (int8_t)  6
#define ORTE_NS_GET_JOB_CHILD_CMD       (int8_t)  7
#define ORTE_NS_GET_ROOT_JOB_CMD        (int8_t)  8
#define ORTE_NS_GET_PARENT_JOB_CMD      (int8_t)  9
#define ORTE_NS_RESERVE_RANGE_CMD       (int8_t) 10
#define ORTE_NS_ASSIGN_OOB_TAG_CMD      (int8_t) 11
#define ORTE_NS_GET_PEERS_CMD           (int8_t) 12
#define ORTE_NS_DEFINE_DATA_TYPE_CMD    (int8_t) 13
#define ORTE_NS_CREATE_MY_NAME_CMD      (int8_t) 14
#define ORTE_NS_DUMP_CELLS_CMD          (int8_t) 15
#define ORTE_NS_DUMP_JOBIDS_CMD         (int8_t) 16
#define ORTE_NS_DUMP_TAGS_CMD           (int8_t) 17
#define ORTE_NS_DUMP_DATATYPES_CMD      (int8_t) 18


/*
 * Base functions that are common to all implementations - can be overridden
 */

ORTE_DECLSPEC    int orte_ns_base_create_process_name(orte_process_name_t **name,
                                  orte_cellid_t cell,
                                  orte_jobid_t job,
                                  orte_vpid_t vpid);

ORTE_DECLSPEC    int orte_ns_base_convert_string_to_process_name(orte_process_name_t **name,
                                             const char* name_string);

ORTE_DECLSPEC    int orte_ns_base_get_proc_name_string(char **name_string,
                                   const orte_process_name_t* name);

ORTE_DECLSPEC    int orte_ns_base_get_vpid_string(char **vpid_string, const orte_process_name_t* name);

ORTE_DECLSPEC    int orte_ns_base_convert_vpid_to_string(char **vpid_string, const orte_vpid_t vpid);

ORTE_DECLSPEC    int orte_ns_base_convert_string_to_vpid(orte_vpid_t *vpid, const char* vpidstring);

ORTE_DECLSPEC    int orte_ns_base_get_jobid_string(char **jobid_string, const orte_process_name_t* name);

ORTE_DECLSPEC    int orte_ns_base_convert_jobid_to_string(char **jobid_string, const orte_jobid_t jobid);

ORTE_DECLSPEC    int orte_ns_base_convert_string_to_jobid(orte_jobid_t *jobid, const char* jobidstring);

ORTE_DECLSPEC    int orte_ns_base_get_cellid_string(char **cellid_string, const orte_process_name_t* name);

ORTE_DECLSPEC    int orte_ns_base_convert_string_to_cellid(orte_cellid_t *cellid, const char *cellidstring);

ORTE_DECLSPEC    int orte_ns_base_convert_cellid_to_string(char **cellid_string, const orte_cellid_t cellid);

ORTE_DECLSPEC    int orte_ns_base_get_vpid(orte_vpid_t *vpid, const orte_process_name_t* name);

ORTE_DECLSPEC    int orte_ns_base_get_jobid(orte_jobid_t *jobid, const orte_process_name_t* name);

ORTE_DECLSPEC    int orte_ns_base_get_cellid(orte_cellid_t *cellid, const orte_process_name_t* name);

ORTE_DECLSPEC    int orte_ns_base_convert_string_to_nodeid(orte_nodeid_t *cellid, const char *string);

ORTE_DECLSPEC    int orte_ns_base_convert_nodeid_to_string(char **nodeid_string, const orte_nodeid_t nodeid);

ORTE_DECLSPEC    int orte_ns_base_compare_fields(orte_ns_cmp_bitmask_t fields,
                                                 const orte_process_name_t* name1,
                                                 const orte_process_name_t* name2);

ORTE_DECLSPEC    int orte_ns_base_print_dump(orte_buffer_t *buffer);


/* not available functions */
ORTE_DECLSPEC    int orte_ns_base_module_init_not_available(void);

ORTE_DECLSPEC    int orte_ns_base_create_cellid_not_available(orte_cellid_t *cellid,
                                    char *site, char *resource);

ORTE_DECLSPEC    int orte_ns_base_get_cell_info_not_available(orte_cellid_t cellid,
                                char **site, char **resource);

ORTE_DECLSPEC    int orte_ns_base_create_nodeids_not_available(orte_nodeid_t **nodeids, orte_std_cntr_t *nnodes,
                                                               orte_cellid_t cellid, char **nodename);

ORTE_DECLSPEC    int orte_ns_base_get_node_info_not_available(char ***nodename, orte_cellid_t cellid,
                                                              orte_std_cntr_t num_nodes, orte_nodeid_t *nodeids);

ORTE_DECLSPEC    int orte_ns_base_create_jobid_not_available(orte_jobid_t *jobid, opal_list_t *attrs);

ORTE_DECLSPEC    int orte_ns_base_get_job_descendants_not_available(orte_jobid_t** descendants,
                                                                    orte_std_cntr_t *num_desc,
                                                                    orte_jobid_t job);

ORTE_DECLSPEC    int orte_ns_base_get_job_children_not_available(orte_jobid_t** children,
                                                                 orte_std_cntr_t *num_childs,
                                                                 orte_jobid_t job);

ORTE_DECLSPEC    int orte_ns_base_get_root_job_not_available(orte_jobid_t *root_job, orte_jobid_t job);

ORTE_DECLSPEC    int orte_ns_base_get_parent_job_not_available(orte_jobid_t *parent, orte_jobid_t job);

ORTE_DECLSPEC    int orte_ns_base_get_vpid_range_not_available(orte_jobid_t job,
                                                               orte_vpid_t range,
                                                               orte_vpid_t *startvpid);

ORTE_DECLSPEC    int orte_ns_base_assign_rml_tag_not_available(orte_rml_tag_t *tag, char *name);

ORTE_DECLSPEC    int orte_ns_base_define_data_type_not_available(
                                  const char *name,
                                  orte_data_type_t *type);

ORTE_DECLSPEC    int orte_ns_base_create_my_name_not_available(void);

ORTE_DECLSPEC    int orte_ns_base_get_peers_not_available(orte_process_name_t **procs,
                                                          orte_std_cntr_t *num_procs, opal_list_t *attributes);

ORTE_DECLSPEC    int orte_ns_base_dump_cells_not_available(void);
ORTE_DECLSPEC    int orte_ns_base_dump_jobs_not_available(void);
ORTE_DECLSPEC    int orte_ns_base_dump_tags_not_available(void);
ORTE_DECLSPEC    int orte_ns_base_dump_datatypes_not_available(void);

/* Base functions used everywhere */
ORTE_DECLSPEC    int orte_ns_base_pack_name(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_pack_cellid(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_pack_nodeid(orte_buffer_t *buffer, void *src,
                                              orte_std_cntr_t num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_pack_jobid(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_pack_vpid(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_unpack_name(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_unpack_cellid(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_unpack_nodeid(orte_buffer_t *buffer, void *dest,
                                                orte_std_cntr_t *num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_unpack_jobid(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type);

ORTE_DECLSPEC    int orte_ns_base_unpack_vpid(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type);

/*
 * copy functions
 */

int orte_ns_base_copy_name(orte_process_name_t **dest, orte_process_name_t *src, orte_data_type_t type);

int orte_ns_base_copy_vpid(orte_vpid_t **dest, orte_vpid_t *src, orte_data_type_t type);

int orte_ns_base_copy_cellid(orte_cellid_t **dest, orte_cellid_t *src, orte_data_type_t type);

int orte_ns_base_copy_nodeid(orte_nodeid_t **dest, orte_nodeid_t *src, orte_data_type_t type);

int orte_ns_base_copy_jobid(orte_jobid_t **dest, orte_jobid_t *src, orte_data_type_t type);

/*
 * compare functions
 */

int orte_ns_base_compare_name(orte_process_name_t *value1,
                              orte_process_name_t *value2,
                              orte_data_type_t type);


int orte_ns_base_compare_vpid(orte_vpid_t *value1,
                              orte_vpid_t *value2,
                              orte_data_type_t type);

int orte_ns_base_compare_jobid(orte_jobid_t *value1,
                               orte_jobid_t *value2,
                               orte_data_type_t type);

int orte_ns_base_compare_cellid(orte_cellid_t *value1,
                                orte_cellid_t *value2,
                                orte_data_type_t type);

int orte_ns_base_compare_nodeid(orte_nodeid_t *value1,
                                orte_nodeid_t *value2,
                                orte_data_type_t type);

/*
 * size functions
 */

int orte_ns_base_std_size(size_t *size, void *src, orte_data_type_t type);

/*
 * release functions
 */

void orte_ns_base_std_release(orte_data_value_t *value);

/*
 * print functions
 */

int orte_ns_base_std_print(char **output, char *prefix, void *src, orte_data_type_t type);

int orte_ns_base_print_name(char **output, char *prefix, orte_process_name_t *name, orte_data_type_t type);


/*
 * external API functions will be documented in the mca/ns/ns.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
