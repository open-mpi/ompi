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

#ifndef ORTE_RDS_PRIVATE_H
#define ORTE_RDS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml_types.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* Define the RDS command flag */
typedef uint8_t orte_rds_cmd_flag_t;
#define ORTE_RDS_CMD	ORTE_UINT8

/* define some commands */
#define ORTE_RDS_QUERY_CMD	0x01
    
/*
 * API function definitions
 */
ORTE_DECLSPEC int orte_rds_base_query(orte_jobid_t job);

/*
 * oob interface
 */
int orte_rds_base_comm_start(void);
int orte_rds_base_comm_stop(void);

void orte_rds_base_recv(int status, orte_process_name_t* sender,
                        orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata);

/*
 * utility functions for use within the RDS
 */
ORTE_DECLSPEC int orte_rds_base_store_resource(opal_list_t *resource_list);

/*
 * the "null" component functions
 */
int orte_rds_base_no_op_query(orte_jobid_t job);
int orte_rds_base_no_op_store_resource(opal_list_t *resource_list);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
