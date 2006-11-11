/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef MCA_ODLS_PRIVATE_H
#define MCA_ODLS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/class/opal_list.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rmgr/rmgr_types.h"
#include "orte/mca/smr/smr_types.h"

#include "orte/mca/odls/odls_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * General ODLS types
 */
    
/*
 * List object to locally store the process names and pids of
 * our children. This can subsequently be used to order termination
 * or pass signals without looking the info up again.
 */
typedef struct orte_odls_child_t {
    opal_list_item_t super;      /* required to place this on a list */
    orte_process_name_t *name;   /* the OpenRTE name of the proc */
    pid_t pid;                   /* local pid of the proc */
    orte_std_cntr_t app_idx;     /* index of the app_context for this proc */
    bool alive;                  /* is this proc alive? */
    orte_proc_state_t state;     /* the state of the process */
} orte_odls_child_t;
OBJ_CLASS_DECLARATION(orte_odls_child_t);
    
    
typedef struct orte_odls_globals_t {
    /** Verbose/debug output stream */
    int output;
    /** Time to allow process to forcibly die */
    int timeout_before_sigkill;
} orte_odls_globals_t;

ORTE_DECLSPEC extern orte_odls_globals_t orte_odls_globals;
        
ORTE_DECLSPEC int orte_odls_base_purge_environment(char ***environ);

ORTE_DECLSPEC int orte_odls_base_report_spawn(opal_list_t *children);

/*
 * data type functions
 */

int orte_odls_compare_daemon_cmd(orte_daemon_cmd_flag_t *value1, orte_daemon_cmd_flag_t *value2, orte_data_type_t type);

int orte_odls_copy_daemon_cmd(orte_daemon_cmd_flag_t **dest, orte_daemon_cmd_flag_t *src, orte_data_type_t type);

int orte_odls_pack_daemon_cmd(orte_buffer_t *buffer, void *src,
                              orte_std_cntr_t num_vals, orte_data_type_t type);

int orte_odls_print_daemon_cmd(char **output, char *prefix, orte_daemon_cmd_flag_t *src, orte_data_type_t type);

void orte_odls_std_release(orte_data_value_t *value);

int orte_odls_size_daemon_cmd(size_t *size, orte_daemon_cmd_flag_t *src, orte_data_type_t type);

int orte_odls_unpack_daemon_cmd(orte_buffer_t *buffer, void *dest,
                                orte_std_cntr_t *num_vals, orte_data_type_t type);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
