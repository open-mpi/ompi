/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 *
 * Remote File Management (FileM) Interface
 *
 */

#ifndef MCA_FILEM_H
#define MCA_FILEM_H

#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/ns/ns.h"

#include "opal/class/opal_object.h"

/**
 * A set of flags that determine the type of the file
 * in question
 */
#define ORTE_FILEM_TYPE_FILE      0
#define ORTE_FILEM_TYPE_DIR       1
#define ORTE_FILEM_TYPE_UNKNOWN   2

/**
 * Definition of a file movement request
 * This will allow:
 *  - The movement of one or more files
 *  - to/from one or more processes
 * in a single call of the API function. Allowing the implementation
 * to optimize the sending/receiving of data.
 */
struct orte_filem_base_request_1_0_0_t {
    /** This is an object, so must have a super */
    opal_list_item_t super;
    
    /** Number of targets in the:
     * - local_targets
     * - remote_targets
     * - target_flags
     * arrays. One variable since they
     * are required to be all the same length
     */
    int num_targets;

    /** Local mapping of targets */
    char **local_targets;
    
    /** Remote mapping of targets */
    char **remote_targets;

    /** For each target, a flag regarding its type */
    int *target_flags;

    /** List of processes to send/receive from */
    orte_process_name_t *proc_name;
    /** Number of processes in that array */
    int num_procs;
};
typedef struct orte_filem_base_request_1_0_0_t orte_filem_base_request_1_0_0_t;
typedef struct orte_filem_base_request_1_0_0_t orte_filem_base_request_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_filem_base_request_t);

/**
 * Query function for FILEM components.
 * Returns a priority to rank it agaianst other available FILEM components.
 */
typedef struct orte_filem_base_module_1_0_0_t *
        (*orte_filem_base_component_query_1_0_0_fn_t)
        (int *priority);

/**
 * Module initialization function.
 * Returns ORTE_SUCCESS
 */
typedef int (*orte_filem_base_module_init_fn_t)
     (void);

/**
 * Module finalization function.
 * Returns ORTE_SUCCESS
 */
typedef int (*orte_filem_base_module_finalize_fn_t)
     (void);

/**
 * Put a file or directory on the remote machine
 *
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the files/directories to send, 
 *        the remote files/directories to use, and the processes to see the change.
 * 
 * @return ORTE_SUCCESS on successful file transer
 * @return ORTE_ERROR on failed file transfer
 */
typedef int (*orte_filem_base_put_fn_t)
     (orte_filem_base_request_t *request);

/**
 * Get a file from the remote machine
 *
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the files/directories to receive, 
 *        the remote files/directories to use, and the processes to see the change.
 * 
 * @return ORTE_SUCCESS on successful file transer
 * @return ORTE_ERROR on failed file transfer
 */
typedef int (*orte_filem_base_get_fn_t)
     (orte_filem_base_request_t *request);

/**
 * Remove a file from the remote machine
 * 
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the remote files/directories to remove, 
 *        the processes to see the change.
 *
 * @return ORTE_SUCCESS on success
 * @return ORTE_ERROR on fail
 */
typedef int (*orte_filem_base_rm_fn_t)
     (orte_filem_base_request_t *request);

/**
 * Structure for FILEM v1.0.0 components.
 */
struct orte_filem_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t filem_version;
    /** MCA base data */
    mca_base_component_data_1_0_0_t filem_data;

    /** Component Query for Selection Function */
    orte_filem_base_component_query_1_0_0_fn_t filem_query;
    
    /** Verbosity Level */
    int verbose;
    /** Output Handle for opal_output */
    int output_handle;
    /** Default Priority */
    int priority;
};
typedef struct orte_filem_base_component_1_0_0_t orte_filem_base_component_1_0_0_t;
typedef struct orte_filem_base_component_1_0_0_t orte_filem_base_component_t;

/**
 * Structure for FILEM v1.0.0 modules
 */
struct orte_filem_base_module_1_0_0_t {
    /** Initialization Function */
    orte_filem_base_module_init_fn_t           filem_init;
    /** Finalization Function */
    orte_filem_base_module_finalize_fn_t       filem_finalize;

    /** Put a file on the remote machine */
    orte_filem_base_put_fn_t                   put;
    /** Get a file from the remote machine */
    orte_filem_base_get_fn_t                   get;

    /** Remove a file on the remote machine */
    orte_filem_base_rm_fn_t                    rm;
};
typedef struct orte_filem_base_module_1_0_0_t orte_filem_base_module_1_0_0_t;
typedef struct orte_filem_base_module_1_0_0_t orte_filem_base_module_t;

ORTE_DECLSPEC extern orte_filem_base_module_t orte_filem;

/**
 * Macro for use in components that are of type FILEM v1.0.0
 */
#define ORTE_FILEM_BASE_VERSION_1_0_0 \
    /* FILEM v1.0 is chained to MCA v1.0 */ \
    MCA_BASE_VERSION_1_0_0, \
    /* FILEM v1.0 */ \
    "filem", 1, 0, 0

#endif /* ORTE_FILEM_H */

