/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#ifndef ORTE_RMGR_TYPES_H
#define ORTE_RMGR_TYPES_H

#include "orte_config.h"

#include "opal/class/opal_object.h"

#include "orte/mca/gpr/gpr_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * REGISTRY KEY NAMES FOR COMMON DATA
 */
#define ORTE_RMGR_LAUNCHER      "orte-rmgr-launcher"

/*
 * RMGR ATTRIBUTES
 */
typedef orte_gpr_keyval_t orte_attribute_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_attribute_t);

/* define some booleans to make code more legible. These
 * control the action of the rmgr.merge_attributes function
 */
#define ORTE_RMGR_ATTR_NO_OVERRIDE  false
#define ORTE_RMGR_ATTR_OVERRIDE     true

/* define some useful attributes */
#define ORTE_RMGR_SPAWN_FLOW    "orte-rmgr-spawn"

/* flags that can be combined to script the RMGR's spawn procedure
 * These flags are used as the value to be associated with
 * the ORTE_RMGR_FLOW attribute. They can be OR'd together
 * to create any desired "flow" through the RMGR's spawn
 * procedure.
 */
#define ORTE_RMGR_SETUP            0x01
#define ORTE_RMGR_RES_DISC         0x02
#define ORTE_RMGR_ALLOC            0x04
#define ORTE_RMGR_MAP              0x08
#define ORTE_RMGR_SETUP_TRIGS      0x10
#define ORTE_RMGR_LAUNCH           0x20

/* direct the RMGR spawn procedure to use the provided jobid
 * instead of getting a new one
 */
#define ORTE_RMGR_USE_GIVEN_JOBID    "orte-rmgr-use-jobid"


/* RESOURCE MANAGER DATA TYPES */

/** Value for orte_app_context_map_t: the data is uninitialized (!) */
#define ORTE_APP_CONTEXT_MAP_INVALID     0
/** Value for orte_app_context_map_t: the data is a comma-delimited
    string of hostnames */
#define ORTE_APP_CONTEXT_MAP_HOSTNAME    1
/** Value for orte_app_context_map_t: the data is a comma-delimited
    list of architecture names */
#define ORTE_APP_CONTEXT_MAP_ARCH        2
/** Value for orte_app_context_map_t: the data is a comma-delimited
    list of C, cX, N, nX mappsing */
#define ORTE_APP_CONTEXT_MAP_CN          3

/**
 * Information about mapping requested by the user
 */
typedef struct {
    /** Parent object */
    opal_object_t super;
    /** One of the ORTE_APP_CONTEXT_MAP_* values */
    uint8_t map_type;
    /** String data */
    char *map_data;
} orte_app_context_map_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_app_context_map_t);


/**
 * Information about a specific application to be launched in the RTE.
 */
typedef struct {
    /** Parent object */
    opal_object_t super;
    /** Unique index when multiple apps per job */
    orte_std_cntr_t idx;
    /** Absolute pathname of argv[0] */
    char   *app;
    /** Number of copies of this process that are to be launched */
    orte_std_cntr_t num_procs;
    /** Standard argv-style array, including a final NULL pointer */
    char  **argv;
    /** Standard environ-style array, including a final NULL pointer */
    char  **env;
    /** Current working directory for this app */
    char   *cwd;
    /** Whether the cwd was set by the user or by the system */
    bool user_specified_cwd;
    /** Length of the map_data array, not including the final NULL entry */
    orte_std_cntr_t num_map;
    /** Mapping data about how this app should be laid out across CPUs
        / nodes */
    orte_app_context_map_t **map_data;
    /** Prefix directory for this app (or NULL if no override
        necessary) */
    char *prefix_dir;
} orte_app_context_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_app_context_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
