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

#ifndef ORTE_RMGR_TYPES_H
#define ORTE_RMGR_TYPES_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * REGISTRY KEY NAMES FOR COMMON DATA
 */
#define ORTE_RMGR_LAUNCHER      "orte-rmgr-launcher"

/*
 * Constants for command values
 */
#define ORTE_RMGR_CMD_QUERY       1
#define ORTE_RMGR_CMD_CREATE      2
#define ORTE_RMGR_CMD_ALLOCATE    3
#define ORTE_RMGR_CMD_DEALLOCATE  4
#define ORTE_RMGR_CMD_MAP         5
#define ORTE_RMGR_CMD_LAUNCH      6
#define ORTE_RMGR_CMD_TERM_JOB    7
#define ORTE_RMGR_CMD_TERM_PROC   8
#define ORTE_RMGR_CMD_SPAWN       9

#define ORTE_RMGR_CMD  ORTE_UINT32
typedef uint32_t orte_rmgr_cmd_t;

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

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(orte_app_context_map_t);


/**
 * Information about a specific application to be launched in the RTE.
 */
typedef struct {
    /** Parent object */
    opal_object_t super;
    /** Unique index when multiple apps per job */
    size_t idx;
    /** Absolute pathname of argv[0] */
    char   *app;
    /** Number of copies of this process that are to be launched */
    size_t num_procs;
    /** Length of the argv array, not including final NULL entry */
    int argc;
    /** Standard argv-style array, including a final NULL pointer */
    char  **argv;
    /** Length of the env array, not including the final NULL entry */
    size_t num_env;
    /** Standard environ-style array, including a final NULL pointer */
    char  **env;
    /** Current working directory for this app */
    char   *cwd;
    /** Length of the map_data array, not including the final NULL entry */
    size_t num_map;
    /** Mapping data about how this app should be laid out across CPUs
        / nodes */
    orte_app_context_map_t **map_data;
} orte_app_context_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(orte_app_context_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
