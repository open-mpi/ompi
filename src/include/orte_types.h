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
/** @file */

#ifndef ORTE_TYPES_H
#define ORTE_TYPES_H
#include "class/ompi_object.h"

/**
 * Supported datatypes for messaging and storage operations.
 * 
 * ANY CHANGES TO THESE DEFINITIONS MUST BE REFLECTED IN THE TEXT ARRAY
 * orte_data_strings DEFINED IN src/runtime/orte_init.c.
 *
 */

typedef uint8_t orte_data_type_t ;

#define    ORTE_BYTE                (orte_data_type_t)    1 /**< a byte of data */
#define    ORTE_BOOL                (orte_data_type_t)    2 /**< boolean */
#define    ORTE_STRING              (orte_data_type_t)    3 /**< a NULL terminated string */
#define    ORTE_SIZE                (orte_data_type_t)    4 /**< the generic size_t */
    /* all the integer flavors */
#define    ORTE_INT                 (orte_data_type_t)    5 /**< generic integer */
#define    ORTE_INT8                (orte_data_type_t)    6 /**< an 8-bit integer */
#define    ORTE_INT16               (orte_data_type_t)    7 /**< a 16-bit integer */
#define    ORTE_INT32               (orte_data_type_t)    8 /**< a 32-bit integer */
#define    ORTE_INT64               (orte_data_type_t)    9 /**< a 64-bit integer */
    /* all the unsigned integer flavors */
#define    ORTE_UINT                (orte_data_type_t)   10 /**< generic unsigned integer */
#define    ORTE_UINT8               (orte_data_type_t)   11 /**< an 8-bit unsigned integer */
#define    ORTE_UINT16              (orte_data_type_t)   12 /**< a 16-bit unsigned integer */
#define    ORTE_UINT32              (orte_data_type_t)   13 /**< a 32-bit unsigned integer */
#define    ORTE_UINT64              (orte_data_type_t)   14 /**< a 64-bit unsigned integer */
    /* all the floating point flavors */
#define    ORTE_FLOAT               (orte_data_type_t)   15 /**< single-precision float */
#define    ORTE_FLOAT4              (orte_data_type_t)   16 /**< 4-byte float - usually equiv to single */
#define    ORTE_DOUBLE              (orte_data_type_t)   17 /**< double-precision float */
#define    ORTE_FLOAT8              (orte_data_type_t)   18 /**< 8-byte float - usually equiv to double */
#define    ORTE_LONG_DOUBLE         (orte_data_type_t)   19 /**< long-double precision float */
#define    ORTE_FLOAT12             (orte_data_type_t)   20 /**< 12-byte float - used as long-double on some systems */
#define    ORTE_FLOAT16             (orte_data_type_t)   21 /**< 16-byte float - used as long-double on some systems */
    /* orte-specific typedefs */
#define    ORTE_NAME                (orte_data_type_t)   22 /**< an ompi_process_name_t */
#define    ORTE_VPID                (orte_data_type_t)   23 /**< a vpid */
#define    ORTE_JOBID               (orte_data_type_t)   24 /**< a jobid */
#define    ORTE_CELLID              (orte_data_type_t)   25 /**< a cellid */
#define    ORTE_NODE_STATE          (orte_data_type_t)   26 /**< node status flag */
#define    ORTE_PROC_STATE          (orte_data_type_t)   27 /**< process/resource status */
#define    ORTE_EXIT_CODE           (orte_data_type_t)   28 /**< process exit code */
#define    ORTE_BYTE_OBJECT         (orte_data_type_t)   29 /**< byte object structure */
#define    ORTE_KEYVAL              (orte_data_type_t)   30 /**< registry key-value pair */
#define    ORTE_NOTIFY_ACTION       (orte_data_type_t)   31 /**< registry notify action */
#define    ORTE_GPR_CMD             (orte_data_type_t)   32 /**< registry command */
#define    ORTE_GPR_NOTIFY_ID       (orte_data_type_t)   33 /**< registry notify id tag */
#define    ORTE_GPR_VALUE           (orte_data_type_t)   34 /**< registry return value */
#define    ORTE_DATA_TYPE           (orte_data_type_t)   35 /**< data type */
#define    ORTE_APP_CONTEXT         (orte_data_type_t)   36 /**< argv and enviro arrays */
#define    ORTE_APP_CONTEXT_MAP     (orte_data_type_t)   37 /**< application context mapping array */
#define    ORTE_GPR_ADDR_MODE       (orte_data_type_t)   38 /**< Addressing mode for registry cmds */
#define    ORTE_GPR_SUBSCRIPTION    (orte_data_type_t)   39 /**< describes data returned by subscription */
#define    ORTE_GPR_NOTIFY_DATA     (orte_data_type_t)   40 /**< data returned from a subscription */
#define    ORTE_NULL                (orte_data_type_t)   41 /**< don't interpret data type */


typedef struct {
    size_t size;
    uint8_t *bytes;
} orte_byte_object_t;


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
    ompi_object_t super;
    /** One of the ORTE_APP_CONTEXT_MAP_* values */
    uint8_t map_type;
    /** String data */
    char *map_data;
} orte_app_context_map_t;

OBJ_CLASS_DECLARATION(orte_app_context_map_t);


/**
 * Information about a specific application to be launched in the RTE.
 */
typedef struct {
    /** Parent object */
    ompi_object_t super;
    /** Unique index when multiple apps per job */
    int32_t idx;
    /** Absolute pathname of argv[0] */
    char   *app;
    /** Number of copies of this process that are to be launched */
    int32_t num_procs;
    /** Length of the argv array, not including final NULL entry */
    int32_t argc;
    /** Standard argv-style array, including a final NULL pointer */
    char  **argv;
    /** Length of the env array, not including the final NULL entry */
    int32_t num_env;
    /** Standard environ-style array, including a final NULL pointer */
    char  **env;
    /** Current working directory for this app */
    char   *cwd;
    /** Length of the map_data array, not including the final NULL entry */
    int32_t num_map;
    /** Mapping data about how this app should be laid out across CPUs
        / nodes */
    orte_app_context_map_t **map_data;
} orte_app_context_t;


OBJ_CLASS_DECLARATION(orte_app_context_t);

#endif
