/*
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013      Inria.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_PROC_H
#define OPAL_PROC_H

#include "opal_config.h"
#include "opal/class/opal_list.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/dss/dss.h"

/**
 * This is a transparent handle proposed to the upper layer as a mean
 * to store whatever information it needs in order to efficiently
 * retrieve the RTE process naming scheme, and get access to the RTE
 * information associated with it. The only direct usage of this type
 * is to be copied from one structure to another, otherwise it should
 * only be used via the accessors defined below.
 */
typedef opal_identifier_t opal_process_name_t;

#define OPAL_PROCESS_NAME_NTOH(guid)
#define OPAL_PROCESS_NAME_HTON(guid)

typedef struct opal_proc_t {
    /** allow proc to be placed on a list */
    opal_list_item_t                super;
    /** this process' name */
    opal_process_name_t             proc_name;
    /** architecture of this process */
    uint32_t                        proc_arch;
    /** flags for this proc */
    opal_hwloc_locality_t           proc_flags;
    /** Base convertor for the proc described by this process */
    struct opal_convertor_t*        proc_convertor;
    /** A pointer to the name of this host - data is
     * actually stored outside of this framework.  */
    char*                           proc_hostname;
} opal_proc_t;
OBJ_CLASS_DECLARATION(opal_proc_t);

typedef struct opal_process_info_t {
    char *nodename;                     /**< string name for this node */
    char *job_session_dir;              /**< Session directory for job */
    char *proc_session_dir;             /**< Session directory for the process */
    int32_t num_local_peers;            /**< number of procs from my job that share my node with me */
    int32_t my_local_rank;    /**< local rank */
#if OPAL_HAVE_HWLOC
    char *cpuset;                       /**< String-representation of bitmap where we are bound */
#endif
} opal_process_info_t;
OPAL_DECLSPEC extern opal_process_info_t opal_process_info;

OPAL_DECLSPEC extern opal_proc_t* opal_proc_local_get(void);
OPAL_DECLSPEC extern int opal_proc_local_set(opal_proc_t* proc);

/**
 * Compare two processor name and return an integer greater than,
 * equal to, or less than 0, according as the proc_name of proc1
 * is greater than, equal to, or less than the proc_name of proc2.
 */
typedef int (*opal_compare_proc_fct_t)(const opal_process_name_t, const opal_process_name_t);
OPAL_DECLSPEC extern opal_compare_proc_fct_t opal_compare_proc;

OPAL_DECLSPEC extern char* (*opal_process_name_print)(const opal_process_name_t);
OPAL_DECLSPEC extern int32_t (*opal_process_name_vpid)(const opal_process_name_t);
OPAL_DECLSPEC extern int32_t (*opal_process_name_jobid)(const opal_process_name_t);

#define OPAL_NAME_PRINT(OPAL_PN)    opal_process_name_print(OPAL_PN)
#define OPAL_PROC_MY_NAME           (opal_proc_local_get()->proc_name)
#define OPAL_PROC_MY_HOSTNAME       (opal_proc_local_get()->proc_hostname)

/**
 * Access to the modex.
 */
OPAL_DECLSPEC extern int
(*opal_modex_send)(const mca_base_component_t *source_component,
                   const void *data, size_t size);
OPAL_DECLSPEC extern int
(*opal_modex_recv)(const mca_base_component_t *component,
                   const opal_proc_t *proc,
                   void **buffer, size_t *size);

#endif  /* OPAL_PROC_H */
