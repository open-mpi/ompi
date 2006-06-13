/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 *
 */
/**
 * @file:
 * Header file for the xcpu launcher. This will use xcpu to launch jobs on
 * the list of nodes that it will get from RAS (resource allocation
 * system
 * -# pls_xcpu is called by orterun. It first setsup environment for the 
 *  process to be launched on remote node, then reads the ompi registry and 
 *  then launch the binary on the nodes specified in the registry.
 */

#ifndef ORTE_PLS_XCPU_H_
#define ORTE_PLS_XCPUC_H_

#include "orte_config.h"
#include "orte/class/orte_pointer_array.h"
#include "orte/orte_constants.h"
#include "orte/mca/pls/base/base.h"
#include "orte/util/proc_info.h"
#include "opal/threads/condition.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close -- defined in component file
 */
int orte_pls_xcpu_component_open(void);
int orte_pls_xcpu_component_close(void);

/*
 * Startup / Shutdown
 */
orte_pls_base_module_t* orte_pls_xcpu_init(int *priority); /* in component file */

/*
 * Interface
 */
int orte_pls_xcpu_launch(orte_jobid_t);
int orte_pls_xcpu_terminate_job(orte_jobid_t);
int orte_pls_xcpu_terminate_proc(const orte_process_name_t* proc_name);
int orte_pls_xcpu_finalize(void);


/**
 * (P)rocess (L)aunch (S)ubsystem xcpu Component
 */
struct orte_pls_xcpu_component_t {
    /*base_class this is needed others below this may or may not*/
    orte_pls_base_component_t super;

    int debug;            /* If greater than 0 print debugging information */
    int priority;         /* The priority of this component. This will be returned if
                           * we determine that xcpu is available and running on this node,
                           */
    int terminate_sig;    /* The signal that gets sent to a process to kill it. */
    size_t num_daemons;   /* The number of daemons that are currently running. */
    orte_pointer_array_t * daemon_names;
    opal_mutex_t lock;    /* Lock used to prevent some race conditions */
    opal_condition_t condition;   /* Condition that is signaled when all the daemons have died */
    orte_cellid_t cellid;
};
typedef struct orte_pls_xcpu_component_t orte_pls_xcpu_component_t;

struct orte_pls_xcpu_tid_stack {
    int tid;
    struct orte_pls_xcpu_tid_stack *next;
};
typedef struct orte_pls_xcpu_tid_stack orte_pls_xcpu_tid_stack;

struct orte_pls_xcpu_mount_nodes{
    char *name;
    struct orte_pls_xcpu_mount_nodes *next;
};
typedef struct orte_pls_xcpu_mount_nodes orte_pls_xcpu_mount_nodes;

struct orte_pls_xcpu_thread_info{
    orte_pls_xcpu_mount_nodes local_mounts;/* can have only *name */
    char *binary;
    char *argv;
    char **env;
    orte_process_name_t *peers;
};
typedef struct orte_pls_xcpu_thread_info orte_pls_xcpu_thread_info;

struct orte_pls_xcpu_stdio_thread_info{
    char *stdio_path;
    int outdes;
};
typedef struct orte_pls_xcpu_stdio_thread_info orte_pls_xcpu_stdio_thread_info;

struct orte_pls_xcpu_pthread_tindex{
    pthread_t *tids;
    int index;
};
typedef struct orte_pls_xcpu_pthread_tindex orte_pls_xcpu_pthread_tindex;

ORTE_DECLSPEC extern orte_pls_xcpu_component_t mca_pls_xcpu_component;
ORTE_DECLSPEC extern orte_pls_base_module_t orte_pls_xcpu_module; /* this is defined in pls_xcpu.c file */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_XCPU_H_ */

