/* -*- C -*-
 *
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
/**
 * @file
 *
 * Name Discovery 
 *
 * Since each launcher/environment can be different, the NDS provides a mechanism
 * by which writers of PLS launchers can provide the necessary corresponding logic
 * for process name discovery. Accordingly, each PLS MUST:
 * 
 * - set the environmental parameter OMPI_MCA_ns_nds to indicate the
 * name discovery facility used by the launcher.
 * 
 * - have a corresponding entry in the orte_ns_base_nds table (defined in
 * src/ns/base/ns_base_nds.c) that identifies the mode and its associated
 * function for obtaining the process name. This information is stored in an array
 * of orte_ns_base_nds_t structures - each of which contains the string name of the
 * mode (that must correspond exactly with the value of the environmental
 * parameter OMPI_MCA_ns_nds as set by the launcher) and a function pointer
 * to the necessary name discovery function.
 *
 * WARNING: Be sure to increment the orte_plsnds_max value so that the name discovery
 * subsystem will correctly search the entire array.
 * 
 */

#ifndef ORTE_NS_BASE_NDS_H_
#define ORTE_NS_BASE_NDS_H_

#include "orte_config.h"
#include "include/orte_types.h"
#include "include/orte_constants.h"
#include "mca/ns/ns_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Define the nds function pointer
 */
typedef int (*orte_ns_nds_fn_t)(void);

/*
 * Define the nds structure
 */
typedef struct {
    char *mode;
    orte_ns_nds_fn_t discover;
} orte_ns_nds_t;


/*
 * Name discovery mechanisms
 */

int orte_ns_nds_env_get(void);
int orte_ns_nds_env_put(const orte_process_name_t* proc, 
                        orte_vpid_t vpid_start, size_t num_procs,
                        char ***environ);

int orte_ns_nds_pipe_get(void);
int orte_ns_nds_pipe_put(const orte_process_name_t* proc, orte_vpid_t vpid_start, size_t num_procs);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLSNDS_H */
