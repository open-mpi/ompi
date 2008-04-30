/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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
 *
 * Populates global structure with system-specific information.
 *
 * Notes: add limits.h, compute size of integer and other types via sizeof(type)*CHAR_BIT
 *
 */

#ifndef _ORTE_UTIL_COMM_H_
#define _ORTE_UTIL_COMM_H_

#include "orte_config.h"
#include "orte/types.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

ORTE_DECLSPEC int orte_util_comm_query_job_info(const orte_process_name_t *hnp, orte_jobid_t job,
                                                int *num_jobs, orte_job_t ***job_info_array);

ORTE_DECLSPEC int orte_util_comm_query_node_info(const orte_process_name_t *hnp, char *node,
                                                 int *num_nodes, orte_node_t ***node_info_array);

ORTE_DECLSPEC int orte_util_comm_query_proc_info(const orte_process_name_t *hnp, orte_jobid_t job, orte_vpid_t vpid,
                                                 int *num_procs, orte_proc_t ***proc_info_array);

ORTE_DECLSPEC int orte_util_comm_attach_stdout(const orte_process_name_t *hnp,
                                               orte_jobid_t job, orte_vpid_t vpid, int fd);

ORTE_DECLSPEC int orte_util_comm_attach_stderr(const orte_process_name_t *hnp,
                                               orte_jobid_t job, orte_vpid_t vpid, int fd);

ORTE_DECLSPEC int orte_util_comm_detach_stdout(const orte_process_name_t *hnp,
                                               orte_jobid_t job, orte_vpid_t vpid);

ORTE_DECLSPEC int orte_util_comm_detach_stderr(const orte_process_name_t *hnp,
                                               orte_jobid_t job, orte_vpid_t vpid);

ORTE_DECLSPEC int orte_util_comm_spawn_job(const orte_process_name_t *hnp, orte_job_t *jdata);

ORTE_DECLSPEC int orte_util_comm_terminate_job(const orte_process_name_t *hnp, orte_jobid_t job);

ORTE_DECLSPEC int orte_util_comm_halt_vm(const orte_process_name_t *hnp);

END_C_DECLS
#endif
