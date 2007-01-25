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
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#ifndef ORTE_PLS_PROXY_H
#define ORTE_PLS_PROXY_H

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "orte/mca/pls/pls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* my replica */
extern orte_process_name_t *orte_pls_proxy_replica;

/*
 * Module open / close
 */
int orte_pls_proxy_open(void);
int orte_pls_proxy_close(void);


/*
 * Startup / Shutdown
 */
orte_pls_base_module_t* orte_pls_proxy_init(int *priority);
int orte_pls_proxy_finalize(void);

/*
 * proxy function prototypes
 */
int orte_pls_proxy_launch(orte_jobid_t job);
int orte_pls_proxy_terminate_job(orte_jobid_t job, struct timeval *timeout, opal_list_t *attrs);
int orte_pls_proxy_terminate_orteds(orte_jobid_t job, struct timeval *timeout, opal_list_t *attrs);
int orte_pls_proxy_terminate_proc(const orte_process_name_t* name);
int orte_pls_proxy_signal_job(orte_jobid_t job, int32_t signal, opal_list_t *attrs);
int orte_pls_proxy_signal_proc(const orte_process_name_t* name, int32_t signal);
int orte_pls_proxy_cancel_operation(void);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
