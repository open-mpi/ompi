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
 *
 *
 */

#ifndef ORTE_PLS_BPROC_PROXY_H_
#define ORTE_PLS_BPROC_PROXY_H_

#include "ompi_config.h"
#include "mca/pls/base/base.h"
#include <sys/bproc.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_pls_bproc_proxy_component_open(void);
int orte_pls_bproc_proxy_component_close(void);

/*
 * Startup / Shutdown
 */
orte_pls_base_module_t* orte_pls_bproc_proxy_init(int *priority);

int orte_pls_bproc_proxy_finalize(void);

/*
 * Interface
 */
int orte_pls_bproc_proxy_launch(orte_jobid_t);
int orte_pls_bproc_proxy_terminate_job(orte_jobid_t);
int orte_pls_bproc_proxy_terminate_proc(const orte_process_name_t* proc_name);

ORTE_DECLSPEC extern orte_pls_base_component_t mca_pls_bproc_proxy_component;
ORTE_DECLSPEC extern orte_pls_base_module_t orte_pls_bproc_proxy_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PCM_BPROCx_H_ */
