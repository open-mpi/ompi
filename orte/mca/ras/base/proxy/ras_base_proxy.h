/* 
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
#ifndef RAS_PROXY_H
#define RAS_PROXY_H

#include "orte_config.h"

#include "orte/mca/ras/ras.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* my replica */
extern orte_process_name_t *orte_ras_base_proxy_replica;

extern orte_ras_base_module_t orte_ras_base_proxy_module;

/*
 * Module open / close
 */
int orte_ras_base_proxy_open(void);
int orte_ras_base_proxy_close(void);


/*
 * Startup / Shutdown
 */
orte_ras_base_module_t* orte_ras_base_proxy_init(int* priority);
int orte_ras_base_proxy_finalize(void);

/*
 * proxy function prototypes
 */
int orte_ras_base_proxy_allocate(orte_jobid_t job, opal_list_t *attributes);
int orte_ras_base_proxy_deallocate(orte_jobid_t job);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
