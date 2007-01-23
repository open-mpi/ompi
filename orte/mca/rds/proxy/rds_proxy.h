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
#ifndef RDS_PROXY_H
#define RDS_PROXY_H

#include "orte_config.h"

#include "orte/mca/rds/rds.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* my replica */
extern orte_process_name_t *orte_rds_proxy_replica;

/*
 * Module open / close
 */
int orte_rds_proxy_open(void);
int orte_rds_proxy_close(void);


/*
 * Startup / Shutdown
 */
orte_rds_base_module_t* orte_rds_proxy_init(void);
int orte_rds_proxy_finalize(void);

/*
 * proxy function prototypes
 */
int orte_rds_proxy_query(orte_jobid_t job);

ORTE_DECLSPEC extern orte_rds_base_component_t mca_rds_proxy_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
