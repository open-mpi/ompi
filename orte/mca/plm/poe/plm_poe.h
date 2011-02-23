/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_PLM_POE_EXPORT_H
#define ORTE_PLM_POE_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/plm/plm.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_plm_poe_component_open(void);
int orte_plm_poe_component_close(void);
int orte_plm_poe_component_query(mca_base_module_t **module, int *priority);

/**
 * PLM Component
 */
struct orte_plm_poe_component_t {
    orte_plm_base_component_t super;
    orte_jobid_t jobid;
    int priority;
    char* path;
    char* env;
    char** argv;
    int argc;
    int debug;
    char* class;
    char* resource_allocation;
    char* hostfile;
    char* cmdfile; 
    char* mp_stdoutmode;
    char* mp_labelio;
    int mp_retry;
    int mp_retrycount; 
    int mp_infolevel;
};
typedef struct orte_plm_poe_component_t orte_plm_poe_component_t;


ORTE_MODULE_DECLSPEC extern orte_plm_poe_component_t mca_plm_poe_component;
extern orte_plm_base_module_t orte_plm_poe_module;

END_C_DECLS

#endif /* ORTE_PLM_POE_EXPORT_H */
