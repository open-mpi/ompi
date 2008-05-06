 /*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file:
 * Process launcher for the Grid Engine
 *
 * See FAQ on how it works:
 * http://www.open-mpi.org/faq/?category=running#run-n1ge-or-sge 
 */

#ifndef ORTE_PLM_GRIDENGINE_EXPORT_H
#define ORTE_PLM_GRIDENGINE_EXPORT_H

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "orte/mca/plm/plm.h"
#include "opal/mca/mca.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_plm_gridengine_component_open(void);
int orte_plm_gridengine_component_close(void);
int orte_plm_gridengine_component_query(mca_base_module_t **module, int *priority);

/**
 * PLM Component
 */
struct orte_plm_gridengine_component_t {
    orte_plm_base_component_t super;
    bool verbose;
    int priority;
    char* orted;
};
typedef struct orte_plm_gridengine_component_t orte_plm_gridengine_component_t;

ORTE_MODULE_DECLSPEC extern orte_plm_gridengine_component_t mca_plm_gridengine_component;
extern orte_plm_base_module_t orte_plm_gridengine_module;


END_C_DECLS

#endif /* ORTE_PLM_GRIDENGINE_EXPORT_H */
