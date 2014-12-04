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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_ESS_ALPS_H
#define ORTE_ESS_ALPS_H

#include "orte_config.h"
#include "opal/mca/mca.h"
#include "orte/mca/ess/ess.h"

#include "alps/alps.h"
#include "alps/alps_toolAssist.h"
#include "alps/libalpsutil.h"
#include "alps/libalpslli.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_ess_alps_component_open(void);
int orte_ess_alps_component_close(void);
int orte_ess_alps_component_query(mca_base_module_t **module, int *priority);

/*
 * alps component internal utility functions
 */

int orte_ess_alps_get_first_rank_on_node(int *first_rank);
int orte_ess_alps_sync_start(void);
int orte_ess_alps_sync_complete(void);

/*
 * ODLS Alps module
 */
extern orte_ess_base_module_t orte_ess_alps_module;
ORTE_MODULE_DECLSPEC extern orte_ess_base_component_t mca_ess_alps_component;

END_C_DECLS

#endif /* ORTE_ESS_ALPS_H */
