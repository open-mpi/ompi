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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file:
 */

#ifndef ORTE_ODLS_H
#define ORTE_ODLS_H

#include "orte_config.h"

#include "opal/mca/mca.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rmaps/rmaps_types.h"

#include "orte/mca/odls/odls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_odls_default_component_open(void);
int orte_odls_default_component_close(void);
orte_odls_base_module_t* orte_odls_default_component_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_odls_default_finalize(void);

/*
 * ODLS Default module
 */
extern orte_odls_base_module_t orte_odls_default_module;
ORTE_MODULE_DECLSPEC extern orte_odls_base_component_t mca_odls_default_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_ODLS_H */
