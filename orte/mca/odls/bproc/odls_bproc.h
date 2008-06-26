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
 * Part of the bproc launching system. This launching system is broken into 2
 * parts: one runs under the PLS on the head node to launch the orteds, and the
 * other serves as the orted's local launcher.
 *
 * The main job of this component is to setup ptys/pipes for IO forwarding.
 * See pls_bproc.h for an overview of how the entire bproc launching system works.
 */
#ifndef ORTE_ODLS_BPROC_H_
#define ORTE_ODLS_BPROC_H_

#include "orte_config.h"

#include "opal/mca/mca.h"

#include "orte/mca/odls/odls.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_odls_bproc_component_open(void);
int orte_odls_bproc_component_close(void);
int orte_odls_bproc_component_query(mca_base_module_t **module, int *priority);

ORTE_MODULE_DECLSPEC extern orte_odls_base_component_t mca_odls_bproc_component;

END_C_DECLS

#endif /* ORTE_ODLS_BPROC_H_ */

