/* -*- C -*-
 * 
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 *
 */
#ifndef ROUTED_CNOS_H
#define ROUTED_CNOS_H

#include "orte_config.h"
#include "orte/orte_types.h"
#include "orte/orte_constants.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/class/opal_object.h"

#include "orte/mca/routed/routed.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
orte_routed_module_t* orte_routed_cnos_init(int *priority);


ORTE_MODULE_DECLSPEC extern orte_routed_component_t mca_routed_cnos_component;
extern orte_routed_module_t orte_routed_cnos_module;

END_C_DECLS

#endif
