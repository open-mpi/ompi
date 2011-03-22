/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef DEBUGGER_MPIR_H
#define DEBUGGER_MPIR_H

#include "orte_config.h"

#include "orte/mca/debugger/debugger.h"

BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern orte_debugger_base_component_t mca_debugger_mpir_component;
extern orte_debugger_base_module_t orte_debugger_mpir_module;

END_C_DECLS

#endif /* ORTE_DEBUGGERS_H */
