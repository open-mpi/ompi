/* -*- C -*-
 * 
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
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
#ifndef NOTIFIER_FTB_H
#define NOTIFIER_FTB_H

#include "orte_config.h"

#include "orte/mca/notifier/notifier.h"

#include "libftb.h"

BEGIN_C_DECLS

/*
 * Component open / close
 */
int orte_notifier_ftb_open(void);
int orte_notifier_ftb_close(void);
int orte_notifier_ftb_component_query(mca_base_module_t **module, int *priority);


/*
 * Notifier interfaces
 */

ORTE_MODULE_DECLSPEC extern orte_notifier_base_component_t mca_notifier_ftb_component;
extern orte_notifier_base_module_t orte_notifier_ftb_module;

END_C_DECLS

#endif
