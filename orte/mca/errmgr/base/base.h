/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_ERRMGR_BASE_H
#define ORTE_MCA_ERRMGR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"

#include "orte/mca/mca.h"
#include "orte/mca/errmgr/errmgr.h"


BEGIN_C_DECLS

/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_errmgr_base_framework;
/* select a component */
ORTE_DECLSPEC    int orte_errmgr_base_select(void);

END_C_DECLS

#endif
