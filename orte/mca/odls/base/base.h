/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_ODLS_BASE_H
#define MCA_ODLS_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/mca/mca.h"

#include "orte/mca/odls/odls.h"


BEGIN_C_DECLS

/**
 * Global instance of odls-wide framework data
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_odls_base_framework;

#if !ORTE_DISABLE_FULL_SUPPORT

/**
 * Struct to hold globals for the odls framework
 */
typedef struct orte_odls_base_t {
    /* component has been selected */
    bool selected;
    /** selected component */
    orte_odls_base_component_t selected_component;
} orte_odls_base_t;

/*
 * Select an available component.
 */
ORTE_DECLSPEC int orte_odls_base_select(void);


#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif
