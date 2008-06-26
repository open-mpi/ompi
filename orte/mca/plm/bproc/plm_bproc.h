/* -*- C -*-
 *
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
 *
 *
 */
/**
 * @file:
 */

#ifndef ORTE_PLM_BPROC_H_
#define ORTE_PLM_BPROC_H_

#include "orte_config.h"
#include "orte/constants.h"

#include "orte/mca/plm/base/base.h"

BEGIN_C_DECLS

/**
 * PLM bproc Component
 */
struct orte_plm_bproc_component_t {
    /**< The base class */
    orte_plm_base_component_t super;
    /**< The orted executable. This can be an absolute path, or if not found
     * we will look for it in the user's path */
    char * orted;
};
/**
 * Convenience typedef
 */
typedef struct orte_plm_bproc_component_t orte_plm_bproc_component_t;

ORTE_DECLSPEC extern orte_plm_bproc_component_t mca_plm_bproc_component;
ORTE_DECLSPEC extern orte_plm_base_module_t orte_plm_bproc_module;

END_C_DECLS

#endif /* ORTE_PLM_BPROC_H_ */

