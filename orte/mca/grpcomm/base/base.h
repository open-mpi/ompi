/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
/** @file:
 */

#ifndef MCA_GRPCOMM_BASE_H
#define MCA_GRPCOMM_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/mca/grpcomm/grpcomm.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/*
 * function definitions
 */
ORTE_DECLSPEC    int orte_grpcomm_base_open(void);
ORTE_DECLSPEC    int orte_grpcomm_base_select(void);
ORTE_DECLSPEC    int orte_grpcomm_base_close(void);

/*
 * globals that might be needed
 */

ORTE_DECLSPEC extern int orte_grpcomm_base_output;
ORTE_DECLSPEC extern bool mca_grpcomm_base_selected;
ORTE_DECLSPEC extern opal_list_t mca_grpcomm_base_components_available;
ORTE_DECLSPEC extern orte_grpcomm_base_component_t mca_grpcomm_base_selected_component;

/*
 * external API functions will be documented in the mca/grpcomm/grpcomm.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
