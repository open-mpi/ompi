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

#ifndef MCA_SMR_BASE_H
#define MCA_SMR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/class/opal_list.h"
#include "orte/dss/dss_types.h"
#include "opal/mca/mca.h"
/* #include "orte/mca/ns/ns_types.h" */
#include "orte/mca/smr/smr.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

ORTE_DECLSPEC    int orte_smr_base_open(void);
ORTE_DECLSPEC    int orte_smr_base_select(void);
ORTE_DECLSPEC    int orte_smr_base_close(void);

typedef struct orte_smr_base_t {
    int smr_output;
    opal_list_t smr_components;
} orte_smr_base_t;

ORTE_DECLSPEC extern orte_smr_base_t orte_smr_base;


/*
 * external API functions will be documented in the mca/smr/smr.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
