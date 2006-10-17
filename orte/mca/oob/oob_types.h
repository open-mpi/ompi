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
 *
 * the oob framework
 */

#ifndef _MCA_OOB_TYPES_H_
#define _MCA_OOB_TYPES_H_

#include "orte_config.h"
#include "orte/orte_constants.h"
#include <limits.h>

#include "orte/mca/rml/rml_types.h"
/*
 * Other constants
 */

/**
 * The wildcard for receives from any peer.
 */
#define MCA_OOB_NAME_ANY  &mca_oob_name_any
/**
 * Process name of self
 */
#define MCA_OOB_NAME_SELF orte_process_info.my_name
/**
 * Process name of seed
 */
#define MCA_OOB_NAME_SEED &mca_oob_name_seed

#endif  /* MCA_OOB_TYPES_H */
