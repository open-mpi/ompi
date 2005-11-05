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

#include "orte_config.h"

#include "include/orte_constants.h"

#include "opal/mca/base/mca_base_param.h"
#include "runtime/runtime.h"

int orte_register_params(bool infrastructure)
{
    int id;

    id = mca_base_param_reg_int_name("orte", "infrastructure",
                                     "Whether we are ORTE infrastructure or an ORTE application",
                                     true, true, (int)infrastructure, NULL);

    
    return ORTE_SUCCESS;
}

