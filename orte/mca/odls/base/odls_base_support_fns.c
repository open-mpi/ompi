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
#include "orte/orte_constants.h"

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/odls/base/odls_private.h"


int orte_odls_base_purge_environment(char ***environ)
{
    char *param;
    
    if(NULL == (param = mca_base_param_environ_variable("rds",NULL,NULL))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, environ);
    free(param);
    if(NULL == (param = mca_base_param_environ_variable("ras",NULL,NULL))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, environ);
    free(param);
    if(NULL == (param = mca_base_param_environ_variable("rmaps",NULL,NULL))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, environ);
    free(param);
    if(NULL == (param = mca_base_param_environ_variable("pls",NULL,NULL))) {
         return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, environ);
    free(param);
    if(NULL == (param = mca_base_param_environ_variable("rmgr",NULL,NULL))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, environ);
    free(param);
    
    return ORTE_SUCCESS;
}

