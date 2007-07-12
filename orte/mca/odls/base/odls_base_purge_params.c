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

#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/odls/base/odls_private.h"


/* Purge mca params not suitable for application procs */
void orte_odls_base_purge_mca_params(char ***env)
{
    char *var;
    
    /* tell critical frameworks to only use their proxy components */
    var = mca_base_param_environ_variable("rds",NULL,NULL);
    opal_setenv(var, "proxy", true, env);
    free(var);
    var = mca_base_param_environ_variable("ras",NULL,NULL);
    opal_setenv(var, "proxy", true, env);
    free(var);
    var = mca_base_param_environ_variable("rmaps",NULL,NULL);
    opal_setenv(var, "proxy", true, env);
    free(var);
    var = mca_base_param_environ_variable("pls",NULL,NULL);
    opal_setenv(var, "proxy", true, env);
    free(var);
    var = mca_base_param_environ_variable("rmgr",NULL,NULL);
    opal_setenv(var, "proxy", true, env);
    free(var);
}
