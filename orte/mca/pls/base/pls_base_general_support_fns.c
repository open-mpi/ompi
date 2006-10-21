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
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/pls/base/pls_private.h"


static int lookup_set(char *a, char *b, char *c, int default_val,
                      char *token, int *argc, char ***argv)
{
    int id, rc;
    
    id = mca_base_param_find(a, b, c);
    if (id < 0) {
        id = mca_base_param_register_int(a, b, c, NULL, default_val);
    }
    mca_base_param_lookup_int(id, &rc);
    if (rc) {
        opal_argv_append(argc, argv, token);
    }
    
    return ORTE_SUCCESS;
}


int orte_pls_base_mca_argv(int *argc, char ***argv)
{
    lookup_set("orted", "spin", NULL, 0, "--spin", argc, argv);
    lookup_set("orte", "no_daemonize", NULL, 0, "--no-daemonize", argc, argv);
    lookup_set("orte", "debug", NULL, 0, "--debug", argc, argv);
    lookup_set("orte", "debug", "daemons", 0, "--debug-daemons", argc, argv);
    lookup_set("orte", "debug", "daemons_file", 0, "--debug-daemons-file", argc, argv);
    
    return ORTE_SUCCESS;
}

void orte_pls_base_purge_mca_params(char ***env)
{
    char *var;
    
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

