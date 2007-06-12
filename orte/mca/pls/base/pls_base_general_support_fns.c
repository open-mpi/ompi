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

#include "orte/util/univ_info.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/params.h"

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

int orte_pls_base_orted_append_basic_args(int *argc, char ***argv,
                                          int *proc_name_index,
                                          int *node_name_index,
                                          orte_std_cntr_t num_procs)
{
    char *param = NULL, *uri = NULL;
    int loc_id;
    char * amca_param_path = NULL;
    char * amca_param_prefix = NULL;

    /* check for debug flags */
    orte_pls_base_mca_argv(argc, argv);

    /* Name */
    if( NULL != proc_name_index ) {
        opal_argv_append(argc, argv, "--name");
        *proc_name_index = *argc;
        opal_argv_append(argc, argv, "<template>");
    }

    /* tell the daemon how many procs are in the daemon's job */
    opal_argv_append(argc, argv, "--num_procs");
    asprintf(&param, "%lu", (unsigned long)(num_procs));
    opal_argv_append(argc, argv, param);
    free(param);

    /* tell the daemon the starting vpid of the daemon's job */
    opal_argv_append(argc, argv, "--vpid_start");
    opal_argv_append(argc, argv, "0");

    /* Node Name */
    if(NULL != node_name_index) {
        opal_argv_append(argc, argv, "--nodename");
        *node_name_index = *argc;
        opal_argv_append(argc, argv, "<template>");
    }

    /* pass along the universe name and location info */
    opal_argv_append(argc, argv, "--universe");
    asprintf(&param, "%s@%s:%s", orte_universe_info.uid,
             orte_universe_info.host, orte_universe_info.name);
    opal_argv_append(argc, argv, param);
    free(param);

    /* setup ns contact info */
    opal_argv_append(argc, argv, "--nsreplica");
    if (NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(argc, argv, param);
    free(uri);
    free(param);

    /* setup gpr contact info */
    opal_argv_append(argc, argv, "--gprreplica");
    if (NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(argc, argv, param);
    free(uri);
    free(param);

    /* 
     * Pass along the Aggregate MCA Parameter Sets
     */
    /* Add the 'prefix' param */
    loc_id = mca_base_param_find("mca", NULL, "base_param_file_prefix");
    mca_base_param_lookup_string(loc_id, &amca_param_prefix);
    if( NULL != amca_param_prefix ) {
        /* Could also use the short version '-am'
         * but being verbose has some value
         */
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_prefix");
        opal_argv_append(argc, argv, amca_param_prefix);
    }

    /* Add the 'path' param */
    loc_id = mca_base_param_find("mca", NULL, "base_param_file_path");
    mca_base_param_lookup_string(loc_id, &amca_param_path);
    if( NULL != amca_param_path ) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_path");
        opal_argv_append(argc, argv, amca_param_path);
    }

    /* Add the ORTED hint 'path' param */
    loc_id = mca_base_param_find("mca", NULL, "base_param_file_path_orted");
    mca_base_param_lookup_string(loc_id, &amca_param_path);
    if( NULL != amca_param_path ) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_path_orted");
        opal_argv_append(argc, argv, amca_param_path);
    }

    if( NULL != amca_param_path ) {
        free(amca_param_path);
        amca_param_path = NULL;
    }
    if( NULL != amca_param_prefix ) {
        free(amca_param_prefix);
        amca_param_prefix = NULL;
    }

    return ORTE_SUCCESS;
}
