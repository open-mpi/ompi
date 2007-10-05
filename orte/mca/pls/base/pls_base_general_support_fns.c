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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/univ_info.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/params.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/base/pls_private.h"


void orte_pls_base_purge_mca_params(char ***env)
{
    char *var;
    
    /* ensure we do not think we are an HNP */
    var = mca_base_param_environ_variable("seed",NULL,NULL);
    opal_setenv(var, "0", true, env);
    free(var);
    
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

int orte_pls_base_orted_append_basic_args(int *argc, char ***argv,
                                          int *proc_name_index,
                                          int *node_name_index)
{
    char *param = NULL, *contact_info = NULL;
    int loc_id;
    char * amca_param_path = NULL;
    char * amca_param_prefix = NULL;
    char * tmp_force = NULL;
    char *purge[] = {
        "seed",
        "rds",
        "ras",
        "rmaps",
        "pls",
        "rmgr",
        NULL
    };
    int i, j, cnt, rc;
    bool pass;
    orte_vpid_t total_num_daemons;

    /* check for debug flags */
    if (orte_debug_flag) {
        opal_argv_append(argc, argv, "--debug");
    }
    if (orte_debug_daemons_flag) {
        opal_argv_append(argc, argv, "--debug-daemons");
    }
    if (orte_debug_daemons_file_flag) {
        opal_argv_append(argc, argv, "--debug-daemons-file");
    }
    if (orted_spin_flag) {
        opal_argv_append(argc, argv, "--spin");
    }
    if (orte_no_daemonize_flag) {
        opal_argv_append(argc, argv, "--no-daemonize");
    }
    
    /* Name */
    if( NULL != proc_name_index ) {
        opal_argv_append(argc, argv, "--name");
        *proc_name_index = *argc;
        opal_argv_append(argc, argv, "<template>");
    }

    /* get the total number of daemons that will be in the system */
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_range(0, &total_num_daemons))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pass that number along */
    opal_argv_append(argc, argv, "--num_procs");
    asprintf(&param, "%lu", (unsigned long)(total_num_daemons));
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
        contact_info = strdup(orte_process_info.ns_replica_uri);
    } else {
        contact_info = orte_rml.get_contact_info();
    }
    asprintf(&param, "\"%s\"", contact_info);
    opal_argv_append(argc, argv, param);
    free(contact_info);
    free(param);

    /* setup gpr contact info */
    opal_argv_append(argc, argv, "--gprreplica");
    if (NULL != orte_process_info.gpr_replica_uri) {
        contact_info = strdup(orte_process_info.gpr_replica_uri);
    } else {
        contact_info = orte_rml.get_contact_info();
    }
    asprintf(&param, "\"%s\"", contact_info);
    opal_argv_append(argc, argv, param);
    free(contact_info);
    free(param);

    /* pass along any cmd line MCA params provided to mpirun,
     * being sure to "purge" any that would cause problems
     * on backend nodes
     */
    cnt = opal_argv_count(orted_cmd_line);    
    for (i=0; i < cnt; i+=3) {
        /* check to see if this is on the purge list */
        pass = true;
        for (j=0; NULL != purge[j]; j++) {
            /* the ith position holds -mca, so need to check
             * against the i+1st position to find the param
             */
            if (0 == strcmp(orted_cmd_line[i+1],purge[j])) {
                /* on purge list - skip it */
                pass = false;
                break;
            }
        }
        if (pass) {
            opal_argv_append(argc, argv, orted_cmd_line[i]);
            opal_argv_append(argc, argv, orted_cmd_line[i+1]);
            opal_argv_append(argc, argv, orted_cmd_line[i+2]);
        }
    }
    
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

    /* Add the 'path' param */
    loc_id = mca_base_param_find("mca", NULL, "base_param_file_path_force");
    mca_base_param_lookup_string(loc_id, &tmp_force);
    if( NULL == tmp_force ) {
        /* Get the current working directory */
        tmp_force = (char *) malloc(sizeof(char) * OMPI_PATH_MAX);
        if( NULL == (tmp_force = getcwd(tmp_force, OMPI_PATH_MAX) )) {
            tmp_force = strdup("");
        }
    }
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "mca_base_param_file_path_force");
    opal_argv_append(argc, argv, tmp_force);

    free(tmp_force);

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
