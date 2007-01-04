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

#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "orte/orte_constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

ORTE_DECLSPEC orte_proc_info_t orte_process_info = {
    /*  .my_name =              */   NULL,
    /*  ,app_num =              */   -1,
    /*  .singleton =            */   false,
    /*  .vpid_start =           */   0,
    /*  .num_procs =            */   1,
    /*  .pid =                  */   0,
    /*  .seed =                 */   false,
    /*  .daemon =               */   false,
    /*  .ns_replica_uri =       */   NULL,
    /*  .gpr_replica_uri =      */   NULL,
    /*  .ns_replica =           */   NULL,
    /*  .gpr_replica =          */   NULL,
    /*  .tmpdir_base =          */   NULL,
    /*  .top_session_dir =      */   NULL,
    /*  .universe_session_dir = */   NULL,
    /*  .job_session_dir =      */   NULL,
    /*  .proc_session_dir =     */   NULL,
    /*  .sock_stdin =           */   NULL,
    /*  .sock_stdout =          */   NULL,
    /*  .sock_stderr =          */   NULL
};


int orte_proc_info(void)
{

    int id, tmp;
    
    /* all other params are set elsewhere */
    
    id = mca_base_param_register_int("seed", NULL, NULL, NULL, orte_process_info.seed);
    mca_base_param_lookup_int(id, &tmp);
    orte_process_info.seed = OPAL_INT_TO_BOOL(tmp);
    /* if we are a seed, then make sure the daemon flag is NOT set so that
     * framework components are properly selected
     */
    if (orte_process_info.seed) {
        orte_process_info.daemon = false;
    }

    id = mca_base_param_register_int("orte", "app", "num", NULL, -1);
    mca_base_param_lookup_int(id, &tmp);
    orte_process_info.app_num = tmp;

    id = mca_base_param_register_string("gpr", "replica", "uri", NULL, orte_process_info.gpr_replica_uri);
    mca_base_param_lookup_string(id, &(orte_process_info.gpr_replica_uri));
    mca_base_param_set_internal(id, true);

    id = mca_base_param_register_string("ns", "replica", "uri", NULL, orte_process_info.ns_replica_uri);
    mca_base_param_lookup_string(id, &(orte_process_info.ns_replica_uri));
    mca_base_param_set_internal(id, true);

    id = mca_base_param_register_string("tmpdir", "base", NULL, NULL, orte_process_info.tmpdir_base);
    mca_base_param_lookup_string(id, &(orte_process_info.tmpdir_base));

    /* get the process id */
    orte_process_info.pid = getpid();

    return ORTE_SUCCESS;
}


int orte_proc_info_finalize(void)
{
    if (NULL != orte_process_info.my_name) {
        free(orte_process_info.my_name);
        orte_process_info.my_name = NULL;
    }
    
    if (NULL != orte_process_info.ns_replica_uri) {
        free(orte_process_info.ns_replica_uri);
        orte_process_info.ns_replica_uri = NULL;
    }
    
    if (NULL != orte_process_info.gpr_replica_uri) {
        free(orte_process_info.gpr_replica_uri);
        orte_process_info.gpr_replica_uri = NULL;
    }
    
    if (NULL != orte_process_info.ns_replica) {
        free(orte_process_info.ns_replica);
        orte_process_info.ns_replica = NULL;
    }
    
    if (NULL != orte_process_info.gpr_replica) {
        free(orte_process_info.gpr_replica);
        orte_process_info.gpr_replica = NULL;
    }
 
     if (NULL != orte_process_info.tmpdir_base) {
        free(orte_process_info.tmpdir_base);
        orte_process_info.tmpdir_base = NULL;
    }
    
    if (NULL != orte_process_info.top_session_dir) {
        free(orte_process_info.top_session_dir);
        orte_process_info.top_session_dir = NULL;
    }
 
     if (NULL != orte_process_info.universe_session_dir) {
        free(orte_process_info.universe_session_dir);
        orte_process_info.universe_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.job_session_dir) {
        free(orte_process_info.job_session_dir);
        orte_process_info.job_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.proc_session_dir) {
        free(orte_process_info.proc_session_dir);
        orte_process_info.proc_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.sock_stdin) {
        free(orte_process_info.sock_stdin);
        orte_process_info.sock_stdin = NULL;
    }
    
     if (NULL != orte_process_info.sock_stdout) {
        free(orte_process_info.sock_stdout);
        orte_process_info.sock_stdout = NULL;
    }
    
   if (NULL != orte_process_info.sock_stderr) {
        free(orte_process_info.sock_stderr);
        orte_process_info.sock_stderr = NULL;
    }

    orte_process_info.seed = false;
    orte_process_info.singleton = false;
    orte_process_info.daemon = false;
    
    return ORTE_SUCCESS;
}
