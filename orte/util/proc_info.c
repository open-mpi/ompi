/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <ctype.h>

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/util/proc_info.h"

#define ORTE_NAME_INVALID {ORTE_JOBID_INVALID, ORTE_VPID_INVALID}

ORTE_DECLSPEC orte_proc_info_t orte_process_info = {
    /*  .my_name =                      */   ORTE_NAME_INVALID,
    /*  .my_daemon =                    */   ORTE_NAME_INVALID,
    /*  .my_daemon_uri =                */   NULL,
    /*  .my_hnp =                       */   ORTE_NAME_INVALID,
    /*  .my_hnp_uri =                   */   NULL,
    /*  .my_parent =                    */   ORTE_NAME_INVALID,
    /*  .hnp_pid =                      */   0,
    /*  .app_num =                      */   0,
    /*  .num_procs =                    */   1,
    /*  .max_procs =                    */   1,
    /*  .num_daemons =                  */   1,
    /*  .num_nodes =                    */   1,
    /*  .nodename =                     */   NULL,
    /*  .pid =                          */   0,
    /*  .proc_type =                    */   ORTE_PROC_TYPE_NONE,
    /*  .sync_buf =                     */   NULL,
    /*  .my_port =                      */   0,
    /*  .num_restarts =                 */   0,
    /*  .my_node_rank =                 */   ORTE_NODE_RANK_INVALID,
    /*  .my_local_rank =                */   ORTE_LOCAL_RANK_INVALID,
    /*  .num_local_peers =              */   0,
    /*  .tmpdir_base =                  */   NULL,
    /*  .top_session_dir =              */   NULL,
    /*  .job_session_dir =              */   NULL,
    /*  .proc_session_dir =             */   NULL,
    /*  .sock_stdin =                   */   NULL,
    /*  .sock_stdout =                  */   NULL,
    /*  .sock_stderr =                  */   NULL,
#if OPAL_HAVE_HWLOC
    /*  .bind_level =                   */   OPAL_HWLOC_NODE_LEVEL,
    /*  .bind_idx =                     */   0,
#endif
    /*  .app_rank =                     */   -1,
    /*  .peer_modex =                   */   -1,
    /*  .peer_init_barrier =            */   -1,
    /*  .peer_fini_barrier =            */   -1,
    /*  .strip_prefix_from_node_names = */ false
};

static bool init=false;

int orte_proc_info(void)
{
    
    int tmp, idx;
    char *uri, *ptr;
    char hostname[ORTE_MAX_HOSTNAME_SIZE];
    
    if (init) {
        return ORTE_SUCCESS;
    }
    init = true;
    
    mca_base_param_reg_string_name("orte", "hnp_uri",
                                   "HNP contact info",
                                   true, false, NULL,  &uri);
    if (NULL != uri) {
        /* the uri value passed to us will have quote marks around it to protect
        * the value if passed on the command line. We must remove those
        * to have a correct uri string
        */
        if ('"' == uri[0]) {
            /* if the first char is a quote, then so will the last one be */
            uri[strlen(uri)-1] = '\0';
            ptr = &uri[1];
        } else {
            ptr = &uri[0];
        }
        orte_process_info.my_hnp_uri = strdup(ptr);
        free(uri);
    }
    
    mca_base_param_reg_string_name("orte", "local_daemon_uri",
                                   "Daemon contact info",
                                   true, false, NULL,  &(uri));
    
    if (NULL != uri) {
        /* the uri value passed to us may have quote marks around it to protect
         * the value if passed on the command line. We must remove those
         * to have a correct uri string
         */
        if ('"' == uri[0]) {
            /* if the first char is a quote, then so will the last one be */
            uri[strlen(uri)-1] = '\0';
            ptr = &uri[1];
        } else {
            ptr = &uri[0];
        }
        orte_process_info.my_daemon_uri = strdup(ptr);
        free(uri);
    }
    
    mca_base_param_reg_int_name("orte", "app_num",
                                "Index of the app_context that defines this proc",
                                true, false, 0, &tmp);
    orte_process_info.app_num = tmp;
    
    /* get the process id */
    orte_process_info.pid = getpid();

    mca_base_param_reg_int_name("orte", "strip_prefix_from_node_names",
                                "Whether to strip leading characters and zeroes from node names returned by daemons",
                                false, false, (int)false, &tmp);
    orte_process_info.strip_prefix_from_node_names = OPAL_INT_TO_BOOL(tmp);

    /* get the nodename */
    gethostname(hostname, ORTE_MAX_HOSTNAME_SIZE);
    /* we have to strip node names here, if user directs, to ensure that
     * the names exchanged in the modex match the names found locally
     */
    if (orte_process_info.strip_prefix_from_node_names) {
        /* remove all leading characters and zeroes */
        idx = 0;
        while (idx < (int)strlen(hostname) &&
               (hostname[idx] <= '0' || '9' < hostname[idx])) {
            idx++;
        }
        if ((int)strlen(hostname) <= idx) {
            /* there were no non-zero numbers in the name */
            orte_process_info.nodename = strdup(hostname);
        } else {
            orte_process_info.nodename = strdup(&hostname[idx]);
        }
    } else {
        orte_process_info.nodename = strdup(hostname);
    }
    opal_output(0, "HOSTNAME: %s", orte_process_info.nodename);

    /* get the number of nodes in the job */
    mca_base_param_reg_int_name("orte", "num_nodes",
                                "Number of nodes in the job",
                                true, false,
                                orte_process_info.num_nodes, &tmp);
    orte_process_info.num_nodes = tmp;
    
    /* get the number of times this proc has restarted */
    mca_base_param_reg_int_name("orte", "num_restarts",
                                "Number of times this proc has restarted",
                                true, false, 0, &tmp);
    orte_process_info.num_restarts = tmp;
    
    mca_base_param_reg_int_name("orte", "app_rank",
                                "Rank of this proc within its app_context",
                                true, false, 0, &tmp);
    orte_process_info.app_rank = tmp;

    /* get my node rank in case we are using static ports - this won't
     * be present for daemons, so don't error out if we don't have it
     */
    mca_base_param_reg_int_name("orte", "ess_node_rank", "Process node rank",
                                true, false, ORTE_NODE_RANK_INVALID, &tmp);
    orte_process_info.my_node_rank = (orte_node_rank_t)tmp;
    
    /* setup the sync buffer */
    orte_process_info.sync_buf = OBJ_NEW(opal_buffer_t);
    
    /* get the collective id info */
    mca_base_param_reg_int_name("orte", "peer_modex_id", "Peer modex collective id",
                                true, false, -1, &tmp);
    orte_process_info.peer_modex = (orte_grpcomm_coll_id_t)tmp;

    mca_base_param_reg_int_name("orte", "peer_init_barrier_id", "Peer init barrier collective id",
                                true, false, -1, &tmp);
    orte_process_info.peer_init_barrier = (orte_grpcomm_coll_id_t)tmp;

    mca_base_param_reg_int_name("orte", "peer_fini_barrier_id", "Peer finalize barrier collective id",
                                true, false, -1, &tmp);
    orte_process_info.peer_fini_barrier = (orte_grpcomm_coll_id_t)tmp;

    return ORTE_SUCCESS;
}


int orte_proc_info_finalize(void)
{
    if (!init) {
        return ORTE_SUCCESS;
    }
    
    if (NULL != orte_process_info.tmpdir_base) {
        free(orte_process_info.tmpdir_base);
        orte_process_info.tmpdir_base = NULL;
    }
    
    if (NULL != orte_process_info.top_session_dir) {
        free(orte_process_info.top_session_dir);
        orte_process_info.top_session_dir = NULL;
    }
 
    if (NULL != orte_process_info.job_session_dir) {
        free(orte_process_info.job_session_dir);
        orte_process_info.job_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.proc_session_dir) {
        free(orte_process_info.proc_session_dir);
        orte_process_info.proc_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.nodename) {
        free(orte_process_info.nodename);
        orte_process_info.nodename = NULL;
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

    if (NULL != orte_process_info.my_hnp_uri) {
        free(orte_process_info.my_hnp_uri);
        orte_process_info.my_hnp_uri = NULL;
    }

    if (NULL != orte_process_info.my_daemon_uri) {
        free(orte_process_info.my_daemon_uri);
        orte_process_info.my_daemon_uri = NULL;
    }

    orte_process_info.proc_type = ORTE_PROC_TYPE_NONE;
    
    OBJ_RELEASE(orte_process_info.sync_buf);
    orte_process_info.sync_buf = NULL;

    init = false;
    return ORTE_SUCCESS;
}
