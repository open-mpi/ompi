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

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/arch.h"
#include "opal/util/output.h"

#include "orte/util/proc_info.h"

ORTE_DECLSPEC orte_proc_info_t orte_process_info = {
    /*  .my_name =              */   {ORTE_JOBID_INVALID, ORTE_VPID_INVALID},
    /*  .my_daemon =            */   {ORTE_JOBID_INVALID, ORTE_VPID_INVALID},
    /*  .my_daemon_uri =        */   NULL,
    /*  .my_hnp =               */   {ORTE_JOBID_INVALID, ORTE_VPID_INVALID},
    /*  .my_hnp_uri =           */   NULL,
    /*  .hnp_pid =              */    0,
    /*  .app_num =              */   -1,
    /*  .num_procs =            */   1,
    /*  .nodename =             */   NULL,
    /*  .arch =                 */   0,
    /*  .pid =                  */   0,
    /*  .singleton =            */   false,
    /*  .daemon =               */   false,
    /*  .hnp =                  */   false,
    /*  .tool =                 */   false,
    /*  .mpi_proc =             */   false,
    /*  .sync_buf =             */   NULL,
    /*  .tmpdir_base =          */   NULL,
    /*  .top_session_dir =      */   NULL,
    /*  .job_session_dir =      */   NULL,
    /*  .proc_session_dir =     */   NULL,
    /*  .sock_stdin =           */   NULL,
    /*  .sock_stdout =          */   NULL,
    /*  .sock_stderr =          */   NULL
};

static bool init=false;

int orte_proc_info(void)
{
    
    int tmp;
    char *uri, *ptr;
    size_t len, i;
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
            ptr = &uri[1];
            len = strlen(ptr) - 1;
        } else {
            ptr = &uri[0];
            len = strlen(uri);
        }
        
        /* we have to copy the string by hand as strndup is a GNU extension
         * and may not be generally available
         */
        orte_process_info.my_hnp_uri = (char*)malloc(len+1);
        for (i=0; i < len; i++) {
            orte_process_info.my_hnp_uri[i] = ptr[i];
        }
        orte_process_info.my_hnp_uri[len] = '\0';  /* NULL terminate */
        free(uri);

    }
    
    mca_base_param_reg_string_name("orte", "local_daemon_uri",
                                   "Daemon contact info",
                                   true, false, NULL,  &(orte_process_info.my_daemon_uri));
    
    mca_base_param_reg_int_name("orte", "app_num",
                                "Index of the app_context that defines this proc",
                                true, false, -1, &tmp);
    orte_process_info.app_num = tmp;
    
    /* get the process id */
    orte_process_info.pid = getpid();

    /* get the nodename */
    gethostname(hostname, ORTE_MAX_HOSTNAME_SIZE);
    orte_process_info.nodename = strdup(hostname);
    
    /* get the arch */
    if (ORTE_SUCCESS != opal_arch_compute_local_id(&orte_process_info.arch)) {
        opal_output(0, "Process on node %s could not obtain local architecture - aborting", orte_process_info.nodename);
        return ORTE_ERROR;
    }
    
    /* setup the sync buffer */
    orte_process_info.sync_buf = OBJ_NEW(opal_buffer_t);
    
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

    orte_process_info.hnp = false;
    orte_process_info.singleton = false;
    orte_process_info.daemon = false;
    
    OBJ_RELEASE(orte_process_info.sync_buf);
    orte_process_info.sync_buf = NULL;

    init = false;
    return ORTE_SUCCESS;
}
