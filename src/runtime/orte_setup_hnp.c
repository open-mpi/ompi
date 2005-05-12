/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

/**
 * @file
 *
 * Establish a Head Node Process on a cluster's front end
 */


#include "orte_config.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>


#include "include/orte_constants.h"
#include "runtime/orte_wait.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/path.h"
#include "util/univ_info.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/os_path.h"
#include "util/session_dir.h"
#include "util/universe_setup_file_io.h"

#include "mca/base/mca_base_param.h"
#include "mca/soh/soh.h"
#include "mca/rml/rml.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "runtime/runtime.h"

extern char **environ;

/*
 * Local data structure
 */
typedef struct {
    char *target_cluster;
    char *headnode;
    orte_process_name_t *name;
    orte_jobid_t jobid;
} orte_setup_hnp_cb_data_t;

static orte_setup_hnp_cb_data_t orte_setup_hnp_cbdata = {NULL, NULL, NULL, 0};

/*
 * NON-BLOCKING RECEIVER
 */
static void orte_setup_hnp_recv(int status, orte_process_name_t* sender,
                                orte_buffer_t* buffer, orte_rml_tag_t tag,
                                void* cbdata);

/*
 * PID WAIT CALLBACK
 */
static void orte_setup_hnp_wait(pid_t wpid, int status, void *data);


/*
 * ORTE_SETUP_HNP
 */
int orte_setup_hnp(char *target_cluster, char *headnode, char *username)
{
    char **argv, *param, *uri, *uid, *hn;
    char *path, *name_string, *orteprobe;
    int argc, rc=ORTE_SUCCESS, id;
    pid_t pid;
    orte_cellid_t cellid;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    
    /* get the nodename for the headnode of the target cluster */
    if (NULL == headnode) {  /* not provided, so try to look it up */
    } else {  /* lookup the headnode's cellid */
        hn = strdup(headnode);
        cellid = 0;
    }

    /* get the user's name on the headnode */
    if (NULL == username) {
        uid = strdup(orte_system_info.user);
    } else {
        uid = strdup(username);
    }
    
    /* SETUP TO LAUNCH PROBE */
    
    /* get a jobid for the probe */
    if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(&jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* get a vpid for the probe */
    if (ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, 1, &vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* initialize probe's process name... */
    rc = orte_ns.create_process_name(&(orte_setup_hnp_cbdata.name), cellid, jobid, vpid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* ...and get string representation */
    if(ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string(&name_string, orte_setup_hnp_cbdata.name))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* setup callback data on sigchild */
    orte_setup_hnp_cbdata.target_cluster = strdup(target_cluster);
    orte_setup_hnp_cbdata.headnode = strdup(headnode);
    orte_setup_hnp_cbdata.jobid = jobid;
   
    /* get rsh/ssh launch mechanism parameters */
    id = mca_base_param_register_string("pls","rsh","agent",NULL,"/usr/bin/ssh");
    mca_base_param_lookup_string(id, &param);

    id = mca_base_param_register_string("orteprobe",NULL,NULL,NULL,"orteprobe");
    mca_base_param_lookup_string(id, &orteprobe);
    
    /* Initialize the argv array */
    argv = ompi_argv_split(param, ' ');
    argc = ompi_argv_count(argv);
    if (argc <= 0) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        rc = ORTE_ERR_BAD_PARAM;
        goto CLEANUP;
    }
    free(param);
    
    /* setup the path */
    path = ompi_path_findv(argv[0], 0, environ, NULL);

    /* add the username and nodename */
    ompi_argv_append(&argc, &argv, "-l");
    ompi_argv_append(&argc, &argv, uid);
    ompi_argv_append(&argc, &argv, hn);

    /* add the probe application */
    ompi_argv_append(&argc, &argv, orteprobe);
    
    /* tell the probe it's name */
    ompi_argv_append(&argc, &argv, "--name");
    ompi_argv_append(&argc, &argv, name_string);

    /* setup probe's ns contact info */
    ompi_argv_append(&argc, &argv, "--nsreplica");
    if(NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    ompi_argv_append(&argc, &argv, param);
    free(uri);

    /* setup probe's gpr contact info */
    ompi_argv_append(&argc, &argv, "--gprreplica");
    if(NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    ompi_argv_append(&argc, &argv, param);
    free(uri);

    /* tell the probe who to report to */
    uri = orte_rml.get_uri();
    ompi_argv_append(&argc, &argv, "--requestor");
    ompi_argv_append(&argc, &argv, uri);
    free(uri);
    
    /* issue the non-blocking recv to get the probe's findings */
    rc = orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_PROBE,
                                 0, orte_setup_hnp_recv, NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* fork a child to exec the rsh/ssh session */
    pid = fork();
    if (pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    if (pid == 0) {     /* child */
        /* exec the probe launch */
ompi_output(0, "exec'ing %s", path);
        execv(path, argv);
        ORTE_ERROR_LOG(ORTE_ERROR);
        ompi_output(0, "orte_setup_hnp: execv failed with errno=%d\n", errno);
        return ORTE_ERROR;

    } else {    /* parent */
        
        orte_wait_cb(pid, orte_setup_hnp_wait, &orte_setup_hnp_cbdata);
    }

CLEANUP:
    return rc;
}

static void orte_setup_hnp_recv(int status, orte_process_name_t* sender,
                                orte_buffer_t* buffer, orte_rml_tag_t tag,
                                void* cbdata)
{
    ompi_output(0, "HE CALLED HOME!!");
    orte_finalize();
    exit(0);
}

static void orte_setup_hnp_wait(pid_t wpid, int status, void *cbdata)
{
    int rc;
    orte_setup_hnp_cb_data_t *data;
    
    data = (orte_setup_hnp_cb_data_t*)cbdata;

    /* if ssh exited abnormally, print something useful to the user and cleanup
     * the registry entries for the HNP jobid.
       This should somehow be pushed up to the calling level, but we
       don't really have a way to do that just yet.
    */
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* set the probe's state-of-health to aborted */
        if (ORTE_SUCCESS != (rc =
                orte_soh.set_proc_soh(data->name, ORTE_PROC_STATE_ABORTED, status))) {
            ORTE_ERROR_LOG(rc);
        }
         /* tell the user something went wrong */
        ompi_output(0, "ERROR: The probe on head node %s of the %s cluster failed to start as expected.",
                    data->headnode, data->target_cluster);
        ompi_output(0, "ERROR: There may be more information available from");
        ompi_output(0, "ERROR: the remote shell (see above).");
        if (WIFEXITED(status)) {
            ompi_output(0, "ERROR: The probe exited unexpectedly with status %d.",
                   WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                ompi_output(0, "The probe received a signal %d (with core).",
                            WTERMSIG(status));
            } else {
                ompi_output(0, "The probe received a signal %d.", WTERMSIG(status));
            }
#else
            ompi_output(0, "The probe received a signal %d.", WTERMSIG(status));
#endif /* WCOREDUMP */
        } else {
            ompi_output(0, "No extra status information is available: %d.", status);
        }
    }
}

