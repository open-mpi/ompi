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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <fcntl.h>


#include "include/orte_constants.h"
#include "dps/dps.h"
#include "opal/event/event.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "runtime/orte_wait.h"
#include "util/argv.h"
#include "util/ompi_environ.h"
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
#include "mca/rds/rds_types.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
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

/* Local condition variables and mutex
 */
static opal_mutex_t orte_setup_hnp_mutex;
static opal_condition_t orte_setup_hnp_condition;
/* Local return code */
static int orte_setup_hnp_rc;
/* Local uri storage */
static char *orte_setup_hnp_orted_uri;

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
#ifndef WIN32
    char **argv, *param, *uri, *uid, *hn=NULL;
    char *path, *name_string, *orteprobe;
    int argc, rc=ORTE_SUCCESS, id, intparam;
    pid_t pid;
    bool can_launch=false, on_gpr=false;
    orte_cellid_t cellid=ORTE_CELLID_MAX;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    size_t i, j, k, cnt=0;
    orte_gpr_value_t **values=NULL, *value;
    orte_gpr_keyval_t **keyvals;
    char *keys[4], *tokens[3], *cellname;
    struct timeval tv;
    struct timespec ts;

    /* get the nodename for the headnode of the target cluster */
    if (NULL == headnode) {  /* not provided, so try to look it up */
        tokens[0] = target_cluster;
        tokens[1] = NULL;
        keys[0] = ORTE_RDS_FE_NAME;
        keys[1] = ORTE_RDS_FE_SSH;
        keys[2] = ORTE_CELLID_KEY;
        keys[3] = NULL;
        if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR,
                                    ORTE_RESOURCE_SEGMENT,
                                    tokens, keys, &cnt, &values))) {
           ORTE_ERROR_LOG(rc);
           return rc;
        }
        if (0 == cnt || 0 == values[0]->cnt) {  /* nothing found */
            goto MOVEON;
        }
        on_gpr = true;
        /* need to decide what to do if more than value found. Some
         * clusters have more than one head node, so which one do
         * we choose? For now, just take the first one returned.
         */
        keyvals = values[0]->keyvals;
        for (i=0; i < values[0]->cnt; i++) {
            if (0 == strcmp(keyvals[i]->key, ORTE_RDS_FE_NAME)) {
                hn = strdup(keyvals[i]->value.strptr);
                continue;
            }
            if (0 == strcmp(keyvals[i]->key, ORTE_RDS_FE_SSH)) {
                can_launch = keyvals[i]->value.tf_flag;
                continue;
            }
            if (0 == strcmp(keyvals[i]->key, ORTE_CELLID_KEY)) {
                cellid = keyvals[i]->value.cellid;
                continue;
            }
        }
        goto MOVEON;
        
    } else {  /* lookup the headnode's cellid */
        hn = strdup(headnode);
        keys[0] = ORTE_RDS_FE_NAME;
        keys[1] = ORTE_RDS_FE_SSH;
        keys[2] = ORTE_CELLID_KEY;
        keys[3] = NULL;
        if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR,
                                    ORTE_RESOURCE_SEGMENT,
                                    NULL, keys, &cnt, &values))) {
           ORTE_ERROR_LOG(rc);
           return rc;
        }
        if (0 == cnt || 0 == values[0]->cnt) {  /* nothing found */
            goto MOVEON;
        }
        on_gpr = true;
        for (i=0; i < cnt; i++) {
            keyvals = values[i]->keyvals;
            for (j=0; j < values[i]->cnt; j++) {
                if ((0 == strcmp(keyvals[j]->key, ORTE_RDS_FE_NAME)) &&
                     0 == strcmp(keyvals[j]->value.strptr, headnode)) {
                    /* okay, this is the right cell - now need to find
                     * the ssh flag (if provided) and cellid
                     */
                    for (k=0; k < values[i]->cnt; k++) {
                        if (0 == strcmp(keyvals[k]->key, ORTE_RDS_FE_SSH)) {
                            can_launch = keyvals[k]->value.tf_flag;
                            continue;
                        }
                        if (0 == strcmp(keyvals[k]->key, ORTE_CELLID_KEY)) {
                            cellid = keyvals[k]->value.cellid;
                            continue;
                        }
                    }
                    goto MOVEON;
                }
            }
        }
    }

MOVEON:
    if (NULL != values) {
        for (i=0; i < cnt; i++) OBJ_RELEASE(values[i]);
        free(values);
    }
    
    if (!on_gpr && (NULL != target_cluster || NULL != headnode)) {
        /* if we couldn't find anything about this cell on the gpr, then
         * we need to put the required headnode data on the registry. We need
         * it to be there so other functions/processes can find it, if needed.
         * User must provide either a target_cluster name (which then must be
         * synonymous with the headnode name), a headnode name (on a named or
         * unnamed target_cluster), or both.
         */
        /* get new cellid for this site/resource */
        if (NULL != target_cluster) {
            cellname = strdup(target_cluster);
        } else {
            /* if the target_cluster was NULL, then headnode CAN'T be NULL
             * or else we wouldn't get here
             */
            cellname = strdup(headnode);
        }
        /* can't know the site name, so it becomes "unknown" */
        if (ORTE_SUCCESS != (rc = orte_ns.create_cellid(&cellid, "UNKNOWN",
                                        cellname))) {
            ORTE_ERROR_LOG(rc);
            free(cellname);
            return rc;
        }
        /* now store the cell info on the resource segment of the registry */
        value = OBJ_NEW(orte_gpr_value_t);
        if (NULL == value) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        value->addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
        value->segment = strdup(ORTE_RESOURCE_SEGMENT);
        
        value->cnt = 4;
        value->keyvals = (orte_gpr_keyval_t**)malloc(value->cnt * sizeof(orte_gpr_keyval_t*));
        if (NULL == value->keyvals) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i < value->cnt; i++) {
            value->keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
            if (NULL == value->keyvals[i]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
        }
        value->keyvals[0]->key = strdup(ORTE_RDS_NAME);
        value->keyvals[0]->type = ORTE_STRING;
        value->keyvals[0]->value.strptr = strdup(cellname);
        value->keyvals[1]->key = strdup(ORTE_CELLID_KEY);
        value->keyvals[1]->type = ORTE_CELLID;
        value->keyvals[1]->value.cellid = cellid;
        value->keyvals[2]->key = strdup(ORTE_RDS_FE_NAME);
        value->keyvals[2]->type = ORTE_STRING;
        if (NULL == headnode) {
            value->keyvals[2]->value.strptr = strdup(cellname);
        } else {
            value->keyvals[2]->value.strptr = strdup(headnode);
        }
        value->keyvals[3]->key = strdup(ORTE_RDS_FE_SSH);
        value->keyvals[3]->type = ORTE_BOOL;
        value->keyvals[3]->value.tf_flag = true;

        value->num_tokens = 3;
        value->tokens = (char**)malloc(3 * sizeof(char*));
        if (NULL == value->tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_cellid_to_string(&(value->tokens[0]), cellid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        value->tokens[1] = strdup("UNKNOWN"); /* site name is unknown */
        value->tokens[2] = strdup(cellname);
        
        if (ORTE_SUCCESS != orte_gpr.put(1, &value)) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            return rc;
        }
        OBJ_RELEASE(value);
        free(cellname);
        can_launch = true;
    }
    
    orte_gpr.dump_segments(0);
    
    if (!can_launch || ORTE_CELLID_MAX == cellid) {
        return ORTE_ERR_UNREACH;
    }

    /* get the user's name on the headnode */
    if (NULL == username) {
        uid = strdup(orte_system_info.user);
    } else {
        uid = strdup(username);
    }
    
    /* SETUP TO LAUNCH PROBE */
    
    /* setup the conditioned wait and mutex variables */
    OBJ_CONSTRUCT(&orte_setup_hnp_mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_setup_hnp_condition, opal_condition_t);

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
   
    /* get name of probe application - just in case user specified something different */
    id = mca_base_param_register_string("orteprobe",NULL,NULL,NULL,"orteprobe");
    mca_base_param_lookup_string(id, &orteprobe);
    
    /* get rsh/ssh launch mechanism parameters */
    id = mca_base_param_register_string("pls","rsh","agent",NULL,"ssh");
    mca_base_param_lookup_string(id, &param);

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
    free(param);
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
    free(param);
    free(uri);

    /* tell the probe who to report to */
    uri = orte_rml.get_uri();
    asprintf(&param, "\"%s\"", uri);
    ompi_argv_append(&argc, &argv, "--requestor");
    ompi_argv_append(&argc, &argv, param);
    free(param);
    free(uri);
    
    /* pass along any parameters for the head node process
     * in case one needs to be created
     */
    id = mca_base_param_register_string("scope",NULL,NULL,NULL,"private");
    mca_base_param_lookup_string(id, &param);
    ompi_argv_append(&argc, &argv, "--scope");
    ompi_argv_append(&argc, &argv, param);
    free(param);
    
    id = mca_base_param_register_int("persistent",NULL,NULL,NULL,(int)false);
    mca_base_param_lookup_int(id, &intparam);
    if (intparam) {
        ompi_argv_append(&argc, &argv, "--persistent");
    }
    
    /* issue the non-blocking recv to get the probe's findings */
    rc = orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_PROBE,
                                 0, orte_setup_hnp_recv, NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* fork a child to exec the rsh/ssh session */
    orte_setup_hnp_rc = ORTE_SUCCESS;
    pid = fork();
    if (pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    if (pid == 0) {     /* child */
        /* exec the probe launch */
        execv(path, argv);
        ORTE_ERROR_LOG(ORTE_ERROR);
        ompi_output(0, "orte_setup_hnp: execv failed with errno=%d\n", errno);
        return ORTE_ERROR;

    } else {    /* parent */
        orte_wait_cb(pid, orte_setup_hnp_wait, &orte_setup_hnp_cbdata);

        /* block until a timeout occurs or probe dies/calls back */
        gettimeofday(&tv, NULL);
        ts.tv_sec = tv.tv_sec + 1000000;
        ts.tv_nsec = 0;
    
        OPAL_THREAD_LOCK(&orte_setup_hnp_mutex);
        opal_condition_timedwait(&orte_setup_hnp_condition, &orte_setup_hnp_mutex, &ts);
        OPAL_THREAD_UNLOCK(&orte_setup_hnp_mutex);

        if (ORTE_SUCCESS == orte_setup_hnp_rc) {
            /* need to restart the local system so it can connect to the remote daemon.
             * we only want to clear the run-time itself - we cannot close the OPAL
             * utilities, though, or we will lose all of our MCA parameters
             */
            orte_system_finalize();
            /*
             * now set the relevant MCA parameters to point us at the remote daemon...
             */
            if (ORTE_SUCCESS != (rc = ompi_setenv("OMPI_MCA_gpr_replica_uri",
                        orte_setup_hnp_orted_uri, true, &environ))) {
                fprintf(stderr, "orte_setup_hnp: could not set gpr_replica_uri in environ\n");
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = ompi_setenv("OMPI_MCA_ns_replica_uri",
                        orte_setup_hnp_orted_uri, true, &environ))) {
                fprintf(stderr, "orte_setup_hnp: could not set ns_replica_uri in environ\n");
                return rc;
            }

            ompi_unsetenv("OMPI_MCA_seed", &environ);
            
            if (ORTE_SUCCESS != (rc = ompi_setenv("OMPI_MCA_universe_uri",
                        orte_setup_hnp_orted_uri, true, &environ))) {
                fprintf(stderr, "orte_setup_hnp: could not set universe_uri in environ\n");
                return rc;
            }

            /*
             * ...re-init ourselves...
             */
            if (ORTE_SUCCESS != (rc = orte_system_init())) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /*
             * ...and we are now ready to go!
             */
            return ORTE_SUCCESS;
        }
        
        return orte_setup_hnp_rc;
    }
    
CLEANUP:
    return rc;

#else
    printf ("This function has not been implemented in windows yet, file %s line %d\n", __FILE__, __LINE__);
    abort();
#endif

}

static void orte_setup_hnp_recv(int status, orte_process_name_t* sender,
                                orte_buffer_t* buffer, orte_rml_tag_t tag,
                                void* cbdata)
{
    size_t n=1;
    int rc;
    
    OPAL_THREAD_LOCK(&orte_setup_hnp_mutex);
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &orte_setup_hnp_orted_uri, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        orte_setup_hnp_rc = rc;
        opal_condition_signal(&orte_setup_hnp_condition);
        OPAL_THREAD_UNLOCK(&orte_setup_hnp_mutex);
        return;
    }
    orte_setup_hnp_rc = ORTE_SUCCESS;
    opal_condition_signal(&orte_setup_hnp_condition);
    OPAL_THREAD_UNLOCK(&orte_setup_hnp_mutex);
}

static void orte_setup_hnp_wait(pid_t wpid, int status, void *cbdata)
{
    orte_setup_hnp_cb_data_t *data;
    
    OPAL_THREAD_LOCK(&orte_setup_hnp_mutex);

    data = (orte_setup_hnp_cb_data_t*)cbdata;

    /* if ssh exited abnormally, print something useful to the user and cleanup
     * the registry entries for the HNP jobid.
       This should somehow be pushed up to the calling level, but we
       don't really have a way to do that just yet.
    */
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
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

    opal_condition_signal(&orte_setup_hnp_condition);
    OPAL_THREAD_UNLOCK(&orte_setup_hnp_mutex);

}

