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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>

#include "include/orte_constants.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/session_dir.h"
#include "event/event.h"
#include "runtime/orte_wait.h"
#include "mca/ns/ns.h"
#include "mca/pls/pls.h"
#include "mca/rml/rml.h"
#include "mca/gpr/gpr.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ras/base/ras_base_node.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/rmgr/base/base.h"
#include "mca/soh/soh.h"
#include "mca/soh/base/base.h"
#include "pls_rsh.h"

#define NUM_CONCURRENT 128


#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
static int orte_pls_rsh_launch_threaded(orte_jobid_t jobid);
#endif


orte_pls_base_module_1_0_0_t orte_pls_rsh_module = {
#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
    orte_pls_rsh_launch_threaded,
#else
    orte_pls_rsh_launch,
#endif
    orte_pls_rsh_terminate_job,
    orte_pls_rsh_terminate_proc,
    orte_pls_rsh_finalize
};

/* struct used to have enough information to clean up the state of the
   universe if a daemon aborts */
struct rsh_daemon_info_t {
    ompi_object_t super;
    orte_ras_base_node_t* node;
    orte_jobid_t jobid;
};
typedef struct rsh_daemon_info_t rsh_daemon_info_t;
static OBJ_CLASS_INSTANCE(rsh_daemon_info_t,
                          ompi_object_t,
                          NULL, NULL);

/**
 * Callback on daemon exit.
 */

static void orte_pls_rsh_wait_daemon(pid_t pid, int status, void* cbdata)
{
    rsh_daemon_info_t *info = (rsh_daemon_info_t*) cbdata;
    ompi_list_t map;
    ompi_list_item_t* item;
    int rc;

    /* if ssh exited abnormally, set the child processes to aborted
       and print something useful to the user.  The usual reasons for
       ssh to exit abnormally all are a pretty good indication that
       the child processes aren't going to start up properly. 

       This should somehow be pushed up to the calling level, but we
       don't really have a way to do that just yet.
    */
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* get the mapping for our node so we can cancel the right things */
        OBJ_CONSTRUCT(&map, ompi_list_t);
        rc = orte_rmaps_base_get_node_map(orte_process_info.my_name->cellid,
                                          info->jobid,
                                          info->node->node_name,
                                          &map);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* set state of all processes associated with the daemon as
           terminated */
        for(item =  ompi_list_get_first(&map);
            item != ompi_list_get_end(&map);
            item =  ompi_list_get_next(item)) {
            orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*) item;
            size_t i;

            for (i = 0 ; i < map->num_procs ; ++i) {
                /* Clean up the session directory as if we were the
                   process itself.  This covers the case where the
                   process died abnormally and didn't cleanup its own
                   session directory. */

                orte_session_dir_finalize(&(map->procs[i])->proc_name);

                rc = orte_soh.set_proc_soh(&(map->procs[i]->proc_name), 
                                           ORTE_PROC_STATE_ABORTED, status);
            }
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }
        }
        OBJ_DESTRUCT(&map);

 cleanup:
        /* tell the user something went wrong */
        ompi_output(0, "ERROR: A daemon on node %s failed to start as expected.",
        			info->node->node_name);
        ompi_output(0, "ERROR: There may be more information available from");
        ompi_output(0, "ERROR: the remote shell (see above).");
        if (WIFEXITED(status)) {
            ompi_output(0, "ERROR: The daemon exited unexpectedly with status %d.",
                   WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                ompi_output(0, "The daemon received a signal %d (with core).",
                			WTERMSIG(status));
            } else {
            	ompi_output(0, "The daemon received a signal %d.", WTERMSIG(status));
            }
#else
			ompi_output(0, "The daemon received a signal %d.", WTERMSIG(status));
#endif /* WCOREDUMP */
        } else {
            ompi_output(0, "No extra status information is available: %d.", status);
        }
    }

    /* release any waiting threads */
    OMPI_THREAD_LOCK(&mca_pls_rsh_component.lock);
    if(mca_pls_rsh_component.num_children-- >= NUM_CONCURRENT ||
       mca_pls_rsh_component.num_children == 0) {
        ompi_condition_signal(&mca_pls_rsh_component.cond);
    }
    OMPI_THREAD_UNLOCK(&mca_pls_rsh_component.lock);

    /* cleanup */
    OBJ_RELEASE(info->node);
    OBJ_RELEASE(info);
}

/**
 * Set the daemons name in the registry.
 */

static int orte_pls_rsh_set_node_name(orte_ras_base_node_t* node, orte_jobid_t jobid, orte_process_name_t* name)
{
    orte_gpr_value_t* values[1];
    orte_gpr_value_t value;
    orte_gpr_keyval_t kv_name = {{OBJ_CLASS(orte_gpr_keyval_t),0},ORTE_NODE_BOOTPROXY_KEY,ORTE_NAME};
    orte_gpr_keyval_t* keyvals[1];
    char* jobid_string;
    int i, rc;
                                                                                                                  
    if(ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
                                                                                                                  
    if(ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&value.tokens, &value.num_tokens, 
        node->node_cellid, node->node_name))) {
        ORTE_ERROR_LOG(rc);
        free(jobid_string);
        return rc;
    }
                                                                                                                  
    asprintf(&kv_name.key, "%s-%s", ORTE_NODE_BOOTPROXY_KEY, jobid_string);
    kv_name.value.proc = *name;
    keyvals[0] = &kv_name;
    value.keyvals = keyvals;
    value.cnt = 1;
    value.addr_mode = ORTE_GPR_OVERWRITE;
    value.segment = ORTE_NODE_SEGMENT;
    values[0] = &value;
                                                                                                                  
    rc = orte_gpr.put(1, values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
                                                                                                                  
    free(kv_name.key);
    free(jobid_string);
    for(i=0; i<value.num_tokens; i++)
        free(value.tokens[i]);
    free(value.tokens);
    return rc;
}


/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */

int orte_pls_rsh_launch(orte_jobid_t jobid)
{
    ompi_list_t nodes;
    ompi_list_item_t* item;
    size_t num_nodes;
    orte_vpid_t vpid;
    int node_name_index1;
    int node_name_index2;
    int proc_name_index;
    char *jobid_string;
    char *uri, *param;
    char** argv;
    int argc;
    int rc;

    /* query the list of nodes allocated to the job - don't need the entire
     * mapping - as the daemon/proxy is responsibe for determining the apps
     * to launch on each node.
     */
    OBJ_CONSTRUCT(&nodes, ompi_list_t);
    rc = orte_ras_base_node_query_alloc(&nodes, jobid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /*
     * Allocate a range of vpids for the daemons.
     */

    num_nodes = ompi_list_get_size(&nodes);
    if(num_nodes == 0) {
        return ORTE_ERR_BAD_PARAM;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* need integer value for command line parameter - NOT hex */
    asprintf(&jobid_string, "%d", jobid);

    /*
     * Build argv/env arrays.
     */
    argv = ompi_argv_copy(mca_pls_rsh_component.argv);
    argc = mca_pls_rsh_component.argc;
    node_name_index1 = argc;
    ompi_argv_append(&argc, &argv, "");  /* placeholder for node name */

    /* application */
    ompi_argv_append(&argc, &argv, mca_pls_rsh_component.orted);
    if (mca_pls_rsh_component.debug) {
         ompi_argv_append(&argc, &argv, "--debug");
    }
    ompi_argv_append(&argc, &argv, "--bootproxy");
    ompi_argv_append(&argc, &argv, jobid_string);
    ompi_argv_append(&argc, &argv, "--name");
    proc_name_index = argc;
    ompi_argv_append(&argc, &argv, "");
    ompi_argv_append(&argc, &argv, "--nodename");
    node_name_index2 = argc;
    ompi_argv_append(&argc, &argv, "");

    /* setup ns contact info */
    ompi_argv_append(&argc, &argv, "--nsreplica");
    if(NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    ompi_argv_append(&argc, &argv, param);
    free(uri);

    /* setup gpr contact info */
    ompi_argv_append(&argc, &argv, "--gprreplica");
    if(NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    ompi_argv_append(&argc, &argv, param);
    free(uri);

    /*
     * Iterate through each of the nodes and spin
     * up a daemon.
     */

    for(item =  ompi_list_get_first(&nodes);
        item != ompi_list_get_end(&nodes);
        item =  ompi_list_get_next(item)) {
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        orte_process_name_t* name;
        pid_t pid;

        /* setup node name */
        argv[node_name_index1] = node->node_name;
        argv[node_name_index2] = node->node_name;

        /* initialize daemons process name */
        rc = orte_ns.create_process_name(&name, node->node_cellid, 0, vpid);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* rsh a child to exec the rsh/ssh session */
        pid = fork();
        if(pid < 0) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }

        /* child */
        if(pid == 0) {
            char* name_string;

            /* setup process name */
            rc = orte_ns.get_proc_name_string(&name_string, name);
            if(ORTE_SUCCESS != rc) {
                ompi_output(0, "orte_pls_rsh: unable to create process name");
                exit(-1);
            }
            argv[proc_name_index] = name_string;

            if (mca_pls_rsh_component.debug > 2) {
                /* debug output */
                char* cmd = ompi_argv_join(argv, ' ');  
                ompi_output(0, "orte_pls_rsh: %s\n", cmd);
                exit(0);
            } 

            if (mca_pls_rsh_component.debug == 0) {
                 /* setup stdin */
                int fd = open("/dev/null", O_RDWR);
                dup2(fd, 0);
                close(fd);
            }

            /* exec the daemon */
            execv(mca_pls_rsh_component.path, argv);
            ompi_output(0, "orte_pls_rsh: execv failed with errno=%d\n", errno);
            exit(-1);

        } else {
            rsh_daemon_info_t *daemon_info;

            OMPI_THREAD_LOCK(&mca_pls_rsh_component.lock);
            if(mca_pls_rsh_component.num_children++ >= NUM_CONCURRENT)
                 ompi_condition_wait(&mca_pls_rsh_component.cond, &mca_pls_rsh_component.lock);
            OMPI_THREAD_UNLOCK(&mca_pls_rsh_component.lock);

            /* setup callback on sigchild */
            daemon_info = OBJ_NEW(rsh_daemon_info_t);
            OBJ_RETAIN(node);
            daemon_info->node = node;
            daemon_info->jobid = jobid;
            orte_wait_cb(pid, orte_pls_rsh_wait_daemon, daemon_info);

            /* save the daemons name on the node */
            if (ORTE_SUCCESS != (rc = orte_pls_rsh_set_node_name(node,jobid,name))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            /* if required - add delay to avoid problems w/ X11 authentication */
            if (mca_pls_rsh_component.debug && mca_pls_rsh_component.delay) {
                sleep(mca_pls_rsh_component.delay);
            }
            vpid++;
        }
        free(name);
    }


cleanup:
    while(NULL != (item = ompi_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    return rc;
}


/**
 * Wait for a pending job to complete.
 */

static void orte_pls_rsh_terminate_job_rsp(
    int status,
    orte_process_name_t* peer,
    orte_buffer_t* rsp,
    orte_rml_tag_t tag,
    void* cbdata)
{
    int rc;
    if(ORTE_SUCCESS != (rc = orte_rmgr_base_unpack_rsp(rsp))) {
        ORTE_ERROR_LOG(rc);
    }
}


static void orte_pls_rsh_terminate_job_cb(
    int status,
    orte_process_name_t* peer,
    orte_buffer_t* req,
    orte_rml_tag_t tag,
    void* cbdata)
{
    /* wait for response */
    int rc;
    if(status < 0) {
        ORTE_ERROR_LOG(status);
        OBJ_RELEASE(req);
        return;
    }

    if(0 > (rc = orte_rml.recv_buffer_nb(peer, ORTE_RML_TAG_RMGR_CLNT, 0, orte_pls_rsh_terminate_job_rsp, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(req);
}


/**
 * Query the registry for all nodes participating in the job
 */
int orte_pls_rsh_terminate_job(orte_jobid_t jobid)
{
    char *keys[2];
    char *jobid_string;
    orte_gpr_value_t** values = NULL;
    int i, j, num_values = 0;
    int rc;
                                                                                                                           
    if(ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    asprintf(&keys[0], "%s-%s", ORTE_NODE_BOOTPROXY_KEY, jobid_string);
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        ORTE_NODE_SEGMENT,
        NULL,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(jobid_string);
        return rc;
    }
    if(0 == num_values) {
        rc = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    for(i=0; i<num_values; i++) {
        orte_gpr_value_t* value = values[i];
        for(j=0; j<value->cnt; j++) {
            orte_gpr_keyval_t* keyval = value->keyvals[j];
            orte_buffer_t *cmd = OBJ_NEW(orte_buffer_t);
            int ret;
            if(cmd == NULL) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if(strcmp(keyval->key, keys[0]) != 0) 
                continue;

            /* construct command */
            ret = orte_rmgr_base_pack_cmd(cmd, ORTE_RMGR_CMD_TERM_JOB, jobid);
            if(ORTE_SUCCESS != ret) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(cmd);
                rc = ret;
                continue;
            }

            /* send a terminate message to the bootproxy on each node */
            if(0 > (ret = orte_rml.send_buffer_nb(
                &keyval->value.proc, 
                cmd, 
                ORTE_RML_TAG_RMGR_SVC, 
                0, 
                orte_pls_rsh_terminate_job_cb, 
                NULL))) {

                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(cmd);
                rc = ret;
                continue;
            }
        }
    }

cleanup:

    free(jobid_string);
    free(keys[0]);
                                                                                                                           
    if(NULL != values) {
        for(i=0; i<num_values; i++) {
            if(NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        free(values);
    }
    return rc;
}

int orte_pls_rsh_terminate_proc(const orte_process_name_t* proc)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_pls_rsh_finalize(void)
{
    if(mca_pls_rsh_component.reap) {
        OMPI_THREAD_LOCK(&mca_pls_rsh_component.lock);
        while(mca_pls_rsh_component.num_children > 0) {
            ompi_condition_wait(&mca_pls_rsh_component.cond, &mca_pls_rsh_component.lock);
        }
        OMPI_THREAD_UNLOCK(&mca_pls_rsh_component.lock);
    }
                                                                                                                          
    /* cleanup any pending recvs */
    orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_RMGR_CLNT);
    return ORTE_SUCCESS;
}


/**
 * Handle threading issues.
 */

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS

struct orte_pls_rsh_stack_t {
    ompi_condition_t cond;
    ompi_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_pls_rsh_stack_t orte_pls_rsh_stack_t;

static void orte_pls_rsh_stack_construct(orte_pls_rsh_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, ompi_condition_t);
    stack->rc = 0;
    stack->complete = false;
}

static void orte_pls_rsh_stack_destruct(orte_pls_rsh_stack_t* stack)
{
    OBJ_DESTRUCT(&stack->mutex);
    OBJ_DESTRUCT(&stack->cond);
}

static OBJ_CLASS_INSTANCE(
    orte_pls_rsh_stack_t,
    ompi_object_t,
    orte_pls_rsh_stack_construct,
    orte_pls_rsh_stack_destruct);

static void orte_pls_rsh_launch_cb(int fd, short event, void* args)
{
    orte_pls_rsh_stack_t *stack = (orte_pls_rsh_stack_t*)args;
    OMPI_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_pls_rsh_launch(stack->jobid);
    stack->complete = true;
    ompi_condition_signal(&stack->cond);
    OMPI_THREAD_UNLOCK(&stack->mutex);
}

static int orte_pls_rsh_launch_threaded(orte_jobid_t jobid)
{
    struct timeval tv = { 0, 0 };
    struct ompi_event event;
    struct orte_pls_rsh_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_pls_rsh_stack_t);

    stack.jobid = jobid;
    ompi_evtimer_set(&event, orte_pls_rsh_launch_cb, &stack);
    ompi_evtimer_add(&event, &tv);

    OMPI_THREAD_LOCK(&stack.mutex);
    while(stack.complete == false)
         ompi_condition_wait(&stack.cond, &stack.mutex);
    OMPI_THREAD_UNLOCK(&stack.mutex);
    OBJ_DESTRUCT(&stack);
    return stack.rc;
}

#endif

