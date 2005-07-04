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
#include <signal.h>

#include "include/orte_constants.h"
#include "opal/util/argv.h"
#include "util/ompi_environ.h"
#include "opal/util/output.h"
#include "util/univ_info.h"
#include "util/session_dir.h"
#include "util/if.h"
#include "util/path.h"
#include "opal/event/event.h"
#include "runtime/orte_wait.h"

#include "mca/base/mca_base_param.h"

#include "mca/ns/ns.h"
#include "util/sys_info.h"
#include "util/if.h"
#include "mca/pls/pls.h"
#include "mca/rml/rml.h"
#include "mca/gpr/gpr.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ras/base/ras_base_node.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/rmgr/base/base.h"
#include "mca/soh/soh.h"
#include "mca/soh/base/base.h"
#include "mca/pls/rsh/pls_rsh.h"

#define NUM_CONCURRENT 128

extern char **environ;


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
    opal_object_t super;
    orte_ras_base_node_t* node;
    orte_jobid_t jobid;
};
typedef struct rsh_daemon_info_t rsh_daemon_info_t;
static OBJ_CLASS_INSTANCE(rsh_daemon_info_t,
                          opal_object_t,
                          NULL, NULL);
static void set_handler_default(int sig);

/**
 * Callback on daemon exit.
 */

static void orte_pls_rsh_wait_daemon(pid_t pid, int status, void* cbdata)
{
    rsh_daemon_info_t *info = (rsh_daemon_info_t*) cbdata;
    opal_list_t map;
    opal_list_item_t* item;
    int rc;

    /* if ssh exited abnormally, set the child processes to aborted
       and print something useful to the user.  The usual reasons for
       ssh to exit abnormally all are a pretty good indication that
       the child processes aren't going to start up properly. 

       This should somehow be pushed up to the calling level, but we
       don't really have a way to do that just yet.
    */
#ifdef WIN32
    printf("This is not implemented yet for windows\n");
    ORTE_ERROR_LOG(ORTE_ERROR);
    return;
#else
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* get the mapping for our node so we can cancel the right things */
        OBJ_CONSTRUCT(&map, opal_list_t);
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
        for(item =  opal_list_get_first(&map);
            item != opal_list_get_end(&map);
            item =  opal_list_get_next(item)) {
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
        opal_output(0, "ERROR: A daemon on node %s failed to start as expected.",
        			info->node->node_name);
        opal_output(0, "ERROR: There may be more information available from");
        opal_output(0, "ERROR: the remote shell (see above).");

        if (WIFEXITED(status)) {
            opal_output(0, "ERROR: The daemon exited unexpectedly with status %d.",
                   WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                opal_output(0, "The daemon received a signal %d (with core).",
                			WTERMSIG(status));
            } else {
            	opal_output(0, "The daemon received a signal %d.", WTERMSIG(status));
            }
#else
			opal_output(0, "The daemon received a signal %d.", WTERMSIG(status));
#endif /* WCOREDUMP */
        } else {
            opal_output(0, "No extra status information is available: %d.", status);
        }
    }
#endif /* WIN32 */

    /* release any waiting threads */
    OPAL_THREAD_LOCK(&mca_pls_rsh_component.lock);
    if(mca_pls_rsh_component.num_children-- >= NUM_CONCURRENT ||
       mca_pls_rsh_component.num_children == 0) {
        opal_condition_signal(&mca_pls_rsh_component.cond);
    }
    OPAL_THREAD_UNLOCK(&mca_pls_rsh_component.lock);

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
    size_t i;
    int rc;
                                                                                                                  
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
    opal_list_t nodes;
    opal_list_item_t* item;
    size_t num_nodes;
    orte_vpid_t vpid;
    int node_name_index1;
    int node_name_index2;
    int proc_name_index;
    int local_exec_index;
    char *jobid_string;
    char *uri, *param;
    char** argv;
    int argc;
    int rc;
    int id;
    sigset_t sigs;
    
    /* query the list of nodes allocated to the job - don't need the entire
     * mapping - as the daemon/proxy is responsibe for determining the apps
     * to launch on each node.
     */
    OBJ_CONSTRUCT(&nodes, opal_list_t);

    rc = orte_ras_base_node_query_alloc(&nodes, jobid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /*
     * Allocate a range of vpids for the daemons.
     */

    num_nodes = opal_list_get_size(&nodes);
    if(num_nodes == 0) {
        return ORTE_ERR_BAD_PARAM;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* need integer value for command line parameter */
    asprintf(&jobid_string, "%lu", (unsigned long) jobid);

    /*
     * Build argv array
     */
    argv = opal_argv_copy(mca_pls_rsh_component.argv);
    argc = mca_pls_rsh_component.argc;
    node_name_index1 = argc;
    opal_argv_append(&argc, &argv, "");  /* placeholder for node name */

    /* add the daemon command (as specified by user) */
    local_exec_index = argc;
    opal_argv_append(&argc, &argv, mca_pls_rsh_component.orted);
    
    /* check for debug flags */
    id = mca_base_param_register_int("orte","debug",NULL,NULL,0);
    mca_base_param_lookup_int(id,&rc);
    if (rc) {
         opal_argv_append(&argc, &argv, "--debug");
    }
    id = mca_base_param_register_int("orte","debug","daemons",NULL,0);
    mca_base_param_lookup_int(id,&rc);
    if (rc) {
         opal_argv_append(&argc, &argv, "--debug-daemons");
    }
    id = mca_base_param_register_int("orte","debug","daemons_file",NULL,0);
    mca_base_param_lookup_int(id,&rc);
    if (rc) {
         opal_argv_append(&argc, &argv, "--debug-daemons-file");
    }

    opal_argv_append(&argc, &argv, "--bootproxy");
    opal_argv_append(&argc, &argv, jobid_string);
    opal_argv_append(&argc, &argv, "--name");
    proc_name_index = argc;
    opal_argv_append(&argc, &argv, "");

    /* tell the daemon how many procs are in the daemon's job */
    opal_argv_append(&argc, &argv, "--num_procs");
    asprintf(&param, "%lu", (unsigned long)(vpid + num_nodes));
    opal_argv_append(&argc, &argv, param);
    free(param);
    /* tell the daemon the starting vpid of the daemon's job */
    opal_argv_append(&argc, &argv, "--vpid_start");
    opal_argv_append(&argc, &argv, "0");
    
    opal_argv_append(&argc, &argv, "--nodename");
    node_name_index2 = argc;
    opal_argv_append(&argc, &argv, "");

    /* pass along the universe name and location info */
    opal_argv_append(&argc, &argv, "--universe");
    asprintf(&param, "%s@%s:%s", orte_universe_info.uid,
                orte_universe_info.host, orte_universe_info.name);
    opal_argv_append(&argc, &argv, param);
    free(param);
    
    /* setup ns contact info */
    opal_argv_append(&argc, &argv, "--nsreplica");
    if(NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);
    free(param);

    /* setup gpr contact info */
    opal_argv_append(&argc, &argv, "--gprreplica");
    if(NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);
    free(param);

    /*
     * Iterate through each of the nodes and spin
     * up a daemon.
     */

    for(item =  opal_list_get_first(&nodes);
        item != opal_list_get_end(&nodes);
        item =  opal_list_get_next(item)) {
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        orte_process_name_t* name;
        pid_t pid;
        char *exec_path;
        char **exec_argv;

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
#ifdef WIN32
        printf("Unimplemented feature for windows\n");
        return;
#if 0
 {
    /* Do fork the windows way: see opal_few() for example */
    HANDLE new_process;
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    DWORD process_id;

    ZeroMemory (&si, sizeof(si));
    ZeroMemory (&pi, sizeof(pi));

    GetStartupInfo (&si);
    if (!CreateProcess (NULL,
                        "new process",
                        NULL,
                        NULL,
                        TRUE,
                        0,
                        NULL,
                        NULL,
                        &si,
                        &pi)){
       /* actual error can be got by simply calling GetLastError() */
       return OMPI_ERROR;
    }
    /* get child pid */
    process_id = GetProcessId(&pi);
    pid = (int) process_id;
 }
#endif

#else
        pid = fork();
#endif
        if(pid < 0) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }

        /* child */
        if(pid == 0) {
            char* name_string;
            char** env;
            char* var;

            /* Is this a local launch?
             *
             * Not all node names may be resolvable (if we found
             * localhost in the hostfile, for example).  So first
             * check trivial case of node_name being same as the
             * current nodename, which must be local.  If that doesn't
             * match, check using ifislocal().
             */
            if (0 == strcmp(node->node_name, orte_system_info.nodename) ||
                ompi_ifislocal(node->node_name)) {
                exec_argv = &argv[local_exec_index];
                exec_path = ompi_path_findv(exec_argv[0], 0, environ, NULL);
            } else {
                exec_argv = argv;
                exec_path = strdup(mca_pls_rsh_component.path);
            }
        
            /* setup process name */
            rc = orte_ns.get_proc_name_string(&name_string, name);
            if(ORTE_SUCCESS != rc) {
                opal_output(0, "orte_pls_rsh: unable to create process name");
                exit(-1);
            }
            argv[proc_name_index] = name_string;

            if (mca_pls_rsh_component.debug > 2) {
                /* debug output */
                char* cmd = opal_argv_join(argv, ' ');  
                opal_output(0, "orte_pls_rsh: %s %s\n", exec_path, cmd);
                exit(0);
            } 

            if (mca_pls_rsh_component.debug == 0) {
                 /* setup stdin */
                int fd = open("/dev/null", O_RDWR);
                dup2(fd, 0);
                close(fd);
            }

            /* Set signal handlers back to the default.  Do this close
               to the exev() because the event library may (and likely
               will) reset them.  If we don't do this, the event
               library may have left some set that, at least on some
               OS's, don't get reset via fork() or exec().  Hence, the
               orted could be unkillable (for example). */

            set_handler_default(SIGTERM);
            set_handler_default(SIGINT);
#ifndef WIN32
            set_handler_default(SIGHUP);
            set_handler_default(SIGPIPE);
#endif
            set_handler_default(SIGCHLD);

            /* Unblock all signals, for many of the same reasons that
               we set the default handlers, above.  This is noticable
               on Linux where the event library blocks SIGTERM, but we
               don't want that blocked by the orted (or, more
               specifically, we don't want it to be blocked by the
               orted and then inherited by the ORTE processes that it
               forks, making them unkillable by SIGTERM). */
#ifndef WIN32
            sigprocmask(0, 0, &sigs);
            sigprocmask(SIG_UNBLOCK, &sigs, 0);
#endif

            /* setup environment */
            env = opal_argv_copy(environ);
            var = mca_base_param_environ_variable("seed",NULL,NULL);
            ompi_setenv(var, "0", true, &env);

            /* set the progress engine schedule for this node.
             * if node_slots is set to zero, then we default to
             * NOT being oversubscribed
             */
            if (node->node_slots > 0 &&
                node->node_slots_inuse > node->node_slots) {
                var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
                ompi_setenv(var, "1", true, &env);
            } else {
                var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
                ompi_setenv(var, "0", true, &env);
            }
            free(var);
    
            /* exec the daemon */
            execve(exec_path, exec_argv, env);
            opal_output(0, "orte_pls_rsh: execv failed with errno=%d\n", errno);
            exit(-1);

        } else {
            rsh_daemon_info_t *daemon_info;

            OPAL_THREAD_LOCK(&mca_pls_rsh_component.lock);
            if(mca_pls_rsh_component.num_children++ >= NUM_CONCURRENT)
                 opal_condition_wait(&mca_pls_rsh_component.cond, &mca_pls_rsh_component.lock);
            OPAL_THREAD_UNLOCK(&mca_pls_rsh_component.lock);

            /* save the daemons name on the node */
            if (ORTE_SUCCESS != (rc = orte_pls_rsh_set_node_name(node,jobid,name))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            /* setup callback on sigchild - wait until setup above is complete 
             * as the callback can occur in the call to orte_wait_cb
             */
            daemon_info = OBJ_NEW(rsh_daemon_info_t);
            OBJ_RETAIN(node);
            daemon_info->node = node;
            daemon_info->jobid = jobid;
            orte_wait_cb(pid, orte_pls_rsh_wait_daemon, daemon_info);


            /* if required - add delay to avoid problems w/ X11 authentication */
            if (mca_pls_rsh_component.debug && mca_pls_rsh_component.delay) {
                sleep(mca_pls_rsh_component.delay);
            }
            vpid++;
        }
        free(name);
    }


cleanup:
    while(NULL != (item = opal_list_remove_first(&nodes))) {
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
    size_t i, j, num_values = 0;
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
        OPAL_THREAD_LOCK(&mca_pls_rsh_component.lock);
        while(mca_pls_rsh_component.num_children > 0) {
            opal_condition_wait(&mca_pls_rsh_component.cond, &mca_pls_rsh_component.lock);
        }
        OPAL_THREAD_UNLOCK(&mca_pls_rsh_component.lock);
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
    opal_condition_t cond;
    opal_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_pls_rsh_stack_t orte_pls_rsh_stack_t;

static void orte_pls_rsh_stack_construct(orte_pls_rsh_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, opal_condition_t);
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
    opal_object_t,
    orte_pls_rsh_stack_construct,
    orte_pls_rsh_stack_destruct);

static void orte_pls_rsh_launch_cb(int fd, short event, void* args)
{
    orte_pls_rsh_stack_t *stack = (orte_pls_rsh_stack_t*)args;
    OPAL_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_pls_rsh_launch(stack->jobid);
    stack->complete = true;
    opal_condition_signal(&stack->cond);
    OPAL_THREAD_UNLOCK(&stack->mutex);
}

static int orte_pls_rsh_launch_threaded(orte_jobid_t jobid)
{
    struct timeval tv = { 0, 0 };
    struct opal_event event;
    struct orte_pls_rsh_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_pls_rsh_stack_t);

    stack.jobid = jobid;
    opal_evtimer_set(&event, orte_pls_rsh_launch_cb, &stack);
    opal_evtimer_add(&event, &tv);

    OPAL_THREAD_LOCK(&stack.mutex);
    while(stack.complete == false)
         opal_condition_wait(&stack.cond, &stack.mutex);
    OPAL_THREAD_UNLOCK(&stack.mutex);
    OBJ_DESTRUCT(&stack);
    return stack.rc;
}

#endif


static void set_handler_default(int sig)
{
#ifndef WIN32
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
#endif
}
