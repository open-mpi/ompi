/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>

#include "include/orte_constants.h"
#include "util/argv.h"
#include "util/output.h"
#include "event/event.h"
#include "runtime/orte_wait.h"
#include "mca/ns/ns.h"
#include "mca/pls/pls.h"
#include "mca/rml/rml.h"
#include "mca/ras/base/ras_base_node.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "pls_rsh.h"

#define NUM_CONCURRENT 128


#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS
static int orte_pls_rsh_launch_threaded(orte_jobid_t jobid);
#endif


orte_pls_base_module_1_0_0_t orte_pls_rsh_module = {
#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS
    orte_pls_rsh_launch_threaded,
#else
    orte_pls_rsh_launch,
#endif
    orte_pls_rsh_terminate_job,
    orte_pls_rsh_terminate_proc,
    orte_pls_rsh_finalize
};


/**
 * Callback on daemon exit.
 */

static void orte_pls_rsh_wait_daemon(pid_t pid, int status, void* cbdata)
{
#if 0
    orte_rmaps_base_node_t* node = (orte_rmaps_base_node_t*)cbdata;
    ompi_list_item_t *item;
    int rc;

    /* set the state of all processes launched by this daemon */
    for(item =  ompi_list_get_first(&node->node_procs);
        item != ompi_list_get_end(&node->node_procs);
        item =  ompi_list_get_next(item)) {
        orte_rmaps_base_proc_t* proc = (orte_rmaps_base_proc_t*)item;
        rc = orte_soh.set_proc_soh(&proc->proc_name, ORTE_PROC_STATE_TERMINATED, status);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
    }
    OBJ_RELEASE(node);
#endif

    /* release any waiting threads */
    OMPI_THREAD_LOCK(&mca_pls_rsh_component.lock);
    if(mca_pls_rsh_component.num_children-- >= NUM_CONCURRENT ||
       mca_pls_rsh_component.num_children == 0) {
        ompi_condition_signal(&mca_pls_rsh_component.cond);
    }
    OMPI_THREAD_UNLOCK(&mca_pls_rsh_component.lock);
}


int orte_pls_rsh_launch(orte_jobid_t jobid)
{
    ompi_list_t nodes;
    ompi_list_item_t* item;
    size_t num_nodes;
    orte_vpid_t vpid_start;
    int node_name_index;
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
    rc = orte_ns.reserve_range(0, num_nodes, &vpid_start);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /*
     * Build argv/env arrays.
     */
    argv = ompi_argv_copy(mca_pls_rsh_component.argv);
    argc = mca_pls_rsh_component.argc;
    node_name_index = argc;
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
        pid_t pid;

        /* setup node name */
        argv[node_name_index] = node->node_name;

        /* rsh a child to exec the rsh/ssh session */
        pid = fork();
        if(pid < 0) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }

        /* child */
        if(pid == 0) {

            orte_process_name_t* name;
            char* name_string;
            int fd = open("/dev/null", O_RDWR);

            /* setup process name */
            rc = orte_ns.create_process_name(&name, node->node_cellid, 0, vpid_start);
            if(ORTE_SUCCESS != rc) {
                ompi_output(0, "orte_pls_rsh: unable to create process name");
                exit(-1);
            }
            rc = orte_ns.get_proc_name_string(&name_string, name);
            if(ORTE_SUCCESS != rc) {
                ompi_output(0, "orte_pls_rsh: unable to create process name");
                exit(-1);
            }
            argv[proc_name_index] = name_string;

            /* debug output */
            if(mca_pls_rsh_component.debug) {
                char* cmd = ompi_argv_join(argv, ' ');  
                ompi_output(0, "orte_pls_rsh: %s\n", cmd);
            }

            /* setup stdin/stdout/stderr */
            dup2(fd, 0);
            dup2(fd, 1);
            dup2(fd, 2);
            close(fd);

            /* exec the daemon */
            execv(mca_pls_rsh_component.path, argv);
            ompi_output(0, "orte_pls_rsh: execv failed with errno=%d\n", errno);
            exit(-1);

        } else {

            OMPI_THREAD_LOCK(&mca_pls_rsh_component.lock);
            if(mca_pls_rsh_component.num_children++ >= NUM_CONCURRENT)
                 ompi_condition_wait(&mca_pls_rsh_component.cond, &mca_pls_rsh_component.lock);
            OMPI_THREAD_UNLOCK(&mca_pls_rsh_component.lock);

            OBJ_RETAIN(node);
            orte_wait_cb(pid, orte_pls_rsh_wait_daemon, node);
        }
    }

cleanup:
    while(NULL != (item = ompi_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    return rc;
}

int orte_pls_rsh_terminate_job(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_pls_rsh_terminate_proc(const orte_process_name_t* proc)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_pls_rsh_finalize(void)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


/**
 * Handle threading issues.
 */

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS

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

