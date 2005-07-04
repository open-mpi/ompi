/* -*- C -*-
 * 
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
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <string.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "util/proc_info.h"
#include "opal/event/event.h"
#include "runtime/orte_wait.h"
#include "runtime/runtime.h"
#include "mca/ns/base/base.h"
#include "mca/ns/base/ns_base_nds.h"
#include "mca/pls/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/iof/iof.h"
#include "mca/rmgr/base/base.h"
#include "mca/rmaps/base/base.h"
#include "mca/rml/rml.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ras/base/base.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "util/sys_info.h"
#include "mca/oob/base/base.h"
#include "pls_bproc.h"


orte_pls_base_module_t orte_pls_bproc_module = {
    orte_pls_bproc_launch,
    orte_pls_bproc_terminate_job,
    orte_pls_bproc_terminate_proc,
    orte_pls_bproc_finalize
};

static int orte_pls_bproc_node_array(orte_rmaps_base_map_t* map, 
                                     int ** node_array, int * node_array_len);
static int orte_pls_bproc_node_list(int * node_array, int node_array_len, 
                                    int ** node_list, int * num_nodes, 
                                    int num_procs);
static int orte_pls_bproc_setup_io(orte_jobid_t jobid, struct bproc_io_t * io, 
                                   int node_rank, size_t app_context);
static int orte_pls_bproc_launch_app(orte_jobid_t jobid, 
                                     orte_rmaps_base_map_t* map, 
                                     orte_vpid_t vpid_start, 
                                     orte_vpid_t vpid_range, size_t app_context);

/**
 * creates an array that is indexed by the node number and each entry contains the
 * number of processes that will be launched on that node.
 *
 * @param map single context mapping 
 * @param node_array a pointer to put the node array into
 * @param node_array_len returns the length of the array
 * @retval >=0 the number of processes
 * @retval <0 ompi err
 */
static int orte_pls_bproc_node_array(orte_rmaps_base_map_t* map, 
                                     int ** node_array, int * node_array_len) {
    opal_list_item_t* item;
    int num_procs = 0;
    int num_on_node;
    
    *node_array_len = 0;
    for(item =  opal_list_get_first(&map->nodes);
        item != opal_list_get_end(&map->nodes);
        item =  opal_list_get_next(item)) {
        if(*node_array_len < atol(((orte_rmaps_base_node_t*)item)->node_name)) {
            *node_array_len = atol(((orte_rmaps_base_node_t*)item)->node_name);
        }
    }
    (*node_array_len)++;
    /* build the node array */
    *node_array = (int*)malloc(sizeof(int) * *node_array_len);
    if(NULL == *node_array) {
        ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset(*node_array, 0, sizeof(int) * *node_array_len);

    for(item =  opal_list_get_first(&map->nodes);
        item != opal_list_get_end(&map->nodes);
        item =  opal_list_get_next(item)) {
        orte_rmaps_base_node_t* node = (orte_rmaps_base_node_t*)item;
        num_on_node = opal_list_get_size(&node->node_procs);
        (*node_array)[atol(node->node_name)] += num_on_node;
        num_procs += num_on_node;
    }
    return num_procs;
}

/**
 * Creates a bproc nodelist from a node array. 
 * @param node_array an array of bproc nodes that contains the number of processes
 *                   to be launched on each node
 * @param node_array_len the length of the node array
 * @param node_list a pointer that the bproc node list will be returned in
 * @param num_nodes a pointer to return the number of nodes in the node list
 * @param num_procs the number of processes that a node must have to be on the 
 *                  node list
 */
static int orte_pls_bproc_node_list(int * node_array, int node_array_len, 
                                   int ** node_list, int * num_nodes, 
                                   int num_procs) {
    int node;
    *num_nodes = 0;
    *node_list = (int*)malloc(sizeof(int) * node_array_len);
    if(NULL == *node_list) {
        ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    /* build the node list */
    for(node = 0; node < node_array_len; node++) {
        if(node_array[node] >= num_procs) {
            (*node_list)[(*num_nodes)++] = node;
        }
    }
    return OMPI_SUCCESS;
}

/**
 * Sets up the bproc io structs for the specified rank on the nodes
 *
 * @param jobid
 * @param io A pointer to an array of 3 bproc_io_t structs
 * @param node_rank the rank on the node we are setting up the structs for
 */
static int orte_pls_bproc_setup_io(orte_jobid_t jobid, struct bproc_io_t * io, 
                                   int node_rank, size_t app_context) {
    char *frontend = NULL, *path = NULL, *job = NULL;
    int rc, i;

    /* ensure that system info is set */
    orte_sys_info();

    if (NULL == orte_system_info.user) { /* error condition */
        return OMPI_ERROR;
    }

    if (NULL == orte_universe_info.name) {  /* error condition */
        return OMPI_ERROR;
    }

    rc = orte_ns_base_convert_jobid_to_string(&job, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    if (0 > asprintf(&frontend, "%stmp%sopenmpi-bproc-%s%s%s%s%s-%d%s%d",
                     orte_system_info.path_sep, orte_system_info.path_sep, 
                     orte_system_info.user, orte_system_info.path_sep, 
                     orte_universe_info.name, orte_system_info.path_sep, job, 
                     (int) app_context, orte_system_info.path_sep, node_rank)) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        goto cleanup;
    }

    for(i = 0; i < 3; i++)
    {
        if(0 > asprintf(&path, "%s%s%d", frontend, 
                        orte_system_info.path_sep, i)) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
            goto cleanup;
        }
        if (mca_pls_bproc_component.debug) {
            opal_output(0, "mpirun bproc io setup. Path: %s\n", path);
        }
        io[i].fd = i;
        io[i].type = BPROC_IO_FILE;
#if defined BPROC_API_VERSION && BPROC_API_VERSION >= 4
        io[i].flags = 0;
#else
        io[i].send_info = 0;
#endif       
        if(0 == i) {
            io[i].d.file.flags = O_RDONLY;
        } else {
            io[i].d.file.flags = O_WRONLY;
        }
        io[i].d.file.offset = 0;
        io[i].d.file.mode = 0;
        strncpy(io[i].d.file.name, path, 256); 
        free(path);
    } 

 cleanup:
    if (NULL != frontend) {
       free(frontend);
    }
    if (NULL != job) {
        free(job);
    }
    return rc;
}

/**
 * 1. Launch the deamons on the backend nodes. 
 * 2. The daemons setup files for io forwarding then connect back to us to 
 *    tells us they are ready for the actual apps.
 * 3. Launch the apps on the backend nodes
 */
static int orte_pls_bproc_launch_app(orte_jobid_t jobid, 
                                     orte_rmaps_base_map_t* map, 
                                     orte_vpid_t vpid_start, 
                                     orte_vpid_t vpid_range, size_t app_context) {
    int * node_list = NULL;
    int * node_array = NULL;
    int node_array_len;
    int num_nodes;
    int num_processes = 0;
    int * pids = NULL;
    orte_vpid_t daemon_vpid_start = 0;
    int rc, i, j;
    char ** argv = NULL;
    int argc;
    char *var;
    char * param;
    int num_env;
    int num_daemons;
    struct bproc_io_t bproc_io[3];
    orte_buffer_t ack;
    orte_jobid_t daemon_jobid;
    orte_cellid_t cellid;
    orte_vpid_t global_vpid_start = vpid_start;

    OBJ_CONSTRUCT(&ack, orte_buffer_t);
    
    /* convert node names to a node array */
    num_processes = orte_pls_bproc_node_array(map, &node_array, &node_array_len);
    if(0 > num_processes) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* create a list of all the nodes that need daemons, which is all the nodes
     * that will have at least 1 process  */
    rc = orte_pls_bproc_node_list(node_array, node_array_len, &node_list, 
                                  &num_daemons, 1);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    if(NULL == (pids = (int*)malloc(sizeof(int) * num_daemons))) {
        ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        goto cleanup;
    }

    /* append mca parameters to our environment */
    num_env = map->app->num_env;
    rc = mca_base_param_build_env(&map->app->env, &num_env, true);
    if(ORTE_SUCCESS != rc) { 
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* set name discovery mode */
    var = mca_base_param_environ_variable("ns","nds",NULL);
    opal_setenv(var, "bproc", true, &map->app->env);
    free(var);

    /* ns replica contact info */
    if(NULL == orte_process_info.ns_replica) {
        rc = orte_ns.copy_process_name(&orte_process_info.ns_replica, 
                                       orte_process_info.my_name);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        orte_process_info.ns_replica_uri = orte_rml.get_uri();
    }
    var = mca_base_param_environ_variable("ns","replica","uri");
    opal_setenv(var,orte_process_info.ns_replica_uri, true, &map->app->env);
    free(var);

    /* make sure the frontend hostname does not get pushed out to the backend */
    var = mca_base_param_environ_variable("orte", "base", "nodename");
    opal_unsetenv(var, &map->app->env);
    free(var);
    opal_unsetenv("HOSTNAME", &map->app->env);

    /* make sure the username used to create the bproc directory is the same on
     * the backend as the frontend */
    var = mca_base_param_environ_variable("pls","bproc","username");
    opal_setenv(var, orte_system_info.user, true, &map->app->env);
    free(var);

    /* tell the bootproxy to use the bproc_orted pls */
    var = mca_base_param_environ_variable("rmgr", "bootproxy", "pls");
    opal_setenv(var, "bproc_orted", true, &map->app->env);
    free(var);

    /* gpr replica contact info */
    if(NULL == orte_process_info.gpr_replica) {
        rc = orte_ns.copy_process_name(&orte_process_info.gpr_replica,
                                       orte_process_info.my_name);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        orte_process_info.gpr_replica_uri = orte_rml.get_uri();
    }
    var = mca_base_param_environ_variable("gpr","replica","uri");
    opal_setenv(var,orte_process_info.gpr_replica_uri, true, &map->app->env);
    free(var);

    asprintf(&param, "%d", (int) app_context);
    var = mca_base_param_environ_variable("pls", "bproc", "app_context");
    opal_setenv(var, param, true, &map->app->env);
    free(param);
    free(var);
  
    /* overwrite previously specified values with the above settings */
    map->app->num_env = opal_argv_count(map->app->env);

    /* allocate a range of vpids for the daemons */
    rc = orte_ns_base_get_jobid(&daemon_jobid, orte_process_info.my_name);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    rc = orte_ns.reserve_range(daemon_jobid, num_daemons, &daemon_vpid_start);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    rc = orte_ns_base_get_cellid(&cellid, orte_process_info.my_name);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    /* set up the environment so the daemons can get their names once launched */
    rc = orte_ns_nds_bproc_put(cellid, daemon_jobid, daemon_vpid_start, 
                               global_vpid_start, num_processes, &map->app->env);
    if(ORTE_SUCCESS != rc) { 
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    argc = 0;
    opal_argv_append(&argc, &argv, mca_pls_bproc_component.orted);
    /* check for debug flags */
    if (mca_pls_bproc_component.debug) {
         opal_argv_append(&argc, &argv, "--debug");
         opal_argv_append(&argc, &argv, "--debug-daemons");
    }

    opal_argv_append(&argc, &argv, "--bootproxy");
    orte_ns.convert_jobid_to_string(&param, jobid);
    opal_argv_append(&argc, &argv, param);
    free(param);

    /* pass along the universe name and location info */
    opal_argv_append(&argc, &argv, "--universe");
    asprintf(&param, "%s@%s:%s", orte_universe_info.uid,
                orte_universe_info.host, orte_universe_info.name);
    opal_argv_append(&argc, &argv, param);
    free(param);

    if(0 < mca_pls_bproc_component.debug) {
        opal_output(0, "PLS_BPROC DEBUG: launching %d daemons. cmd: %s ", 
                    num_daemons, mca_pls_bproc_component.orted);
    }
    
    /* launch the daemons */
    rc = bproc_vexecmove(num_daemons, node_list, pids, mca_pls_bproc_component.orted,
                         argv, map->app->env);

    if(0 < mca_pls_bproc_component.debug) {
        opal_output(0, "PLS_BPROC DEBUG: %d daemons launched. First pid: %d\n", 
                    rc, *pids);
    }

    if(rc != num_daemons) {
        opal_output(0, "Failed to launch proper number of daemons.");
        rc = ORTE_ERROR;
        goto cleanup;
    }
    for(i = 0; i < num_daemons; i++) {
        if(0 >= pids[i]) {
            opal_output(0, "pls_bproc: failed to launch all daemons. " 
                        "Daemon pid was %d on node %d\n", pids[i], node_list[i]);
            rc = ORTE_ERROR;
            goto cleanup;
        }
    }
    
    /* wait for communication back */
    for(i = 0; i < num_daemons; i++) {
        rc = mca_oob_recv_packed(MCA_OOB_NAME_ANY, &ack, MCA_OOB_TAG_BPROC);
        if(0 > rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* launch the processes */
    i = 1;
    rc = orte_pls_bproc_node_list(node_array, node_array_len, &node_list,
                                  &num_nodes, i);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    while(0 != num_nodes) {
        /* setup environment so the procs can figure out their names */
        rc = orte_ns_nds_bproc_put(cellid, jobid, vpid_start, global_vpid_start,
                                   num_processes, &map->app->env);
        if(ORTE_SUCCESS != rc) { 
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        rc = orte_pls_bproc_setup_io(jobid, bproc_io, i - 1, app_context);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if(0 < mca_pls_bproc_component.debug) {
            opal_output(0, "pls_bproc: launching %d processes", num_nodes);
        }
        rc = bproc_vexecmove_io(num_nodes, node_list, pids, bproc_io, 3, 
                                map->app->app, map->app->argv, map->app->env);
        if(0 < mca_pls_bproc_component.debug) {
            opal_output(0, "pls_bproc: %d processes launched. First pid: %d",
                        rc, *pids);
        }
        if(rc != num_nodes) {
            opal_output(0, "pls_bproc: Failed to launch proper number of processes.");
            rc = ORTE_ERROR;
            goto cleanup;
        }
        for(j = 0; j < num_nodes; j++) {
            if(0 >= pids[j]) {
                opal_output(0, "pls_bproc: failed to launch all processes. Process"
                            " pid was %d on node %d\n", pids[j], node_list[j]);
                rc = ORTE_ERROR;
                goto cleanup;
            }
        }
        free(node_list);
        node_list = NULL;
        i++;
        rc = orte_ns.derive_vpid(&vpid_start, vpid_start, num_nodes);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        rc = orte_pls_bproc_node_list(node_array, node_array_len, &node_list,
                                      &num_nodes, i);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
   
cleanup:
    if(NULL != node_list) {
        free(node_list);
    }
    if(NULL != node_array) {
        free(node_array);
    }
    if(NULL != argv) {
        opal_argv_free(argv);
    }
    if(NULL != pids) {
        free(pids);
    }
    OBJ_DESTRUCT(&ack);
    return rc;
}

/**
 * Query for the default mapping.  Launch each application context 
 * w/ a distinct set of daemons.
 */
int orte_pls_bproc_launch(orte_jobid_t jobid)
{
    opal_list_item_t* item;
    opal_list_t mapping;
    orte_vpid_t vpid_start;
    orte_vpid_t vpid_range;
    int rc;

    /* query for the application context and allocated nodes */
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_map(jobid, &mapping))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_vpid_range(jobid, &vpid_start, 
                                                            &vpid_range))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* for each application context - launch across the first n nodes required */
    for(item =  opal_list_get_first(&mapping);
        item != opal_list_get_end(&mapping);
        item =  opal_list_get_next(item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)item;
        rc = orte_pls_bproc_launch_app(jobid, map, vpid_start, vpid_range, 
                                       map->app->idx); 
        if(rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
  
cleanup:
    while(NULL != (item = opal_list_remove_first(&mapping)))
        OBJ_RELEASE(item);
    OBJ_DESTRUCT(&mapping);
    return rc;
}


/**
 * Terminate all processes associated with this job - including
 * daemons.
 */

int orte_pls_bproc_terminate_job(orte_jobid_t jobid)
{
    pid_t* pids;
    size_t i, num_pids;
    int rc;

    /* kill application process */
    if(ORTE_SUCCESS != (rc = orte_pls_base_get_proc_pids(jobid, &pids, &num_pids)))
        return rc;
    for(i=0; i<num_pids; i++) {
        if(mca_pls_bproc_component.debug) {
            opal_output(0, "orte_pls_bproc: killing proc: %d\n", pids[i]);
        }
        kill(pids[i], mca_pls_bproc_component.terminate_sig);
    }
    if(NULL != pids)
        free(pids);

    /* kill daemons */
    if(ORTE_SUCCESS != (rc = orte_pls_base_get_node_pids(jobid, &pids, &num_pids)))
        return rc;
    for(i=0; i<num_pids; i++) { 
        if(mca_pls_bproc_component.debug) {
            opal_output(0, "orte_pls_bproc: killing daemon: %d\n", pids[i]);
        }
        kill(pids[i], mca_pls_bproc_component.terminate_sig);
    }
    if(NULL != pids)
        free(pids);
    return ORTE_SUCCESS;
}


/**
 * Terminate a specific process.
 */
int orte_pls_bproc_terminate_proc(const orte_process_name_t* proc_name)
{
    int rc;
    pid_t pid;
    if(ORTE_SUCCESS != (rc = orte_pls_base_get_proc_pid(proc_name, &pid)))
        return rc;
    if(kill(pid, mca_pls_bproc_component.terminate_sig) != 0) {
        switch(errno) {
            case EINVAL:
                return ORTE_ERR_BAD_PARAM;
            case ESRCH:
                return ORTE_ERR_NOT_FOUND;
            case EPERM:
                return ORTE_ERR_PERM;
            default:
                return ORTE_ERROR;
        }
    }
    return ORTE_SUCCESS;
}

/**
 * Module cleanup
 */
int orte_pls_bproc_finalize(void)
{
    return ORTE_SUCCESS;
}

