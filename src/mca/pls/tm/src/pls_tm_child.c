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

#include "orte_config.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/environ.h"
#include "runtime/runtime.h"
#include "runtime/orte_wait.h"
#include "mca/base/mca_base_param.h"
#include "mca/rmgr/base/base.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/pls/pls.h"
#include "mca/pls/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/soh/soh_types.h"
#include "mca/gpr/gpr.h"
#include "mca/ns/base/ns_base_nds.h"
#include "mca/soh/soh.h"
#include "mca/rml/rml.h"
#include "mca/ns/ns.h"
#include "pls_tm.h"


/*
 * Local functions
 */
static int do_tm_resolve(char *hostnames, tm_node_id *tnodeid);
static int query_tm_hostnames(void);
static char* get_tm_hostname(tm_node_id node);


/*
 * Local variables.  Note that these are only used *per child
 * process*, so we're guaranteed that only one thread will be using
 * these -- no need for locking.
 */
static char **tm_hostnames = NULL;
static tm_node_id *tm_node_ids = NULL;
static tm_task_id *task_ids = NULL;
static size_t num_spawned = 0;
static int num_tm_hostnames, num_node_ids;
static orte_process_name_t *names = NULL;


int orte_pls_tm_child_init(void)
{
    int ret;
    char* uri;
    orte_cellid_t new_cellid;
    orte_jobid_t new_jobid;
    orte_vpid_t new_vpid;
    orte_process_name_t *new_child_name;

    /* Re-start us as a new ORTE process */

    ompi_set_using_threads(false);
    ompi_output(orte_pls_base.pls_output,
                "pls:tm:launch:child: starting");
    if (NULL == (uri = orte_rml.get_uri())) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit(-1);
    }
    ompi_output(orte_pls_base.pls_output,
                "pls:tm:launch:child: got uri: %s", uri);

    orte_ns.get_cellid(&new_cellid, orte_process_info.my_name);
    orte_ns.get_jobid(&new_jobid, orte_process_info.my_name);
    new_vpid = 1;
    orte_ns.reserve_range(new_jobid, 1, &new_vpid);
    if (ORTE_JOBID_MAX == new_jobid ||
        ORTE_CELLID_MAX == new_cellid ||
        ORTE_VPID_MAX == new_vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        exit(-1);
    }
    ret = orte_ns.create_process_name(&new_child_name, new_cellid,
                                      new_jobid, new_vpid);
    if (ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
        exit(-1);
    }
    ompi_output(orte_pls_base.pls_output,
                "pls:tm:launch:child: restarting ORTE");
    ret = orte_restart(new_child_name, uri);
    if (ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
        exit(-1);
    }
    ompi_output(orte_pls_base.pls_output,
                "pls:tm:launch:child: am now a new ORTE process");

    /* All done */

    return ORTE_SUCCESS;
}


int orte_pls_tm_child_launch(orte_jobid_t jobid)
{
    int ret, local_errno;
    size_t i, j;
    tm_event_t event;
    char *flat;
    char old_cwd[OMPI_PATH_MAX];
    ompi_list_t mapping;
    bool mapping_valid = false;
    ompi_list_item_t *item;
    char **mca_env = NULL, **tmp_env, **local_env;
    char *path, *new_path;
    int num_mca_env;
    orte_rmaps_base_proc_t *proc;
    orte_app_context_t *app;
    bool failure;
    tm_node_id tnodeid;
    struct tm_roots tm_root;

    /* Open up our connection to tm */

    ret = tm_init(NULL, &tm_root);
    if (TM_SUCCESS != ret) {
        return ORTE_ERR_RESOURCE_BUSY;
    }
    orte_pls_tm_connected = true;

    /* Get the hostnames from the output of the mapping.  Since we
       have to cross reference against TM, it's much more efficient to
       do all the nodes in the entire map all at once. */

    OBJ_CONSTRUCT(&mapping, ompi_list_t);
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_get_map(jobid, &mapping))) {
        goto cleanup;
    }
    mapping_valid = true;

    /* Count how many processes we're starting so that we can allocate
       space for all the tid's */

    for (failure = false, i = 0, item = ompi_list_get_first(&mapping);
         !failure && item != ompi_list_get_end(&mapping);
         item = ompi_list_get_next(item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*) item;
        i += map->num_procs;
    }
    ompi_output(orte_pls_base.pls_output,
                "pls:tm:launch:child: found a total of %d procs", i);
    task_ids = malloc((sizeof(tm_task_id) * i) + 
                      (sizeof(orte_process_name_t) * i));
    if (NULL == task_ids) {
        ret = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    names = (orte_process_name_t*) (task_ids + i);
    memset(names, 0, sizeof(orte_process_name_t) * i);

    /* Make up an environment for all the job processes. */

    mca_env = NULL;
    num_mca_env = 0;
    mca_base_param_build_env(&mca_env, &num_mca_env, true);

    /* While we're traversing these data structures, also setup the
       proc_status array for later a "put" to the registry */

    getcwd(old_cwd, OMPI_PATH_MAX);
    failure = false;
    for (num_spawned = i = 0, item = ompi_list_get_first(&mapping);
         !failure && item != ompi_list_get_end(&mapping);
         item = ompi_list_get_next(item), ++i) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*) item;
        app = map->app;

        /* See if the app cwd exists; try changing to the cwd and then
           changing back */

        if (0 != chdir(app->cwd)) {
            ret = ORTE_ERR_NOT_FOUND;
            ORTE_ERROR_LOG(ret);
            goto cleanup; 
        }
        ompi_output(orte_pls_base.pls_output,
                    "pls:tm:launch:child: app %d cwd (%s) exists", 
                    i, app->cwd);

        /* Get a full pathname for argv[0] -- tm won't spawn without
           an absolute pathname.  :-( app->app is already an absolute
           pathname, so don't even bother to check -- just replace
           argv[0] with app->app. */

        free(app->argv[0]);
        app->argv[0] = strdup(app->app);
        flat = ompi_argv_join(app->argv, ' ');

        /* Make a global env for the app */

        tmp_env = ompi_environ_merge(app->env, mca_env);
        local_env = ompi_environ_merge(environ, tmp_env);
        if (NULL != tmp_env) {
            ompi_argv_free(tmp_env);
        }

        /* Ensure "." is in the PATH.  If it's not there, add it at
           the end */

        for (j = 0; NULL != local_env[j]; ++j) {
            if (0 == strncmp("PATH=", local_env[j], 5)) {
                path = local_env[j] + 5;
                if (0 != strcmp(".", path) &&
                    0 != strncmp(".:", path, 2) &&
                    NULL == strstr(":.:", path) &&
                    0 != strncmp(":.", path + strlen(path) - 2, 2)) {
                    asprintf(&new_path, "PATH=%s:.", path);
                    free(local_env[j]);
                    local_env[j] = new_path;
                    ompi_output(orte_pls_base.pls_output,
                                "pls:tm:launch:child: appended \".\" to PATH");
                    break;
                }
            }
        }

        /* Now iterate through all the procs in this app and launch them */

        for (j = 0; j < map->num_procs; ++j, ++num_spawned) {
            proc = map->procs[j];

            /* Get a TM node ID for the node for this proc */

            if (ORTE_SUCCESS != do_tm_resolve(proc->proc_node->node_name,
                                              &tnodeid)) {
                ret = ORTE_ERR_NOT_FOUND;
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }

            /* Set the job name in the environment */

            orte_ns_nds_env_put(&proc->proc_name, num_spawned, 1, &local_env);

            /* Launch it */
            
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:launch:child: starting process %d (%s) on %s (TM node id %d)",
                        num_spawned, flat, proc->proc_node->node_name, 
                        tnodeid);

            if (TM_SUCCESS != tm_spawn(app->argc, app->argv,
                                       local_env, tnodeid, 
                                       &task_ids[num_spawned], &event)) {
                ret = ORTE_ERR_RESOURCE_BUSY;
                ORTE_ERROR_LOG(ret);
                goto loop_error;
            }
            
            ret = tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
            if (TM_SUCCESS != ret) {
                ret = ORTE_ERR_RESOURCE_BUSY;
                ORTE_ERROR_LOG(ret);
                goto loop_error;
            }
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:launch:child: launch successful (tid %d); posting to registry", task_ids[num_spawned]);

            /* Write this proc's TID to the registry (so that we can
               kill it if we need to) */

            orte_pls_tm_put_tid(&(proc->proc_name), task_ids[num_spawned], 
                                ORTE_PROC_STATE_LAUNCHED);

            /* Bastardize this function to set our state to
               ORTE_PROC_STATE_LAUNCHED with a bogus PID (make it
               equal this proc's index in the overall job -- i.e.,
               rank in MPI_COMM_WORLD) */

            ret = orte_pls_base_set_proc_pid(&(proc->proc_name), num_spawned);
            if (ORTE_SUCCESS != ret) {
                ret = ORTE_ERR_RESOURCE_BUSY;
                ORTE_ERROR_LOG(ret);
                goto loop_error;
            }

            /* Save the name so that we can use it later */

            names[num_spawned] = proc->proc_name;

            /* Ok, we succeeded in lauching that process.  Loop around
               to get the next. */

            continue;

        loop_error:
            /* Hack so that we don't have to make the
               pls_tm_terminate_job globally scoped */
            orte_pls_tm_module.terminate_job(jobid);
            failure = true;
            break;
        }

        /* Now go back to the original cwd */

        if (0 != chdir(old_cwd)) {
            ret = ORTE_ERR_NOT_FOUND;
            ORTE_ERROR_LOG(ret);
            goto cleanup; 
        }

        /* Free things from the last app */

        ompi_argv_free(local_env);
        free(flat);
    }

    /* All done */

 cleanup:
    ompi_output(orte_pls_base.pls_output,
                "pls:tm:launch:child: launched %d processes", num_spawned);

    if (NULL != mca_env) {
        ompi_argv_free(mca_env);
    }
    if (mapping_valid) {
        while (NULL != (item = ompi_list_remove_first(&mapping))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&mapping);
    }

    tm_finalize();
    orte_pls_tm_connected = false;

    return ret;
}


/*
 * Waiting for the death of all the tm_spawn'ed processes.
 */
int orte_pls_tm_child_wait(orte_jobid_t jobid)
{
    size_t i, j;
    int ret, local_errno, *exit_statuses;
    tm_event_t event, *events;
    struct tm_roots tm_root;

    ompi_output(orte_pls_base.pls_output,
                "pls:tm:wait:child: waiting for processes to exit");

    /* Open up our connection to tm */

    ret = tm_init(NULL, &tm_root);
    if (TM_SUCCESS != ret) {
        ret = ORTE_ERR_RESOURCE_BUSY;
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    orte_pls_tm_connected = true;

    /* Setup to wait for all the tid's to die */

    events = malloc((sizeof(tm_event_t) * num_spawned) +
                    (sizeof(int) * num_spawned));
    if (NULL == events) {
        ret = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    exit_statuses = (int*) (events + num_spawned);

    /* Ask for all obituaries */

    for (i = 0; i < num_spawned; ++i) {
        ret = tm_obit(task_ids[i], &exit_statuses[i], &events[i]);
        if (TM_SUCCESS != ret) {
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:kill: tm_obit failed with %d", ret);
            ret = ORTE_ERROR;
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
    }

    /* Poll until we get all obituaries */

    for (i = 0; i < num_spawned; ++i) {
        tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
        for (j = 0; j < num_spawned; ++j) {
            if (event == events[j]) {
                ompi_output(orte_pls_base.pls_output,
                            "pls:tm:wait:child: caught obit for tid %d",
                            task_ids[j]);
                ret = orte_soh.set_proc_soh(&names[j], 
                                            ORTE_PROC_STATE_TERMINATED, 
                                            exit_statuses[j]);
                events[j] = TM_NULL_EVENT;
                break;
            }
        }

        if (j >= num_spawned) {
            fprintf(stderr, "Whoops!  Didn't find return event!\n");
        }
    }

 cleanup:
    ompi_output(orte_pls_base.pls_output,
                "pls:tm:wait:child: done waiting for process obits");

    if (NULL != events) {
        free(events);
    }

    /* All done */

    if (orte_pls_tm_connected) {
        tm_finalize();
    }
    orte_pls_tm_connected = false;

    return ORTE_SUCCESS;
}


int orte_pls_tm_child_finalize(void)
{
    if (NULL != tm_hostnames) {
        ompi_argv_free(tm_hostnames);
        tm_hostnames = NULL;
    }
    if (NULL != tm_node_ids) {
        free(tm_node_ids);
        tm_node_ids = NULL;
    }

    /* All done */

    ompi_output(orte_pls_base.pls_output,
                "pls:tm:finalize:child: all done -- exiting");
    orte_finalize();
    return ORTE_SUCCESS;
}

/***********************************************************************/

/*
 * Take a list of hostnames and return their corresponding TM node
 * ID's.  This is not the most efficient method of doing this, but
 * it's not much of an issue here (this is not a performance-critical
 * section of code)
 */
static int do_tm_resolve(char *hostname, tm_node_id *tnodeid)
{
    int i, ret;

    /* Have we already queried TM for all the node info? */

    if (NULL == tm_hostnames) {
        ret = query_tm_hostnames();
        if (ORTE_SUCCESS != ret) {
            return ret;
        }
    }

    /* Find the TM ID of the hostname that we're looking for */

    for (i = 0; i < num_tm_hostnames; ++i) {
        if (0 == strcmp(hostname, tm_hostnames[i])) {
            *tnodeid = tm_node_ids[i];
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:launch: resolved host %s to node ID %d",
                        hostname, tm_node_ids[i]);
            break;
        }
    }

    /* All done */

    if (i < num_tm_hostnames) {
        ret = ORTE_SUCCESS;
    } else { 
        ret = ORTE_ERR_NOT_FOUND;
    }

    return ret;
}


static int query_tm_hostnames(void)
{
    char *h;
    int i, ret;

    /* Get the list of nodes allocated in this PBS job */

    ret = tm_nodeinfo(&tm_node_ids, &num_node_ids);
    if (TM_SUCCESS != ret) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* TM "nodes" may actually correspond to PBS "VCPUs", which means
       there may be multiple "TM nodes" that correspond to the same
       physical node.  This doesn't really affect what we're doing
       here (we actually ignore the fact that they're duplicates --
       slightly inefficient, but no big deal); just mentioned for
       completeness... */

    tm_hostnames = NULL;
    num_tm_hostnames = 0;
    for (i = 0; i < num_node_ids; ++i) {
        h = get_tm_hostname(tm_node_ids[i]);
        ompi_argv_append(&num_tm_hostnames, &tm_hostnames, h);
        free(h);
    }

    /* All done */

    return ORTE_SUCCESS;
}


/*
 * For a given TM node ID, get the string hostname corresponding to
 * it.
 */
static char* get_tm_hostname(tm_node_id node)
{
    int ret, local_errno;
    char *hostname;
    tm_event_t event;
    char buffer[256];
    char **argv;

    /* Get the info string corresponding to this TM node ID */

    ret = tm_rescinfo(node, buffer, sizeof(buffer) - 1, &event);
    if (TM_SUCCESS != ret) {
        return NULL;
    }

    /* Now wait for that event to happen */

    ret = tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
    if (TM_SUCCESS != ret) {
        return NULL;
    }

    /* According to the TM man page, we get back a space-separated
       string array.  The hostname is the second item.  Use a cheap
       trick to get it. */

    buffer[sizeof(buffer) - 1] = '\0';
    argv = ompi_argv_split(buffer, ' ');
    if (NULL == argv) {
        return NULL;
    }
    hostname = strdup(argv[1]);
    ompi_argv_free(argv);

    /* All done */

    return hostname;
}


