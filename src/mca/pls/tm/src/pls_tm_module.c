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
#include "mca/base/mca_base_param.h"
#include "mca/rmgr/base/base.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/pls/pls.h"
#include "mca/pls/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/soh/soh_types.h"
#include "mca/gpr/gpr.h"
#include "mca/ns/base/ns_base_nds.h"
#include "pls_tm.h"


/*
 * Local functions
 */
static int pls_tm_launch(orte_jobid_t jobid);
static int pls_tm_terminate_job(orte_jobid_t jobid);
static int pls_tm_terminate_proc(const orte_process_name_t *name);
static int pls_tm_finalize(void);

static int do_tm_resolve(char *hostnames, tm_node_id *tnodeid);
static int query_tm_hostnames(void);
static char* get_tm_hostname(tm_node_id node);
static int kill_tids(tm_task_id *tids, int num_tids);


/*
 * Global variable
 */
orte_pls_base_module_1_0_0_t orte_pls_tm_module = {
    pls_tm_launch,
    pls_tm_terminate_job,
    pls_tm_terminate_proc,
    pls_tm_finalize
};

extern char **environ;
#define NUM_SIGNAL_POLL_ITERS 50


/*
 * Local variables
 */
static bool tm_connected = false;
static struct tm_roots tm_root;
static char **tm_hostnames = NULL;
static tm_node_id *tm_node_ids;
static int num_node_ids, num_tm_hostnames;



static int pls_tm_launch(orte_jobid_t jobid)
{
    int ret, local_errno;
    size_t i, j, count;
    tm_event_t event;
    char *flat;
    char old_cwd[OMPI_PATH_MAX];
    ompi_list_t mapping;
    bool mapping_valid = false;
    ompi_list_item_t *item;
    char **mca_env, **tmp_env, **local_env;
    int num_mca_env;
    orte_rmaps_base_proc_t *proc;
    orte_app_context_t *app;
    bool failure;
    tm_node_id tnodeid;
    tm_task_id tid;

    /* Open up our connection to tm */

    ret = tm_init(NULL, &tm_root);
    if (TM_SUCCESS != ret) {
        return ORTE_ERR_RESOURCE_BUSY;
    }
    tm_connected = true;

    /* Get the hostnames from the output of the mapping.  Since we
       have to cross reference against TM, it's much more efficient to
       do all the nodes in the entire map all at once. */

    OBJ_CONSTRUCT(&mapping, ompi_list_t);
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_get_map(jobid, &mapping))) {
        goto cleanup;
    }
    mapping_valid = true;

    /* While we're traversing these data structures, also setup the
       proc_status array for later a "put" to the registry */

    getcwd(old_cwd, OMPI_PATH_MAX);
    failure = false;
    for (count = i = 0, item = ompi_list_get_first(&mapping);
         !failure && item != ompi_list_get_end(&mapping);
         item = ompi_list_get_next(item), ++i) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*) item;
        app = map->app;

        /* See if the app cwd exists; try changing to the cwd and then
           changing back */

        if (0 != chdir(app->cwd) ||
            0 != chdir(old_cwd)) {
            ret = ORTE_ERR_NOT_FOUND;
            ORTE_ERROR_LOG(ret);
            goto cleanup; 
        }
        ompi_output(orte_pls_base.pls_output,
                    "pls:tm:launch: app %d cwd (%s) exists", i, app->cwd);

        /* A few things global to the app */

        flat = ompi_argv_join(app->argv, ' ');

        num_mca_env = 0;
        mca_env = ompi_argv_copy(environ);
        mca_base_param_build_env(&mca_env, &num_mca_env, true);
        tmp_env = ompi_environ_merge(app->env, mca_env);
        local_env = ompi_environ_merge(environ, tmp_env);
        if (NULL != mca_env) {
            ompi_argv_free(mca_env);
        }
        if (NULL != tmp_env) {
            ompi_argv_free(tmp_env);
        }

        /* Now iterate through all the procs in this app and launch them */

        for (j = 0; j < map->num_procs; ++j, ++count) {
            proc = map->procs[j];

            /* Get a TM node ID for the node for this proc */

            if (ORTE_SUCCESS != do_tm_resolve(proc->proc_node->node_name,
                                              &tnodeid)) {
                ret = ORTE_ERR_NOT_FOUND;
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }

            /* Set the job name in the environment */

            orte_ns_nds_env_put(&proc->proc_name, count, 1, &local_env);

            /* Launch it */
            
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:launch: starting process %d (%s) on %s (TM node id %d)",
                        count, flat, proc->proc_node->node_name, tnodeid);

            if (TM_SUCCESS != tm_spawn(app->argc, app->argv,
                                       local_env, tnodeid, &tid, &event)) {
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

            /* Write this proc's status to the registry */

            orte_pls_tm_put_tid(&(proc->proc_name), tid, 
                                ORTE_PROC_STATE_LAUNCHED);
            continue;

        loop_error:
            pls_tm_terminate_job(jobid);
            failure = true;
            break;
        }

        /* Free things from the last app */

        ompi_argv_free(local_env);
        free(flat);
    }

    /* All done */

 cleanup:
    if (mapping_valid) {
        while (NULL != (item = ompi_list_remove_first(&mapping))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&mapping);
    }
    tm_finalize();
    tm_connected = false;
    return ret;
}


static int pls_tm_terminate_job(orte_jobid_t jobid)
{
    tm_task_id *tids;
    size_t num_tids;
    int ret;

    /* Open up our connection to tm.  Note that we may be called from
       launch, above, in which case we don't need to tm_init */

    if (!tm_connected) {
        ret = tm_init(NULL, &tm_root);
        if (TM_SUCCESS != ret) {
            return ORTE_ERR_RESOURCE_BUSY;
        }
    }

    /* Get the TIDs from the registry */

    ret = orte_pls_tm_get_tids(jobid, &tids, &num_tids);
    if (ORTE_SUCCESS == ret) {
        ret = kill_tids(tids, num_tids);
        free(tids);
    }

    /* All done */

    if (!tm_connected) {
        tm_finalize();
    }
    return ret;
}


/*
 * TM can't kill individual processes -- PBS will kill the entire job
 */
static int pls_tm_terminate_proc(const orte_process_name_t *name)
{
    return ORTE_ERR_NOT_SUPPORTED;
}


/*
 * Free stuff
 */
static int pls_tm_finalize(void)
{
    if (NULL != tm_hostnames) {
        free(tm_node_ids);
        ompi_argv_free(tm_hostnames);
        tm_hostnames = NULL;
    }

    return ORTE_SUCCESS;
}


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


/*
 * Kill a bunch of tids.  Don't care about errors here -- just make a
 * best attempt to kill kill kill; if we fail, oh well.
 */
static int kill_tids(tm_task_id *tids, int num_tids)
{
    int j, i, ret, local_errno, exit_status;
    tm_event_t event;
    bool killed;

    for (i = 0; i < num_tids; ++i) {

        /* First, kill with SIGTERM */

        ret = tm_kill(tids[i], SIGTERM, &event);
        if (TM_SUCCESS != ret) {
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:kill: tm_kill failed with %d", ret);
            ret = ORTE_ERROR;
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
        ompi_output(orte_pls_base.pls_output,
                    "pls:tm:kill: killed TID %d with SIGTERM", tids[i]);

        /* Did it die? */

        ret = tm_obit(tids[i], &exit_status, &event);
        if (TM_SUCCESS != ret) {
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:kill: tm_obit failed with %d", ret);
            ret = ORTE_ERROR;
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        tm_poll(TM_NULL_EVENT, &event, 0, &local_errno);

        /* It didn't seem to die right away; poll a few times */

        if (TM_NULL_EVENT == event) {
            killed = false;
            for (j = 0; j < NUM_SIGNAL_POLL_ITERS; ++j) {
                tm_poll(TM_NULL_EVENT, &event, 0, &local_errno);
                if (TM_NULL_EVENT != event) {
                    killed = true;
                    ompi_output(orte_pls_base.pls_output,
                                "pls:tm:kill: TID %d died", tids[i]);
                    break;
                }
                usleep(1);
            }

            /* No, it did not die.  Try with SIGKILL */

            if (!killed) {
                ret = tm_kill(tids[i], SIGKILL, &event);
                if (TM_SUCCESS != ret) {
                    ompi_output(orte_pls_base.pls_output,
                                "pls:tm:kill: tm_kill failed with %d", ret);
                    ret = ORTE_ERROR;
                    ORTE_ERROR_LOG(ret);
                    return ret;
                }
                tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
                ompi_output(orte_pls_base.pls_output,
                            "pls:tm:kill: killed TID %d with SIGKILL", tids[i]);
                /* Did it die this time? */
                
                ret = tm_obit(tids[i], &exit_status, &event);
                if (TM_SUCCESS != ret) {
                    ompi_output(orte_pls_base.pls_output,
                                "pls:tm:kill: tm_obit failed with %d", ret);
                    ret = ORTE_ERROR;
                    ORTE_ERROR_LOG(ret);
                    return ret;
                }
                 
                tm_poll(TM_NULL_EVENT, &event, 0, &local_errno);

                /* No -- poll a few times -- just to try to clean it
                   up...  If we don't get it here, oh well.  Just let
                   the resources hang; TM will clean them up when the
                   job completed */

                if (TM_NULL_EVENT == event) {
                    for (j = 0; j < NUM_SIGNAL_POLL_ITERS; ++j) {
                        tm_poll(TM_NULL_EVENT, &event, 0, &local_errno);
                        if (TM_NULL_EVENT != event) {
                            ompi_output(orte_pls_base.pls_output,
                                        "pls:tm:kill: TID %d died", tids[i]);
                            break;
                        }
                        usleep(1);
                    }

                    if (j >= NUM_SIGNAL_POLL_ITERS) {
                        ompi_output(orte_pls_base.pls_output,
                                    "pls:tm:kill: TID %d did not die!", tids[i]);
                    }
                }
            }
        }
    }

    /* All done */

    return ORTE_SUCCESS;
}
