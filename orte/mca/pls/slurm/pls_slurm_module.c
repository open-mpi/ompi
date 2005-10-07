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

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/runtime/runtime.h"
#include "orte/include/orte_constants.h"
#include "orte/include/orte_types.h"
#include "orte/include/orte_constants.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/base/rmaps_base_map.h"
#include "pls_slurm.h"


/*
 * Local functions
 */
static int pls_slurm_launch(orte_jobid_t jobid);
static int pls_slurm_terminate_job(orte_jobid_t jobid);
static int pls_slurm_terminate_proc(const orte_process_name_t *name);
static int pls_slurm_finalize(void);

static int pls_slurm_start_proc(int argc, char **argv, char **env,
                                char *prefix);


/*
 * Global variable
 */
orte_pls_base_module_1_0_0_t orte_pls_slurm_module = {
    pls_slurm_launch,
    pls_slurm_terminate_job,
    pls_slurm_terminate_proc,
    pls_slurm_finalize
};

/*
 * Local variable
 */
static pid_t srun_pid = 0;


/*
 * External
 */
extern char **environ;


static int pls_slurm_launch(orte_jobid_t jobid)
{
    opal_list_t nodes, mapping_list;
    opal_list_item_t *item, *item2;
    size_t num_nodes;
    orte_vpid_t vpid;
    char *jobid_string;
    char *uri, *param;
    char **argv;
    int argc;
    int rc;
    char *tmp;
    char** env;
    char* var;
    char *nodelist_flat;
    char **nodelist_argv;
    int nodelist_argc;
    orte_process_name_t* name;
    char *name_string;
    char **custom_strings;
    int num_args, i;
    char *cur_prefix;

    /* Query the list of nodes allocated and mapped to this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     * All other mapping responsibilities fall to orted in the fork PLS
     */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    OBJ_CONSTRUCT(&mapping_list, opal_list_t);
    rc = orte_rmaps_base_mapped_node_query(&mapping_list, &nodes, jobid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /*
     * Allocate a range of vpids for the daemons.
     */
    num_nodes = opal_list_get_size(&nodes);
    if (num_nodes == 0) {
        return ORTE_ERR_BAD_PARAM;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* need integer value for command line parameter */
    asprintf(&jobid_string, "%lu", (unsigned long) jobid);

    /*
     * start building argv array
     */
    argv = NULL;
    argc = 0;

    /*
     * SLURM srun OPTIONS
     */

    /* add the srun command */
    opal_argv_append(&argc, &argv, "srun");

    /* Append user defined arguments to srun */
    if ( NULL != mca_pls_slurm_component.custom_args ) {
        custom_strings = opal_argv_split(mca_pls_slurm_component.custom_args, ' ');
        num_args       = opal_argv_count(custom_strings);
        for (i = 0; i < num_args; ++i) {
            opal_argv_append(&argc, &argv, custom_strings[i]);
        }
        opal_argv_free(custom_strings);
    }

    asprintf(&tmp, "--nodes=%lu", (unsigned long) num_nodes);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);

    asprintf(&tmp, "--ntasks=%lu", (unsigned long) num_nodes);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);

    /* create nodelist */
    nodelist_argv = NULL;
    nodelist_argc = 0;

    for (item =  opal_list_get_first(&nodes);
         item != opal_list_get_end(&nodes);
         item =  opal_list_get_next(item)) {
        orte_ras_node_t* node = (orte_ras_node_t*)item;

        opal_argv_append(&nodelist_argc, &nodelist_argv, node->node_name);
    }
    nodelist_flat = opal_argv_join(nodelist_argv, ',');
    asprintf(&tmp, "--nodelist=%s", nodelist_flat);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);
    free(nodelist_flat);


    /*
     * ORTED OPTIONS
     */

    /* add the daemon command (as specified by user) */
    opal_argv_append(&argc, &argv, mca_pls_slurm_component.orted);
    opal_argv_append(&argc, &argv, "--no-daemonize");
    
    /* check for debug flags */
    orte_pls_base_proxy_mca_argv(&argc, &argv);

    /* proxy information */
    opal_argv_append(&argc, &argv, "--bootproxy");
    opal_argv_append(&argc, &argv, jobid_string);

    /* force orted to use the slurm sds */
    opal_argv_append(&argc, &argv, "--ns-nds");
    opal_argv_append(&argc, &argv, "slurm");

    /* set orte process name to be the base of the name list for the daemons */
    rc = orte_ns.create_process_name(&name, 
                                     orte_process_info.my_name->cellid, 
                                     0, vpid);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    rc = orte_ns.get_proc_name_string(&name_string, name);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "orte_pls_rsh: unable to create process name");
        goto cleanup;
    }
    opal_argv_append(&argc, &argv, "--name");
    opal_argv_append(&argc, &argv, name_string);
    free(name_string);

    /* tell the daemon how many procs are in the daemon's job */
    opal_argv_append(&argc, &argv, "--num_procs");
    asprintf(&param, "%lu", (unsigned long) num_nodes);
    opal_argv_append(&argc, &argv, param);
    free(param);

    /* tell the daemon the starting vpid of the daemon's job */
    opal_argv_append(&argc, &argv, "--vpid_start");
    asprintf(&param, "%lu", (unsigned long) 0);
    opal_argv_append(&argc, &argv, param);
    free(param);

    /* pass along the universe name and location info */
    opal_argv_append(&argc, &argv, "--universe");
    asprintf(&param, "%s@%s:%s", orte_universe_info.uid,
             orte_universe_info.host, orte_universe_info.name);
    opal_argv_append(&argc, &argv, param);
    free(param);
    
    /* setup ns contact info */
    opal_argv_append(&argc, &argv, "--nsreplica");
    if (NULL != orte_process_info.ns_replica_uri) {
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
    if (NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);
    free(param);

    if (mca_pls_slurm_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "pls:slurm: final top-level argv:");
            opal_output(0, "pls:slurm:     %s", param);
            free(param);
        }
    }

    /* Bookkeeping -- save the node names */
    cur_prefix = NULL;
    for (item =  opal_list_get_first(&nodes);
         item != opal_list_get_end(&nodes);
         item =  opal_list_get_next(item)) {
        orte_ras_node_t* node = (orte_ras_node_t*)item;
        orte_process_name_t* name;
        opal_list_t map;

        OBJ_CONSTRUCT(&map, opal_list_t);
        /* Get the mapping of this very node */
        rc = orte_rmaps_base_get_node_map(orte_process_info.my_name->cellid,
                                          jobid,
                                          node->node_name,
                                          &map);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* Copy the prefix-directory specified within the
           corresponding app_context.  If there are multiple,
           different prefix's in the app context, complain (i.e., only
           allow one --prefix option for the entire slurm run -- we
           don't support different --prefix'es for different nodes in
           the SLURM pls) */
        for (item2 =  opal_list_get_first(&map);
             item2 != opal_list_get_end(&map);
             item2 =  opal_list_get_next(item2)) {
            orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*) item2;
            char * app_prefix_dir = map->app->prefix_dir;

            /* Check for already set cur_prefix_dir -- if different,
               complain */
            if (NULL != app_prefix_dir) {
                if (NULL != cur_prefix &&
                    0 != strcmp (cur_prefix, app_prefix_dir)) {
                    opal_show_help("help-pls-slurm.txt", "multiple-prefixes",
                                   true, cur_prefix, app_prefix_dir);
                    return ORTE_ERR_FATAL;
                }

                /* If not yet set, copy it; iff set, then it's the
                   same anyway */
                if (NULL == cur_prefix) {
                    cur_prefix = strdup(map->app->prefix_dir);
                    if (mca_pls_slurm_component.debug) {
                        opal_output (0, "pls:slurm: Set prefix:%s",
                                     cur_prefix);
                    }
                }
            }
        }

        /* initialize daemons process name */
        rc = orte_ns.create_process_name(&name, node->node_cellid, 0, vpid);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* save the daemons name on the node */
        if (ORTE_SUCCESS != 
            (rc = orte_pls_base_proxy_set_node_name(node, jobid, name))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        vpid++;
        free(name);
    }
    
    /* setup environment */
    env = opal_argv_copy(environ);
    var = mca_base_param_environ_variable("seed", NULL, NULL);
    opal_setenv(var, "0", true, &env);

#if 0
    /* JMS What to do for sched_yield? */

    /* set the progress engine schedule for this node.  if node_slots
       is set to zero, then we default to NOT being oversubscribed */
    if (node->node_slots > 0 &&
        node->node_slots_inuse > node->node_slots) {
        if (mca_pls_slurm_component.debug) {
            opal_output(0, "pls:slurm: oversubscribed -- setting mpi_yield_when_idle to 1");
        }
        var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
        opal_setenv(var, "1", true, &env);
    } else {
        if (mca_pls_slurm_component.debug) {
            opal_output(0, "pls:slurm: not oversubscribed -- setting mpi_yield_when_idle to 0");
        }
        var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
        opal_setenv(var, "0", true, &env);
    }
    free(var);
#endif
    
    /* exec the daemon */
    rc = pls_slurm_start_proc(argc, argv, env, cur_prefix);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "pls:slurm: start_procs returned error %d", rc);
        goto cleanup;
    }

    /* JMS: short we stash the srun pid in the gpr somewhere for cleanup? */
    /* JMS: how do we catch when srun dies? */
    
cleanup:
    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    while (NULL != (item = opal_list_remove_first(&mapping_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&mapping_list);
    return rc;
}


static int pls_slurm_terminate_job(orte_jobid_t jobid)
{
    if (0 != srun_pid) {
        kill(srun_pid, SIGHUP);
        /* JMS need appropriate code here to reap */
        srun_pid = 0;
    }
    return orte_pls_base_proxy_terminate_job(jobid);
}


/*
 * The way we've used SLURM, we can't kill individual processes --
 * we'll kill the entire job
 */
static int pls_slurm_terminate_proc(const orte_process_name_t *name)
{
    opal_output(orte_pls_base.pls_output,
                "pls:slurm:terminate_proc: not supported");
    return ORTE_ERR_NOT_SUPPORTED;
}


static int pls_slurm_finalize(void)
{
    /* cleanup any pending recvs */
    orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_RMGR_CLNT);

    return ORTE_SUCCESS;
}


static int pls_slurm_start_proc(int argc, char **argv, char **env,
                                char *prefix)
{
    char *exec_argv = opal_path_findv(argv[0], 0, env, NULL);

    if (NULL == exec_argv) {
        return ORTE_ERR_NOT_FOUND;
    }

    srun_pid = fork();
    if (-1 == srun_pid) {
        opal_output(orte_pls_base.pls_output,
                    "pls:slurm:start_proc: fork failed");
        return ORTE_ERR_IN_ERRNO;
    } else if (0 == srun_pid) {
        
        /* If we have a prefix, then modify the PATH and
           LD_LIBRARY_PATH environment variables.  We're already in
           the child process, so it's ok to modify environ. */
        if (NULL != prefix) {
            char *oldenv, *newenv;
            
            /* Reset PATH */
            oldenv = getenv("PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/bin:%s\n", prefix, oldenv);
            } else {
                asprintf(&newenv, "%s/bin", prefix);
            }
            opal_setenv("PATH", newenv, true, &environ);
            if (mca_pls_slurm_component.debug) {
                opal_output(0, "pls:slurm: reset PATH: %s", newenv);
            }
            free(newenv);
            
            /* Reset LD_LIBRARY_PATH */
            oldenv = getenv("LD_LIBRARY_PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/lib:%s\n", prefix, oldenv);
            } else {
                asprintf(&newenv, "%s/lib", prefix);
            }
            opal_setenv("LD_LIBRARY_PATH", newenv, true, &environ);
            if (mca_pls_slurm_component.debug) {
                opal_output(0, "pls:slurm: reset LD_LIBRARY_PATH: %s",
                            newenv);
            }
            free(newenv);
        }

        /* get the srun process out of orterun's process group so that
           signals sent from the shell (like those resulting from
           cntl-c) don't get sent to srun */
        setpgid(0, 0);

        opal_output(orte_pls_base.pls_output,
                    "pls:slurm:start_proc: exec failed");
        execve(exec_argv, argv, env);
        /* don't return - need to exit - returning would be bad -
           we're not in the calling process anymore */
        exit(1);
    }

    /* just in case, make sure that the srun process is not in our
       process group any more.  Stevens says always do this on both
       sides of the fork... */
    setpgid(srun_pid, srun_pid);

    return ORTE_SUCCESS;
}
