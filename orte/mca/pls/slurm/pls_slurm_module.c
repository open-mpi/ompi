/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "opal/util/basename.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/runtime/runtime.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"
#include "pls_slurm.h"


/*
 * Local functions
 */
static int pls_slurm_launch_job(orte_jobid_t jobid);
static int pls_slurm_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int pls_slurm_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int pls_slurm_terminate_proc(const orte_process_name_t *name);
static int pls_slurm_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs);
static int pls_slurm_signal_proc(const orte_process_name_t *name, int32_t signal);
static int pls_slurm_finalize(void);
static int pls_slurm_cancel_operation(void);

static int pls_slurm_start_proc(int argc, char **argv, char **env,
                                char *prefix);


/*
 * Global variable
 */
orte_pls_base_module_1_3_0_t orte_pls_slurm_module = {
    pls_slurm_launch_job,
    pls_slurm_terminate_job,
    pls_slurm_terminate_orteds,
    pls_slurm_terminate_proc,
    pls_slurm_signal_job,
    pls_slurm_signal_proc,
    pls_slurm_cancel_operation,
    pls_slurm_finalize
};

/*
 * Local variable
 */
static pid_t srun_pid = 0;


static int pls_slurm_launch_job(orte_jobid_t jobid)
{
    orte_job_map_t *map;
    opal_list_item_t *item;
    size_t num_nodes;
    orte_vpid_t vpid;
    orte_vpid_t start_vpid;
    char *jobid_string = NULL;
    char *uri, *param;
    char **argv;
    int argc;
    int rc;
    char *tmp;
    char** env = NULL;
    char* var;
    char *nodelist_flat;
    char **nodelist_argv;
    int nodelist_argc;
    orte_process_name_t* name;
    char *name_string;
    char **custom_strings;
    int num_args, i;
    char *cur_prefix;
    opal_list_t daemons;
    orte_pls_daemon_info_t *dmn;
    struct timeval joblaunchstart, launchstart, launchstop;

    if (mca_pls_slurm_component.timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "pls_slurm: could not obtain job start time");
        }        
    }
    
    /* setup a list that will contain the info for all the daemons
     * so we can store it on the registry when done
     */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    
    /* Query the map for this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     * All other mapping responsibilities fall to orted in the fork PLS
     */
    rc = orte_rmaps.get_job_map(&map, jobid);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&daemons);
        return rc;
    }

    /* if the user requested that we re-use daemons,
     * launch the procs on any existing, re-usable daemons
     */
    if (orte_pls_base.reuse_daemons) {
        if (ORTE_SUCCESS != (rc = orte_pls_base_launch_on_existing_daemons(map))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(map);
            OBJ_DESTRUCT(&daemons);
            return rc;
        }
    }
    
    /*
     * Allocate a range of vpids for the daemons.
     */
    num_nodes = opal_list_get_size(&map->nodes);
    if (num_nodes == 0) {
        /* nothing further to do - job must have been launched
         * on existing daemons, so we can just return
         */
        OBJ_RELEASE(map);
        OBJ_DESTRUCT(&daemons);
        return ORTE_SUCCESS;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    start_vpid = vpid; 

    /* setup the orted triggers for passing their launch info */
    if (ORTE_SUCCESS != (rc = orte_smr.init_orted_stage_gates(jobid, num_nodes, NULL, NULL))) {
        ORTE_ERROR_LOG(rc);
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

    for (item =  opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item =  opal_list_get_next(item)) {
        orte_mapped_node_t* node = (orte_mapped_node_t*)item;

        opal_argv_append(&nodelist_argc, &nodelist_argv, node->nodename);
    }
    nodelist_flat = opal_argv_join(nodelist_argv, ',');
    opal_argv_free(nodelist_argv);
    asprintf(&tmp, "--nodelist=%s", nodelist_flat);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);


    /*
     * ORTED OPTIONS
     */

    /* add the daemon command (as specified by user) */
    opal_argv_append(&argc, &argv, mca_pls_slurm_component.orted);
    opal_argv_append(&argc, &argv, "--no-daemonize");

    /* check for debug flags */
    orte_pls_base_mca_argv(&argc, &argv);

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
    free(name);

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

    /* Copy the prefix-directory specified in the
       corresponding app_context.  If there are multiple,
       different prefix's in the app context, complain (i.e., only
       allow one --prefix option for the entire slurm run -- we
       don't support different --prefix'es for different nodes in
       the SLURM pls) */
    cur_prefix = NULL;
    for (i=0; i < map->num_apps; i++) {
        char * app_prefix_dir = map->apps[i]->prefix_dir;
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
                cur_prefix = strdup(app_prefix_dir);
                if (mca_pls_slurm_component.debug) {
                    opal_output (0, "pls:slurm: Set prefix:%s",
                                 cur_prefix);
                }
            }
        }
    }

    /* setup the daemon info for each node */
    vpid = start_vpid;
    for (item = opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item = opal_list_get_next(item)) {
        orte_mapped_node_t* node = (orte_mapped_node_t*)item;
        
        /* record the daemons info for this node */
        dmn = OBJ_NEW(orte_pls_daemon_info_t);
        dmn->active_job = jobid;
        dmn->cell = node->cell;
        dmn->nodename = strdup(node->nodename);
        if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&(dmn->name), dmn->cell, 0, vpid))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        opal_list_append(&daemons, &dmn->super);
        vpid++;
    }

    /* store the daemon info on the registry */
    if (ORTE_SUCCESS != (rc = orte_pls_base_store_active_daemons(&daemons))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* setup environment */
    env = opal_argv_copy(environ);
    var = mca_base_param_environ_variable("seed", NULL, NULL);
    opal_setenv(var, "0", true, &env);
    free(var);
    var = mca_base_param_environ_variable("orte", "slurm", "nodelist");
    opal_setenv(var, nodelist_flat, true, &env);
    free(nodelist_flat);
    free(var);

    if (mca_pls_slurm_component.timing) {
        if (0 != gettimeofday(&launchstart, NULL)) {
            opal_output(0, "pls_slurm: could not obtain start time");
        }        
    }
    
    /* exec the daemon */
    rc = pls_slurm_start_proc(argc, argv, env, cur_prefix);
    
    if (mca_pls_slurm_component.timing) {
        if (0 != gettimeofday(&launchstop, NULL)) {
             opal_output(0, "pls_slurm: could not obtain stop time");
         } else {
             opal_output(0, "pls_slurm: daemon block launch time is %ld usec",
                         (launchstop.tv_sec - launchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - launchstart.tv_usec));
             opal_output(0, "pls_slurm: total job launch time is %ld usec",
                         (launchstop.tv_sec - joblaunchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - joblaunchstart.tv_usec));
         }
    }

    if (ORTE_SUCCESS != rc) {
        opal_output(0, "pls:slurm: start_procs returned error %d", rc);
        goto cleanup;
    }

    /* JMS: short we stash the srun pid in the gpr somewhere for cleanup? */
    /* JMS: how do we catch when srun dies? */

cleanup:
    OBJ_RELEASE(map);
    opal_argv_free(argv);
    opal_argv_free(env);

    if(NULL != jobid_string) {
        free(jobid_string);
    }
    
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);
    
    return rc;
}


static int pls_slurm_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid, attrs))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_kill_local_procs(&daemons, jobid, timeout))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
CLEANUP:
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);
    return rc;
}


/**
* Terminate the orteds for a given job
 */
static int pls_slurm_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid, attrs))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* order them to go away */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(&daemons, timeout))) {
        ORTE_ERROR_LOG(rc);
    }
    
CLEANUP:
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);
    return rc;
}


/*
 * The way we've used SLURM, we can't kill individual processes --
 * we'll kill the entire job
 */
static int pls_slurm_terminate_proc(const orte_process_name_t *name)
{
    opal_output(0, "pls:slurm:terminate_proc: not supported");
    return ORTE_ERR_NOT_SUPPORTED;
}


/**
 * Signal all the processes in the child srun by sending the signal directly to it
 */
static int pls_slurm_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    if (0 != srun_pid) {
        kill(srun_pid, (int)signal);
   }
    return ORTE_SUCCESS;
}


/*
 * Signal a specific process
 */
static int pls_slurm_signal_proc(const orte_process_name_t *name, int32_t signal)
{
    opal_output(0, "pls:slurm:signal_proc: not supported");
    return ORTE_ERR_NOT_SUPPORTED;
}


/**
 * Cancel an operation involving comm to an orted
 */
int pls_slurm_cancel_operation(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_cancel_operation())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


static int pls_slurm_finalize(void)
{
    int rc;

    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return ORTE_SUCCESS;
}


static int pls_slurm_start_proc(int argc, char **argv, char **env,
                                char *prefix)
{
    int fd, id, debug_daemons;
    char *exec_argv = opal_path_findv(argv[0], 0, env, NULL);

    if (NULL == exec_argv) {
        return ORTE_ERR_NOT_FOUND;
    }

    srun_pid = fork();
    if (-1 == srun_pid) {
        opal_output(0, "pls:slurm:start_proc: fork failed");
        return ORTE_ERR_IN_ERRNO;
    } else if (0 == srun_pid) {
        char *bin_base = NULL, *lib_base = NULL;

        /* Figure out the basenames for the libdir and bindir.  There
           is a lengthy comment about this in pls_rsh_module.c
           explaining all the rationale for how / why we're doing
           this. */

        lib_base = opal_basename(opal_install_dirs.libdir);
        bin_base = opal_basename(opal_install_dirs.bindir);

        /* If we have a prefix, then modify the PATH and
           LD_LIBRARY_PATH environment variables.  */
        if (NULL != prefix) {
            char *oldenv, *newenv;

            /* Reset PATH */
            oldenv = getenv("PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/%s:%s", prefix, bin_base, oldenv);
            } else {
                asprintf(&newenv, "%s/%s", prefix, bin_base);
            }
            opal_setenv("PATH", newenv, true, &env);
            if (mca_pls_slurm_component.debug) {
                opal_output(0, "pls:slurm: reset PATH: %s", newenv);
            }
            free(newenv);

            /* Reset LD_LIBRARY_PATH */
            oldenv = getenv("LD_LIBRARY_PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/%s:%s", prefix, lib_base, oldenv);
            } else {
                asprintf(&newenv, "%s/%s", prefix, lib_base);
            }
            opal_setenv("LD_LIBRARY_PATH", newenv, true, &env);
            if (mca_pls_slurm_component.debug) {
                opal_output(0, "pls:slurm: reset LD_LIBRARY_PATH: %s",
                            newenv);
            }
            free(newenv);
        }

        fd = open("/dev/null", O_CREAT|O_WRONLY|O_TRUNC, 0666);
        if(fd > 0) {
            dup2(fd, 0);
        }

        /* When not in debug mode and --debug-daemons was not passed,
         * tie stdout/stderr to dev null so we don't see messages from orted */
        id = mca_base_param_find("orte", "debug", "daemons");
        if(id < 0) {
            id = mca_base_param_register_int("orte", "debug", "daemons", NULL, 0);
        }
        mca_base_param_lookup_int(id, &debug_daemons);
        if (0 == mca_pls_slurm_component.debug && 0 == debug_daemons) {
            if (fd >= 0) {
                if (fd != 1) {
                    dup2(fd,1);
                }
                if (fd != 2) {
                    dup2(fd,2);
                }
            }
        }

        if (fd > 2) {
            close(fd);
        }

        /* get the srun process out of orterun's process group so that
           signals sent from the shell (like those resulting from
           cntl-c) don't get sent to srun */
        setpgid(0, 0);

        execve(exec_argv, argv, env);

        opal_output(0, "pls:slurm:start_proc: exec failed");
        /* don't return - need to exit - returning would be bad -
           we're not in the calling process anymore */
        exit(1);
    }

    free(exec_argv);

    /* just in case, make sure that the srun process is not in our
       process group any more.  Stevens says always do this on both
       sides of the fork... */
    setpgid(srun_pid, srun_pid);

    return ORTE_SUCCESS;
}
