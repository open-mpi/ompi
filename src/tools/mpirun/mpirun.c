/* -*- C -*-
 *
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
 */

#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>

#include "include/constants.h"

#include "event/event.h"
#include "util/proc_info.h"
#include "util/argv.h"
#include "util/cmd_line.h"
#include "util/sys_info.h"
#include "util/session_dir.h"
#include "util/output.h"
#include "util/os_path.h"
#include "util/universe_setup_file_io.h"
#include "util/show_help.h"

#include "mca/base/base.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "mca/pcm/base/base.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"

#include "runtime/runtime.h"
#include "runtime/ompi_rte_wait.h"

extern char** environ;


int
main(int argc, char *argv[])
{
    bool multi_thread = false;
    bool hidden_thread = false;
    int ret;
    ompi_cmd_line_t *cmd_line = NULL;
    ompi_list_t *nodelist = NULL;
    ompi_list_t schedlist;
    mca_ns_base_jobid_t new_jobid;
    int num_procs = 1;
    ompi_rte_node_schedule_t *sched;
    char cwd[MAXPATHLEN];
    char *my_contact_info, *tmp;
    char *contact_file, *filenm, *segment;
    ompi_rte_spawn_handle_t *spawn_handle;
    ompi_registry_notify_id_t rc_tag;
    ompi_rte_process_status_t *proc_status;
    ompi_list_t *status_list;
    ompi_registry_value_t *value;
    /*
     * Intialize our Open MPI environment
     */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);

    if (OMPI_SUCCESS != (ret = ompi_init(argc, argv))) {
        ompi_show_help("help-mpirun.txt", "mpirun:init-failure", true,
                       "ompi_init()", ret);
        return ret;
    }

    /* setup to read common command line options that span all Open
       MPI programs */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);

    ompi_cmd_line_make_opt(cmd_line, 'v', "version", 0,
			   "Show version of Open MPI and this program");

    ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0,
			   "Show help for this function");


    /* setup rte command line arguments */
    ompi_rte_cmd_line_setup(cmd_line);

    /*
     * setup  mca command line arguments
     */
    if (OMPI_SUCCESS != (ret = mca_base_cmd_line_setup(cmd_line))) {
        ompi_show_help("help-mpirun.txt", "mpirun:init-failure", true, 
                       "mca_base_cmd_line_setup()", ret);
	return ret;
    }

    if (OMPI_SUCCESS != mca_base_cmd_line_process_args(cmd_line)) {
        ompi_show_help("help-mpirun.txt", "mpirun:init-failure", true, 
                       "mca_base_cmd_line_process_args()", ret);
	return ret;
    }

    /* parse the local commands */
    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv)) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(cmd_line);
        ompi_show_help("help-mpirun.txt", "mpirun:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    if (ompi_cmd_line_is_taken(cmd_line, "help") || 
        ompi_cmd_line_is_taken(cmd_line, "h")) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(cmd_line);
        ompi_show_help("help-mpirun.txt", "mpirun:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    if (ompi_cmd_line_is_taken(cmd_line, "version") ||
	ompi_cmd_line_is_taken(cmd_line, "v")) {
        /* BWB - show version message */
	printf("...showing off my version!\n");
	exit(1);
    }

    /*
     * Setup mpirun-specific command line arguments
     */
    ompi_cmd_line_make_opt3(cmd_line, 'n', "np", "np", 1,
                            "Number of processes to start");
    ompi_cmd_line_make_opt3(cmd_line, '\0', "hostfile", "hostfile", 1,
			    "Host description file");

    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv) ||
        ompi_cmd_line_is_taken(cmd_line, "help") || 
        ompi_cmd_line_is_taken(cmd_line, "h")) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(cmd_line);
        ompi_show_help("help-mpirun.txt", "mpirun:usage", false,
                       argv[0], args);
        free(args);
        exit(1);
    }

    if (OMPI_SUCCESS != mca_base_cmd_line_process_args(cmd_line)) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(cmd_line);
        ompi_show_help("help-mpirun.txt", "mpirun:usage", false, 
                       argv[0], args);
        free(args);
        return ret;
    }

    /* get our hostfile, if we have one */
    if (ompi_cmd_line_is_taken(cmd_line, "hostfile")) {
        /* BWB - XXX - fix me.  We really should be setting this via
         * an API rather than setenv.  But we don't have such an API just
         * yet. */
        char *buf = NULL;
        asprintf(&buf, "OMPI_MCA_hostfile=%s", 
                 ompi_cmd_line_get_param(cmd_line, "hostfile", 0, 0));
        /* yeah, it leaks.  Can't do nothin' about that */
        putenv(buf);
   }

    /* get our numprocs */
    if (ompi_cmd_line_is_taken(cmd_line, "np")) {
        num_procs = atoi(ompi_cmd_line_get_param(cmd_line, "np", 0, 0));
    }

    /*
     * Start the Open MPI Run Time Environment
     */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        ompi_show_help("help-mpirun.txt", "mpirun:init-failure", true, 
                       "mca_base_open()", ret);
        return ret;
    }

    multi_thread = true;
    hidden_thread=false;
    if (OMPI_SUCCESS != ompi_rte_init(cmd_line, &multi_thread, &hidden_thread)) {
        ompi_show_help("help-mpirun.txt", "mpirun:init-failure", true,
                       "mca_rte_init()", ret);
	return ret;
    }

    /* Finish setting up the RTE - contains commands
     * that need to be inside a compound command, if one is active
     */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_cleanup())) {
        printf("failed in ompi_rte_init_cleanup");
	return ret;
    }

    /* if i'm the seed, get my contact info and write my setup file for others to find */
    if (ompi_process_info.seed) {
	if (NULL != ompi_universe_info.seed_contact_info) {
	    free(ompi_universe_info.seed_contact_info);
	    ompi_universe_info.seed_contact_info = NULL;
	}
	ompi_universe_info.seed_contact_info = mca_oob_get_contact_info();
	contact_file = ompi_os_path(false, ompi_process_info.universe_session_dir,
				    "universe-setup.txt", NULL);

	if (OMPI_SUCCESS != (ret = ompi_write_universe_setup_file(contact_file))) {
	    if (ompi_rte_debug_flag) {
		ompi_output(0, "[%d,%d,%d] ompid: couldn't write setup file", ompi_rte_get_self()->cellid,
			    ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
	    }
	}
    }

    /*****    PREP TO START THE APPLICATION    *****/

    /* get the jobid for the application */
    new_jobid = ompi_name_server.create_jobid();

    /* get the spawn handle to start spawning stuff */
    spawn_handle = ompi_rte_get_spawn_handle(OMPI_RTE_SPAWN_HIGH_QOS, true);

    /* BWB - fix jobid, procs, and nodes */
    nodelist = ompi_rte_allocate_resources(spawn_handle, new_jobid, 0, num_procs);
    if (NULL == nodelist) {
        ompi_show_help("help-mpirun.txt", "mpirun:allocate-resources",
                       true, argv[0], errno);
	return -1;
    }

    /*
     * Process mapping
     */
    OBJ_CONSTRUCT(&schedlist,  ompi_list_t);
    sched = OBJ_NEW(ompi_rte_node_schedule_t);
    ompi_list_append(&schedlist, (ompi_list_item_t*) sched);
    ompi_cmd_line_get_tail(cmd_line, &(sched->argc), &(sched->argv));

    /*
     * build environment to be passed
     */
    mca_pcm_base_build_base_env(environ, &(sched->envc), &(sched->env));
    /* set initial contact info */
    if (ompi_process_info.seed) {  /* i'm the seed - direct them towards me */
	my_contact_info = mca_oob_get_contact_info();
    } else { /* i'm not the seed - direct them to it */
	my_contact_info = strdup(ompi_universe_info.ns_replica);
    }
    asprintf(&tmp, "OMPI_MCA_ns_base_replica=%s", my_contact_info);
    ompi_argv_append(&(sched->envc), &(sched->env), tmp);
    free(tmp);
    asprintf(&tmp, "OMPI_MCA_gpr_base_replica=%s", my_contact_info);
    ompi_argv_append(&(sched->envc), &(sched->env), tmp);
    free(tmp);
    if (NULL != ompi_universe_info.name) {
	asprintf(&tmp, "OMPI_universe_name=%s", ompi_universe_info.name);
	ompi_argv_append(&(sched->envc), &(sched->env), tmp);
	free(tmp);
    }
    if (ompi_cmd_line_is_taken(cmd_line, "tmpdir")) {  /* user specified the tmp dir base */
	asprintf(&tmp, "OMPI_tmpdir_base=%s", ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0));
	ompi_argv_append(&(sched->envc), &(sched->env), tmp);
	free(tmp);
    }

    getcwd(cwd, MAXPATHLEN);
    sched->cwd = strdup(cwd);
    sched->nodelist = nodelist;

    if (sched->argc == 0) {
        ompi_show_help("help-mpirun.txt", "mpirun:no-application", true,
                       argv[0], argv[0]);
	return 1;
    }


    /*
     * register to monitor the startup and shutdown processes
     */
    /* setup segment for this job */
    asprintf(&segment, "%s-%s", OMPI_RTE_JOB_STATUS_SEGMENT,
	     ompi_name_server.convert_jobid_to_string(new_jobid));

    /* register a synchro on the segment so we get notified when everyone registers */
    rc_tag = ompi_registry.synchro(
	     OMPI_REGISTRY_SYNCHRO_MODE_LEVEL|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT,
	     OMPI_REGISTRY_OR,
	     segment,
	     NULL,
	     num_procs,
	     ompi_rte_all_procs_registered, NULL);
    /* register a synchro on the segment so we get notified when everyone is gone
     */
    rc_tag = ompi_registry.synchro(
             OMPI_REGISTRY_SYNCHRO_MODE_DESCENDING|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT,
	     OMPI_REGISTRY_OR,
	     segment,
	     NULL,
	     0,
	     ompi_rte_all_procs_unregistered, NULL);

    /*
     * spawn procs
     */
    if (OMPI_SUCCESS != (ret = ompi_rte_spawn_procs(spawn_handle, new_jobid, &schedlist))) {
        ompi_show_help("help-mpirun.txt", "mpirun:error-spawning",
                       true, argv[0], ret);
	return 1;
    }
    
   
    if (OMPI_SUCCESS != (ret = ompi_rte_monitor_procs_registered())) {
        ompi_show_help("help-mpirun.txt", "mpirun:proc-reg-failed", 
                       true, argv[0], ret);
	ompi_rte_job_shutdown(new_jobid);
	return -1;
    } else {
	ompi_rte_job_startup(new_jobid);
	ompi_rte_monitor_procs_unregistered();
	ompi_rte_job_shutdown(new_jobid);
    }
    /*
     *   - ompi_rte_kill_job()
     */

    /*
     * Determine if the processes all exited normally - if not, flag the output of mpirun
     */
    ret = 0;
    status_list = ompi_registry.get(OMPI_REGISTRY_OR, segment, NULL);
    while (NULL != (value = (ompi_registry_value_t*)ompi_list_remove_first(status_list))) {
	proc_status = ompi_rte_unpack_process_status(value);
	if (OMPI_PROC_TERMINATING != proc_status->status_key) {
	    ret = -1;
	}
	if (0 != proc_status->exit_code) {
	    ret = proc_status->exit_code;
	}
    }

    /*
     * Clean up
     */
    if (NULL != nodelist) ompi_rte_deallocate_resources(spawn_handle, new_jobid, nodelist);
    if (NULL != cmd_line) OBJ_RELEASE(cmd_line);
    if (NULL != spawn_handle) OBJ_RELEASE(spawn_handle);

    /* eventually, mpirun won't be the seed and so won't have to do this.
     * for now, though, remove the universe-setup.txt file so the directories
     * can cleanup
     */
    if (ompi_process_info.seed) {
	filenm = ompi_os_path(false, ompi_process_info.universe_session_dir, "universe-setup.txt", NULL);
	unlink(filenm);
    }

    ompi_rte_finalize();
    mca_base_close();
    ompi_finalize();

    OBJ_DESTRUCT(&schedlist);
    return ret;
}

