/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "util/proc_info.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "mca/pcm/base/base.h"
#include "runtime/runtime.h"
#include "mca/base/base.h"
#include "util/argv.h"
#include "mca/oob/base/base.h"
#include "util/cmd_line.h"
#include "util/sys_info.h"
#include "runtime/runtime.h"
#include "util/session_dir.h"
#include "include/constants.h"
#include "util/output.h"

#include <stdio.h>
#include <unistd.h>
#include <sys/param.h>

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
    char *my_contact_info, *tmp, *jobid_str, *procid_str;

    /*
     * Intialize our Open MPI environment
     */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);

    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

    /*
     * Start command line arguments
     */
    if (OMPI_SUCCESS != (ret = mca_base_cmd_line_setup(cmd_line))) {
        /* BWB show_help */
        printf("show_help: mca_base_cmd_line_setup failed\n");
        return ret;
    }

    ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0, 
                           "Show this help message");
    ompi_cmd_line_make_opt3(cmd_line, 'n', "np", "np", 1,
                            "Number of processes to start");
    ompi_cmd_line_make_opt3(cmd_line, '\0', "hostfile", "hostfile", 1,
			    "Host description file");

    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv) ||
        ompi_cmd_line_is_taken(cmd_line, "help") || 
        ompi_cmd_line_is_taken(cmd_line, "h")) {
        printf("...showing ompi_info help message...\n");
        exit(1);
    }

    if (OMPI_SUCCESS != mca_base_cmd_line_process_args(cmd_line)) {
        /* BWB show_help */
        printf("show_help: mca_base_cmd_line_process_args\n");
        return ret;
    }

    /* get our numprocs */
    if (ompi_cmd_line_is_taken(cmd_line, "np")) {
        num_procs = atoi(ompi_cmd_line_get_param(cmd_line, "np", 0, 0));
        printf("num_procs: %d\n", num_procs);
    }

    /* get the rte command line options */
    ompi_rte_parse_cmd_line(cmd_line);

    /*
     * TSW - temporarily force to be a seed - and to use tcp oob.
     * 
     */
    ompi_process_info.seed = true;
    ompi_process_info.ns_replica = NULL;
    ompi_process_info.gpr_replica = NULL;

    /*
     * Start the Open MPI Run Time Environment
     */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        /* JMS show_help */
        printf("show_help: mca_base_open failed\n");
        return ret;
    }

    if (OMPI_SUCCESS != ompi_rte_init_stage1(&multi_thread, &hidden_thread) ||
	OMPI_SUCCESS != ompi_rte_init_stage2(&multi_thread, &hidden_thread)) {
        /* BWB show_help */
        printf("show_help: ompi_rte_init failed\n");
        return ret;
    }

    /*****    SET MY NAME   *****/
    if (NULL == ompi_process_info.name) { /* don't overwrite an existing name */
	if (ompi_process_info.seed) {
	    ompi_process_info.name = ompi_name_server.create_process_name(0, 0, 0);
	} else {
	    ompi_process_info.name = ompi_rte_get_self();
	}
    }

    /* get my process info */
    ompi_proc_info();

/* setup my session directory */
    jobid_str = ompi_name_server.get_jobid_string(ompi_process_info.name);
    procid_str = ompi_name_server.get_vpid_string(ompi_process_info.name);
 
    if (ompi_rte_debug_flag) {
	ompi_output(0, "[%d,%d,%d] setting up session dir with", ompi_process_info.name->cellid, ompi_process_info.name->jobid, ompi_process_info.name->vpid);
	if (NULL != ompi_process_info.tmpdir_base) {
	    ompi_output(0, "\ttmpdir %s", ompi_process_info.tmpdir_base);
	}
	ompi_output(0, "\tuniverse %s", ompi_process_info.my_universe);
	ompi_output(0, "\tuser %s", ompi_system_info.user);
	ompi_output(0, "\thost %s", ompi_system_info.nodename);
	ompi_output(0, "\tjobid %s", jobid_str);
	ompi_output(0, "\tprocid %s", procid_str);
    }
   if (OMPI_ERROR == ompi_session_dir(true,
				       ompi_process_info.tmpdir_base,
				       ompi_system_info.user,
				       ompi_system_info.nodename, NULL, 
				       ompi_process_info.my_universe,
				       jobid_str, procid_str)) {
	if (jobid_str != NULL) free(jobid_str);
	if (procid_str != NULL) free(procid_str);
	exit(-1);
    }

   /*
    *  Register my process info with my replica.
    */
   if (OMPI_SUCCESS != (ret = ompi_rte_register())) {
       ompi_output(0, "ompi_rte_init: failed in ompi_rte_register()\n");
       return ret;
   }
 
   /* finalize the rte startup */
   if (OMPI_SUCCESS != (ret = ompi_rte_init_finalstage(&multi_thread,
						       &hidden_thread))) {
        /* JMS show_help */
        printf("show_help: ompid failed in ompi_rte_init\n");
        return ret;
    }

       /*****    PREP TO START THE APPLICATION    *****/

    /* get the jobid for the application */
    new_jobid = ompi_name_server.create_jobid();

    /* BWB - fix jobid, procs, and nodes */
    nodelist = ompi_rte_allocate_resources(new_jobid, 0, num_procs);
    if (NULL == nodelist) {
        /* BWB show_help */
        printf("show_help: ompi_rte_allocate_resources failed\n");
        return -1;
    }

    /*
     * Process mapping
     */
    OBJ_CONSTRUCT(&schedlist,  ompi_list_t);
    sched = OBJ_NEW(ompi_rte_node_schedule_t);
    ompi_list_append(&schedlist, (ompi_list_item_t*) sched);
    ompi_cmd_line_get_tail(cmd_line, &(sched->argc), &(sched->argv));
    /* set initial contact info */
    my_contact_info = mca_oob_get_contact_info();
    mca_pcm_base_build_base_env(environ, &(sched->envc), &(sched->env));
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
        printf("no app to start\n");
        return 1;
    }


    /*
     * register the monitor
     */

    ompi_rte_notify(new_jobid,num_procs);

    /*
     * spawn procs
     */
    if (OMPI_SUCCESS != ompi_rte_spawn_procs(new_jobid, &schedlist)) {
        printf("show_help: woops!  we didn't spawn :( \n");
        return -1;
    }

    /*
     * 
     */
   
    ompi_rte_monitor();

    /*
     *   - ompi_rte_kill_job()
     */

    /*
     * Clean up
     */
    if (NULL != nodelist) ompi_rte_deallocate_resources(new_jobid, nodelist);
    if (NULL != cmd_line) OBJ_RELEASE(cmd_line);
    ompi_rte_finalize();
    mca_base_close();
    ompi_finalize();

    OBJ_DESTRUCT(&schedlist);
    return 0;
}

