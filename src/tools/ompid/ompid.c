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
 */
/** @file **/

#include "ompi_config.h"

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>

#include "include/constants.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "util/output.h"
#include "util/show_help.h"
#include "util/sys_info.h"
#include "util/os_path.h"
#include "util/cmd_line.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/printf.h"
#include "util/daemon_init.h"
#include "util/universe_setup_file_io.h"

#include "mca/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"

#include "runtime/runtime.h"

#include "tools/ompid/ompid.h"

static bool ompi_daemon_debug;
static ompi_mutex_t ompi_daemon_mutex;
static ompi_condition_t ompi_daemon_condition;
static bool ompi_daemon_exit_condition = false;

static void ompi_daemon_recv(int status, ompi_process_name_t* sender,
			     ompi_buffer_t buffer, int tag,
			     void* cbdata);


int main(int argc, char *argv[])
{
    int ret = 0;
    ompi_cmd_line_t *cmd_line = NULL;
    bool allow_multi_user_threads   = false;
    bool have_hidden_threads  = false;
    char *enviro_val, *contact_file;
    char *filenm, *segment;
    ompi_rte_process_status_t my_status;

    /*
     * Intialize the Open MPI environment
     */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);

    if (OMPI_SUCCESS != (ret = ompi_init(argc, argv))) {
        ompi_show_help("help-mpirun.txt", "mpirun:init-failure", true,
                       "ompi_init()", ret);
        return ret;
    }

   /* check for debug flag */
    enviro_val = getenv("OMPI_daemon_debug");
    if (NULL != enviro_val) {  /* flag was set */
		ompi_daemon_debug = true;
		ompi_output(0, "ompid: entered debug mode");
	} else {
		ompi_daemon_debug = false;
    }

    /* daemonize myself */
#ifndef WIN32
    if (!ompi_daemon_debug) {
        ompi_output(0, "ompid: daemonizing");
        ompi_daemon_init(NULL);
    } else {
       ompi_output(0, "ompid: debug mode - not daemonizing");
    }
#endif

    /* setup the thread lock and condition variable */
    OBJ_CONSTRUCT(&ompi_daemon_mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&ompi_daemon_condition, ompi_condition_t);

    /* setup to read common command line options that span all Open MPI programs */
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
	printf("...showing off my version!\n");
	exit(1);
    }

    /* Open up the MCA */

    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        ompi_show_help("help-mpirun.txt", "mpirun:init-failure", true, 
                       "mca_base_open()", ret);
        return ret;
    }

    /* Join the run-time environment */
    allow_multi_user_threads = true;
    have_hidden_threads = false;
    if (OMPI_SUCCESS != (ret = ompi_rte_init(cmd_line, &allow_multi_user_threads,
					     &have_hidden_threads))) {
        ompi_show_help("help-mpirun.txt", "mpirun:init-failure", true,
                       "mca_rte_init()", ret);
        return ret;
    }

	if (!ompi_process_info.seed) { /* if I'm not the seed... */
	    /* start recording the compound command that starts us up */
	    ompi_registry.begin_compound_cmd();
	}
    /* Finish setting up the RTE - contains commands
     * that need to be inside the compound command
     */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_cleanup())) {
	printf("ompi_rte_init_cleanup failed\n");
	return ret;
    }

    /*
     *  Set my process status to "starting". Note that this must be done
     *  after the rte init is completed.
     *
     *  Ensure we own the job status segment first
     */
    asprintf(&segment, "%s-%s", OMPI_RTE_JOB_STATUS_SEGMENT,
	     ompi_name_server.get_jobid_string(ompi_rte_get_self()));
    ompi_registry.assign_ownership(segment, ompi_name_server.get_jobid(ompi_rte_get_self()));
    free(segment);
    
    asprintf(&segment, "%s-%s", OMPI_RTE_OOB_SEGMENT,
        ompi_name_server.get_jobid_string(ompi_rte_get_self()));
    ompi_registry.assign_ownership(segment, ompi_name_server.get_jobid(ompi_rte_get_self()));
    free(segment);

    my_status.rank = ompi_name_server.get_vpid(ompi_rte_get_self());
    my_status.nodename = strdup(ompi_system_info.nodename);
    my_status.status_key = OMPI_PROC_STARTING;
    my_status.exit_code = 0;
    if (OMPI_SUCCESS != (ret = ompi_rte_set_process_status(&my_status, ompi_rte_get_self()))) {
        printf("ompid: failed in ompi_rte_set_process_status()\n");
        return ret;
    } 

	if (!ompi_process_info.seed) {  /* if I'm not the seed... */
	    /* execute the compound command - no return data requested
	    *  we'll get it all from the startup message
	    */
	    ompi_registry.exec_compound_cmd(OMPI_REGISTRY_NO_RETURN_REQUESTED);
		
	    /* wait to receive startup message and info distributed */
	    if (OMPI_SUCCESS != (ret = ompi_rte_wait_startup_msg())) {
		printf("ompid: failed to see all procs register\n");
		return ret;
	    }
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
	ompi_output(0, "ompid: contact_file %s", contact_file);

	if (OMPI_SUCCESS != (ret = ompi_write_universe_setup_file(contact_file))) {
	    if (ompi_daemon_debug) {
		ompi_output(0, "[%d,%d,%d] ompid: couldn't write setup file", ompi_rte_get_self()->cellid,
			    ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
	    }
	} else if (ompi_daemon_debug) {
	    ompi_output(0, "[%d,%d,%d] ompid: wrote setup file", ompi_rte_get_self()->cellid,
			ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
	}
    }


    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: registering", ompi_rte_get_self()->cellid,
		    ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
    }


    /* register this node on the virtual machine */
    ompi_rte_vm_register();

    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: issuing callback", ompi_rte_get_self()->cellid,
		    ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
    }

     /* register the daemon main callback function */
    ret = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_DAEMON, 0, ompi_daemon_recv, NULL);
    if(ret != OMPI_SUCCESS && ret != OMPI_ERR_NOT_IMPLEMENTED) {
	ompi_output(0, "daemon callback not registered: error code %d", ret);
	return ret;
    }

   /* go through the universe fields and see what else I need to do
     * - could be setup a virtual machine, spawn a console, etc.
     */

    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: setting up event monitor", ompi_rte_get_self()->cellid,
		    ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
    }

     /* setup and enter the event monitor */
    OMPI_THREAD_LOCK(&ompi_daemon_mutex);

    while (false == ompi_daemon_exit_condition) {
	ompi_condition_wait(&ompi_daemon_condition, &ompi_daemon_mutex);
    }

    OMPI_THREAD_UNLOCK(&ompi_daemon_mutex);

    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: mutex cleared - finalizing", ompi_rte_get_self()->cellid,
		    ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
    }

    /* if i'm the seed, remove the universe-setup file */
    if (ompi_process_info.seed) {
	filenm = ompi_os_path(false, ompi_process_info.universe_session_dir, "universe-setup.txt", NULL);
	unlink(filenm);
    }

    /* finalize the system */
    ompi_rte_finalize();
    mca_base_close();
    ompi_finalize();

    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: done - exiting", ompi_rte_get_self()->cellid,
		    ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
    }

    exit(0);
}

static void ompi_daemon_recv(int status, ompi_process_name_t* sender,
			     ompi_buffer_t buffer, int tag,
			     void* cbdata)
{
    ompi_buffer_t answer;
    ompi_daemon_cmd_flag_t command;
    int ret;
    char *contact_info;

    OMPI_THREAD_LOCK(&ompi_daemon_mutex);

    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: received message", ompi_rte_get_self()->cellid,
		    ompi_rte_get_self()->jobid, ompi_rte_get_self()->vpid);
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&answer, 0)) {
	/* RHC -- not sure what to do if this fails */
	goto DONE;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, OMPI_DAEMON_OOB_PACK_CMD)) {
	goto CLEANUP;
    }

        /****    EXIT COMMAND    ****/
    if (OMPI_DAEMON_EXIT_CMD == command) {
	ompi_daemon_exit_condition = true;
	ompi_condition_signal(&ompi_daemon_condition);

	/****     CONTACT QUERY COMMAND    ****/
    } else if (OMPI_DAEMON_CONTACT_QUERY_CMD == command) {
	/* send back contact info */

	contact_info = mca_oob_get_contact_info();

	if (NULL != contact_info) {
	    if (OMPI_SUCCESS != ompi_pack_string(answer, contact_info)) {
		/* RHC -- not sure what to do if this fails */
	    }

	    if (0 > (ret = mca_oob_send_packed(sender, answer, tag, 0))) {
		if (ompi_daemon_debug) {
		    ompi_output(0, "ompid_recv: send failed with return %d", ret);
		}
	    }
	}

    }
 CLEANUP:
    ompi_buffer_free(answer);

 DONE:
    /* reissue the non-blocking receive */
    ret = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_DAEMON, 0, ompi_daemon_recv, NULL);
    if(ret != OMPI_SUCCESS && ret != OMPI_ERR_NOT_IMPLEMENTED) {
	ompi_output(0, "daemon callback not registered: error code %d", ret);
	return;
    }

    OMPI_THREAD_UNLOCK(&ompi_daemon_mutex);
    return;
}
