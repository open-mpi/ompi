/*
 * $HEADER$
 */
/** @file **/

#include "ompi_config.h"

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/param.h>
#include <errno.h>

#include "include/constants.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "util/output.h"
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
    char *jobid_str, *procid_str, *enviro_val, *contact_file;
    char *filenm, *universe;
    pid_t pid;
    mca_ns_base_jobid_t jobid;
    mca_ns_base_vpid_t vpid;

    /*
     * Intialize the Open MPI environment
     */
    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

    /* check for debug flag */
    enviro_val = getenv("OMPI_daemon_debug");
    if (NULL != enviro_val) {  /* flag was set */
	ompi_daemon_debug = true;
	ompi_output(0, "ompid: entered daemon");
    } else {
	ompi_daemon_debug = false;
    }

    ompi_daemon_debug = true;  /**** DEBUGGING PURPOSES */

    if (ompi_daemon_debug) {
	ompi_output(0, "ompid: daemonizing");
    }

    /* daemonize myself */
    ompi_daemon_init(NULL);

    /* setup the thread lock and condition variable */
    OBJ_CONSTRUCT(&ompi_daemon_mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&ompi_daemon_condition, ompi_condition_t);

    /* setup to read common command line options that span all Open MPI programs */
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
	/* BWB show_help */
	printf("show_help: mca_base_cmd_line_setup failed\n");
	return ret;
    }

    if (OMPI_SUCCESS != mca_base_cmd_line_process_args(cmd_line)) {
	/* BWB show_help */
	printf("show_help: mca_base_cmd_line_process_args\n");
	return ret;
    }

    /* parse the local commands */
    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv)) {
	exit(ret);
    }

    if (ompi_cmd_line_is_taken(cmd_line, "help") || 
        ompi_cmd_line_is_taken(cmd_line, "h")) {
        printf("...showing ompi_info help message...\n");
        exit(1);
    }

    if (ompi_cmd_line_is_taken(cmd_line, "version") ||
	ompi_cmd_line_is_taken(cmd_line, "v")) {
	printf("...showing off my version!\n");
	exit(1);
    }

    /* Open up the MCA */

    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        /* JMS show_help */
        printf("show_help: ompid failed in mca_base_open\n");
        return ret;
    }

    /* Join the run-time environment */
    allow_multi_user_threads = true;
    have_hidden_threads = false;
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage1(&allow_multi_user_threads,
						    &have_hidden_threads))) {
        /* JMS show_help */
        printf("show_help: ompid failed in ompi_rte_init\n");
        return ret;
    }

    /* parse environmental variables and fill corresponding info structures
     * need the oob to be open so we can pass the contact info we extract
     */
    ompi_rte_parse_environ();

    /* parse the cmd_line for rte options - override settings from enviro, where necessary
     * copy everything into enviro variables for passing later on
     */
    ompi_rte_parse_cmd_line(cmd_line);

    /* parse the cmd_line for daemon options - gets all the options relating
     * specifically to seed behavior, in case i'm a seed, but also gets
     * options about scripts and hostfiles that might be of use to me
     * overrride enviro variables where necessary
     */
    ompi_rte_parse_daemon_cmd_line(cmd_line);

    /* check for existing universe to join */
    if (OMPI_SUCCESS != (ret = ompi_rte_universe_exists())) {
	if (ompi_rte_debug_flag) {
	    ompi_output(0, "ompi_mpi_init: could not join existing universe");
	}
	if (OMPI_ERR_NOT_FOUND != ret) {
	    /* if it exists but no contact could be established,
	     * define unique name based on current one.
	     * and start new universe with me as seed
	     */
	    universe = strdup(ompi_universe_info.name);
	    free(ompi_universe_info.name);
	    ompi_universe_info.name = NULL;
	    pid = getpid();
	    if (0 > asprintf(&ompi_universe_info.name, "%s-%d", universe, pid) && ompi_rte_debug_flag) {
		ompi_output(0, "mpi_init: error creating unique universe name");
	    }
	}

	ompi_process_info.my_universe = strdup(ompi_universe_info.name);
	ompi_process_info.seed = true;
	if (NULL != ompi_universe_info.ns_replica) {
	    free(ompi_universe_info.ns_replica);
	    ompi_universe_info.ns_replica = NULL;
	}
	if (NULL != ompi_process_info.ns_replica) {
	    free(ompi_process_info.ns_replica);
	    ompi_process_info.ns_replica = NULL;
	}
	if (NULL != ompi_universe_info.gpr_replica) {
	    free(ompi_universe_info.gpr_replica);
	    ompi_universe_info.gpr_replica = NULL;
	}
	if (NULL != ompi_process_info.gpr_replica) {
	    free(ompi_process_info.gpr_replica);
	    ompi_process_info.gpr_replica = NULL;
	}
    }

    /* setup the rest of the rte */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage2(&allow_multi_user_threads,
						    &have_hidden_threads))) {
        /* JMS show_help */
        printf("show_help: ompid failed in ompi_rte_init\n");
        return ret;
    }

    /*****    SET MY NAME   *****/
    if (NULL != ompi_process_info.name) { /* should not have been previously set */
	free(ompi_process_info.name);
	ompi_process_info.name = NULL;
    }

    if (NULL != ompi_rte_get_self()) {  /* name set in environment - record name */
	ompi_process_info.name = ompi_rte_get_self();
    } else if (NULL == ompi_process_info.ns_replica) { /* couldn't join existing univ */
	ompi_process_info.name = ompi_name_server.create_process_name(0,0,0);
    } else {  /* name server exists elsewhere - get a name for me */
	jobid = ompi_name_server.create_jobid();
	vpid = ompi_name_server.reserve_range(jobid, 1);
	ompi_process_info.name = ompi_name_server.create_process_name(0, jobid, vpid);
    }

    /* setup my session directory */
    jobid_str = ompi_name_server.get_jobid_string(ompi_process_info.name);
    procid_str = ompi_name_server.get_vpid_string(ompi_process_info.name);
 
    if (ompi_daemon_debug) {
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


    /* finalize the rte startup */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_finalstage(&allow_multi_user_threads,
							&have_hidden_threads))) {
        /* JMS show_help */
        ompi_output(0, "show_help: ompid failed in ompi_rte_init");
        return ret;
    }

    /*
     *  Register my process info with my replica. Note that this must be done
     *  after the rte init is completed.
     */
    if (OMPI_SUCCESS != (ret = ompi_rte_register())) {
        ompi_output(0, "ompid: failed in ompi_rte_register()");
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
	ompi_output(0, "ompid: contact_file %s", contact_file);

	if (OMPI_SUCCESS != (ret = ompi_write_universe_setup_file(contact_file))) {
	    if (ompi_daemon_debug) {
		ompi_output(0, "[%d,%d,%d] ompid: couldn't write setup file", ompi_process_info.name->cellid,
			    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
	    }
	} else if (ompi_daemon_debug) {
	    ompi_output(0, "[%d,%d,%d] ompid: wrote setup file", ompi_process_info.name->cellid,
			ompi_process_info.name->jobid, ompi_process_info.name->vpid);
	}
    }


    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: registering", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }


    /* register this node on the virtual machine */
    ompi_vm_register();

    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: issuing callback", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
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
	ompi_output(0, "[%d,%d,%d] ompid: setting up event monitor", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }

     /* setup and enter the event monitor */
    OMPI_THREAD_LOCK(&ompi_daemon_mutex);

    while (false == ompi_daemon_exit_condition) {
	ompi_condition_wait(&ompi_daemon_condition, &ompi_daemon_mutex);
    }

    OMPI_THREAD_UNLOCK(&ompi_daemon_mutex);

    if (ompi_daemon_debug) {
	ompi_output(0, "[%d,%d,%d] ompid: mutex cleared - finalizing", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
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
	ompi_output(0, "[%d,%d,%d] ompid: done - exiting", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
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
	ompi_output(0, "[%d,%d,%d] ompid: received message", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
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
