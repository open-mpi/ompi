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

#include "runtime/runtime.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "util/cmd_line.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/printf.h"
#include "util/daemon_init.h"

#include "mca/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"

#include "tools/ompid/ompid.h"

/*
 * Public variables
 */

bool pretty = true;
ompi_cmd_line_t *cmd_line = NULL;

const char *type_all  = "all";
const char *type_ompi = "ompi";
const char *type_base = "base";

int main(int argc, char *argv[])
{
    int ret = 0;

    bool allow_multi_user_threads   = false;
    bool have_hidden_threads  = false;
    char *jobid_str, *procid_str;

    /* daemonize myself */
    ompi_daemon_init(NULL);

    /*
     * Intialize the Open MPI environment
     */
    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

    /* get the system info and setup defaults */
    ompi_sys_info();
    ompi_universe_info.host = strdup(ompi_system_info.nodename);
    ompi_universe_info.uid = strdup(ompi_system_info.user);

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

    /* parse environmental variables and fill corresponding info structures */
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

    /* if I'm not the seed and don't have my replica info, look for them in the
     * named universe
     */
    if (!ompi_process_info.seed &&
	NULL == ompi_process_info.gpr_replica &&
	NULL == ompi_process_info.ns_replica) {
    }

    /* setup the rest of the rte */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage2(&allow_multi_user_threads,
						    &have_hidden_threads))) {
        /* JMS show_help */
        printf("show_help: ompid failed in ompi_rte_init\n");
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
    if (OMPI_SUCCESS != (ret = ompi_rte_init_finalstage(&allow_multi_user_threads,
							 &have_hidden_threads))) {
        /* JMS show_help */
        printf("show_help: ompid failed in ompi_rte_init\n");
        return ret;
    }

    /* register this node on the virtual machine */
    /* 	ompi_vm_register(); */

    /* register the daemon callback function */

    /* setup and enter the event monitor */

    ompi_rte_finalize();
    mca_base_close();
    ompi_finalize();
    return 0;
}


/*     /\* convert myself to be a daemon *\/ */
/*     if (OMPI_SUCCESS != ompi_daemon_init(ompi_process_info.universe_session_dir)) { */
/* 	fprintf(stderr, "could not convert to daemon - please report error to bugs@open-mpi.org\n"); */
/* 	exit(1); */
/*     } */

/*      * as file "contact-info" so others can find us. */
/*      *\/ */

/*     /\* Add in the calls to initialize the services *\/ */

/*     /\* Add the section for the event loop... *\/ */

/*     /\* All done *\/ */

/*     /\* Close services *\/ */

/*     OBJ_RELEASE(cmd_line); */
/*     mca_base_close(); */
