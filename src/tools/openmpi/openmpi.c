/*
  openmpi.c - main program for spawning persistent universe.

  --------------------------------------------------------------------------

  Authors:	Ralph H. Castain <rhc@lanl.gov>

  --------------------------------------------------------------------------

*/
#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "runtime/runtime.h"
#include "runtime/universe_connect.h"
#include "util/output.h"
#include "util/os_path.h"
#include "util/sys_info.h"
#include "util/cmd_line.h"
#include "util/common_cmd_line.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/printf.h"
#include "util/daemon_init.h"
#include "mca/base/base.h"
#include "mca/oob/base/base.h"
#include "tools/openmpi/openmpi.h"


ompi_universe_t ompi_universe = {
    /* .name =              */    NULL,
    /* .host =              */    NULL,
    /* .uid =               */    NULL,
    /* .persistence =       */    false,
    /* .silent_mode =       */    false,
    /* .script_mode =       */    false,
    /* .web_server =        */    false,
    /* .console_connected = */    false
};


int main(int argc, char **argv)
{
    ompi_cmd_line_t *cmd_line = NULL, *mca_cmd_line=NULL;
    char *tmpdir = NULL;
    char *universe = NULL;
    char *tmp, *universe_name, *remote_host, *remote_uid;
    char *script_file, *socket_contact_info, *oob_contact_info;
    char *contact_file;
    int ret;
    bool persistent, silent, script, webserver;
    bool multi_thread = false;
    bool hidden_thread = false;
    FILE *fp;

    tmp = universe_name = remote_host = remote_uid = script_file = NULL;
    persistent = silent = script = webserver = false;

    /* require tcp oob */
    setenv("OMPI_MCA_oob_base_include", "tcp", 1);

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
    ompi_universe.host = strdup(ompi_system_info.nodename);
    ompi_universe.uid = strdup(ompi_system_info.user);
    ompi_universe.name = strdup("default-universe");


    /* setup to read common command line options that span all Open MPI programs */
    if (OMPI_SUCCESS != (ret = ompi_common_cmd_line_init(argc, argv))) {
	exit(ret);
    }

    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "help") || 
        ompi_cmd_line_is_taken(ompi_common_cmd_line, "h")) {
        printf("...showing ompi_info help message...\n");
        exit(1);
    }

    /* setup the rte command line arguments */
    cmd_line = ompi_cmd_line_create();
    ompi_cmd_line_make_opt(cmd_line, 's', "seed", 0, 
			   "Set the daemon seed to true.");

    ompi_cmd_line_make_opt(cmd_line, 
			   'u', "universe", 1,
			   "Specify the Open MPI universe");

    ompi_cmd_line_make_opt(cmd_line, 
			   't', "tmpdir", 1,
			   "Specify the Open MPI prefix for the session directory");

    ompi_cmd_line_make_opt(cmd_line, 'w', "webserver", 0,
			   "Web server available");

    ompi_cmd_line_make_opt(cmd_line, 's', "silent", 0,
			   "No console prompt - operate silently");

    ompi_cmd_line_make_opt(cmd_line, 'f', "script", 1,
			   "Read commands from script file");

    /*
     * setup  mca command line arguments
     */
    mca_cmd_line = ompi_cmd_line_create();
    if (OMPI_SUCCESS != (ret = mca_base_cmd_line_setup(mca_cmd_line))) {
	/* BWB show_help */
	printf("show_help: mca_base_cmd_line_setup failed\n");
	return ret;
    }

    if (OMPI_SUCCESS != mca_base_cmd_line_process_args(mca_cmd_line)) {
	/* BWB show_help */
	printf("show_help: mca_base_cmd_line_process_args\n");
	return ret;
    }

    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv)) {
	exit(ret);
    }

    /* get universe name and store it, if user specified it */
    /* otherwise, stick with default name */
    if (ompi_cmd_line_is_taken(cmd_line, "universe")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "universe", 0, 0)) {
	    fprintf(stderr, "error retrieving universe name - please report error to bugs@open-mpi.org\n");
	    exit(1);
        }
        universe = strdup(ompi_cmd_line_get_param(cmd_line, "universe", 0, 0));

	if (NULL != (tmp = strchr(universe, ':'))) { /* name contains remote host */
	    /* get the host name, and the universe name separated */
	    /* could be in form remote-uid@remote-host:universe */
	    *tmp = '\0';
	    tmp++;
	    ompi_universe.name = strdup(tmp);
	    if (NULL != (tmp = strchr(universe, '@'))) {  /* remote name includes remote uid */
		*tmp = '\0';
		tmp++;
		ompi_universe.host = strdup(tmp);
		ompi_universe.uid = strdup(universe);
	    } else {  /* no remote id - just remote host */
		ompi_universe.host = strdup(universe);
	    }
	} else { /* no remote host - just universe name provided */
	    ompi_universe.name = strdup(universe);
	}
    }

    /* get the temporary directory name for the session directory, if provided on command line */
    if (ompi_cmd_line_is_taken(cmd_line, "tmpdir")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0)) {
	    fprintf(stderr, "error retrieving tmpdir name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	tmpdir = strdup(ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0));
    } else {
	tmpdir = NULL;
    }

    /* find out if silent */
    if (ompi_cmd_line_is_taken(cmd_line, "silent")) {
	silent = true;
    }

    /* find out if web interface is desired */
    if (ompi_cmd_line_is_taken(cmd_line, "webserver")) {
	webserver = true;
    }

    /* find out if script is to be executed */
    if (ompi_cmd_line_is_taken(cmd_line, "script")) {
	script = true;
	if (NULL == ompi_cmd_line_get_param(cmd_line, "script", 0, 0)) {
	    fprintf(stderr, "error retrieving script file name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	script_file = strdup(ompi_cmd_line_get_param(cmd_line, "script", 0, 0));
    }

    /* does universe already exist on specified host? Check session directory to see */
    /* don't know how to handle remote host yet - only cover localhost */

    if (0 == strncmp(ompi_universe.host, ompi_system_info.nodename, strlen(ompi_system_info.nodename))) { /* localhost specified or defaulted */
	if (OMPI_SUCCESS == ompi_session_dir(false, tmpdir, ompi_system_info.user, ompi_system_info.nodename, NULL,
					     ompi_universe.name, NULL, NULL)) { /* found */
	    fprintf(stderr, "think i found something\n");
	    /* check for "contact-info" file. if present, read it in. if not present, wait one second (might
	     * be race condition) and try again. second failure => abnormal termination, go ahead and create own
	     * file and assume prior seed daemon died
	     */
	    /* read the universe info - see if it's persistent */
	    /* if not persistent, augment universe name until unique, create own universe and continue */
	    /* if persistent, use contact info to connect */

	    /* 	    if (OMPI_ERROR == ompi_universe_connect(tmp)) { /\* try to connect *\/ */
	    /* 		/\* first failure - try to start universe and then try again *\/ */
	    /* 		if (NULL == (tmp = ompi_universe_init(tmpdir, ompi_system_info.user, */
	    /* 						      ompi_universe.name))) { /\* couldn't create universe - error *\/ */
	    /* 		    fprintf(stderr, "could not create universe session directory tree - please report error to bugs@open-mpi.org\n"); */
	    /* 		    exit(1); */
	    /* 		} */
	    /* 		if (OMPI_ERROR == ompi_universe_connect(tmp)) { /\* try to connect *\/ */
	    /* 		    /\* second failure - we're doomed *\/ */
	    /* 		    fprintf(stderr, "could not connect to universe - please report error to bugs@open-mpi.org\n"); */
	    /* 		    exit(1); */
	    /* 		} */
	    /* 	    } */
	} else {
	    fprintf(stderr, "session dir not found - creating it - calling univ_init\n");
	    /* setup universe session directory */
	    if (OMPI_SUCCESS != ompi_session_dir(true, tmpdir, ompi_system_info.user, ompi_system_info.nodename, NULL,
						 ompi_universe.name, NULL, NULL)) { /* couldn't create session dir - error */
		fprintf(stderr, "could not create universe session directory tree - please report error to bugs@open-mpi.org\n");
		exit(1);
	    }

	    /* convert myself to be the seed daemon */
	    ompi_process_info.seed = true;
	    ompi_process_info.my_universe = strdup(ompi_universe.name);

	    if (OMPI_SUCCESS != daemon_init(ompi_process_info.universe_session_dir)) {
		fprintf(stderr, "could not convert to daemon - please report error to bugs@open-mpi.org\n");
		exit(1);
	    }

	    /*
	     * Start the Open MPI Run Time Environment
	     */
	    if (OMPI_SUCCESS != (ret = mca_base_open())) {
		/* JMS show_help */
		printf("show_help: mca_base_open failed\n");
		return ret;
	    }

	    if (OMPI_SUCCESS != ompi_rte_init(&multi_thread, &hidden_thread)) {
		/* BWB show_help */
		printf("show_help: ompi_rte_init failed\n");
		return ret;
	    }

	    /* get OOB contact info */
	    oob_contact_info = mca_oob_get_contact_info();

	    /* get Web contact info */
	    socket_contact_info = strdup("dum.add.for.tst");

	    /* save all pertinent info in universe file */
	    contact_file = ompi_os_path(false, ompi_process_info.universe_session_dir,
				   "universe-setup.txt", NULL);
	    fp = fopen(contact_file, "w");
	    if (NULL == fp) {
		ompi_output(0, "cannot open file to save contact info");
		exit(1);
	    }
	    fprintf(fp, "name: %s\n", ompi_universe.name);
	    fprintf(fp, "host: %s\n", ompi_universe.host);
	    fprintf(fp, "user: %s\n", ompi_universe.uid);
	    if (persistent) {
		fprintf(fp, "state: persistent\n");
	    } else {
		fprintf(fp, "state: non-persistent\n");
	    }
	    if (silent) {
		fprintf(fp, "mode: silent\n");
	    } else {
		fprintf(fp, "mode: console\n");
	    }
	    if (webserver) {
		fprintf(fp, "socket: %s\n", socket_contact_info);
	    }
	    fprintf(fp, "oob: %s\n", oob_contact_info);
	    fclose(fp);

	}
    }
    /* spawn console process */
    if (!silent) {
	fprintf(stderr, "SUCCESS - spawned console process!\n");
    }

    return(0);
}
