/*
  Copyright (c) 2004-2005 The Trustees of Indiana University.
                          All rights reserved.
  Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
                          All rights reserved.
  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
                          University of Stuttgart.  All rights reserved.
  $COPYRIGHT$
  
  Additional copyrights may follow
  
  $HEADER$

  openmpi.c - main program for spawning persistent universe.

  --------------------------------------------------------------------------

  Authors:	Ralph H. Castain <rhc@lanl.gov>

  --------------------------------------------------------------------------

*/
#include "orte_config.h"

#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>

#include "event/event.h"
#include "include/constants.h"

#include "util/argv.h"
#include "util/output.h"
#include "util/os_path.h"
#include "util/sys_info.h"
#include "util/univ_info.h"
#include "util/cmd_line.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/universe_setup_file_io.h"

#include "mca/base/base.h"
#include "mca/oob/base/base.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"

#include "runtime/runtime.h"


int main(int argc, char **argv)
{
#if 0
    ompi_cmd_line_t *cmd_line = NULL;
    char *universe = NULL;
    pid_t pid;
    bool multi_thread = false;
    bool hidden_thread = false;
    int ret=0;

   /*
     * Intialize the Open MPI environment
     */
    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

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

    /* start the initial barebones RTE (just OOB) so we can check universe existence */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        /* JMS show_help */
        printf("show_help: mca_base_open failed\n");
        exit(ret);
    }

    multi_thread = true;
    hidden_thread = false;
/*     if (OMPI_SUCCESS != ompi_rte_init_stage1(&multi_thread, &hidden_thread)) { */
/* 	printf("show_help: openmpi failed in ompi_rte_init\n"); */
/* 	exit(1); */
/*     } */

    /* parse environmental variables and fill corresponding info structures
     * need the oob to be open so we can pass the contact info we extract
     */
    ompi_rte_parse_environ();

    /* parse the cmd_line for rte options - override settings from enviro, where necessary
     * copy everything into enviro variables for passing later on
     */
    ompi_rte_parse_cmd_line(cmd_line);

    /* parse the cmd_line for daemon options - gets all the options relating
     * specifically to seed behavior, but also gets
     * options about scripts and hostfiles that might be of use to me
     * puts everything into enviro variables for future passing
     */
    ompi_rte_parse_daemon_cmd_line(cmd_line);

    /* check for local universe existence */
    if (0 != strncmp(ompi_universe_info.host, ompi_system_info.nodename, strlen(ompi_system_info.nodename))) {
	fprintf(stderr, "remote universe operations not supported at this time\n");
	exit(1);
    }

    if (OMPI_SUCCESS != (ret = ompi_rte_universe_exists()) &&
	(OMPI_ERR_NOT_IMPLEMENTED != ret)) {

	if (OMPI_ERR_NOT_FOUND != ret) {
	    /* if not found, then keep current name. otherwise,
	     * define unique name based on current one.
	     * either way, start new universe
	     */
	    universe = strdup(ompi_universe_info.name);
	    free(ompi_universe_info.name);
	    ompi_universe_info.name = NULL;
	    pid = getpid();
	    if (0 < asprintf(&ompi_universe_info.name, "%s-%d", universe, pid)) {
		fprintf(stderr, "error creating unique universe name - please report error to bugs@open-mpi.org\n");
		exit(1);
	    }
	}

	if (NULL != ompi_process_info.my_universe) {
	    free(ompi_process_info.my_universe);
	    ompi_process_info.my_universe = NULL;
	}
	ompi_process_info.my_universe = strdup(ompi_universe_info.name);

	/* ensure the enviro variables do NOT specify any replicas so that seed
	 * will start them up. set seed flag
	 */
	setenv("OMPI_universe_seed", "1", 1);
	unsetenv("OMPI_MCA_ns_base_replica");
	unsetenv("OMPI_MCA_gpr_base_replica");

	/* we're set - fork/exec ompid for local start of seed
	 */
	if ((pid = fork()) < 0) {
	    fprintf(stderr, "unable to fork - please report error to bugs@open-mpi.org\n");
	    exit(1);
	} else if (pid != 0) {
	    exit(0);   /* parent goes bye-bye */
	}
	if (0 > execvp("ompid", argv)) {
	    fprintf(stderr, "unable to exec daemon - please report error to bugs@open-mpi.org\n");
	    fprintf(stderr, "errno: %s\n", strerror(errno));
	    exit(1);

	}
    } else {
	fprintf(stderr, "local universe check reports not implemented code\n");
    }
/*     ompi_rte_init_stage2(&multi_thread, &hidden_thread);  /\* stick this in for now just for static compiles *\/ */
#endif
    return -1;
}
