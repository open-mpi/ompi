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
#include <unistd.h>
#include <sys/types.h>

#include "util/output.h"
#include "util/os_path.h"
#include "util/sys_info.h"
#include "util/cmd_line.h"
#include "util/common_cmd_line.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "event/event.h"
#include "mca/base/base.h"
#include "mca/oob/base/base.h"

#include "util/universe_setup_file_io.h"
#include "runtime/universe_connect.h"
#include "runtime/runtime.h"


int main(int argc, char **argv)
{
    ompi_cmd_line_t *cmd_line = NULL, *mca_cmd_line=NULL;
    char *tmpdir = NULL;
    char *universe = NULL;
    char *tmp, *universe_name, *remote_host, *remote_uid;
    char *script_file;
    char *contact_file;
    int ret;
    pid_t pid;
    bool persistent, silent, script, webserver;
    bool multi_thread = false;
    bool hidden_thread = false;

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
    ompi_universe_info.host = strdup(ompi_system_info.nodename);
    ompi_universe_info.uid = strdup(ompi_system_info.user);


    /* setup to read common command line options that span all Open MPI programs */
    if (OMPI_SUCCESS != (ret = ompi_common_cmd_line_init(argc, argv))) {
	exit(ret);
    }

    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "help") || 
        ompi_cmd_line_is_taken(ompi_common_cmd_line, "h")) {
        printf("...showing ompi_info help message...\n");
        exit(1);
    }

    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "version") ||
	ompi_cmd_line_is_taken(ompi_common_cmd_line, "v")) {
	printf("...showing off my version!\n");
	exit(1);
    }

    /* setup rte command line arguments */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);
    ompi_rte_cmd_line_setup(cmd_line);

    /*
     * setup  mca command line arguments
     */
    mca_cmd_line = OBJ_NEW(ompi_cmd_line_t);
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

    /* parse the local commands */
    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv)) {
	exit(ret);
    }


/*     /\* parse the cmd_line for seed daemon options *\/ */
/*     ompi_rte_parse_seed_cmd_line(cmd_line); */

/*     /\* check for universe existence *\/ */
/*     if (OMPI_SUCCESS != ompi_rte_universe_exists()) { */
/* 	/\* check exit codes */
/* 	 * exists, but connect not allowed or refused *\/ */
/* 	    /\*define unique name based on current one and start new universe *\/ */
/* 	    pid = getpid(); */
/* 	    if (0 < asprintf(&ompi_universe.name, "%s-%d", universe, pid)) { */
/* 		fprintf(stderr, "error creating unique universe name - please report error to bugs@open-mpi.org\n"); */
/* 		exit(1); */
/* 	    } */

/* 	/\* does not exist - need to start *\/ */
/* 	    if (OMPI_SUCCESS != ompi_rte_universe_initiate()) { */
/* 		fprintf(stderr, "unable to start universe services - please report error to bugs@open-mpi.org\n"); */
/* 		exit(1); */
/* 	    } */
/*     } */

/* 	/\* universe already exists and we can connect - see what else needs doing *\/ */
/* 	goto OPERATE; */

/*     } */

/*     /\* universe must not already exist, so create it from scratch *\/ */
/*  STARTUP: */

/*     /\* spinoff ompid with "seed" flag set - it will become the seed daemon for this universe *\/ */

/*    /\* */
/*      * Start the Open MPI Run Time Environment */
/*      *\/ */
/*     if (OMPI_SUCCESS != (ret = mca_base_open())) { */
/* 	/\* JMS show_help *\/ */
/* 	printf("show_help: mca_base_open failed\n"); */
/* 	return ret; */
/*     } */

/*     if (OMPI_SUCCESS != ompi_rte_init(&multi_thread, &hidden_thread)) { */
/* 	/\* BWB show_help *\/ */
/* 	printf("show_help: ompi_rte_init failed\n"); */
/* 	return ret; */
/*     } */

/*     /\* get OOB contact info *\/ */
/*     ompi_universe.oob_contact_info = mca_oob_get_contact_info(); */

/*     /\* get Web contact info *\/ */
/*     ompi_universe.socket_contact_info = strdup("dum.add.for.tst"); */

/*     /\* save all pertinent info in universe file *\/ */
/*     contact_file = ompi_os_path(false, ompi_process_info.universe_session_dir, */
/* 				"universe-setup.txt", NULL); */

/*     if (OMPI_SUCCESS != ompi_write_universe_setup_file(contact_file, &ompi_universe)) { */
/* 	fprintf(stderr, "couldn't write universe setup file: %s\n", contact_file); */
/* 	exit(1); */
/*     } */

/*     /\* put info on the registry *\/ */

/*     fprintf(stderr, "openmpi: entering event loop\n"); */
/*     /\* event loop *\/ */
/*     ompi_event_loop(0); */
/*     /\* spawn console process *\/ */
/*     if (!silent) { */
/* 	fprintf(stderr, "SUCCESS - spawned console process!\n"); */
/*     } */

/*  OPERATE: */
/* 	/\* if hostfile, startup virtual machine *\/ */
/* 	/\* check registry for nodes in hostfile - if not found, add them *\/ */
/* 	/\* send command - ompi_vm_startup to seed that causes it to read registry segment, check if ompid already */
/* 	 * on each node, spin one up if not *\/ */

/* 	/\* if console, kickoff console - point comm at universe *\/ */

/* 	/\* all done, so exit! *\/ */
/* 	exit(0); */

    exit(0);
}
