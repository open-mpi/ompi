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
#include "util/common_cmd_line.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/printf.h"
#include "util/daemon_init.h"
#include "mca/base/base.h"
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
    int ret = 0, i;

    bool allow_multi_user_threads   = false;
    bool have_hidden_threads  = false;

    for (i=0; i<argc; i++) {
	ompi_output(0, "i %d argv %s", i, argv[i]);
    }

    /*
     * Intialize the Open MPI environment
     */
    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

    /* Open up the MCA */

    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_base_open\n");
        return ret;
    }

    /* Join the run-time environment */
    allow_multi_user_threads = true;
    have_hidden_threads = false;
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage1(&allow_multi_user_threads,
						    &have_hidden_threads))) {
	return ret;
    }

    /* get the system info */
    ompi_sys_info();

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


    /* parse the cmd_line for rte options - provides the universe name
     * and temp directory base, if provided by user. Both loaded into
     * ompi_universe_info and ompi_process_info structures as specified
     * Also provides name server and gpr replicas, if provided, and the
     * initial contact info for the "i'm alive" callback.
     */
    ompi_rte_parse_cmd_line(cmd_line);

    /* parse the cmd_line for daemon options - gets all the options relating
     * specifically to seed behavior, in case i'm a seed, but also gets
     * options about scripts and hostfiles that might be of use to me
     */
    ompi_rte_parse_daemon_cmd_line(cmd_line);

    /* start the rest of the rte */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage2(&allow_multi_user_threads,
						    &have_hidden_threads))) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_rte_init\n");
        return ret;
    }

    ompi_output(0, "HEY - I DID IT");
 
    /*     /\* Execute the desired action(s) *\/ */

    /*     if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "version")) { */
    /* 	printf ("ompid (OpenMpi Daemon) version: 0\n"); */
    /*     } */
 

    /*     /\* setup universe session directory *\/ */
    /*     if (OMPI_SUCCESS != ompi_session_dir(true, tmpdir, ompi_system_info.user, ompi_system_info.nodename, NULL, */
    /* 					 ompi_universe.name, NULL, NULL)) { /\* couldn't create session dir - error *\/ */
    /* 	fprintf(stderr, "could not create universe session directory tree - please report error to bugs@open-mpi.org\n"); */
    /* 	exit(1); */
    /*     } */

    /*     /\* If there is a seed argument, this is the magic */
    /*      * seed daemon. */
    /*      *\/ */
    /*     if ( ompi_cmd_line_is_taken(cmd_line, "seed")) { */
    /* 	ompi_process_info.seed = true; */
    /* 	ompi_process_info.my_universe = strdup(ompi_universe.name); */
    /*     } */

    /*     /\* convert myself to be a daemon *\/ */
    /*     if (OMPI_SUCCESS != ompi_daemon_init(ompi_process_info.universe_session_dir)) { */
    /* 	fprintf(stderr, "could not convert to daemon - please report error to bugs@open-mpi.org\n"); */
    /* 	exit(1); */
    /*     } */

 
    /*     /\* before calling anything else we to call rte init *\/ */
    /*     if (OMPI_SUCCESS != ompi_rte_init(&multi_thread, &hidden_thread)) { */
    /* 	printf("ompid: ompi_rte_init failed\n"); */
 
    /* 	/\* Do a partial clean-up.  This needs to be reviewed */
    /* 	 * at a later date to make certain we are not  */
    /* 	 * missing soemthing */
    /* 	 *\/ */
    /* 	ompi_rte_finalize(); */
    /* 	OBJ_RELEASE(cmd_line); */
    /* 	mca_base_close(); */

    /* 	return 1; */
    /*     } */


    /*     /\* */
    /*      * if seed, call open functions of comm frameworks (oob, socket, etc.) to */
    /*      * get contact info. write contact info into universe session directory */

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
 

    /* 	/\* if hostfile, startup virtual machine *\/ */
    /* 	/\* check registry for nodes in hostfile - if not found, add them *\/ */
    /* 	/\* send command - ompi_vm_startup to seed that causes it to read registry segment, check if ompid already */
    /* 	 * on each node, spin one up if not *\/ */



    /*      * as file "contact-info" so others can find us. */
    /*      *\/ */

    /*     /\* Add in the calls to initialize the services *\/ */

    /*     /\* Add the section for the event loop... *\/ */

    /*     /\* All done *\/ */

    /*     /\* Close services *\/ */

    /*     OBJ_RELEASE(cmd_line); */
    /*     mca_base_close(); */
    ompi_finalize();
    return 0;
}
