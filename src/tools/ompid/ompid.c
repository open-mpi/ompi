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
#include "runtime/universe_connect.h"
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
    int ret = 0;

    bool multi_thread   = false;
    bool hidden_thread  = false;

    ompi_cmd_line_t *mca_cmd_line=NULL;

/*     /\* */
/*      * Intialize the Open MPI environment */
/*      *\/ */
/*     if (OMPI_SUCCESS != ompi_init(argc, argv)) { */
/*         /\* BWB show_help *\/ */
/*         printf("show_help: ompi_init failed\n"); */
/*         return ret; */
/*     } */

/*     /\* get the system info *\/ */
/*     ompi_sys_info(); */

/*     /\* setup to read common command line options that span all Open MPI programs *\/ */
/*     if (OMPI_SUCCESS != (ret = ompi_common_cmd_line_init(argc, argv))) { */
/* 	exit(ret); */
/*     } */

/*     if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "help") ||  */
/*         ompi_cmd_line_is_taken(ompi_common_cmd_line, "h")) { */
/*         printf("...showing ompi_info help message...\n"); */
/*         exit(1); */
/*     } */

/*     /\* setup the rte command line arguments *\/ */
/*     cmd_line = OBJ_NEW(ompi_cmd_line_t); */
/*     ompi_cmd_line_make_opt(cmd_line, 's', "seed", 0,  */
/* 			   "Set the daemon seed to true."); */

/*     ompi_cmd_line_make_opt(cmd_line,  */
/* 			   'u', "universe", 1, */
/* 			   "Specify the Open MPI universe"); */

/*     ompi_cmd_line_make_opt(cmd_line,  */
/* 			   't', "tmpdir", 1, */
/* 			   "Specify the Open MPI prefix for the session directory"); */

/*     ompi_cmd_line_make_opt(cmd_line, 'w', "webserver", 0, */
/* 			   "Web server available"); */

/*     ompi_cmd_line_make_opt(cmd_line, 's', "silent", 0, */
/* 			   "No console prompt - operate silently"); */

/*     ompi_cmd_line_make_opt(cmd_line, 'f', "script", 1, */
/* 			   "Read commands from script file"); */

/*     /\* */
/*      * setup  mca command line arguments */
/*      *\/ */
/*     mca_cmd_line = OBJ_NEW(ompi_cmd_line_t); */
/*     if (OMPI_SUCCESS != (ret = mca_base_cmd_line_setup(mca_cmd_line))) { */
/* 	/\* BWB show_help *\/ */
/* 	printf("show_help: mca_base_cmd_line_setup failed\n"); */
/* 	return ret; */
/*     } */

/*     if (OMPI_SUCCESS != mca_base_cmd_line_process_args(mca_cmd_line)) { */
/* 	/\* BWB show_help *\/ */
/* 	printf("show_help: mca_base_cmd_line_process_args\n"); */
/* 	return ret; */
/*     } */

/*     if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv)) { */
/* 	exit(ret); */
/*     } */

/*     /\* Do the parsing *\/ */

/*     if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "help")) { */
/* #if 1 */
/* 	printf("...showing ompid help message...\n"); */
/* 	printf("\nThe following optional arguments are available: --v --h --seeded\n"); */
/* 	printf("\t ompid -h      is used to display this information\n"); */
/* 	printf("\t ompid -v      is used to display the version of this application\n"); */
/* 	printf("\t ompid -seed is used to start the universe (seed) daemon.\n"); */
/* 	printf("\n"); */
/* #else */
/* 	show_help("ompid", "usage", NULL); */
/* #endif */
/* 	exit(1); */
/*     } */


/*     if (OMPI_SUCCESS != (ret = mca_base_open())) { */
/* 	/\* JMS show_help *\/ */
/* 	printf("show_help: mca_base_open failed\n"); */
/* 	return ret; */
/*     } */

 
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
/*      * as file "contact-info" so others can find us. */
/*      *\/ */

/*     /\* Add in the calls to initialize the services *\/ */

/*     /\* Add the section for the event loop... *\/ */

/*     /\* All done *\/ */

/*     /\* Close services *\/ */

/*     OBJ_RELEASE(cmd_line); */
/*     mca_base_close(); */
/*     ompi_finalize(); */
/*     return 0; */
}
