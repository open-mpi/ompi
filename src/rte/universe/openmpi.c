/*
openmpi.c - main program for spawning persistent universe.

 --------------------------------------------------------------------------

 Authors:	Ralph H. Castain <rhc@lanl.gov>

 --------------------------------------------------------------------------

*/

#include "lam_config.h"

#include <stdio.h>
#include <string.h>

#include "util/sys_info.h"
#include "util/cmd_line.h"

#include "rte/universe/os_session_dir.h"
#include "rte/universe/openmpi.h"

/**
* Parse command line options and check for validity. Track which
* ones have been provided to assess completeness of information.
*
* @retval OMPI_SUCCESS if all options provided are valid
* @retval OMPI_ERROR if any option is not valid. Invalid options
* will be reported to user but will not terminate processing.
*/

ompi_universe_t universe = {.name = NULL,
			    .pid = -1,
			    .session_file = NULL,
                            .persistence = false,
			    .web_server = false,
			    .console_connected = false
};


int main(int argc, char *argv[])
{
    lam_cmd_line_t *cmd_line = NULL;
    char *tmpdir_option = NULL;

    cmd_line = lam_cmd_line_create();
    if (NULL == cmd_line) {
        fprintf(stderr,"openmpi: Command line handle could not be created - please report error to bugs@open-mpi.org"); 
        exit(errno);
    }

    lam_cmd_line_make_opt(cmd_line, 'v', "version", 0,
                        "Show version of Open MPI and this program");
    lam_cmd_line_make_opt(cmd_line, 'u', "universe", 1,
                        "User specified name for universe");
    lam_cmd_line_make_opt(cmd_line, 't', "tmpdir", 1,
			  "Temp directory to be used by universe");
    lam_cmd_line_make_opt(cmd_line, 'w', "webserver", 1,
                          "Web server available");

    if ((LAM_SUCCESS != lam_cmd_line_parse(cmd_line, false, argc, argv)) ||
        lam_cmd_line_is_taken(cmd_line, "help") || 
	lam_cmd_line_is_taken(cmd_line, "h")) {
        fprintf(stderr, "...showing openmpi help message...\n");
        exit(1);
    }

    /* get universe name and store it, if user specified it */
    /* otherwise, stick with default name */
    if (lam_cmd_line_is_taken(cmd_line, "universe")) {
	if (NULL == lam_cmd_line_get_param(cmd_line, "universe", 0, 0)) {
	    fprintf(stderr, "error retrieving universe name - please report error to bugs@open-mpi.org\n");
		exit(1);
        }
        universe.name = strdup(lam_cmd_line_get_param(cmd_line, "universe", 0, 0));
    } else {
	universe.name = strdup("ompi-default-universe");
    }

    /* get the pid and store it for later use */
    universe.pid = getpid();

    /* get the temporary directory name for the session directory, if provided on command line */
    if (lam_cmd_line_is_taken(cmd_line, "tmpdir")) {
	if (NULL == lam_cmd_line_get_param(cmd_line, "tmpdir", 0, 0)) {
	    fprintf(stderr, "error retrieving tmpdir name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	tmpdir_option = strdup(lam_cmd_line_get_param(cmd_line, "tmpdir", 0, 0));
    }

    /* get the system information */
    ompi_system_info.init = false;
    ompi_sys_info();
    if (!ompi_system_info.init) {
	fprintf(stderr, "Couldn't get system information\n");
	exit(1);
    }

    /* create the session directory */
    if (LAM_ERROR == ompi_session_dir_init(tmpdir_option, "test-universe")) {
	fprintf(stderr, "error creating session directory - please report error to bugs@open-mpi.org");
	exit(1);
    }

    /* store this session information */
    /* first, check to see if we are rejoining an existing session */
    /* yes, so read info from file */
    /* no existing session, so create the file */
    return(0);
}
