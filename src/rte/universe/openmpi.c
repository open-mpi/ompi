/*
openmpi.c - main program for spawning persistent universe.

 --------------------------------------------------------------------------

 Authors:	Ralph H. Castain <rhc@lanl.gov>

 --------------------------------------------------------------------------

*/

#include <stdio.h>
#include <string.h>
#include <pwd.h>

#include "ompi_config.h"

#include "util/sys_info.h"
#include "util/cmd_line.h"

#include "rte/universe/os_session_dir.h"
#include "rte/universe/ompi_init.h"
#include "rte/universe/openmpi.h"

/**
* Parse command line options and check for validity. Track which
* ones have been provided to assess completeness of information.
*
* @retval OMPI_SUCCESS if all options provided are valid
* @retval OMPI_ERROR if any option is not valid. Invalid options
* will be reported to user but will not terminate processing.
*/

ompi_universe_t universe = {
    /* .name =              */    NULL,
    /* .host =              */    NULL,
    /* .user_name =         */    NULL,
    /* .user_id =           */    -1,
    /* .pid =               */    -1,
    /* .session_file =      */    NULL,
    /* .persistence =       */    false,
    /* .silent_mode =       */    false,
    /* .script_mode =       */    false,
    /* .web_server =        */    false,
    /* .console_connected = */    false
};


int main(int argc, char *argv[])
{
    ompi_cmd_line_t *cmd_line = NULL;
    char *tmpdir_option = NULL;
    char *universe_option = NULL;
    char *tmp, *universe_name, *remote_host, *remote_uid;
    struct passwd *pwdent;

    tmp = universe_name = remote_host = remote_uid = NULL;

    cmd_line = ompi_cmd_line_create();
    if (NULL == cmd_line) {
        fprintf(stderr,"openmpi: Command line handle could not be created - please report error to bugs@open-mpi.org"); 
        exit(errno);
    }

    ompi_cmd_line_make_opt(cmd_line, 'v', "version", 0,
                        "Show version of Open MPI and this program");
    ompi_cmd_line_make_opt(cmd_line, 'u', "universe", 1,
                        "User specified name for universe");
    ompi_cmd_line_make_opt(cmd_line, 't', "tmpdir", 1,
			  "Temp directory to be used by universe");
    ompi_cmd_line_make_opt(cmd_line, 'w', "webserver", 1,
                          "Web server available");
    ompi_cmd_line_make_opt(cmd_line, 's', "silent", 1,
                          "No console prompt - operate silently");
    ompi_cmd_line_make_opt(cmd_line, 'f', "script", 1,
                          "Read commands from script file");

    if ((OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv)) ||
        ompi_cmd_line_is_taken(cmd_line, "help") || 
	ompi_cmd_line_is_taken(cmd_line, "h")) {
        fprintf(stderr, "...showing openmpi help message...\n");
        exit(1);
    }

    /* get universe name and store it, if user specified it */
    /* otherwise, stick with default name */
    if (ompi_cmd_line_is_taken(cmd_line, "universe")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "universe", 0, 0)) {
	    fprintf(stderr, "error retrieving universe name - please report error to bugs@open-mpi.org\n");
		exit(1);
        }
        universe_option = strdup(ompi_cmd_line_get_param(cmd_line, "universe", 0, 0));

	if (NULL != (tmp = strchr(universe_option, ':'))) { /* name contains remote host */
	    /* get the host name, and the universe name separated */
	    /* could be in form remote-uid@remote-host:universe */
	    *tmp = '\0';
	    tmp++;
	    universe_name = strdup(tmp);
	    if (NULL != (tmp = strchr(universe_option, '@'))) {  /* remote name includes remote uid */
		*tmp = '\0';
		tmp++;
		remote_host = strdup(tmp);
		remote_uid = strdup(universe_option);
	    }
	}
    } else {
	universe_name = strdup("default");
    }

    /* get the pid and store it for later use */
    universe.pid = getpid();

    /* get the temporary directory name for the session directory, if provided on command line */
    if (ompi_cmd_line_is_taken(cmd_line, "tmpdir")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0)) {
	    fprintf(stderr, "error retrieving tmpdir name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	tmpdir_option = strdup(ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0));
    } else {
	tmpdir_option = NULL;
    }

    /* Store all the information in the Universe structure for later use */
    universe.name = strdup(universe_name);
    if (NULL != remote_host) {
	universe.host = strdup(remote_host);
    } else {
	universe.host = (char *)malloc(OMPI_RIDICULOUS_NAMELEN);
	if (NULL == universe.host) {
	    fprintf(stderr, "openmpi(error): unable to get memory allocation - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	gethostname(universe.host, OMPI_RIDICULOUS_NAMELEN);
    }

    if (NULL != remote_uid) {
	universe.user_name = strdup(remote_uid);
    } else {   /* get the name of the user */
	if ((pwdent = getpwuid(getuid())) != 0) {
	    universe.user_name = strdup(pwdent->pw_name);
	} else {
	    universe.user_name = strdup("unknown");
	}
    }


    /* initialize the Open MPI system */
    if (OMPI_ERROR == ompi_init(universe_name, tmpdir_option)) {
	fprintf(stderr, "Unable to initialize system - please report error to bugs@open-mpi.org\n");
	exit(1);
    }

    return(0);
}
