/*
openmpi.c - main program for spawning persistent universe.

 --------------------------------------------------------------------------

 Authors:	Ralph H. Castain <rhc@lanl.gov>

 --------------------------------------------------------------------------

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "ompi_config.h"

#include "util/sys_info.h"
#include "util/cmd_line.h"
#include "util/common_cmd_line.h"

#include "util/proc_info.h"
#include "util/session_dir.h"
#include "runtime/universe_init.h"
#include "runtime/universe_connect.h"
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
    ompi_cmd_line_t *cmd_line = NULL;
    char *tmpdir = NULL;
    char *universe = NULL;
    char *tmp, *universe_name, *remote_host, *remote_uid;

    tmp = universe_name = remote_host = remote_uid = NULL;

    /* setup to read common command line options that span all Open MPI programs */
    ompi_common_cmd_line_init(argc, argv);

    /* setup to check non-common command line options - ones specific to this program */
    cmd_line = ompi_cmd_line_create();
    if (NULL == cmd_line) {
        fprintf(stderr,"openmpi: Command line handle could not be created - please report error to bugs@open-mpi.org"); 
        exit(errno);
    }

    ompi_cmd_line_make_opt(cmd_line, 'v', "version", 0,
			   "Show version of Open MPI and this program");
    ompi_cmd_line_make_opt(cmd_line, 'w', "webserver", 1,
			   "Web server available");
    ompi_cmd_line_make_opt(cmd_line, 's', "silent", 1,
			   "No console prompt - operate silently");
    ompi_cmd_line_make_opt(cmd_line, 'f', "script", 1,
			   "Read commands from script file");
    ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0,
			   "Show help for this function");

    if ((OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv)) ||
        ompi_cmd_line_is_taken(cmd_line, "help")) {
        fprintf(stderr, "...showing openmpi help message...\n");
        exit(1);
    }

    /* get universe name and store it, if user specified it */
    /* otherwise, stick with default name */
    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "universe")) {
	if (NULL == ompi_cmd_line_get_param(ompi_common_cmd_line, "universe", 0, 0)) {
	    fprintf(stderr, "error retrieving universe name - please report error to bugs@open-mpi.org\n");
	    exit(1);
        }
        universe = strdup(ompi_cmd_line_get_param(ompi_common_cmd_line, "universe", 0, 0));

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
    } else {
	ompi_universe.name = strdup("default-universe");
    }

    /* get the temporary directory name for the session directory, if provided on command line */
    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "tmpdir")) {
	if (NULL == ompi_cmd_line_get_param(ompi_common_cmd_line, "tmpdir", 0, 0)) {
	    fprintf(stderr, "error retrieving tmpdir name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	tmpdir = strdup(ompi_cmd_line_get_param(ompi_common_cmd_line, "tmpdir", 0, 0));
    } else {
	tmpdir = NULL;
    }

    /* startup the MCA so we can use OOB */
    /*   if (OMPI_ERROR == ompi_mca_init()) {
	 fprintf(stderr, "MCA could not start - please report error to bugs@open-mpi.org\n");
	 exit (1);
	 }
    */
    /* does universe already exist on specified host? Check session directory to see */
    /* don't know how to handle remote host yet - only cover localhost */

    if (NULL == ompi_universe.host) { /* localhost specified */
	if (NULL == (tmp = ompi_session_dir(false, tmpdir, ompi_system_info.user, ompi_universe.name,
					       NULL, NULL))) { /* not found */
	    fprintf(stderr, "session dir not found - creating it - calling univ_init\n");
	    /* setup universe and connections */
	    if (NULL == (tmp = ompi_universe_init(tmpdir, ompi_system_info.user,
						  ompi_universe.name))) { /* couldn't create universe - error */
		fprintf(stderr, "could not create universe session directory tree - please report error to bugs@open-mpi.org\n");
		exit(1);
	    }
	    if (OMPI_ERROR == ompi_universe_connect(tmp)) { /* try to connect */
		/* failed - we're doomed */
		fprintf(stderr, "could not connect to universe - please report error to bugs@open-mpi.org\n");
		exit(1);
	    }
	} else { /* was found! read session info and try to connect */
	    fprintf(stderr, "think i found something\n");
	    if (OMPI_ERROR == ompi_universe_connect(tmp)) { /* try to connect */
		/* first failure - try to start universe and then try again */
		if (NULL == (tmp = ompi_universe_init(tmpdir, ompi_system_info.user,
						      ompi_universe.name))) { /* couldn't create universe - error */
		    fprintf(stderr, "could not create universe session directory tree - please report error to bugs@open-mpi.org\n");
		    exit(1);
		}
		if (OMPI_ERROR == ompi_universe_connect(tmp)) { /* try to connect */
		    /* second failure - we're doomed */
		    fprintf(stderr, "could not connect to universe - please report error to bugs@open-mpi.org\n");
		    exit(1);
		}
	    }
	}
    }

    return(0);
}
