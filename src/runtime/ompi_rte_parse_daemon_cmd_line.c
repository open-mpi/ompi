/*
 * $HEADER$
 */

/**
 * @file
 *
 * Parse command line options for the Open MPI Run Time Environment
 */
#include "ompi_config.h"

#include <string.h>

#include "util/output.h"
#include "util/cmd_line.h"
#include "util/sys_info.h"
#include "util/proc_info.h"

#include "runtime/runtime.h"

void ompi_rte_parse_daemon_cmd_line(ompi_cmd_line_t *cmd_line)
{

    /* see if I'm the seed */
    if (ompi_cmd_line_is_taken(cmd_line, "seed") &&
	false == ompi_process_info.seed) {
	ompi_process_info.seed = true;
	setenv("OMPI_universe_seed", "1", 1);
    }

    /* see if I'm a probe */
    if (ompi_cmd_line_is_taken(cmd_line, "probe") &&
	false == ompi_universe_info.probe) {
	setenv("OMPI_universe_probe", "1", 1);
	ompi_universe_info.probe = true;
    }

    /* get desired universe scope, if specified */
    if (ompi_cmd_line_is_taken(cmd_line, "scope")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "scope", 0, 0)) {
	    fprintf(stderr, "error retrieving universe scope - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	ompi_universe_info.scope = strdup(ompi_cmd_line_get_param(cmd_line, "scope", 0, 0));
	setenv("OMPI_universe_scope", ompi_universe_info.scope, 1);
    }

    /* find out if persistent */
    if (ompi_cmd_line_is_taken(cmd_line, "persistent")) {
	setenv("OMPI_universe_persistent", "1", 1);
	ompi_universe_info.persistence = true;
    }

    /* find out if silent */
    if (ompi_cmd_line_is_taken(cmd_line, "silent")) {
	setenv("OMPI_universe_silent", "1", 1);
	ompi_universe_info.silent_mode = true;
    }

    /* find out if web interface is desired */
    if (ompi_cmd_line_is_taken(cmd_line, "webserver")) {
	setenv("OMPI_universe_webserver", "1", 1);
	ompi_universe_info.web_server = true;
    }

    /* find out if script is to be executed */
    if (ompi_cmd_line_is_taken(cmd_line, "script")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "script", 0, 0)) {
	    fprintf(stderr, "error retrieving script file name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	ompi_universe_info.scriptfile = strdup(ompi_cmd_line_get_param(cmd_line, "script", 0, 0));
	setenv("OMPI_universe_script", ompi_universe_info.scriptfile, 1);
    }

    /* Find out if hostfile specified */
    if (ompi_cmd_line_is_taken(cmd_line, "hostfile")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "hostfile", 0, 0)) {
	    fprintf(stderr, "error retrieving host file name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	ompi_universe_info.hostfile = strdup(ompi_cmd_line_get_param(cmd_line, "hostfile", 0, 0));
	setenv("OMPI_universe_hostfile", ompi_universe_info.hostfile, 1);
    }
}
