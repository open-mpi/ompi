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

#include "mca/ns/base/base.h"

#include "runtime/runtime.h"

void ompi_rte_parse_daemon_cmd_line(ompi_cmd_line_t *cmd_line)
{

    /* see if I'm the seed */
    if (ompi_cmd_line_is_taken(cmd_line, "seed")) {
	ompi_process_info.seed = true;
	ompi_process_info.name = ns_base_create_process_name(0,0,0);
    } else {
	ompi_process_info.seed = false;
	ompi_process_info.name = NULL;
    }

    /* see if I'm a probe */
    if (ompi_cmd_line_is_taken(cmd_line, "probe")) {
	ompi_universe_info.probe = true;
    } else {
	ompi_universe_info.probe = false;
    }

    /* get desired universe scope, if specified */
    if (ompi_cmd_line_is_taken(cmd_line, "scope")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "scope", 0, 0)) {
	    fprintf(stderr, "error retrieving universe scope - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	ompi_universe_info.scope = strdup(ompi_cmd_line_get_param(cmd_line, "scope", 0, 0));
    }

    /* find out if persistent */
    if (ompi_cmd_line_is_taken(cmd_line, "persistent")) {
	ompi_universe_info.persistence = true;
    } else {
	ompi_universe_info.persistence = false;
    }

    /* find out if silent */
    if (ompi_cmd_line_is_taken(cmd_line, "silent")) {
	ompi_universe_info.silent_mode = true;
    } else {
	ompi_universe_info.silent_mode = false;
    }

    /* find out if web interface is desired */
    if (ompi_cmd_line_is_taken(cmd_line, "webserver")) {
	ompi_universe_info.web_server = true;
    } else {
	ompi_universe_info.web_server = false;
    }

    /* find out if script is to be executed */
    if (ompi_cmd_line_is_taken(cmd_line, "script")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "script", 0, 0)) {
	    fprintf(stderr, "error retrieving script file name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	ompi_universe_info.scriptfile = strdup(ompi_cmd_line_get_param(cmd_line, "script", 0, 0));
    } else {
	ompi_universe_info.scriptfile = NULL;
    }

    /* Find out if hostfile specified */
    if (ompi_cmd_line_is_taken(cmd_line, "hostfile")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "hostfile", 0, 0)) {
	    fprintf(stderr, "error retrieving host file name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	ompi_universe_info.hostfile = strdup(ompi_cmd_line_get_param(cmd_line, "hostfile", 0, 0));
    } else {
	ompi_universe_info.hostfile = NULL;
    }
}
