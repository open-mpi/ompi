/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Parse command line options for the Open MPI Run Time Environment
 */
#include "ompi_config.h"

#include <string.h>

#include "mca/ns/base/base.h"

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

    /* see if seed contact info is provided */
    if (ompi_cmd_line_is_taken(cmd_line, "seedcontact")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "seedcontact", 0, 0)) {
	    fprintf(stderr, "error retrieving seed contact info - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	if (NULL != ompi_universe_info.seed_contact_info) {  /* overwrite it */
	    free(ompi_universe_info.seed_contact_info);
	    ompi_universe_info.seed_contact_info = NULL;
	}
	ompi_universe_info.seed_contact_info = strdup(ompi_cmd_line_get_param(cmd_line, "seedcontact", 0, 0));
	setenv("OMPI_universe_contact", ompi_universe_info.seed_contact_info, 1);
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
	if (NULL != ompi_universe_info.scope) {
	    free(ompi_universe_info.scope);
	    ompi_universe_info.scope = NULL;
	}
	ompi_universe_info.scope = strdup(ompi_cmd_line_get_param(cmd_line, "scope", 0, 0));
	setenv("OMPI_universe_scope", ompi_universe_info.scope, 1);
    }

    /* find out if persistent */
    if (ompi_cmd_line_is_taken(cmd_line, "persistent")) {
	setenv("OMPI_universe_persistent", "1", 1);
	ompi_universe_info.persistence = true;
    }

    /* find out if we desire a console */
    if (ompi_cmd_line_is_taken(cmd_line, "console")) {
	setenv("OMPI_universe_console", "1", 1);
	ompi_universe_info.console = true;
	ompi_universe_info.console_connected = false;
    }

    /* find out if script is to be executed */
    if (ompi_cmd_line_is_taken(cmd_line, "script")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "script", 0, 0)) {
	    fprintf(stderr, "error retrieving script file name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	if (NULL != ompi_universe_info.scriptfile) {
	    free(ompi_universe_info.scriptfile);
	    ompi_universe_info.scriptfile = NULL;
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
	if (NULL != ompi_universe_info.hostfile) {
	    free(ompi_universe_info.hostfile);
	    ompi_universe_info.hostfile = NULL;
	}
	ompi_universe_info.hostfile = strdup(ompi_cmd_line_get_param(cmd_line, "hostfile", 0, 0));
	setenv("OMPI_universe_hostfile", ompi_universe_info.hostfile, 1);
    }
}
