/*
 * $HEADER$
 */

/**
 * @file
 *
 * Setup command line options for the Open MPI Run Time Environment
 */

#include "ompi_config.h"

#include "util/cmd_line.h"

void ompi_rte_cmd_line_setup(ompi_cmd_line_t *cmd_line)
{
    /* setup the rte command line arguments */
    ompi_cmd_line_make_opt(cmd_line,
			   's', "seed", 0,
			   "Set the daemon seed to true.");

    ompi_cmd_line_make_opt(cmd_line, 
			   'u', "universe", 1,
			   "Specify the Open MPI universe");

    ompi_cmd_line_make_opt(cmd_line, 
			   't', "tmpdir", 1,
			   "Specify the Open MPI prefix for the session directory");

    ompi_cmd_line_make_opt(cmd_line, 'w', "webserver", 0,
			   "Web server available");

    ompi_cmd_line_make_opt(cmd_line, 's', "silent", 0,
			   "No console prompt - operate silently");

    ompi_cmd_line_make_opt(cmd_line, 'f', "script", 1,
			   "Read commands from script file");

    ompi_cmd_line_make_opt(cmd_line, 'c', "scope", 1,
			   "Scope of this universe");

    ompi_cmd_line_make_opt(cmd_line, 'h', "hostfile", 1,
			   "Hostfile for this universe");
}
