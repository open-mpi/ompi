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

#include "runtime/runtime.h"

void ompi_rte_cmd_line_setup(ompi_cmd_line_t *cmd_line)
{
    /* setup the rte command line arguments */
    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_daemon_cmd_line */
			    's', "seed", "seed", 0,
			   "Set the daemon seed to true.");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_daemon_cmd_line */
			    '\0', "probe", "probe", 0,
			    "Define as probe");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_cmd_line */
			    'u', "universe", "universe", 1,
			   "Specify the Open MPI universe");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_cmd_line */
			    '\0', "tmpdir", "tmpdir", 1,
			   "Specify the Open MPI prefix for the session directory");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_daemon_cmd_line */
			   '\0', "persistent", "persistent", 0,
			   "Universe is to be persistent");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_daemon_cmd_line */
			    's', "console", "console", 0,
			   "Provide a console for user interaction");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_daemon_cmd_line */
			    'f', "script", "script", 1,
			   "Read commands from script file");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_daemon_cmd_line */
			    '\0', "scope", "scope", 1,
			   "Scope of this universe");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_daemon_cmd_line */
			    '\0', "hostfile", "hostfile", 1,
			   "Hostfile for this universe");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_cmd_line */
			    '\0', "nsreplica", "nsreplica", 1,
			    "OOB contact info for name server replica assigned to this process");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_cmd_line */
			    '\0', "gprreplica", "gprreplica", 1,
			    "OOB contact info for GPR replica assigned to this process");

    ompi_cmd_line_make_opt3(cmd_line,  /* read in ompi_rte_parse_cmd_line */
			    '\0', "seedcontact", "seedcontact", 1,
			    "OOB contact info for seed of this universe");
}
