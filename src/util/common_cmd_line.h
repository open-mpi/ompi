/*
 * $HEADER$
 */

/** 
 * @file 
 *
 * Several command line parameters are common to many/most Open MPI
 * executables / commands.  This file provides a clearinghouse for
 * parsing and holding all these command line options that can be
 * retrieved from elsewhere in Open MPI.  For example, "--universe
 * [name]"
 */

#ifndef OMPI_COMMON_CMD_LINE_H
#define OMPI_COMMON_CMD_LINE_H

#include <util/cmd_line.h>

/**
 * Global variable to hold the parsed set of "common" command line
 * parameters.  This global variable should be treated as a "read
 * only" structure -- various entites in Open MPI can read from this
 * structure, but should not write to it.
 *
 * See the interface defined in cmd_line.h for instructions on how to
 * access the data in this variable.
 */
extern ompi_cmd_line_t *ompi_common_cmd_line;

/**
 * Setup for the command line parameters that are common to all/many
 * Open MPI executables (e.g., --universe).
 *
 * @param argv The argv from main().
 * @param argc The argc from main().
 *
 * @returns OMPI_SUCCESS Always.
 *
 * This function sets up a ompi_cmd_line_t for all command line
 * parameters that are common to a number of Open MPI executables.  It
 * then parses the argc and argv and puts the results in the gloabl
 * ompi_cmd_line_t.
 *
 * This function is only invoked by the normal startup of an Open MPI
 * process; you should not need to invoke it manually.  Anyone who
 * wants to access the common command line parameters should use the
 * global variable ompi_common_cmd_line.
 */
int ompi_common_cmd_line_init(int argc, char **argv);

/**
 * Release resources allocated by ompi_common_cmd_line_init().
 *
 * @returns OMPI_SUCCESS Always.
 *
 * Self-explanitory; frees the resources associated with the global
 * variable ompi_common_cmd_line.  This function is invoked by the
 * normal shutdown of an Open MPI process; you should not need to
 * invoke it manually.
 */
int ompi_common_cmd_line_finalize(void);


#endif /* OMPI_COMMON_CMD_LINE_H */
