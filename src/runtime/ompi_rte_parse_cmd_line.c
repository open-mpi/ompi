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

void ompi_rte_parse_cmd_line(ompi_cmd_line_t *cmd_line)
{
    char *universe, *tmp;

    /* get universe name and store it, if user specified it */
    /* otherwise, stick with default name */
    universe = strdup(ompi_universe_info.name); /* save the default */
    if (ompi_cmd_line_is_taken(cmd_line, "universe") ||
	ompi_cmd_line_is_taken(cmd_line, "u")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "universe", 0, 0)) {
	    fprintf(stderr, "error retrieving universe name - please report error to bugs@open-mpi.org\n");
	    exit(1);
        }
        universe = strdup(ompi_cmd_line_get_param(cmd_line, "universe", 0, 0));


	if (NULL != (tmp = strchr(universe, ':'))) { /* name contains remote host */
	    /* get the host name, and the universe name separated */
	    /* could be in form remote-uid@remote-host:universe */
	    *tmp = '\0';
	    tmp++;
	    ompi_universe_info.name = strdup(tmp);
	    if (NULL != (tmp = strchr(universe, '@'))) {  /* remote name includes remote uid */
		*tmp = '\0';
		tmp++;
		ompi_universe_info.host = strdup(tmp);
		ompi_universe_info.uid = strdup(universe);
	    } else {  /* no remote id - just remote host */
		ompi_universe_info.host = strdup(universe);
	    }
	} else { /* no remote host - just universe name provided */
	    ompi_universe_info.name = strdup(universe);
	}
    }
    /* copy the universe name into the process_info structure */
    ompi_process_info.my_universe = strdup(ompi_universe_info.name);

    /* get the temporary directory name for the session directory, if provided on command line */
    if (ompi_cmd_line_is_taken(cmd_line, "tmpdir")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0)) {
	    fprintf(stderr, "error retrieving tmpdir name - please report error to bugs@open-mpi.org\n");
	    exit(1);
	}
	ompi_process_info.tmpdir_base = strdup(ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0));
    } else {
	ompi_process_info.tmpdir_base = NULL;
    }
}
