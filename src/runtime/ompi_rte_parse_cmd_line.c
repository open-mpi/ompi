/*
 * $HEADER$
 */

/**
 * @file
 *
 * Parse command line options for the Open MPI Run Time Environment. This program MUST be called before
 * any call to ompi_rte_init_stage1 and/or ompi_rte_init_stage2 !!!
 *
 */
#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "mca/oob/base/base.h"

#include "util/output.h"
#include "util/cmd_line.h"
#include "util/sys_info.h"
#include "util/proc_info.h"

#include "runtime/runtime.h"

void ompi_rte_parse_cmd_line(ompi_cmd_line_t *cmd_line)
{
    char *universe, *nsreplica, *gprreplica, *tmp;


    /* get universe name and store it, if user specified it */
    /* otherwise, stick with default name */
    if (NULL != ompi_universe_info.name) {
	universe = strdup(ompi_universe_info.name); /* save the current value, if exists */
    }

    if (ompi_cmd_line_is_taken(cmd_line, "universe") ||
	ompi_cmd_line_is_taken(cmd_line, "u")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "universe", 0, 0)) {
	    ompi_output(0, "error retrieving universe name - please report error to bugs@open-mpi.org\n");
	    return;
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
    if (NULL != ompi_universe_info.name) {
	ompi_process_info.my_universe = strdup(ompi_universe_info.name);
    } else {  /* set it to default value */
	ompi_universe_info.name = strdup("default-universe");
	if (NULL != ompi_process_info.my_universe) { /* overwrite it */
	    free(ompi_process_info.my_universe);
	}
	ompi_process_info.my_universe = strdup(ompi_universe_info.name);
    }

    /* and set the appropriate enviro variable */
    setenv("OMPI_universe_name", ompi_universe_info.name, 1);

    /* get the temporary directory name for the session directory, if provided on command line */
    if (ompi_cmd_line_is_taken(cmd_line, "tmpdir")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0)) {
	    ompi_output(0, "error retrieving tmpdir name - please report error to bugs@open-mpi.org\n");
	    return;
	}
	if (NULL != ompi_process_info.tmpdir_base) { /* overwrite it */
	    free(ompi_process_info.tmpdir_base);
	}
	ompi_process_info.tmpdir_base = strdup(ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0));
	setenv("OMPI_tmpdir_base", ompi_process_info.tmpdir_base, 1);
    } /* otherwise, leave it alone */

    /* see if name server replica provided */
    if (ompi_cmd_line_is_taken(cmd_line, "nsreplica")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "nsreplica", 0, 0)) {
	    ompi_output(0, "error retrieving name server replica - please report error to bugs@open-mpi.org");
	    return;
	}
	nsreplica = strdup(ompi_cmd_line_get_param(cmd_line, "nsreplica", 0, 0));
	setenv("OMPI_MCA_ns_base_replica", nsreplica, 1);  /* set the ns_replica enviro variable */
    } /* otherwise, leave it alone */

    /* see if GPR replica provided */
    if (ompi_cmd_line_is_taken(cmd_line, "gprreplica")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "gprreplica", 0, 0)) {
	    ompi_output(0, "error retrieving GPR replica - please report error to bugs@open-mpi.org");
	    return;
	}
	gprreplica = strdup(ompi_cmd_line_get_param(cmd_line, "gprreplica", 0, 0));
	setenv("OMPI_MCA_gpr_base_replica", gprreplica, 1);  /* set the gpr_replica enviro variable */
    }  /* otherwise leave it alone */
}
