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
    universe = strdup(ompi_universe_info.name); /* save the default */
    if (ompi_cmd_line_is_taken(cmd_line, "universe") ||
	ompi_cmd_line_is_taken(cmd_line, "u")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "universe", 0, 0)) {
	    ompi_output(0, "error retrieving universe name - please report error to bugs@open-mpi.org\n");
	    return;
        }
        universe = strdup(ompi_cmd_line_get_param(cmd_line, "universe", 0, 0));
	ompi_output(0, "got universe name %s", universe);


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
    ompi_output(0, "my universe name is %s", ompi_process_info.my_universe);

    /* get the temporary directory name for the session directory, if provided on command line */
    if (ompi_cmd_line_is_taken(cmd_line, "tmpdir")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0)) {
	    ompi_output(0, "error retrieving tmpdir name - please report error to bugs@open-mpi.org\n");
	    return;
	}
	ompi_process_info.tmpdir_base = strdup(ompi_cmd_line_get_param(cmd_line, "tmpdir", 0, 0));
    } else {
	ompi_process_info.tmpdir_base = NULL;
    }

    /* see if name server replica provided */
    if (ompi_cmd_line_is_taken(cmd_line, "nsreplica")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "nsreplica", 0, 0)) {
	    ompi_output(0, "error retrieving name server replica - please report error to bugs@open-mpi.org");
	    return;
	}
	nsreplica = strdup(ompi_cmd_line_get_param(cmd_line, "nsreplica", 0, 0));
	mca_oob_parse_contact_info(nsreplica, ompi_process_info.ns_replica, NULL);
    } else {
	ompi_process_info.ns_replica = NULL;
    }

    /* see if GPR replica provided */
    if (ompi_cmd_line_is_taken(cmd_line, "gprreplica")) {
	if (NULL == ompi_cmd_line_get_param(cmd_line, "gprreplica", 0, 0)) {
	    ompi_output(0, "error retrieving GPR replica - please report error to bugs@open-mpi.org");
	    return;
	}
	gprreplica = strdup(ompi_cmd_line_get_param(cmd_line, "gprreplica", 0, 0));
	if (NULL != nsreplica &&
	    0 != strcmp(nsreplica, gprreplica)) { /* check to see if different */
	    mca_oob_set_contact_info(gprreplica);
	}
	mca_oob_parse_contact_info(gprreplica, ompi_process_info.gpr_replica, NULL);
    } else {
	ompi_process_info.gpr_replica = NULL;
    }
}
