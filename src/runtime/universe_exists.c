/*
 * $HEADER$
 */

/**
 * @file
 *
 * Setup command line options for the Open MPI Run Time Environment
 */


#include "ompi_config.h"

#include <string.h>
#include <sys/time.h>

#include "include/constants.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/os_path.h"
#include "util/pack.h"
#include "util/session_dir.h"
#include "util/universe_setup_file_io.h"

#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"

#include "runtime/runtime.h"


static struct timeval ompi_rte_ping_wait = {2, 0};


int ompi_rte_universe_exists()
{
    char *contact_file;
    int ret, i;
    ompi_process_name_t proc={0,0,0};
    bool ns_found, gpr_found, ping_success;

    /* if both ns_replica and gpr_replica were provided, check for contact with them */
    if (NULL != ompi_universe_info.ns_replica && NULL != ompi_universe_info.gpr_replica) {
	mca_oob_parse_contact_info(ompi_universe_info.ns_replica, &proc, NULL);
	/* ping to verify ns_replica alive */
	if (OMPI_SUCCESS != mca_oob_ping(&proc, &ompi_rte_ping_wait)) {
	    if (ompi_rte_debug_flag) {
		ompi_output(0, "univ_exists: ns_replica ping failed");
	    }
	    free(ompi_universe_info.ns_replica);
	    if (NULL != ompi_process_info.ns_replica) {
		free(ompi_process_info.ns_replica);
	    }
	} else {  /* name server found, now try gpr */
	    ns_found = true;
	}

	mca_oob_parse_contact_info(ompi_universe_info.gpr_replica, &proc, NULL);
	/* ping to verify gpr_replica alive */
	if (OMPI_SUCCESS != mca_oob_ping(&proc, &ompi_rte_ping_wait)) {
	    if (ompi_rte_debug_flag) {
		ompi_output(0, "univ_exists: gpr_replica ping failed");
	    }
	    free(ompi_universe_info.gpr_replica);
	    if (NULL != ompi_process_info.gpr_replica) {
		free(ompi_process_info.gpr_replica);
	    }
	} else { 
	    gpr_found = true;
	}

	if (ns_found && gpr_found) { /* success on both counts - report it */
	    return OMPI_SUCCESS;
	}
    }

    /* if we are missing one or both, we need to get the missing info. first check
     * to see if seed_contact_info already provided. if so, then contact that daemon
     * to get missing info.
     */

    /* otherwise, need to find an initial "seed" contact point so we can get the info.
     * check if local or remote host specified
     */

    if (0 != strncmp(ompi_universe_info.host, ompi_system_info.nodename, strlen(ompi_system_info.nodename))) { /* remote host specified */
	ompi_output(0, "remote hosts not currently supported");
	return OMPI_ERR_NOT_IMPLEMENTED;
    }

    if (ompi_rte_debug_flag) {
	ompi_output(0, "looking for session directory");
    }

    /* check to see if local universe already exists */
    if (OMPI_SUCCESS == ompi_session_dir(false,
					 ompi_process_info.tmpdir_base,
					 ompi_system_info.user,
					 ompi_system_info.nodename,
					 NULL,
					 ompi_universe_info.name,
					 NULL,
					 NULL)) { /* found */

	if (ompi_rte_debug_flag) {
	    ompi_output(0, "check for contact info file");
	}

	/* check for "contact-info" file. if present, read it in. */
	contact_file = ompi_os_path(false, ompi_process_info.universe_session_dir,
				    "universe-setup.txt", NULL);

	if (OMPI_SUCCESS != (ret = ompi_read_universe_setup_file(contact_file))) {
	    if (ompi_rte_debug_flag) {
		ompi_output(0, "could not read contact file %s", contact_file);
	    }
	    return ret;
	}

	if (ompi_rte_debug_flag) {
	    ompi_output(0, "contact info read");
	}

	if (!ompi_universe_info.persistence ||   /* not persistent... */
	    (0 == strncmp(ompi_universe_info.scope, "exclusive", strlen("exclusive")))) {  /* ...or no connection allowed */
	    /* also need to check "local" and that we did not specify the exact
	     * matching universe name
	     */
	    if (ompi_rte_debug_flag) {
		ompi_output(0, "connection not allowed");
	    }
	    return OMPI_ERR_NO_CONNECTION_ALLOWED;
	}

    if (ompi_rte_debug_flag) {
	ompi_output(0, "contact info to set: %s", ompi_universe_info.seed_contact_info);
    }


	/* if persistent, set contact info... */
	if (OMPI_SUCCESS != mca_oob_set_contact_info(ompi_universe_info.seed_contact_info)) { /* set contact info */
	    if (ompi_rte_debug_flag) {
		ompi_output(0, "error setting oob contact info - please report error to bugs@open-mpi.org\n");
	    }
	    return OMPI_ERR_FATAL;
	}

	mca_oob_parse_contact_info(ompi_universe_info.seed_contact_info, &proc, NULL);

	if (ompi_rte_debug_flag) {
	    ompi_output(0, "contact info set: %s", ompi_universe_info.seed_contact_info);
	    ompi_output(0, "issuing ping: %d %d %d", proc.cellid, proc.jobid, proc.vpid);
	}


	/* ...and ping to verify it's alive */
	ping_success = false;
	for (i=0; i<5 && !ping_success; i++) {
	    ompi_output(0, "univ_exists: attempting ping number %d", i);
	    if (OMPI_SUCCESS == mca_oob_ping(&proc, &ompi_rte_ping_wait)) {
		ping_success = true;
	    }
	}
	if (!ping_success) {
	    if (ompi_rte_debug_flag) {
		ompi_output(0, "ping failed");
	    }
	    return OMPI_ERR_CONNECTION_FAILED;
	}

	/* set the my_universe field */
	ompi_process_info.my_universe = strdup(ompi_universe_info.name);
	ompi_process_info.ns_replica = ns_base_copy_process_name(&proc);
	ompi_process_info.gpr_replica = ns_base_copy_process_name(&proc);

	/* request ns_replica and gpr_replica info for this process
	 * only request info required - check ns_found/gpr_found
	 */

	return OMPI_SUCCESS;
    }

    return OMPI_ERR_NOT_FOUND;
}
