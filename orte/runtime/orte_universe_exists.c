/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
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
 * Setup command line options for the Open MPI Run Time Environment
 */


#include "orte_config.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "include/orte_constants.h"
#include "opal/util/output.h"
#include "util/univ_info.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/os_path.h"
#include "util/session_dir.h"
#include "util/universe_setup_file_io.h"

#include "mca/rml/rml.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "runtime/runtime.h"


static struct timeval ompi_rte_ping_wait = {2, 0};


int orte_universe_exists(orte_universe_t *univ)
{
    char *contact_file;
    int ret;

    /* check to see if local universe session directory already exists */
    if (ORTE_SUCCESS != orte_session_dir(false,
					 orte_process_info.tmpdir_base,
					 orte_system_info.user,
					 orte_system_info.nodename,
					 NULL,
					 orte_universe_info.name,
					 NULL,
					 NULL)) { /* not found */
        /* NOTE: NOT FINDING THE DIRECTORY IS NOT AN ERROR - DON'T ERROR_LOG IT */
        return ORTE_ERR_NOT_FOUND;
    }

	/* check for "contact-info" file. if present, read it in. */
	if (NULL == (contact_file = orte_os_path(false, orte_process_info.universe_session_dir,
				    "universe-setup.txt", NULL))) {
        /* NOTE: NOT FINDING THE FILE IS NOT AN ERROR - DON'T ERROR_LOG IT */
        return ORTE_ERR_NOT_FOUND;
    }

	if (ORTE_SUCCESS != (ret = orte_read_universe_setup_file(contact_file, univ))) {
        /* NOTE: THIS IS NOT AN ERROR - DON'T ERROR_LOG IT */
        free(contact_file);
	    return ret;
	}

	if (orte_debug_flag) {
	    opal_output(0, "connect_uni: contact info read");
	}

	if (!orte_universe_info.console) {  /* if we aren't trying to connect a console */
	    if (!univ->persistence ||   /* if the target universe is not persistent... */
		(0 == strncmp(univ->scope, "exclusive", strlen("exclusive")))) {  /* ...or no connection allowed */
		/* also need to check "local" and that we did not specify the exact
		 * matching universe name
		 */
		if (orte_debug_flag) {
		    opal_output(0, "connect_uni: connection not allowed");
		}
        /* NOTE: THIS IS NOT AN ERROR - DON'T ERROR_LOG IT */
		return ORTE_ERR_NO_CONNECTION_ALLOWED;
	    }
	}

	if (orte_debug_flag) {
	    opal_output(0, "connect_uni: contact info to set: %s", univ->seed_uri);
	}


	/* if persistent, ping to verify it's alive */
	if (ORTE_SUCCESS != orte_rml.ping(univ->seed_uri, &ompi_rte_ping_wait)) {
        if (orte_debug_flag) {
            ORTE_ERROR_LOG(ORTE_ERR_CONNECTION_FAILED);
        }
	    return ORTE_ERR_CONNECTION_FAILED;
	}

	return ORTE_SUCCESS;
}
