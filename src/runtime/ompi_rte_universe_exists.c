/*
 * $HEADER$
 */

/**
 * @file
 *
 * Setup command line options for the Open MPI Run Time Environment
 */


#include "ompi_config.h"

#include "include/constants.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/os_path.h"
#include "util/pack.h"
#include "util/session_dir.h"
#include "util/universe_setup_file_io.h"

#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"

#include "runtime/runtime.h"

#define OMPI_DAEMON_OOB_PACK_CMD OMPI_INT16

#define OMPI_DAEMON_INITIAL_CONTACT_CMD 0x01
#define OMPI_DAEMON_CONTACT_ACK_CMD     0x02


int ompi_rte_universe_exists(char *host, char *name, char *tmpdir, char *oob_contact_info)
{
    char *contact_file;
    int32_t command, recv_tag;
    int ret;
    ompi_buffer_t msg, response;
    ompi_process_name_t seed={0,0,0};

    /* does universe already exist on specified host? Check session directory to see */
    /* don't know how to handle remote host yet - only cover localhost */

    if (0 != strncmp(host, ompi_system_info.nodename, strlen(ompi_system_info.nodename))) { /* remote host specified */
	fprintf(stderr, "remote hosts not currently supported\n");
	return OMPI_ERR_NOT_IMPLEMENTED;
    }

    /* check to see if local universe already exists */
    if (OMPI_SUCCESS == ompi_session_dir(false, tmpdir, ompi_system_info.user, ompi_system_info.nodename, NULL,
					 name, NULL, NULL)) { /* found */
	/* check for "contact-info" file. if present, read it in. */
	contact_file = ompi_os_path(false, ompi_process_info.universe_session_dir,
				    "universe-setup.txt", NULL);

	if (OMPI_SUCCESS != (ret = ompi_read_universe_setup_file(contact_file))) {
	    return ret;
	}

	if (!ompi_universe_info.persistence ||   /* not persistent... */
	    (0 == strncmp(ompi_universe_info.scope, "local", strlen("local")))) {  /* ...or no connection allowed */
	    return OMPI_ERR_NO_CONNECTION_ALLOWED;
	}

	/* if persistent, use contact info to connect */
	if (OMPI_SUCCESS != mca_oob_set_contact_info(ompi_universe_info.oob_contact_info)) { /* set contact info */
	    fprintf(stderr, "error setting oob contact info - please report error to bugs@open-mpi.org\n");
	    return OMPI_ERR_FATAL;
	}

	command = OMPI_DAEMON_INITIAL_CONTACT_CMD;
	recv_tag = MCA_OOB_TAG_DAEMON;

	if (OMPI_SUCCESS != ompi_buffer_init(&msg, 0) ||
	    OMPI_SUCCESS != ompi_buffer_init(&response, 0)) { /* can't get a message buffer */
	    fprintf(stderr, "can't get message buffer - please report error to bugs@open-mpi.org\n");
	    return OMPI_ERR_FATAL;
	}

	ompi_pack(msg, &command, 1, OMPI_DAEMON_OOB_PACK_CMD);
	if (0 > mca_oob_send_packed(&seed, msg, MCA_OOB_TAG_DAEMON, 0)) {
	    /* failure means contact couldn't be made - probably universe has failed,
	     * but could be that it just isn't available for some reason like swapping,
	     * timeout for other reasons, etc. regardless, must start new universe */
	    /* for safety, will restart with "unique"-ified name */
	    return OMPI_ERR_CONNECTION_FAILED;
	}

	if (0 > mca_oob_recv_packed(&seed, &response, &recv_tag)) {
	    fprintf(stderr, "garbled response\n");
	}

	ompi_unpack(response, &command, 1, OMPI_DAEMON_OOB_PACK_CMD);
	if (OMPI_DAEMON_CONTACT_ACK_CMD != command) {
	    /* universe refuses to acknowledge contact - probably local scope
	     * so need to start our own universe instead */
	    return OMPI_ERR_CONNECTION_REFUSED;
	}

    }
}
