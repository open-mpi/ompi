/* -*- C -*-
 * 
 * $HEADER$
 *
 */
/** @file 
 */

#include "ompi_config.h"

#include "gpr_proxy.h"


void mca_gpr_proxy_deliver_notify_msg(ompi_registry_notify_action_t state,
				      ompi_registry_notify_message_t *message)
{
    int namelen;
    mca_gpr_proxy_notify_request_tracker_t *trackptr;

    if (mca_gpr_proxy_debug) {
	if (OMPI_REGISTRY_NOTIFY_ON_STARTUP == state) {
	ompi_output(0, "[%d,%d,%d] special delivery of startup msg",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()));
	} else {
	ompi_output(0, "[%d,%d,%d] special delivery of shutdown msg",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()));
	}
    }

    /* protect system from threadlock */
    if ((OMPI_REGISTRY_NOTIFY_ON_STARTUP & state) ||
	(OMPI_REGISTRY_NOTIFY_ON_SHUTDOWN & state)) {

	OMPI_THREAD_LOCK(&mca_gpr_proxy_mutex);

	namelen = strlen(message->segment);

	/* find the request corresponding to this notify */
	for (trackptr = (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_proxy_notify_request_tracker);
	     trackptr != (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_proxy_notify_request_tracker);
	     trackptr = (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_next(trackptr)) {
	    if ((trackptr->action & state) &&
		(0 == strcmp(message->segment, trackptr->segment))) {
		OMPI_THREAD_UNLOCK(&mca_gpr_proxy_mutex);
		/* process request - callback function responsible for releasing memory */
		trackptr->callback(message, trackptr->user_tag);
		return;
	    }
	}
    }
    OBJ_RELEASE(message);
}
