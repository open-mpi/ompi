/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
/** @file 
 */

#include "ompi_config.h"

#include "gpr_replica.h"


void mca_gpr_replica_deliver_notify_msg(ompi_registry_notify_action_t state,
					ompi_registry_notify_message_t *message)
{
    int namelen;
    mca_gpr_replica_notify_request_tracker_t *trackptr;
    mca_gpr_replica_segment_t *seg;

	/* don't deliver messages with zero data */
	if (0 < ompi_list_get_size(&message->data)) {
	    /* protect system from threadlock */
	    if ((OMPI_REGISTRY_NOTIFY_ON_STARTUP & state) ||
		(OMPI_REGISTRY_NOTIFY_ON_SHUTDOWN & state)) {
	
			OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
		
			namelen = strlen(message->segment);
		
			/* find the request corresponding to this notify */
			for (trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_replica_notify_request_tracker);
			     trackptr != (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker);
			     trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_next(trackptr)) {
			    seg = trackptr->segptr;
			    if ((trackptr->action & state) &&
				(0 == strncmp(message->segment, seg->name, namelen))) {
                      OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
                      /* process request - callback function responsible for releasing memory */
			         trackptr->callback(message, trackptr->user_tag);
            			 return;
			    }
			}
		
			OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	    }
	}
	mca_gpr_base_release_notify_msg(message);

}
