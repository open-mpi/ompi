/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_PROGRESS_THREADS_H
#define OPAL_PROGRESS_THREADS_H

#include "opal_config.h"

#include "opal/mca/event/event.h"

/* start a progress thread, assigning it the provided name for
 * tracking purposes. If create_block is true, then this function
 * will also create a pipe so that libevent has something to block
 * against, thus keeping the thread from free-running
 */
OPAL_DECLSPEC opal_event_base_t *opal_start_progress_thread(char *name,
                                                            bool create_block);

/* stop the progress thread of the provided name. This function will
 * also cleanup the blocking pipes and release the event base if
 * the cleanup param is true */
OPAL_DECLSPEC void opal_stop_progress_thread(char *name, bool cleanup);

/* restart the progress thread of the provided name */
OPAL_DECLSPEC int opal_restart_progress_thread(char *name);

#endif
