/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PROGRESS_THREADS_H
#define PROGRESS_THREADS_H

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>

#include PMIX_EVENT_HEADER

/* start a progress thread, assigning it the provided name for
 * tracking purposes. If create_block is true, then this function
 * will also create a pipe so that libevent has something to block
 * against, thus keeping the thread from free-running
 */
PMIX_DECLSPEC pmix_event_base_t* pmix_start_progress_thread(void);

/* stop the progress thread of the provided name. This function will
 * also cleanup the blocking pipes and release the event base if
 * the cleanup param is true */
PMIX_DECLSPEC void pmix_stop_progress_thread(pmix_event_base_t *ev_base);

#endif
