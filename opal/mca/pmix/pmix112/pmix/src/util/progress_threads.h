/*
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PROGRESS_THREADS_H
#define PROGRESS_THREADS_H

#include <src/include/pmix_config.h>

#include <src/include/types.h>

#include PMIX_EVENT_HEADER

/* start a progress thread, assigning it the provided name for
 * tracking purposes. If create_block is true, then this function
 * will also create a pipe so that libevent has something to block
 * against, thus keeping the thread from free-running
 */
PMIX_EXPORT pmix_event_base_t* pmix_start_progress_thread(void);

/* stop the progress thread of the provided name. This function will
 * also cleanup the blocking pipes and release the event base if
 * the cleanup param is true */
PMIX_EXPORT void pmix_stop_progress_thread(pmix_event_base_t *ev_base);

#endif
