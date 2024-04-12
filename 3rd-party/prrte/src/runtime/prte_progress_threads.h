/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_PROGRESS_THREADS_H
#define PRTE_PROGRESS_THREADS_H

#include "prte_config.h"

#include "src/event/event-internal.h"

/**
 * Initialize a progress thread name; if a progress thread is not
 * already associated with that name, start a progress thread.
 *
 * If you have general events that need to run in *a* progress thread
 * (but not necessarily a your own, dedicated progress thread), pass
 * NULL the "name" argument to the prte_progress_thead_init() function
 * to glom on to the general PRTE-wide progress thread.
 *
 * If a name is passed that was already used in a prior call to
 * prte_progress_thread_init(), the event base associated with that
 * already-running progress thread will be returned (i.e., no new
 * progress thread will be started).
 */
PRTE_EXPORT prte_event_base_t *prte_progress_thread_init(const char *name);

/**
 * Finalize a progress thread name (reference counted).
 *
 * Once this function is invoked as many times as
 * prte_progress_thread_init() was invoked on this name (or NULL), the
 * progress function is shut down and the event base associated with
 * it is destroyed.
 *
 * Will return PRTE_ERR_NOT_FOUND if the progress thread name does not
 * exist; PRTE_SUCCESS otherwise.
 */
PRTE_EXPORT int prte_progress_thread_finalize(const char *name);

/**
 * Temporarily pause the progress thread associated with this name.
 *
 * This function does not destroy the event base associated with this
 * progress thread name, but it does stop processing all events on
 * that event base until prte_progress_thread_resume() is invoked on
 * that name.
 *
 * Will return PRTE_ERR_NOT_FOUND if the progress thread name does not
 * exist; PRTE_SUCCESS otherwise.
 */
PRTE_EXPORT int prte_progress_thread_pause(const char *name);

/**
 * Restart a previously-paused progress thread associated with this
 * name.
 *
 * Will return PRTE_ERR_NOT_FOUND if the progress thread name does not
 * exist; PRTE_SUCCESS otherwise.
 */
PRTE_EXPORT int prte_progress_thread_resume(const char *name);

#endif
