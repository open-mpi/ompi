/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2022 IBM Corporation. All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2021      Argonne National Laboratory.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal/mca/threads/wait_sync.h"

static opal_mutex_t wait_sync_lock = OPAL_MUTEX_STATIC_INIT;
ompi_wait_sync_t *opal_threads_base_wait_sync_list = NULL; /* not static for inline "wait_sync_st" */

void opal_threads_base_wait_sync_global_wakeup_st(int status)
{
    ompi_wait_sync_t *sync;
    for (sync = opal_threads_base_wait_sync_list; sync != NULL; sync = sync->next) {
        wait_sync_update(sync, 0, status);
    }
}

void opal_threads_base_wait_sync_global_wakeup_mt(int status)
{
    ompi_wait_sync_t *sync;
    opal_mutex_lock(&wait_sync_lock);
    for (sync = opal_threads_base_wait_sync_list; sync != NULL; sync = sync->next) {
        /* sync_update is going to  take the sync->lock from within
         * the wait_sync_lock. Thread lightly here: Idealy we should
         * find a way to not take a lock in a lock as this is deadlock prone,
         * but as of today we are the only place doing this so it is safe.
         */
        wait_sync_update(sync, 0, status);
        if (sync->next == opal_threads_base_wait_sync_list) {
            break; /* special case for rings */
        }
    }
    opal_mutex_unlock(&wait_sync_lock);
}

static opal_atomic_int32_t num_thread_in_progress = 0;

#define WAIT_SYNC_PASS_OWNERSHIP(who)                        \
    do {                                                     \
        opal_thread_internal_mutex_lock(&(who)->lock);       \
        opal_thread_internal_cond_signal(&(who)->condition); \
        opal_thread_internal_mutex_unlock(&(who)->lock);     \
    } while (0)

int ompi_sync_wait_mt(ompi_wait_sync_t *sync)
{
    /* Don't stop if the waiting synchronization is completed. We avoid the
     * race condition around the release of the synchronization using the
     * signaling field.
     */
    if (sync->count <= 0) {
        return (0 == sync->status) ? OPAL_SUCCESS : OPAL_ERROR;
    }

    /* lock so nobody can signal us during the list updating */
    opal_thread_internal_mutex_lock(&sync->lock);

    /* Now that we hold the lock make sure another thread has not already
     * call cond_signal.
     */
    if (sync->count <= 0) {
        opal_thread_internal_mutex_unlock(&sync->lock);
        return (0 == sync->status) ? OPAL_SUCCESS : OPAL_ERROR;
    }

    /* Insert sync on the list of pending synchronization constructs */
    OPAL_THREAD_LOCK(&wait_sync_lock);
    if (NULL == opal_threads_base_wait_sync_list) {
        sync->next = sync->prev = sync;
        opal_threads_base_wait_sync_list = sync;
    } else {
        sync->prev = opal_threads_base_wait_sync_list->prev;
        sync->prev->next = sync;
        sync->next = opal_threads_base_wait_sync_list;
        opal_threads_base_wait_sync_list->prev = sync;
    }
    OPAL_THREAD_UNLOCK(&wait_sync_lock);

    /**
     * If we are not responsible for progressing, go silent until something
     * worth noticing happen:
     *  - this thread has been promoted to take care of the progress
     *  - our sync has been triggered.
     */
check_status:
    if (sync != opal_threads_base_wait_sync_list && num_thread_in_progress >= opal_max_thread_in_progress) {
        opal_thread_internal_cond_wait(&sync->condition, &sync->lock);

        /**
         * At this point either the sync was completed in which case
         * we should remove it from the wait list, or/and I was
         * promoted as the progress manager.
         */

        if (sync->count <= 0) { /* Completed? */
            opal_thread_internal_mutex_unlock(&sync->lock);
            goto i_am_done;
        }
        /* either promoted, or spurious wakeup ! */
        goto check_status;
    }
    opal_thread_internal_mutex_unlock(&sync->lock);

    OPAL_THREAD_ADD_FETCH32(&num_thread_in_progress, 1);
    while (sync->count > 0) { /* progress till completion */
        /* don't progress with the sync lock locked or you'll deadlock */
        opal_progress();
    }
    OPAL_THREAD_ADD_FETCH32(&num_thread_in_progress, -1);

i_am_done:
    /* My sync is now complete. Trim the list: remove self, wake next */
    OPAL_THREAD_LOCK(&wait_sync_lock);
    sync->prev->next = sync->next;
    sync->next->prev = sync->prev;
    /* In case I am the progress manager, pass the duties on */
    if (sync == opal_threads_base_wait_sync_list) {
        opal_threads_base_wait_sync_list = (sync == sync->next) ? NULL : sync->next;
        if (NULL != opal_threads_base_wait_sync_list) {
            WAIT_SYNC_PASS_OWNERSHIP(opal_threads_base_wait_sync_list);
        }
    }
    OPAL_THREAD_UNLOCK(&wait_sync_lock);

    return (0 == sync->status) ? OPAL_SUCCESS : OPAL_ERROR;
}
