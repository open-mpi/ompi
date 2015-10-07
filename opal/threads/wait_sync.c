/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "wait_sync.h"

static opal_mutex_t wait_sync_lock = OPAL_MUTEX_STATIC_INIT;
static ompi_wait_sync_t* wait_sync_list = NULL;

#define WAIT_SYNC_PASS_OWNERSHIP(who)                  \
    do {                                               \
        pthread_mutex_lock( &(who)->lock);             \
        pthread_cond_signal( &(who)->condition );      \
        pthread_mutex_unlock( &(who)->lock);           \
    } while(0)


int sync_wait_st(ompi_wait_sync_t *sync)
{
    while(sync->count > 0) {
        opal_progress();
    }
    return OPAL_SUCCESS;
}

int sync_wait_mt(ompi_wait_sync_t *sync)
{
    if(sync->count <= 0)
        return OPAL_SUCCESS;

    /* lock so nobody can signal us during the list updating */
    pthread_mutex_lock(&sync->lock);

    /* Insert sync to the list */
    OPAL_THREAD_LOCK(&wait_sync_lock);
    if( NULL == wait_sync_list ) {
        sync->next = sync->prev = sync;
        wait_sync_list = sync;
    } else {
        sync->prev = wait_sync_list->prev;
        sync->prev->next = sync;
        sync->next = wait_sync_list;
        wait_sync_list->prev = sync;
    }
    OPAL_THREAD_UNLOCK(&wait_sync_lock);

    /**
     * If we are not responsible for progresing, let's go silent until something worth noticing happen:
     *  - this thread has been promoted to take care of the progress
     *  - our sync has been triggered.
     */
    if( sync != wait_sync_list ) {
        pthread_cond_wait(&sync->condition, &sync->lock);

        /**
         * At this point either the sync was completed in which case we should remove it from the wait
         * list, or/and I was promoted as the progress manager.
         */

        if( sync->count <= 0 ) {  /* Completed? */
            pthread_mutex_unlock(&sync->lock);
            goto i_am_done;
        }
        /* promoted ! */
        assert(sync == wait_sync_list);
    }

    pthread_mutex_unlock(&sync->lock);
    while(sync->count > 0) {  /* progress till completion */
        opal_progress();  /* don't progress with the sync lock locked or you'll deadlock */
    }

    assert(sync == wait_sync_list);
 i_am_done:
    /* My sync is now complete. Trim the list: remove self, wake next */
    OPAL_THREAD_LOCK(&wait_sync_lock);
    sync->prev->next = sync->next;
    sync->next->prev = sync->prev;
    /* In case I am the progress manager, pass the duties on */
    if( sync == wait_sync_list ) {
        wait_sync_list = (sync == sync->next) ? NULL : sync->next;
        if( NULL != wait_sync_list )
            WAIT_SYNC_PASS_OWNERSHIP(wait_sync_list);
    }
    OPAL_THREAD_UNLOCK(&wait_sync_lock);

    return OPAL_SUCCESS;
}
