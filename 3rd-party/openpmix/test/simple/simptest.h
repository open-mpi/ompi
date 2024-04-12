/*
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool active;
    pmix_status_t status;
} mylock_t;

#define DEBUG_CONSTRUCT_LOCK(l)                \
    do {                                       \
        pthread_mutex_init(&(l)->mutex, NULL); \
        pthread_cond_init(&(l)->cond, NULL);   \
        (l)->active = true;                    \
        (l)->status = PMIX_SUCCESS;            \
    } while (0)

#define DEBUG_DESTRUCT_LOCK(l)              \
    do {                                    \
        pthread_mutex_destroy(&(l)->mutex); \
        pthread_cond_destroy(&(l)->cond);   \
    } while (0)

#define DEBUG_WAIT_THREAD(lck)                              \
    do {                                                    \
        pthread_mutex_lock(&(lck)->mutex);                  \
        while ((lck)->active) {                             \
            pthread_cond_wait(&(lck)->cond, &(lck)->mutex); \
        }                                                   \
        pthread_mutex_unlock(&(lck)->mutex);                \
    } while (0)

#define DEBUG_WAKEUP_THREAD(lck)              \
    do {                                      \
        pthread_mutex_lock(&(lck)->mutex);    \
        (lck)->active = false;                \
        pthread_cond_broadcast(&(lck)->cond); \
        pthread_mutex_unlock(&(lck)->mutex);  \
    } while (0)

#define SIMPTEST_THREADSHIFT(r, c)                                              \
    do {                                                                        \
        pmix_event_assign(&((r)->ev), simptest_evbase, -1, EV_WRITE, (c), (r)); \
        PMIX_POST_OBJECT((r));                                                  \
        pmix_event_active(&((r)->ev), EV_WRITE, 1);                             \
    } while (0)
