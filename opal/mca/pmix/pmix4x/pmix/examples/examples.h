/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>

#include <pmix_common.h>

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool active;
    pmix_status_t status;
    int count;
    size_t evhandler_ref;
} mylock_t;

#define DEBUG_CONSTRUCT_LOCK(l)                     \
    do {                                            \
        pthread_mutex_init(&(l)->mutex, NULL);      \
        pthread_cond_init(&(l)->cond, NULL);        \
        (l)->active = true;                         \
        (l)->status = PMIX_SUCCESS;                 \
        (l)->count = 0;                             \
        (l)->evhandler_ref = 0;                     \
    } while(0)

#define DEBUG_DESTRUCT_LOCK(l)              \
    do {                                    \
        pthread_mutex_destroy(&(l)->mutex); \
        pthread_cond_destroy(&(l)->cond);   \
    } while(0)

#define DEBUG_WAIT_THREAD(lck)                                      \
    do {                                                            \
        pthread_mutex_lock(&(lck)->mutex);                          \
        while ((lck)->active) {                                     \
            pthread_cond_wait(&(lck)->cond, &(lck)->mutex);         \
        }                                                           \
        pthread_mutex_unlock(&(lck)->mutex);                        \
    } while(0)

#define DEBUG_WAKEUP_THREAD(lck)                        \
    do {                                                \
        pthread_mutex_lock(&(lck)->mutex);              \
        (lck)->active = false;                          \
        pthread_cond_broadcast(&(lck)->cond);           \
        pthread_mutex_unlock(&(lck)->mutex);            \
    } while(0)

/* define a structure for collecting returned
 * info from a query */
typedef struct {
    mylock_t lock;
    pmix_info_t *info;
    size_t ninfo;
} myquery_data_t;

#define DEBUG_CONSTRUCT_MYQUERY(q)                  \
    do {                                            \
        DEBUG_CONSTRUCT_LOCK(&((q)->lock));         \
        (q)->info = NULL;                           \
        (q)->ninfo = 0;                             \
    } while(0)

#define DEBUG_DESTRUCT_MYQUERY(q)                   \
    do {                                            \
        DEBUG_DESTRUCT_LOCK(&((q)->lock));          \
        if (NULL != (q)->info) {                    \
            PMIX_INFO_FREE((q)->info, (q)->ninfo);  \
        }                                           \
    } while(0)

/* define a structure for releasing when a given
 * nspace terminates */
typedef struct {
    mylock_t lock;
    char *nspace;
    int exit_code;
    bool exit_code_given;
} myrel_t;


#define DEBUG_CONSTRUCT_MYREL(r)                \
    do {                                        \
        DEBUG_CONSTRUCT_LOCK(&((r)->lock));     \
        (r)->nspace = NULL;                     \
        (r)->exit_code = 0;                     \
        (r)->exit_code_given = false;           \
    } while(0)

#define DEBUG_DESTRUCT_MYREL(r)                 \
    do {                                        \
        DEBUG_DESTRUCT_LOCK(&((r)->lock));      \
        if (NULL != (r)->nspace) {              \
            free((r)->nspace);                  \
        }                                       \
    } while(0)
