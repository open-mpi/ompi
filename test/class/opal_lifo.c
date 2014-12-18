/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <assert.h>

#include "support.h"
#include "opal/class/opal_lifo.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"

#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>

#include <sys/time.h>

#define OPAL_LIFO_TEST_THREAD_COUNT 8
#define ITERATIONS 1000000
#define ITEM_COUNT 100

#if !defined(timersub)
#define timersub(a, b, r) \
    do {                  \
        (r)->tv_sec = (a)->tv_sec - (b)->tv_sec;        \
        if ((a)->tv_usec < (b)->tv_usec) {              \
            (r)->tv_sec--;                              \
            (a)->tv_usec += 1000000;                    \
        }                                               \
        (r)->tv_usec = (a)->tv_usec - (b)->tv_usec;     \
    } while (0)
#endif

static void *thread_test (void *arg) {
    opal_lifo_t *lifo = (opal_lifo_t *) arg;
    opal_list_item_t *item;
    struct timeval start, stop, total;
    double timing;

    gettimeofday (&start, NULL);
    for (int i = 0 ; i < ITERATIONS ; ++i) {
        item = opal_lifo_pop_atomic (lifo);
        if (NULL != item) {
            (void) opal_lifo_push_atomic (lifo, item);
        }
    }
    gettimeofday (&stop, NULL);

    timersub(&stop, &start, &total);

    timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) ITERATIONS;

    printf ("Atomics thread finished. Time: %d s %d us %d nsec/poppush\n", (int) total.tv_sec,
            (int)total.tv_usec, (int)(timing / 1e-9));

    return NULL;
}

static bool check_lifo_consistency (opal_lifo_t *lifo, int expected_count)
{
    opal_list_item_t *item;
    int count;

    for (count = 0, item = lifo->opal_lifo_head.data.item ; item != &lifo->opal_lifo_ghost ;
         item = opal_list_get_next(item), count++);

    return count == expected_count;
}

int main (int argc, char *argv[]) {
    pthread_t threads[OPAL_LIFO_TEST_THREAD_COUNT];
    opal_list_item_t *item, *prev, *item2;
    struct timeval start, stop, total;
    opal_lifo_t lifo;
    bool success;
    double timing;
    int rc;

    rc = opal_init_util (&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit (1);
    }

    test_init("opal_lifo_t");

    OBJ_CONSTRUCT(&lifo, opal_lifo_t);

    item = OBJ_NEW(opal_list_item_t);
    prev = opal_lifo_push_st (&lifo, item);
    if (&lifo.opal_lifo_ghost == prev) {
        test_success ();
    } else {
        test_failure (" opal_lifo_push_st on empty lifo");
    }

    item2 = opal_lifo_pop_st (&lifo);
    if (item == item2) {
        test_success ();
    } else {
        test_failure (" opal_lifo_pop_st");
    }

    OBJ_RELEASE(item);

    for (int i = 0 ; i < ITEM_COUNT ; ++i) {
        item = OBJ_NEW(opal_list_item_t);
        item->item_free = 0;
        opal_lifo_push_st (&lifo, item);
    }

    if (check_lifo_consistency (&lifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" opal_lifo_push_st(multiple items)");
    }

    gettimeofday (&start, NULL);
    for (int i = 0 ; i < ITERATIONS ; ++i) {
        item = opal_lifo_pop_st (&lifo);
        (void) opal_lifo_push_st (&lifo, item);
    }
    gettimeofday (&stop, NULL);

    timersub(&stop, &start, &total);

    timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) ITERATIONS;

    if (check_lifo_consistency (&lifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" lifo push/pop");
    }

    printf ("Single thread test. Time: %d s %d us %d nsec/poppush\n", (int) total.tv_sec,
            (int)total.tv_usec, (int)(timing / 1e-9));

    thread_test (&lifo);

    if (check_lifo_consistency (&lifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" lifo push/pop single-threaded with atomics");
    }

    gettimeofday (&start, NULL);
    for (int i = 0 ; i < OPAL_LIFO_TEST_THREAD_COUNT ; ++i) {
        pthread_create (threads + i, NULL, thread_test, &lifo);
    }

    for (int i = 0 ; i < OPAL_LIFO_TEST_THREAD_COUNT ; ++i) {
        void *ret;

        pthread_join (threads[i], &ret);
    }
    gettimeofday (&stop, NULL);

    timersub(&stop, &start, &total);

    timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) (ITERATIONS * OPAL_LIFO_TEST_THREAD_COUNT);

    if (check_lifo_consistency (&lifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" lifo push/pop multi-threaded with atomics");
    }

    printf ("All threads finished. Thread count: %d Time: %d s %d us %d nsec/poppush\n",
            OPAL_LIFO_TEST_THREAD_COUNT, (int) total.tv_sec, (int)total.tv_usec, (int)(timing / 1e-9));

    success = true;
    for (int i = 0 ; i < ITEM_COUNT ; ++i) {
        item = opal_lifo_pop_st (&lifo);
        if (NULL == item) {
            success = false;
            break;
        }
        OBJ_RELEASE(item);
    }

    if (success) {
        test_success ();
    } else {
        test_failure (" list pop all items");
    }

    OBJ_DESTRUCT(&lifo);

    opal_finalize_util ();

    return test_finalize ();
}
