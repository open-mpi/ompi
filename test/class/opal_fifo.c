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
#include "opal/class/opal_fifo.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"

#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>

#include <sys/time.h>

#define OPAL_FIFO_TEST_THREAD_COUNT 8
#define ITERATIONS 1000000
#define ITEM_COUNT 100
#define ITEMS_PER_LOOP 30

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
    opal_fifo_t *fifo = (opal_fifo_t *) arg;
    opal_list_item_t *item;
    struct timeval start, stop, total;
    double timing;

    gettimeofday (&start, NULL);
    for (int i = 0 ; i < ITERATIONS ; ++i) {
        item = opal_fifo_pop_atomic (fifo);
        if (NULL != item) {
            (void) opal_fifo_push_atomic (fifo, item);
        }
    }
    gettimeofday (&stop, NULL);

    timersub(&stop, &start, &total);

    timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) ITERATIONS;

    printf ("Atomics thread finished. Time: %d s %d us %d nsec/poppush\n", (int) total.tv_sec,
            (int)total.tv_usec, (int)(timing / 1e-9));

    return NULL;
}

static void *thread_test_exhaust (void *arg) {
  opal_fifo_t *fifo = (opal_fifo_t *) arg;
  opal_list_item_t *items[ITEMS_PER_LOOP];
  struct timeval start, stop, total;
  int item_count = 0;
  double timing;

  gettimeofday (&start, NULL);

  for (int i = 0 ; i < ITERATIONS ; i += ITEMS_PER_LOOP) {
    for (int j = 0 ; j < ITEMS_PER_LOOP ; ++j) {
      items[j] = opal_fifo_pop_atomic (fifo);
      if (items[j]) {
	++item_count;
      }
    }

    for (int j = 0 ; j < ITEMS_PER_LOOP ; ++j) {
      if (items[j]) {
	(void) opal_fifo_push_atomic (fifo, items[j]);
      }
    }
  }

  gettimeofday (&stop, NULL);

  timersub(&stop, &start, &total);

  timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) item_count;

  fprintf (stderr, "Exhaustive atomics thread finished. Popped %d items. Time: %d s %d us %d nsec/poppush\n", item_count,
	   (int) total.tv_sec, (int)total.tv_usec, (int)(timing / 1e-9));

  return NULL;
}

static bool check_fifo_consistency (opal_fifo_t *fifo, int expected_count)
{
    opal_list_item_t *item;
    int count;

    for (count = 0, item = fifo->opal_fifo_head.data.item ; item != &fifo->opal_fifo_ghost ;
         item = opal_list_get_next(item), count++);

    return count == expected_count;
}

int main (int argc, char *argv[]) {
    pthread_t threads[OPAL_FIFO_TEST_THREAD_COUNT];
    opal_list_item_t *item, *prev, *item2;
    struct timeval start, stop, total;
    opal_fifo_t fifo;
    bool success;
    double timing;
    int rc;

    rc = opal_init_util (&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit (1);
    }

    test_init("opal_fifo_t");

    OBJ_CONSTRUCT(&fifo, opal_fifo_t);

    item = OBJ_NEW(opal_list_item_t);
    prev = opal_fifo_push_st (&fifo, item);
    if (&fifo.opal_fifo_ghost == prev) {
        test_success ();
    } else {
        test_failure (" opal_fifo_push_st on empty fifo");
    }

    item2 = opal_fifo_pop_st (&fifo);
    if (item == item2) {
        test_success ();
    } else {
        test_failure (" opal_fifo_pop_st");
    }

    OBJ_RELEASE(item);

    for (int i = 0 ; i < ITEM_COUNT ; ++i) {
        item = OBJ_NEW(opal_list_item_t);
        item->item_free = 0;
        opal_fifo_push_st (&fifo, item);
    }

    if (check_fifo_consistency (&fifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" opal_fifo_push_st(multiple items)");
    }

    gettimeofday (&start, NULL);
    for (int i = 0 ; i < ITERATIONS ; ++i) {
        item = opal_fifo_pop_st (&fifo);
        (void) opal_fifo_push_st (&fifo, item);
    }
    gettimeofday (&stop, NULL);

    timersub(&stop, &start, &total);

    timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) ITERATIONS;

    if (check_fifo_consistency (&fifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" fifo push/pop");
    }

    printf ("Single thread test. Time: %d s %d us %d nsec/poppush\n", (int) total.tv_sec,
            (int)total.tv_usec, (int)(timing / 1e-9));

    thread_test (&fifo);

    if (check_fifo_consistency (&fifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" fifo push/pop single-threaded with atomics");
    }

    gettimeofday (&start, NULL);
    for (int i = 0 ; i < OPAL_FIFO_TEST_THREAD_COUNT ; ++i) {
        pthread_create (threads + i, NULL, thread_test, &fifo);
    }

    for (int i = 0 ; i < OPAL_FIFO_TEST_THREAD_COUNT ; ++i) {
        void *ret;

        pthread_join (threads[i], &ret);
    }
    gettimeofday (&stop, NULL);

    timersub(&stop, &start, &total);

    timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) (ITERATIONS * OPAL_FIFO_TEST_THREAD_COUNT);

    if (check_fifo_consistency (&fifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" fifo push/pop multi-threaded with atomics");
    }

    printf ("All threads finished. Thread count: %d Time: %d s %d us %d nsec/poppush\n",
            OPAL_FIFO_TEST_THREAD_COUNT, (int) total.tv_sec, (int)total.tv_usec, (int)(timing / 1e-9));


    gettimeofday (&start, NULL);
    for (int i = 0 ; i < OPAL_FIFO_TEST_THREAD_COUNT ; ++i) {
        pthread_create (threads + i, NULL, thread_test_exhaust, &fifo);
    }

    for (int i = 0 ; i < OPAL_FIFO_TEST_THREAD_COUNT ; ++i) {
        void *ret;

        pthread_join (threads[i], &ret);
    }
    gettimeofday (&stop, NULL);

    timersub(&stop, &start, &total);

    timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) (ITERATIONS * OPAL_FIFO_TEST_THREAD_COUNT);

    if (check_fifo_consistency (&fifo, ITEM_COUNT)) {
        test_success ();
    } else {
        test_failure (" fifo push/pop multi-threaded with atomics when there are insufficient items");
    }

    printf ("All threads finished. Thread count: %d Time: %d s %d us %d nsec/poppush\n",
            OPAL_FIFO_TEST_THREAD_COUNT, (int) total.tv_sec, (int)total.tv_usec, (int)(timing / 1e-9));

    success = true;
    for (int i = 0 ; i < ITEM_COUNT ; ++i) {
        item = opal_fifo_pop_st (&fifo);
        if (NULL == item) {
            success = false;
            break;
        }
        OBJ_RELEASE(item);
    }

    if (success) {
        test_success ();
    } else {
        test_failure (" fifo pop all items");
    }

    OBJ_DESTRUCT(&fifo);

    opal_finalize_util ();

    return test_finalize ();
}
