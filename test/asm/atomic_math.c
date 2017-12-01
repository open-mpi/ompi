/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OMPI_BUILDING 0
#include "opal_config.h"

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <stdlib.h>
#include <stdio.h>

#include "opal/sys/atomic.h"

#define TEST_REPS 500

int32_t val32 = 0;
#if OPAL_HAVE_ATOMIC_MATH_64
int64_t val64 = 0;
#endif
int valint = 0;

static void* atomic_math_test(void* arg)
{
    int count = *((int*) arg);
    int i;

    for (i = 0 ; i < count ; ++i) {
        (void)opal_atomic_add_fetch_32(&val32, 5);
#if OPAL_HAVE_ATOMIC_MATH_64
        (void)opal_atomic_add_fetch_64(&val64, 6);
#endif
        opal_atomic_add (&valint, 4);
    }

    return NULL;
}


static int
atomic_math_test_th(int count, int thr_count)
{
    int value;
    pthread_t *th;
    int tid;

    th = (pthread_t *) malloc(thr_count * sizeof(pthread_t));
    if (!th) {
        perror("malloc");
        exit(EXIT_FAILURE);
    }

    /* Ok to use a single instance of ip_union_t from the stack here
       because a) we're passing the same count to all threads, and b)
       we're waiting for all the threads to finish before leaving this
       function, so there's no race condition of the instance
       disappearing before the threads start. */
    value = count;
    for (tid = 0; tid < thr_count; tid++) {
        if (pthread_create(&th[tid], NULL, atomic_math_test,
                           (void*) &value) != 0) {
            perror("pthread_create");
            exit(EXIT_FAILURE);
        }
    }

    /* -- wait for the thread set to finish -- */
    for (tid = 0; tid < thr_count; tid++) {
        void *thread_return;

        if (pthread_join(th[tid], &thread_return) != 0) {
            perror("pthread_join");
            exit(EXIT_FAILURE);
        }
    }
    free(th);

    return 0;
}


int
main(int argc, char *argv[])
{
    int32_t test32;
#if OPAL_HAVE_ATOMIC_MATH_64
    int64_t test64;
#endif
    int ret = 77;
    int num_threads = 1;

    if (argc != 2) {
        printf("*** Incorrect number of arguments.  Skipping test\n");
        return 77;
    }
    num_threads = atoi(argv[1]);

    test32 = opal_atomic_add_fetch_32 (&val32, 17);
    if (test32 != 17 || val32 != 17) {
        fprintf (stderr, "error in opal_atomic_add_fetch_32. expected (17, 17), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }

    test32 = opal_atomic_fetch_add_32 (&val32, 13);
    if (test32 != 17 || val32 != 30) {
        fprintf (stderr, "error in opal_atomic_fetch_add_32. expected (17, 30), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }



    test32 = opal_atomic_and_fetch_32 (&val32, 0x18);
    if (test32 != 24 || val32 != 24) {
        fprintf (stderr, "error in opal_atomic_and_fetch_32. expected (24, 24), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }

    test32 = opal_atomic_fetch_and_32 (&val32, 0x10);
    if (test32 != 24 || val32 != 16) {
        fprintf (stderr, "error in opal_atomic_fetch_and_32. expected (24, 16), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }



    test32 = opal_atomic_or_fetch_32 (&val32, 0x03);
    if (test32 != 19 || val32 != 19) {
        fprintf (stderr, "error in opal_atomic_or_fetch_32. expected (19, 19), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }

    test32 = opal_atomic_fetch_or_32 (&val32, 0x04);
    if (test32 != 19 || val32 != 23) {
        fprintf (stderr, "error in opal_atomic_fetch_or_32. expected (19, 23), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }


    test32 = opal_atomic_xor_fetch_32 (&val32, 0x03);
    if (test32 != 20 || val32 != 20) {
        fprintf (stderr, "error in opal_atomic_xor_fetch_32. expected (20, 20), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }

    test32 = opal_atomic_fetch_xor_32 (&val32, 0x05);
    if (test32 != 20 || val32 != 17) {
        fprintf (stderr, "error in opal_atomic_fetch_xor_32. expected (20, 17), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }



    test32 = opal_atomic_sub_fetch_32 (&val32, 14);
    if (test32 != 3 || val32 != 3) {
        fprintf (stderr, "error in opal_atomic_sub_fetch_32. expected (3, 3), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }

    test32 = opal_atomic_fetch_xor_32 (&val32, 3);
    if (test32 != 3 || val32 != 0) {
        fprintf (stderr, "error in opal_atomic_fetch_sub_32. expected (3, 0), got (%d, %d)\n", test32, val32);
        exit(EXIT_FAILURE);
    }

#if OPAL_HAVE_ATOMIC_MATH_64
    test64 = opal_atomic_add_fetch_64 (&val64, 17);
    if (test64 != 17 || val64 != 17) {
        fprintf (stderr, "error in opal_atomic_add_fetch_64. expected (17, 17), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }

    test64 = opal_atomic_fetch_add_64 (&val64, 13);
    if (test64 != 17 || val64 != 30) {
        fprintf (stderr, "error in opal_atomic_fetch_add_64. expected (17, 30), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }



    test64 = opal_atomic_and_fetch_64 (&val64, 0x18);
    if (test64 != 24 || val64 != 24) {
        fprintf (stderr, "error in opal_atomic_and_fetch_64. expected (24, 24), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }

    test64 = opal_atomic_fetch_and_64 (&val64, 0x10);
    if (test64 != 24 || val64 != 16) {
        fprintf (stderr, "error in opal_atomic_fetch_and_64. expected (24, 16), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }



    test64 = opal_atomic_or_fetch_64 (&val64, 0x03);
    if (test64 != 19 || val64 != 19) {
        fprintf (stderr, "error in opal_atomic_or_fetch_64. expected (19, 19), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }

    test64 = opal_atomic_fetch_or_64 (&val64, 0x04);
    if (test64 != 19 || val64 != 23) {
        fprintf (stderr, "error in opal_atomic_fetch_or_64. expected (19, 23), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }


    test64 = opal_atomic_xor_fetch_64 (&val64, 0x03);
    if (test64 != 20 || val64 != 20) {
        fprintf (stderr, "error in opal_atomic_xor_fetch_64. expected (20, 20), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }

    test64 = opal_atomic_fetch_xor_64 (&val64, 0x05);
    if (test64 != 20 || val64 != 17) {
        fprintf (stderr, "error in opal_atomic_fetch_xor_64. expected (20, 17), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }



    test64 = opal_atomic_sub_fetch_64 (&val64, 14);
    if (test64 != 3 || val64 != 3) {
        fprintf (stderr, "error in opal_atomic_sub_fetch_64. expected (3, 3), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }

    test64 = opal_atomic_fetch_xor_64 (&val64, 3);
    if (test64 != 3 || val64 != 0) {
        fprintf (stderr, "error in opal_atomic_fetch_sub_64. expected (3, 0), got (%" PRId64 ", %" PRId64 ")\n", test64, val64);
        exit(EXIT_FAILURE);
    }
#endif

    ret = atomic_math_test_th(TEST_REPS, num_threads);
    if (ret == 77) return ret;
    opal_atomic_mb();
    if (val32 != TEST_REPS * num_threads * 5) {
        printf("opal_atomic_add_fetch32 failed.  Expected %d, got %d.\n",
               TEST_REPS * num_threads * 5, val32);
        ret = 1;
    }
#if OPAL_HAVE_ATOMIC_MATH_64
    if (val64 != TEST_REPS * num_threads * 6) {
        /* Safe to case to (int) here because we know it's going to be
           a small value */
        printf("opal_atomic_add_fetch32 failed.  Expected %d, got %d.\n",
               TEST_REPS * num_threads * 6, (int) val64);
        ret = 1;
    }
#else
    printf("      * skipping 64 bit tests\n");
#endif
    if (valint != TEST_REPS * num_threads * 4) {
        printf("opal_atomic_add_fetch32 failed.  Expected %d, got %d.\n",
               TEST_REPS * num_threads * 4, valint);
        ret = 1;
    }

    return ret;
}
