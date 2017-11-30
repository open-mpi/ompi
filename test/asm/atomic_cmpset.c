/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OMPI_BUILDING 0
#include "opal_config.h"

#undef NDEBUG
#define DEBUG

#include <assert.h>
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "opal/sys/atomic.h"


/* default options */
int nreps = 100;
int nthreads = 2;
int enable_verbose = 0;

volatile int32_t vol32 = 0;
int32_t val32 = 0;
int32_t old32 = 0;
int32_t new32 = 0;

#if OPAL_HAVE_ATOMIC_MATH_64
volatile int64_t vol64 = 0;
int64_t val64 = 0;
int64_t old64 = 0;
int64_t new64 = 0;
#endif

#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128
volatile opal_int128_t vol128;
opal_int128_t val128;
opal_int128_t old128;
opal_int128_t new128;
#endif

volatile int volint = 0;
int valint = 0;
int oldint = 0;
int newint = 0;

volatile void *volptr = NULL;
void *oldptr = NULL;
void *newptr = NULL;


static void *thread_main(void *arg)
{
    int rank = (int) (unsigned long) arg;
    int i;

    /* thread tests */

    for (i = 0; i < nreps; i++) {
        opal_atomic_add_fetch_32(&val32, 5);
#if OPAL_HAVE_ATOMIC_MATH_64
        opal_atomic_add_fetch_64(&val64, 5);
#endif
        opal_atomic_add (&valint, 5);
    }

    return (void *) (unsigned long) (rank + 1000);
}

int main(int argc, char *argv[])
{
    int tid;
    pthread_t *th;

    if (argc != 2) {
        printf("*** Incorrect number of arguments.  Skipping test\n");
        return 77;
    }
    nthreads = atoi(argv[1]);


    /* first test single-threaded functionality */

    /* -- cmpset 32-bit tests -- */

    vol32 = 42, old32 = 42, new32 = 50;
    assert(opal_atomic_compare_exchange_strong_32 (&vol32, &old32, new32) == true);
    opal_atomic_rmb();
    assert(vol32 == new32);
    assert(old32 == 42);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(opal_atomic_compare_exchange_strong_32 (&vol32, &old32, new32) ==  false);
    opal_atomic_rmb();
    assert(vol32 == 42);
    assert(old32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    assert(opal_atomic_compare_exchange_strong_32 (&vol32, &old32, new32) == true);
    assert(vol32 == new32);
    assert(old32 == 42);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(opal_atomic_compare_exchange_strong_acq_32 (&vol32, &old32, new32) == false);
    assert(vol32 == 42);
    assert(old32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    assert(opal_atomic_compare_exchange_strong_rel_32 (&vol32, &old32, new32) ==  true);
    opal_atomic_rmb();
    assert(vol32 == new32);
    assert(old32 == 42);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(opal_atomic_compare_exchange_strong_rel_32 (&vol32, &old32, new32) == false);
    opal_atomic_rmb();
    assert(vol32 == 42);
    assert(old32 == 42);

    /* -- cmpset 64-bit tests -- */

#if OPAL_HAVE_ATOMIC_MATH_64
    vol64 = 42, old64 = 42, new64 = 50;
    assert(opal_atomic_compare_exchange_strong_64 (&vol64, &old64, new64) == true);
    opal_atomic_rmb();
    assert(new64 == vol64);
    assert(old64 == 42);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(opal_atomic_compare_exchange_strong_64 (&vol64, &old64, new64) == false);
    opal_atomic_rmb();
    assert(vol64 == 42);
    assert(old64 == 42);

    vol64 = 42, old64 = 42, new64 = 50;
    assert(opal_atomic_compare_exchange_strong_acq_64 (&vol64, &old64, new64) == true);
    assert(vol64 == new64);
    assert(old64 == 42);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(opal_atomic_compare_exchange_strong_acq_64 (&vol64, &old64, new64) == false);
    assert(vol64 == 42);
    assert(old64 == 42);

    vol64 = 42, old64 = 42, new64 = 50;
    assert(opal_atomic_compare_exchange_strong_rel_64 (&vol64, &old64, new64) == true);
    opal_atomic_rmb();
    assert(vol64 == new64);
    assert(old64 == 42);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(opal_atomic_compare_exchange_strong_rel_64 (&vol64, &old64, new64) == false);
    opal_atomic_rmb();
    assert(vol64 == 42);
    assert(old64 == 42);
#endif

    /* -- cmpset 128-bit tests -- */

#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128
    vol128 = 42, old128 = 42, new128 = 50;
    assert(opal_atomic_compare_exchange_strong_128 (&vol128, &old128, new128) == true);
    opal_atomic_rmb();
    assert(new128 == vol128);
    assert(old128 == 42);

    vol128 = 42, old128 = 420, new128 = 50;
    assert(opal_atomic_compare_exchange_strong_128 (&vol128, &old128, new128) == false);
    opal_atomic_rmb();
    assert(vol128 == 42);
    assert(old128 == 42);
#endif

    /* -- cmpset int tests -- */

    volint = 42, oldint = 42, newint = 50;
    assert(opal_atomic_compare_exchange_strong (&volint, &oldint, newint) == true);
    opal_atomic_rmb();
    assert(volint == newint);
    assert(oldint == 42);

    volint = 42, oldint = 420, newint = 50;
    assert(opal_atomic_compare_exchange_strong (&volint, &oldint, newint) == false);
    opal_atomic_rmb();
    assert(volint == 42);
    assert(oldint == 42);

    volint = 42, oldint = 42, newint = 50;
    assert(opal_atomic_compare_exchange_strong_acq (&volint, &oldint, newint) == true);
    assert(volint == newint);
    assert(oldint == 42);

    volint = 42, oldint = 420, newint = 50;
    assert(opal_atomic_compare_exchange_strong_acq (&volint, &oldint, newint) == false);
    assert(volint == 42);
    assert(oldint == 42);

    volint = 42, oldint = 42, newint = 50;
    assert(opal_atomic_compare_exchange_strong_rel (&volint, &oldint, newint) == true);
    opal_atomic_rmb();
    assert(volint == newint);
    assert(oldint == 42);

    volint = 42, oldint = 420, newint = 50;
    assert(opal_atomic_compare_exchange_strong_rel (&volint, &oldint, newint) == false);
    opal_atomic_rmb();
    assert(volint == 42);
    assert(oldint == 42);


    /* -- cmpset ptr tests -- */

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(opal_atomic_compare_exchange_strong_ptr (&volptr, &oldptr, newptr) == true);
    opal_atomic_rmb();
    assert(volptr == newptr);
    assert(oldptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(opal_atomic_compare_exchange_strong_ptr (&volptr, &oldptr, newptr) == false);
    opal_atomic_rmb();
    assert(volptr == (void *) 42);
    assert(oldptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(opal_atomic_compare_exchange_strong_acq_ptr (&volptr, &oldptr, newptr) == true);
    assert(volptr == newptr);
    assert(oldptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(opal_atomic_compare_exchange_strong_acq_ptr (&volptr, &oldptr, newptr) == false);
    assert(volptr == (void *) 42);
    assert(oldptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(opal_atomic_compare_exchange_strong_rel_ptr (&volptr, &oldptr, newptr) == true);
    opal_atomic_rmb();
    assert(volptr == newptr);
    assert(oldptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(opal_atomic_compare_exchange_strong_rel_ptr (&volptr, &oldptr, newptr) == false);
    opal_atomic_rmb();
    assert(volptr == (void *) 42);
    assert(oldptr == (void *) 42);

    /* -- add_32 tests -- */

    val32 = 42;
    assert(opal_atomic_add_fetch_32(&val32, 5) == (42 + 5));
    opal_atomic_rmb();
    assert((42 + 5) == val32);

    /* -- add_64 tests -- */
#if OPAL_HAVE_ATOMIC_MATH_64
    val64 = 42;
    assert(opal_atomic_add_fetch_64(&val64, 5) == (42 + 5));
    opal_atomic_rmb();
    assert((42 + 5) == val64);
#endif
    /* -- add_int tests -- */

    valint = 42;
    opal_atomic_add (&valint, 5);
    opal_atomic_rmb();
    assert((42 + 5) == valint);


    /* threaded tests */

    val32 = 0;
#if OPAL_HAVE_ATOMIC_MATH_64
    val64 = 0ul;
#endif
    valint = 0;

    /* -- create the thread set -- */
    th = (pthread_t *) malloc(nthreads * sizeof(pthread_t));
    if (!th) {
        perror("malloc");
        exit(EXIT_FAILURE);
    }
    for (tid = 0; tid < nthreads; tid++) {
        if (pthread_create(&th[tid], NULL, thread_main, (void *) (unsigned long) tid) != 0) {
            perror("pthread_create");
            exit(EXIT_FAILURE);
        }
    }

    /* -- wait for the thread set to finish -- */

    for (tid = 0; tid < nthreads; tid++) {
        void *thread_return;

        if (pthread_join(th[tid], &thread_return) != 0) {
            perror("pthread_join");
            exit(EXIT_FAILURE);
        }
    }
    free(th);

    opal_atomic_rmb();
    assert((5 * nthreads * nreps) == val32);
#if OPAL_HAVE_ATOMIC_MATH_64
    opal_atomic_rmb();
    assert((5 * nthreads * nreps) ==  val64);
#endif
    opal_atomic_rmb();
    assert((5 * nthreads * nreps) == valint);

    return 0;
}
