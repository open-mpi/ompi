/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#undef OMPI_BUILDING
#include "ompi_config.h"

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <stdlib.h>
#include <stdio.h>

#include "include/sys/atomic.h"

#define TEST_REPS 500

int32_t val32 = 0;
#if OMPI_HAVE_ATOMIC_MATH_64
int64_t val64 = 0;
#endif
int valint = 0;


static void* atomic_math_test(void* arg)
{
    int count = (int) (unsigned long) arg;
    int i;

    for (i = 0 ; i < count ; ++i) {
        ompi_atomic_add_32(&val32, 5);
#if OMPI_HAVE_ATOMIC_MATH_64
        ompi_atomic_add_64(&val64, 6);
#endif
        ompi_atomic_add(&valint, 4);
    }

    return NULL;
}


static int
atomic_math_test_th(int count, int thr_count)
{
#if OMPI_HAVE_POSIX_THREADS
    pthread_t *th;
    int tid, ret = 0;

    th = (pthread_t *) malloc(thr_count * sizeof(pthread_t));
    if (!th) { perror("malloc"); exit(EXIT_FAILURE); }

    for (tid = 0; tid < thr_count; tid++) {
        if (pthread_create(&th[tid], NULL, atomic_math_test, (void*) count) != 0) {
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

    return ret;
#else
    if (thr_count == 1) {
        atomic_math_test((void*) count);
    } else {
        return 77;
    }
#endif

    return 0;
}


int
main(int argc, char *argv[])
{
    int ret = 77;
    int num_threads = 1;

    if (argc != 2) {
        printf("*** Incorrect number of arguments.  Skipping test\n");
        return 77;
    }
    num_threads = atoi(argv[1]);

    ret = atomic_math_test_th(TEST_REPS, num_threads);
    if (ret == 77) return ret;
    ompi_atomic_mb();
    if (val32 != TEST_REPS * num_threads * 5) {
        printf("ompi_atomic_add32 failed.  Expected %d, got %d.\n",
               TEST_REPS * num_threads * 5, val32);
        ret = 1;
    }
#if OMPI_HAVE_ATOMIC_MATH_64
    if (val64 != TEST_REPS * num_threads * 6) {
        printf("ompi_atomic_add32 failed.  Expected %d, got %d.\n",
               TEST_REPS * num_threads * 6, val64);
        ret = 1;
    }
#else
    printf("      * skipping 64 bit tests\n");
#endif
    if (valint != TEST_REPS * num_threads * 4) {
        printf("ompi_atomic_add32 failed.  Expected %d, got %d.\n",
               TEST_REPS * num_threads * 4, valint);
        ret = 1;
    }

    return ret;
}
