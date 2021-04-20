/*
 * Copyright (c) 2021 IBM Corporation.  All rights reserved.
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <time.h>

#include "support.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/condition.h"
#include "opal/sys/atomic.h"


#define OPAL_TEST_THREAD_COUNT 8
#define ITERATIONS 1000000
#define ITEM_COUNT 100

#ifdef __APPLE__
/* pthread barrier implementation copied from stack overflow.
 * Source:
 * https://stackoverflow.com/questions/3640853/performance-test-sem-t-v-s-dispatch-semaphore-t-and-pthread-once-t-v-s-dispatch
 */
typedef int pthread_barrierattr_t;
struct pthread_barrier_t {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int count;
    int tripCount;
};

typedef struct pthread_barrier_t pthread_barrier_t;

int pthread_barrier_init(pthread_barrier_t *barrier, const pthread_barrierattr_t *attr,
                         unsigned int count)
{
    if (count == 0) {
        errno = EINVAL;
        return -1;
    }
    if (pthread_mutex_init(&barrier->mutex, 0) < 0) {
        return -1;
    }
    if (pthread_cond_init(&barrier->cond, 0) < 0) {
        pthread_mutex_destroy(&barrier->mutex);
        return -1;
    }
    barrier->tripCount = count;
    barrier->count = 0;

    return 0;
}

int pthread_barrier_destroy(pthread_barrier_t *barrier)
{
    pthread_cond_destroy(&barrier->cond);
    pthread_mutex_destroy(&barrier->mutex);
    return 0;
}

int pthread_barrier_wait(pthread_barrier_t *barrier)
{
    pthread_mutex_lock(&barrier->mutex);
    ++(barrier->count);
    if (barrier->count >= barrier->tripCount) {
        barrier->count = 0;
        pthread_cond_broadcast(&barrier->cond);
        pthread_mutex_unlock(&barrier->mutex);
        return 1;
    }

    pthread_cond_wait(&barrier->cond, &(barrier->mutex));
    pthread_mutex_unlock(&barrier->mutex);
    return 0;
}
#endif

static opal_atomic_int64_t var_64 = 0;
static opal_atomic_int32_t var_32 = 0;
static pthread_barrier_t   barrier;

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


#if !defined(OPAL_TEST_DONE)
#define OPAL_TEST_DONE(func, val) { \
    gettimeofday (&stop, NULL); \
    timersub(&stop, &start, &total); \
    timing = ((double) total.tv_sec + (double) total.tv_usec * 1e-6) / (double) ITERATIONS; \
    printf ("%s() thread finished. Time: %d s %d us %d nsec/per\n", func, (int) total.tv_sec, \
            (int)total.tv_usec, (int)(timing / 1e-9)); \
    memset(&stop, 0, sizeof(struct timeval)); \
    memset(&start, 0, sizeof(struct timeval)); \
    memset(&total, 0, sizeof(struct timeval)); \
    /* printf("%ld\n", val); */ \
    fflush(stdout); \
    pthread_barrier_wait (&barrier); \
}
#endif

#if !defined(OPAL_RESET_VAR)
#define OPAL_RESET_VAR(var) { \
    var = 0; \
    pthread_barrier_wait (&barrier); \
}
#endif

static void *thread_test (void *arg) {
    struct timeval start, stop, total;
    double timing;

    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_compare_exchange_strong_64(&var_64, &i, i+1);
    }
    OPAL_TEST_DONE("opal_atomic_compare_exchange_strong_64", var_64);

    OPAL_RESET_VAR(var_64);

    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_compare_exchange_strong_rel_64(&var_64, &i, i+1);
    }
    OPAL_TEST_DONE("opal_atomic_compare_exchange_strong_rel_64", var_64);

    OPAL_RESET_VAR(var_64);

    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_compare_exchange_strong_acq_64(&var_64, &i, i+1);
    }
    OPAL_TEST_DONE("opal_atomic_compare_exchange_strong_acq_64", var_64);

    OPAL_RESET_VAR(var_64);

    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_fetch_add_64(&var_64, 1);
    }
    OPAL_TEST_DONE("opal_atomic_fetch_add_64", var_64);

    OPAL_RESET_VAR(var_64);

    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_fetch_sub_64(&var_64, 1);
    }
    OPAL_TEST_DONE("opal_atomic_fetch_sub_64", var_64);

    OPAL_RESET_VAR(var_64);

    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_fetch_xor_64(&var_64, i);
    }
    OPAL_TEST_DONE("opal_atomic_fetch_xor_64", var_64);

    OPAL_RESET_VAR(var_64);

    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_swap_64(&var_64, i);
    }
    OPAL_TEST_DONE("opal_atomic_swap_64", var_64);

    OPAL_RESET_VAR(var_64);

#if OPAL_HAVE_ATOMIC_LLSC_64
    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       int ret;
       opal_atomic_sc_64(&var_64, i, ret);
    }
    OPAL_TEST_DONE("opal_atomic_sc_64", var_64);

    OPAL_RESET_VAR(var_64);

    gettimeofday (&start, NULL);
    for (int64_t i = 0 ; i < ITERATIONS ; ++i) {
       int ret;
       opal_atomic_sc_64(&var_64, i, ret);
    }
    OPAL_TEST_DONE("opal_atomic_ll_64", var_64);

    OPAL_RESET_VAR(var_64);
#endif

    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_compare_exchange_strong_32(&var_32, &i, i+1);
    }
    OPAL_TEST_DONE("opal_atomic_compare_exchange_strong_32", var_32);

    OPAL_RESET_VAR(var_32);
 
    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_compare_exchange_strong_rel_32(&var_32, &i, i+1);
    }
    OPAL_TEST_DONE("opal_atomic_compare_exchange_strong_rel_32", var_32);

    OPAL_RESET_VAR(var_32);

    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_compare_exchange_strong_acq_32(&var_32, &i, i+1);
    }
    OPAL_TEST_DONE("opal_atomic_compare_exchange_strong_acq_32", var_32);

    OPAL_RESET_VAR(var_32);

    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_fetch_add_32(&var_32, 1);
    }
    OPAL_TEST_DONE("opal_atomic_fetch_add_32", var_32);

    OPAL_RESET_VAR(var_32);

    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_fetch_sub_32(&var_32, 1);
    }
    OPAL_TEST_DONE("opal_atomic_fetch_sub_32", var_32);

    OPAL_RESET_VAR(var_32);

    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_fetch_xor_32(&var_32, i);
    }
    OPAL_TEST_DONE("opal_atomic_fetch_xor_32", var_32);

    OPAL_RESET_VAR(var_32);

    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       opal_atomic_swap_32(&var_32, i);
    }
    OPAL_TEST_DONE("opal_atomic_swap_32", var_32);

    OPAL_RESET_VAR(var_32);

#if OPAL_HAVE_ATOMIC_LLSC_32
    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       int ret;
       opal_atomic_sc_32(&var_32, i, ret);
    }
    OPAL_TEST_DONE("opal_atomic_sc_32", var_32);

    OPAL_RESET_VAR(var_32);

    gettimeofday (&start, NULL);
    for (int32_t i = 0 ; i < ITERATIONS ; ++i) {
       int ret;
       opal_atomic_sc_32(&var_32, i, ret);
    }
    OPAL_TEST_DONE("opal_atomic_ll_32", var_32);

    OPAL_RESET_VAR(var_32);
#endif
   
    return NULL;
}

int main(void) {
    
    pthread_barrier_init (&barrier, NULL, OPAL_TEST_THREAD_COUNT);

    pthread_t ts[OPAL_TEST_THREAD_COUNT];
    for(int i = 0; i < OPAL_TEST_THREAD_COUNT; i++) {
        pthread_create(&ts[i], NULL,  &thread_test, NULL);
    }

    for(int i = 0; i < OPAL_TEST_THREAD_COUNT; i++) {
        pthread_join(ts[i], NULL);
    }
    return 0;
}
