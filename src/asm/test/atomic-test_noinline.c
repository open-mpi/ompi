/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#undef OMPI_BUILDING
#include "ompi_config.h"

#undef OMPI_GCC_INLINE_ASSEMBLY
#define OMPI_GCC_INLINE_ASSEMBLY 0

#undef OMPI_DEC_INLINE_ASSEMBLY
#define OMPI_DEC_INLINE_ASSEMBLY 0

#undef OMPI_XLC_INLINE_ASSEMBLY
#define OMPI_XLC_INLINE_ASSEMBLY 0


#include <assert.h>
#include <getopt.h>
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "include/sys/atomic.h"


/* default options */

int nreps = 100;
int nthreads = 2;
int enable_verbose = 0;
int enable_64_bit_tests = 0;

volatile int32_t vol32;
int32_t val32;
int32_t old32;
int32_t new32;

#if OMPI_HAVE_ATOMIC_MATH_64
volatile int64_t vol64;
int64_t val64;
int64_t old64;
int64_t new64;
#endif

volatile int volint;
int valint;
int oldint;
int newint;

volatile void *volptr;
void *oldptr;
void *newptr;


static void help(void)
{
    printf("Usage: threadtest [flags]\n"
           "\n"
           "   Flags may be any of\n"
#if OMPI_HAVE_ATOMIC_MATH_64
           "      -l                do 64-bit tests\n"
#endif
           "      -r NREPS          number of repetitions\n"
           "      -t NTRHEADS       number of threads\n"
           "      -v                verbose output\n"
           "      -h                print this info\n" "\n"
           "   Numbers may be postfixed with 'k' or 'm'\n\n");

#ifndef OMPI_HAVE_ATOMIC_MATH_64
    printf("   64-bit tests are not enabled in this build of the tests\n\n");
#endif

    exit(EXIT_SUCCESS);
}


static void usage(void)
{
    fprintf(stderr,
            "Usage: threadtest [flags]\n" "       threadtest -h\n");
    exit(EXIT_FAILURE);
}


static void verbose(const char *fmt, ...)
{
    if (enable_verbose) {
        va_list ap;
        va_start(ap, fmt);
        vfprintf(stderr, fmt, ap);
        va_end(ap);
    }
}


static int str2size(char *str)
{
    int size;
    char mod[32];

    switch (sscanf(str, "%d%1[mMkK]", &size, mod)) {
    case 1:
        return (size);
    case 2:
        switch (*mod) {
        case 'm':
        case 'M':
            return (size << 20);
        case 'k':
        case 'K':
            return (size << 10);
        default:
            return (size);
        }
    default:
        return (-1);
    }
}


static void *thread_main(void *arg)
{
    int rank = (int) (unsigned long) arg;
    int i;

    verbose("thread-%d: Hello\n", rank);

    /* thread tests */

    for (i = 0; i < nreps; i++) {
        ompi_atomic_add_32(&val32, 5);
#if OMPI_HAVE_ATOMIC_MATH_64
        if (enable_64_bit_tests) {
            ompi_atomic_add_64(&val64, 5);
        }
#endif
        ompi_atomic_add(&valint, 5);
    }

    return (void *) (unsigned long) (rank + 1000);
}


int main(int argc, char *argv[])
{
    int c;
#if OMPI_HAVE_POSIX_THREADS
    int tid;
    pthread_t *th;
#endif

    /* option processing */

    while ((c = getopt(argc, argv, "hlr:t:v")) != -1) {
        switch (c) {
        case 'h':
            help();
            break;
        case 'l':
#if OMPI_HAVE_ATOMIC_MATH_64
            enable_64_bit_tests = 1;
#else
	    usage();
#endif
            break;
        case 'r':
            if ((nreps = str2size(optarg)) <= 0) {
                usage();
            }
            break;
        case 't':
            if ((nthreads = str2size(optarg)) <= 0) {
                usage();
            }
            break;
        case 'v':
            enable_verbose = 1;
            break;
        default:
            usage();
        }
    }
    if (optind != argc) {
        usage();
    }

    verbose("main: %s\n", argv[0]);
    verbose("main: nthreads = %d\n", nthreads);
    verbose("main: nreps = %d\n", nreps);

    /* first test single-threaded functionality */

    /* -- cmpset 32-bit tests -- */

    vol32 = 42, old32 = 42, new32 = 50;
    assert(ompi_atomic_cmpset_32(&vol32, old32, new32) == 1);
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(ompi_atomic_cmpset_32(&vol32, old32, new32) ==  0);
    assert(vol32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    assert(ompi_atomic_cmpset_acq_32(&vol32, old32, new32) == 1);
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(ompi_atomic_cmpset_acq_32(&vol32, old32, new32) == 0);
    assert(vol32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    assert(ompi_atomic_cmpset_rel_32(&vol32, old32, new32) ==  1);
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(ompi_atomic_cmpset_rel_32(&vol32, old32, new32) == 0);
    assert(vol32 == 42);

    /* -- cmpset 64-bit tests -- */

#if OMPI_HAVE_ATOMIC_MATH_64
    if (enable_64_bit_tests) {
	verbose("64 bit serial tests\n");
        vol64 = 42, old64 = 42, new64 = 50;
        assert(1 == ompi_atomic_cmpset_64(&vol64, old64, new64));
        assert(new64 == vol64);

	verbose("64 bit serial test 2\n");
        vol64 = 42, old64 = 420, new64 = 50;
        assert(ompi_atomic_cmpset_64(&vol64, old64, new64) == 0);
        assert(vol64 == 42);

        vol64 = 42, old64 = 42, new64 = 50;
        assert(ompi_atomic_cmpset_acq_64(&vol64, old64, new64) == 1);
        assert(vol64 == new64);

        vol64 = 42, old64 = 420, new64 = 50;
        assert(ompi_atomic_cmpset_acq_64(&vol64, old64, new64) == 0);
        assert(vol64 == 42);

        vol64 = 42, old64 = 42, new64 = 50;
        assert(ompi_atomic_cmpset_rel_64(&vol64, old64, new64) == 1);
        assert(vol64 == new64);

        vol64 = 42, old64 = 420, new64 = 50;
        assert(ompi_atomic_cmpset_rel_64(&vol64, old64, new64) == 0);
        assert(vol64 == 42);
    }
#endif
    /* -- cmpset int tests -- */

    volint = 42, oldint = 42, newint = 50;
    assert(ompi_atomic_cmpset(&volint, oldint, newint) == 1);
    assert(volint ==newint);

    volint = 42, oldint = 420, newint = 50;
    assert(ompi_atomic_cmpset(&volint, oldint, newint) == 0);
    assert(volint == 42);

    volint = 42, oldint = 42, newint = 50;
    assert(ompi_atomic_cmpset_acq(&volint, oldint, newint) == 1);
    assert(volint == newint);

    volint = 42, oldint = 420, newint = 50;
    assert(ompi_atomic_cmpset_acq(&volint, oldint, newint) == 0);
    assert(volint == 42);

    volint = 42, oldint = 42, newint = 50;
    assert(ompi_atomic_cmpset_rel(&volint, oldint, newint) == 1);
    assert(volint == newint);

    volint = 42, oldint = 420, newint = 50;
    assert(ompi_atomic_cmpset_rel(&volint, oldint, newint) == 0);
    assert(volint == 42);


    /* -- cmpset ptr tests -- */

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(ompi_atomic_cmpset_ptr(&volptr, oldptr, newptr) == 1);
    assert(volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(ompi_atomic_cmpset_ptr(&volptr, oldptr, newptr) == 0);
    assert(volptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(ompi_atomic_cmpset_acq_ptr(&volptr, oldptr, newptr) == 1);
    assert(volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(ompi_atomic_cmpset_acq_ptr(&volptr, oldptr, newptr) == 0);
    assert(volptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(ompi_atomic_cmpset_rel_ptr(&volptr, oldptr, newptr) == 1);
    assert(volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(ompi_atomic_cmpset_rel_ptr(&volptr, oldptr, newptr) == 0);
    assert(volptr == (void *) 42);

    /* -- add_32 tests -- */

    val32 = 42;
    assert(ompi_atomic_add_32(&val32, 5) == (42 + 5));
    assert((42 + 5) == val32);

    /* -- add_64 tests -- */
#if OMPI_HAVE_ATOMIC_MATH_64
    if (enable_64_bit_tests) {
        val64 = 42;
        assert(ompi_atomic_add_64(&val64, 5) == (42 + 5));
        assert((42 + 5) == val64);
    }
#endif
    /* -- add_int tests -- */

    valint = 42;
    ompi_atomic_add(&valint, 5);
    assert((42 + 5) == valint);


    /* threaded tests */

    val32 = 0;
#if OMPI_HAVE_ATOMIC_MATH_64
    val64 = 0ul;
#endif
    valint = 0;

    /* -- create the thread set -- */
#if OMPI_HAVE_POSIX_THREADS
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
        verbose("main: thread %d returned %d\n", tid, (int) (unsigned long) thread_return);
    }
    free(th);

    assert((5 * nthreads * nreps) == val32);
#if OMPI_HAVE_ATOMIC_MATH_64
    if (enable_64_bit_tests) {
        assert((5 * nthreads * nreps) ==  val64);
    }
#endif
    assert((5 * nthreads * nreps) == valint);
#endif

    return 0;
}
