#include <assert.h>
#include <getopt.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "support.h"

#include "atomic.h"

/* default options */

int nreps = 100;
int nthreads = 2;
int enable_verbose = 0;
int enable_64_bit_tests = 0;

volatile uint32_t vol32;
uint32_t val32;
uint32_t old32;
uint32_t new32;

volatile uint64_t vol64;
uint64_t val64;
uint64_t old64;
uint64_t new64;

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
           "      -l                do 64-bit tests\n"
           "      -r NREPS          number of repetitions\n"
           "      -t NTRHEADS       number of threads\n"
           "      -v                verbose output\n"
           "      -h                print this info\n" "\n"
           "   Numbers may be postfixed with 'k' or 'm'\n\n");

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


void *thread_main(void *arg)
{
    int rank = (int) arg;
    int i;

    verbose("thread-%d: Hello\n", rank);

    /* thread tests */

    for (i = 0; i < nreps; i++) {
        ompi_atomic_add_32(&val32, 5);
        if (enable_64_bit_tests) {
            ompi_atomic_add_64(&val64, 5);
        }
        ompi_atomic_add_int(&valint, 5);
    }

    return (void *) (rank + 1000);
}


int main(int argc, char *argv[])
{
    int c;
    int tid;
    pthread_t *th;

    /* option processing */

    test_init("atomic operations");

    while ((c = getopt(argc, argv, "hlr:t:v")) != -1) {
        switch (c) {
        case 'h':
            help();
            break;
        case 'l':
            enable_64_bit_tests = 1;
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
    test_verify("atomic", ompi_atomic_cmpset_32(&vol32, old32, new32) == 1);
    test_verify("atomic", vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    test_verify("atomic", ompi_atomic_cmpset_32(&vol32, old32, new32) == 0);
    test_verify("atomic", vol32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    test_verify("atomic", ompi_atomic_cmpset_acq_32(&vol32, old32, new32) == 1);
    test_verify("atomic", vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    test_verify("atomic", ompi_atomic_cmpset_acq_32(&vol32, old32, new32) == 0);
    test_verify("atomic", vol32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    test_verify("atomic", ompi_atomic_cmpset_rel_32(&vol32, old32, new32) == 1);
    test_verify("atomic", vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    test_verify("atomic", ompi_atomic_cmpset_rel_32(&vol32, old32, new32) == 0);
    test_verify("atomic", vol32 == 42);

    /* -- cmpset 64-bit tests -- */

    if (enable_64_bit_tests) {
        vol64 = 42, old64 = 42, new64 = 50;
        test_verify("atomic", ompi_atomic_cmpset_64(&vol64, old64, new64) == 1);
        test_verify("atomic", vol64 == new64);

        vol64 = 42, old64 = 420, new64 = 50;
        test_verify("atomic", ompi_atomic_cmpset_64(&vol64, old64, new64) == 0);
        test_verify("atomic", vol64 == 42);

        vol64 = 42, old64 = 42, new64 = 50;
        test_verify("atomic", ompi_atomic_cmpset_acq_64(&vol64, old64, new64) == 1);
        test_verify("atomic", vol64 == new64);

        vol64 = 42, old64 = 420, new64 = 50;
        test_verify("atomic", ompi_atomic_cmpset_acq_64(&vol64, old64, new64) == 0);
        test_verify("atomic", vol64 == 42);

        vol64 = 42, old64 = 42, new64 = 50;
        test_verify("atomic", ompi_atomic_cmpset_rel_64(&vol64, old64, new64) == 1);
        test_verify("atomic", vol64 == new64);

        vol64 = 42, old64 = 420, new64 = 50;
        test_verify("atomic", ompi_atomic_cmpset_rel_64(&vol64, old64, new64) == 0);
        test_verify("atomic", vol64 == 42);
    }

    /* -- cmpset int tests -- */

    volint = 42, oldint = 42, newint = 50;
    test_verify("atomic", ompi_atomic_cmpset_int(&volint, oldint, newint) == 1);
    test_verify("atomic", volint == newint);

    volint = 42, oldint = 420, newint = 50;
    test_verify("atomic", ompi_atomic_cmpset_int(&volint, oldint, newint) == 0);
    test_verify("atomic", volint == 42);

    volint = 42, oldint = 42, newint = 50;
    test_verify("atomic", ompi_atomic_cmpset_acq_int(&volint, oldint, newint) == 1);
    test_verify("atomic", volint == newint);

    volint = 42, oldint = 420, newint = 50;
    test_verify("atomic", ompi_atomic_cmpset_acq_int(&volint, oldint, newint) == 0);
    test_verify("atomic", volint == 42);

    volint = 42, oldint = 42, newint = 50;
    test_verify("atomic", ompi_atomic_cmpset_rel_int(&volint, oldint, newint) == 1);
    test_verify("atomic", volint == newint);

    volint = 42, oldint = 420, newint = 50;
    test_verify("atomic", ompi_atomic_cmpset_rel_int(&volint, oldint, newint) == 0);
    test_verify("atomic", volint == 42);


    /* -- cmpset ptr tests -- */

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    test_verify("atomic", ompi_atomic_cmpset_ptr(&volptr, oldptr, newptr) == 1);
    test_verify("atomic", volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    test_verify("atomic", ompi_atomic_cmpset_ptr(&volptr, oldptr, newptr) == 0);
    test_verify("atomic", volptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    test_verify("atomic", ompi_atomic_cmpset_acq_ptr(&volptr, oldptr, newptr) == 1);
    test_verify("atomic", volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    test_verify("atomic", ompi_atomic_cmpset_acq_ptr(&volptr, oldptr, newptr) == 0);
    test_verify("atomic", volptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    test_verify("atomic", ompi_atomic_cmpset_rel_ptr(&volptr, oldptr, newptr) == 1);
    test_verify("atomic", volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    test_verify("atomic", ompi_atomic_cmpset_rel_ptr(&volptr, oldptr, newptr) == 0);
    test_verify("atomic", volptr == (void *) 42);

    /* -- add_32 tests -- */

    val32 = 42;
    test_verify("atomic", ompi_atomic_add_32(&val32, 5) == (42 + 5));
    test_verify("atomic", (42 + 5) == val32);

    /* -- add_64 tests -- */

    if (enable_64_bit_tests) {
        val64 = 42;
        test_verify("atomic", ompi_atomic_add_64(&val64, 5) == (42 + 5));
        test_verify("atomic", (42 + 5) == val64);
    }

    /* -- add_int tests -- */

    valint = 42;
    test_verify("atomic", ompi_atomic_add_int(&valint, 5) == (42 + 5));
    test_verify("atomic", (42 + 5) == valint);


    /* threaded tests */

    val32 = 0;
    val64 = 0ul;
    valint = 0;

    /* -- create the thread set -- */

    th = (pthread_t *) malloc(nthreads * sizeof(pthread_t));
    if (!th) {
        perror("malloc");
        exit(EXIT_FAILURE);
    }
    for (tid = 0; tid < nthreads; tid++) {
        if (pthread_create(&th[tid], NULL, thread_main, (void *) tid) != 0) {
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
        verbose("main: thread %d returned %d\n", tid, (int) thread_return);
    }
    free(th);

    test_verify("atomic", val32 == (5 * nthreads * nreps));
    if (enable_64_bit_tests) {
        test_verify("atomic", val64 == (5 * nthreads * nreps));
    }
    test_verify("atomic", valint == (5 * nthreads * nreps));

    return test_finalize();
}
