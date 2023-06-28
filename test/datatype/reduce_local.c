/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

// TODO: detect through configure
//#define HAVE_CUDA 1
#define HAVE_ROCM 1

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/runtime/mpiruntime.h"

typedef struct op_name_s {
    char *name;
    char *mpi_op_name;
    MPI_Op op;
} op_name_t;
static op_name_t array_of_ops[] = {
    {"max", "MPI_MAX", MPI_MAX},
    {"min", "MPI_MIN", MPI_MIN},
    {"sum", "MPI_SUM", MPI_SUM},
    {"prod", "MPI_PROD", MPI_PROD},
    {"land", "MPI_LAND", MPI_LAND},
    {"band", "MPI_BAND", MPI_BAND},
    {"lor", "MPI_LOR", MPI_LOR},
    {"bor", "MPI_BOR", MPI_BOR},
    {"lxor", "MPI_LXOR", MPI_LXOR},
    {"bxor", "MPI_BXOR", MPI_BXOR},
    {"replace", "MPI_REPLACE", MPI_REPLACE},
    {NULL, "MPI_OP_NULL", MPI_OP_NULL},
};
static int do_ops[12] = {
    -1,
}; /* index of the ops to do. Size +1 larger than the array_of_ops */
static int verbose = 0;
static int total_errors = 0;

#define max(a, b)               \
    ({                          \
        __typeof__(a) _a = (a); \
        __typeof__(b) _b = (b); \
        _a > _b ? _a : _b;      \
    })

#define min(a, b)               \
    ({                          \
        __typeof__(a) _a = (a); \
        __typeof__(b) _b = (b); \
        _a < _b ? _a : _b;      \
    })

static void print_header(int max_shift) {
    printf("%-10s %-10s %-10s %-10s %-10s", "Op", "Type", "TypeSize", "Check", "Count");
    if (1 == max_shift) {
        printf("%-10s", "'Time (seconds)'");
    } else {
        for (int i = 0; i < max_shift; ++i) {
            char str[128];
            snprintf(str, 128, "'Shift %d [s]'", i);
            printf(" %-10s ", str);
        }
    }
    printf("\n");
}

static void print_status(char *op, char *type, int type_size, int count, int max_shift,
                         double *duration, int repeats, int correct)
{
    if (correct) {
        printf("%-10s %s %-10d success", op, type, type_size);
    } else {
        printf("%-10s %s %-10d [\033[1;31mfail\033[0m]", op, type, type_size);
        total_errors++;
    }
    if (1 == max_shift) {
        printf(" %-10d %.8f\n", count, duration[0] / repeats);
    } else {
        printf(" %-10d ", count);
        for (int i = 0; i < max_shift; i++) {
            printf("%.8f ", duration[i] / repeats);
        }
        printf("\n");
    }
}

static int do_ops_built = 0;
static int build_do_ops(char *optarg, int *do_ops)
{
    int i;
    if (0 == strcmp(optarg, "all")) {
        for (i = 0; NULL != array_of_ops[i].name; i++) {
            do_ops[i] = i;
        }
        do_ops[i] = -1; /* stop */
    } else {
        int n, idx = 0;
        char *token, *arg = optarg;
        while ((token = strsep(&arg, ",")) != NULL) {
            for (i = 0; NULL != array_of_ops[i].name; i++) { /* find the op */
                if (0 == strcmp(array_of_ops[i].name, token)) {
                    /* check if the op was not already selected */
                    for (n = 0; n < idx; n++) {
                        if (i == do_ops[n]) {
                            break;
                        }
                    }
                    if (n >= idx) {
                        do_ops[idx++] = i;
                        do_ops[idx] = -1;
                    }
                    break;
                }
            }
            if (NULL == array_of_ops[i].name) {
                fprintf(stderr, "Unknown op %s. Ignored.\n", token);
            }
        }
    }
    do_ops_built = 1;
    return 0;
}

/* clang-format off */
#define MPI_OP_TEST(OPNAME, MPIOP, MPITYPE, TYPE, INIT_INBUF, INBUF, INIT_INOUT_BUF, INOUT_BUF, CHECK_BUF, COUNT, TYPE_PREFIX) \
do { \
    skip_op_type = 0; \
    allocator->memcpy(INBUF, INIT_INBUF, sizeof(TYPE) * (COUNT)); \
    for(int _k = 0; _k < min((COUNT), max_shift); +_k++ ) { \
        duration[_k] = 0.0; \
        for(int _r = repeats; _r > 0; _r--) { \
            allocator->memcpy(INOUT_BUF, INIT_INOUT_BUF, sizeof(TYPE) * (COUNT)); \
            tstart = MPI_Wtime(); \
            MPI_Reduce_local(INBUF+_k, INOUT_BUF+_k, (COUNT)-_k, (MPITYPE), (MPIOP)); \
            tend = MPI_Wtime(); \
            duration[_k] += (tend - tstart); \
            if( check ) { \
                allocator->memcpy(CHECK_BUF, INOUT_BUF, sizeof(TYPE) * (COUNT)); \
                for( i = 0; i < (COUNT)-_k; i++ ) { \
                    if(((CHECK_BUF+_k)[i]) == (((INIT_INBUF+_k)[i]) OPNAME ((INIT_INOUT_BUF+_k)[i]))) \
                        continue; \
                    printf("First error at alignment %d position %d (%" TYPE_PREFIX " %s %" TYPE_PREFIX " != %" TYPE_PREFIX ")\n", \
                           _k, i, (INBUF+_k)[i], (#OPNAME), (INIT_INOUT_BUF+_k)[i], (INOUT_BUF+_k)[i]); \
                    correctness = 0; \
                    break; \
                } \
            } \
        } \
    } \
    goto check_and_continue; \
} while (0)

#define MPI_OP_MINMAX_TEST(OPNAME, MPIOP, MPITYPE, TYPE, INIT_INBUF, INBUF, INIT_INOUT_BUF, INOUT_BUF, CHECK_BUF, COUNT, TYPE_PREFIX) \
do { \
    skip_op_type = 0; \
    allocator->memcpy(INBUF, INIT_INBUF, sizeof(TYPE) * (COUNT)); \
    for(int _k = 0; _k < min((COUNT), max_shift); +_k++ ) { \
        duration[_k] = 0.0; \
        for(int _r = repeats; _r > 0; _r--) { \
            allocator->memcpy(INOUT_BUF, INIT_INOUT_BUF, sizeof(TYPE) * (COUNT)); \
            tstart = MPI_Wtime(); \
            MPI_Reduce_local(INBUF+_k, INOUT_BUF+_k, (COUNT)-_k, (MPITYPE), (MPIOP)); \
            tend = MPI_Wtime(); \
            duration[_k] += (tend - tstart); \
            if( check ) { \
                allocator->memcpy(CHECK_BUF, INOUT_BUF, sizeof(TYPE) * (COUNT)); \
                for( i = 0; i < (COUNT); i++ ) { \
                    TYPE _v1 = *(INIT_INBUF+_k), _v2 = *(CHECK_BUF+_k), _v3 = *(INIT_INOUT_BUF+_k); \
                    if(_v2 == OPNAME(_v1, _v3)) \
                        continue; \
                    printf("First error at alignment %d position %d (%" TYPE_PREFIX " !=  %s(%" TYPE_PREFIX ", %" TYPE_PREFIX ")\n", \
                           _k, i, _v1, (#OPNAME), _v3, _v2); \
                    correctness = 0; \
                    break; \
                } \
            } \
        } \
    } \
    goto check_and_continue; \
} while (0)
/* clang-format on */

static
void *host_allocate(size_t size, size_t align) {
    void *ptr;
    posix_memalign(&ptr, align, size);
    return ptr;
}
static void host_free(void *ptr) {
    free(ptr);
}
static void host_init(void) {
    // nothing to do
}
static void host_fini(void) {
    // nothing to do
}
static void* host_memcpy(void *dst, const void *src, size_t size) {
    return memcpy(dst, src, size);
}

typedef void*(*allocate_fn_t)(size_t, size_t);
typedef void*(*memcpy_fn_t)(void*, const void*, size_t);
typedef void(*free_fn_t)(void*);
typedef void(*init_fn_t)(void);
typedef void(*fini_fn_t)(void);

enum ALLOCATOR_FLAGS {
    ALLOCATOR_DISCRETE = 1,
};

typedef struct {
    int           flags;
    init_fn_t     init;
    allocate_fn_t allocate;
    memcpy_fn_t   memcpy;
    free_fn_t     free;
    fini_fn_t     fini;
} allocator_t;

static allocator_t host_allocator = {
    .flags     = 0,
    .init     = &host_init,
    .allocate = &host_allocate,
    .memcpy   = &host_memcpy,
    .free     = &host_free,
    .fini     = &host_fini};

#if defined(HAVE_CUDA)
#include <cuda_runtime.h>
static void cuda_init() {
    // nothing to be done
}
static void *cuda_allocate(size_t size, size_t align) {
    (void)align; // ignored
    void *ptr;
    int err;
    if (cudaSuccess != (err = cudaMalloc(&ptr, size))) {
        fprintf(stderr, "cudaMalloc failed to allocate %zuB: %s", size, cudaGetErrorName(err));
        return NULL;
    }
    return ptr;
}
static void* cuda_memcpy(void *dst, const void *src, size_t size) {
    cudaMemcpy(dst, src, size, cudaMemcpyDefault);
    cudaDeviceSynchronize();
    return dst;
}
static void cuda_free(void *ptr) {
    cudaFree(ptr);
}
static void cuda_fini() {
    // nothing to be done
}
static allocator_t cuda_allocator = {
    .flags    = ALLOCATOR_DISCRETE,
    .init     = &cuda_init,
    .allocate = &cuda_allocate,
    .memcpy   = &cuda_memcpy,
    .free     = &cuda_free,
    .fini     = &cuda_fini};

#elif defined(HAVE_ROCM)
#include <hip/hip_runtime.h>
static void rocm_init() {
    hipError_t ret = hipInit(0);
    assert(hipSuccess == ret);
    int num_devs = 0;
    ret = hipGetDeviceCount(&num_devs);
    assert(hipSuccess == ret);
    assert(num_devs > 0);
    ret = hipSetDevice(0);
    assert(hipSuccess == ret);
}
static void *rocm_allocate(size_t size, size_t align) {
    (void)align; // ignored
    void *ptr;
    int err;
    if (hipSuccess != (err = hipMalloc(&ptr, size))) {
        fprintf(stderr, "hipMalloc failed to allocate %zuB: %s", size, hipGetErrorName(err));
        return NULL;
    }
    return ptr;
}
static void* rocm_memcpy(void *dst, const void *src, size_t size) {
    hipMemcpy(dst, src, size, hipMemcpyDefault);
    return dst;
}
static void rocm_free(void *ptr) {
    hipFree(ptr);
}
static void rocm_fini() {
    // nothing to be done
}
static allocator_t rocm_allocator = {
    .flags    = ALLOCATOR_DISCRETE,
    .init     = &rocm_init,
    .allocate = &rocm_allocate,
    .memcpy   = &rocm_memcpy,
    .free     = &rocm_free,
    .fini     = &rocm_fini};

#endif

int main(int argc, char **argv)
{
    static void *in_buf = NULL, *inout_buf = NULL, *inout_check_buf = NULL, *init_in_buf = NULL, *init_inout_buf = NULL;
    int count, type_size = 8, rank, size, provided, correctness = 1;
    int repeats = 1, i, c, op1_alignment = 0, res_alignment = 0;
    int max_shift = 4;
    double *duration, tstart, tend;
    allocator_t *allocator = &host_allocator;
    bool check = true;
    char type[5] = "uifd", *op = "sum", *mpi_type;
    int lower = 1, upper = 1000000, skip_op_type;
    MPI_Op mpi_op;

    while (-1 != (c = getopt(argc, argv, "l:u:r:t:o:i:s:n:1:2:d:vfh"))) {
        switch (c) {
        case 'l':
            lower = atoi(optarg);
            if (lower <= 0) {
                fprintf(stderr, "The lower number of elements must be positive\n");
                exit(-1);
            }
            break;
        case 'u':
            upper = atoi(optarg);
            if (lower <= 0) {
                fprintf(stderr, "The upper number of elements must be positive\n");
                exit(-1);
            }
            break;
        case 'i':
            max_shift = atoi(optarg);
            if (max_shift <= 0) {
                fprintf(stderr, "The max shift must be positive\n");
                exit(-1);
            }
            break;
        case 'f':
            check = false;
            break;
        case 'v':
            verbose++;
            break;
        case 'r':
            repeats = atoi(optarg);
            if (repeats <= 0) {
                fprintf(stderr, "The number of repetitions (%d) must be positive\n", repeats);
                exit(-1);
            }
            break;
        case 't':
            for (i = 0; i < (int) strlen(optarg); i++) {
                if (!(('i' == optarg[i]) || ('u' == optarg[i]) || ('f' == optarg[i])
                      || ('d' == optarg[i]))) {
                    fprintf(
                        stderr,
                        "type must be i (signed int), u (unsigned int), f (float) or d (double)\n");
                    exit(-1);
                }
            }
            strncpy(type, optarg, 4);
            break;
        case 'o':
            build_do_ops(optarg, do_ops);
            break;
        case 's':
            type_size = atoi(optarg);
            if (!((8 == type_size) || (16 == type_size) || (32 == type_size)
                  || (64 == type_size))) {
                fprintf(stderr, "type_size must be 8, 16, 32 or 64. %d is an invalid value\n",
                        type_size);
                exit(-1);
            }
            break;
        case '1':
            op1_alignment = atoi(optarg);
            if (op1_alignment < 0) {
                fprintf(stderr, "alignment for the first operand must be positive\n");
                exit(-1);
            }
            break;
        case '2':
            res_alignment = atoi(optarg);
            if (res_alignment < 0) {
                fprintf(stderr, "alignment for the result must be positive\n");
                exit(-1);
            }
            break;
        case 'd':
            if (0 == strncmp("host", optarg, 4)) {
                // default allocator
                break;
            } else
#if defined(HAVE_CUDA)
            if (0 == strncmp("cuda", optarg, 4)) {
                allocator = &cuda_allocator;
                break;
            } else
#elif defined(HAVE_ROCM)
            if (0 == strncmp("rocm", optarg, 4)) {
                allocator = &rocm_allocator;
                break;
            } else
#endif
            {
                fprintf(stderr, "Unsupported allocator: %s\n", optarg);
                // fall-through
            }
        case 'h':
            fprintf(stdout,
                    "%s options are:\n"
                    " -l <number> : lower number of elements\n"
                    " -u <number> : upper number of elements\n"
                    " -s <type_size> : 8, 16, 32 or 64 bits elements\n"
                    " -t [i,u,f,d] : type of the elements to apply the operations on\n"
                    " -r <number> : number of repetitions for each test\n"
                    " -o <op> : comma separated list of operations to execute among\n"
                    "           sum, min, max, prod, bor, bxor, band\n"
                    " -d <memory-space> : host"
#ifdef HAVE_CUDA
                    ", cuda"
#endif
#ifdef HAVE_ROCM
                    ", rocm"
#endif
                    "\n"
                    " -i <number> : shift on all buffers to check alignment\n"
                    " -1 <number> : (mis)alignment in elements for the first op\n"
                    " -2 <number> : (mis)alignment in elements for the result\n"
                    " -v: increase the verbosity level\n"
                    " -f: turn off correctness checks\n"
                    " -h: this help message\n",
                    argv[0]);
            exit(0);
        }
    }

    if (!do_ops_built) { /* not yet done, take the default */
        build_do_ops("all", do_ops);
    }
    ompi_mpi_init(argc, argv, MPI_THREAD_SERIALIZED, &provided, false);

    allocator->init();
    in_buf    = allocator->allocate((upper + op1_alignment) * sizeof(double), 64);
    inout_buf = allocator->allocate((upper + op1_alignment) * sizeof(double), 64);
    init_in_buf = malloc((upper + op1_alignment) * sizeof(double));
    init_inout_buf = malloc((upper + op1_alignment) * sizeof(double));
    duration = (double *) malloc(max_shift * sizeof(double));
    inout_check_buf = malloc(upper * sizeof(double));

    rank = ompi_comm_rank(MPI_COMM_WORLD);
    (void) rank;
    size = ompi_comm_size(MPI_COMM_WORLD);
    (void) size;

    print_header(max_shift);

    for (uint32_t type_idx = 0; type_idx < strlen(type); type_idx++) {
        for (uint32_t op_idx = 0; do_ops[op_idx] >= 0; op_idx++) {
            op = array_of_ops[do_ops[op_idx]].name;
            mpi_op = array_of_ops[do_ops[op_idx]].op;
            skip_op_type = 1;

            for (count = lower; count <= upper; count += count) {
                mpi_type = NULL;
                correctness = 1;
                if ('i' == type[type_idx]) {
                    if (8 == type_size) {
                        int8_t *in_int8 = (int8_t *) ((char *) in_buf
                                                      + op1_alignment * sizeof(int8_t)),
                               *inout_int8 = (int8_t *) ((char *) inout_buf
                                                         + res_alignment * sizeof(int8_t)),
                               *inout_int8_for_check = (int8_t *) inout_check_buf,
                               *init_inout_int8 = (int8_t *)init_inout_buf,
                               *init_in_int8 = (int8_t *)init_in_buf;
                        for (i = 0; i < count; i++) {
                            init_in_int8[i] = 5;
                            init_inout_int8[i] = -3;
                        }
                        mpi_type = "MPI_INT8_T";

                        if (0 == strcmp(op, "sum")) {
                            MPI_OP_TEST(+, mpi_op, MPI_INT8_T, int8_t,
                                        init_in_int8, in_int8,
                                        init_inout_int8, inout_int8,
                                        inout_int8_for_check, count, PRId8);
                        }
                        if (0 == strcmp(op, "bor")) {
                            MPI_OP_TEST(|, mpi_op, MPI_INT8_T, int8_t,
                                        init_in_int8, in_int8,
                                        init_inout_int8, inout_int8,
                                        inout_int8_for_check, count, PRId8);
                        }
                        if (0 == strcmp(op, "bxor")) {
                            MPI_OP_TEST(^, mpi_op, MPI_INT8_T, int8_t,
                                        init_in_int8, in_int8,
                                        init_inout_int8, inout_int8,
                                        inout_int8_for_check, count, PRId8);
                        }
                        if (0 == strcmp(op, "prod")) {
                            MPI_OP_TEST(*, mpi_op, MPI_INT8_T, int8_t,
                                        init_in_int8, in_int8,
                                        init_inout_int8, inout_int8,
                                        inout_int8_for_check, count, PRId8);
                        }
                        if (0 == strcmp(op, "band")) {
                            MPI_OP_TEST(&, mpi_op, MPI_INT8_T, int8_t,
                                        init_in_int8, in_int8,
                                        init_inout_int8, inout_int8,
                                        inout_int8_for_check, count, PRId8);
                        }
                        if (0 == strcmp(op, "max")) {
                            MPI_OP_MINMAX_TEST(max, mpi_op, MPI_INT8_T, int8_t,
                                               init_in_int8, in_int8,
                                               init_inout_int8, inout_int8,
                                               inout_int8_for_check, count, PRId8);
                        }
                        if (0 == strcmp(op, "min")) { // intentionally reversed in and out
                            MPI_OP_MINMAX_TEST(min, mpi_op, MPI_INT8_T, int8_t,
                                               init_in_int8, in_int8,
                                               init_inout_int8, inout_int8,
                                               inout_int8_for_check, count, PRId8);
                        }
                    }
                    if (16 == type_size) {
                        int16_t *in_int16 = (int16_t *) ((char *) in_buf
                                                         + op1_alignment * sizeof(int16_t)),
                                *inout_int16 = (int16_t *) ((char *) inout_buf
                                                            + res_alignment * sizeof(int16_t)),
                                *inout_int16_for_check = (int16_t *) inout_check_buf,
                                *init_inout_int16 = (int16_t *)init_inout_buf,
                                *init_in_int16 = (int16_t *)init_in_buf;
                        for (i = 0; i < count; i++) {
                            init_in_int16[i] = 5;
                            init_inout_int16[i] = -3;
                        }
                        mpi_type = "MPI_INT16_T";

                        if (0 == strcmp(op, "sum")) {
                            MPI_OP_TEST(+, mpi_op, MPI_INT16_T, int16_t,
                                        init_in_int16, in_int16,
                                        init_inout_int16, inout_int16,
                                        inout_int16_for_check, count, PRId16);
                        }
                        if (0 == strcmp(op, "bor")) {
                            MPI_OP_TEST(|, mpi_op, MPI_INT16_T, int16_t,
                                        init_in_int16, in_int16,
                                        init_inout_int16, inout_int16,
                                        inout_int16_for_check, count, PRId16);
                        }
                        if (0 == strcmp(op, "bxor")) {
                            MPI_OP_TEST(^, mpi_op, MPI_INT16_T, int16_t,
                                        init_in_int16, in_int16,
                                        init_inout_int16, inout_int16,
                                        inout_int16_for_check, count, PRId16);
                        }
                        if (0 == strcmp(op, "prod")) {
                            MPI_OP_TEST(*, mpi_op, MPI_INT16_T, int16_t,
                                        init_in_int16, in_int16,
                                        init_inout_int16, inout_int16,
                                        inout_int16_for_check, count, PRId16);
                        }
                        if (0 == strcmp(op, "band")) {
                            MPI_OP_TEST(&, mpi_op, MPI_INT16_T, int16_t,
                                        init_in_int16, in_int16,
                                        init_inout_int16, inout_int16,
                                        inout_int16_for_check, count, PRId16);
                        }
                        if (0 == strcmp(op, "max")) {
                            MPI_OP_MINMAX_TEST(max, mpi_op, MPI_INT16_T, int16_t,
                                               init_in_int16, in_int16,
                                               init_inout_int16, inout_int16,
                                               inout_int16_for_check, count, PRId16);
                        }
                        if (0 == strcmp(op, "min")) { // intentionally reversed in and out
                            MPI_OP_MINMAX_TEST(min, mpi_op, MPI_INT16_T, int16_t,
                                               init_in_int16, in_int16,
                                               init_inout_int16, inout_int16,
                                               inout_int16_for_check, count, PRId16);
                        }
                    }
                    if (32 == type_size) {
                        int32_t *in_int32 = (int32_t *) ((char *) in_buf
                                                         + op1_alignment * sizeof(int32_t)),
                                *inout_int32 = (int32_t *) ((char *) inout_buf
                                                            + res_alignment * sizeof(int32_t)),
                                *inout_int32_for_check = (int32_t *) inout_check_buf,
                                *init_inout_int32 = (int32_t *)init_inout_buf,
                                *init_in_int32 = (int32_t *)init_in_buf;
                        for (i = 0; i < count; i++) {
                            init_in_int32[i] = 5;
                            init_inout_int32[i] = inout_int32_for_check[i] = 3;
                        }
                        mpi_type = "MPI_INT32_T";

                        if (0 == strcmp(op, "sum")) {
                            MPI_OP_TEST(+, mpi_op, MPI_INT32_T, int32_t,
                                        init_in_int32, in_int32,
                                        init_inout_int32, inout_int32,
                                        inout_int32_for_check, count, PRId32);
                        }
                        if (0 == strcmp(op, "bor")) {
                            MPI_OP_TEST(|, mpi_op, MPI_INT32_T, int32_t,
                                        init_in_int32, in_int32,
                                        init_inout_int32, inout_int32,
                                        inout_int32_for_check, count, PRId32);
                        }
                        if (0 == strcmp(op, "bxor")) {
                            MPI_OP_TEST(^, mpi_op, MPI_INT32_T, int32_t,
                                        init_in_int32, in_int32,
                                        init_inout_int32, inout_int32,
                                        inout_int32_for_check, count, PRId32);
                        }
                        if (0 == strcmp(op, "prod")) {
                            MPI_OP_TEST(*, mpi_op, MPI_INT32_T, int32_t,
                                        init_in_int32, in_int32,
                                        init_inout_int32, inout_int32,
                                        inout_int32_for_check, count, PRId32);
                        }
                        if (0 == strcmp(op, "band")) {
                            MPI_OP_TEST(&, mpi_op, MPI_INT32_T, int32_t,
                                        init_in_int32, in_int32,
                                        init_inout_int32, inout_int32,
                                        inout_int32_for_check, count, PRId32);
                        }
                        if (0 == strcmp(op, "max")) {
                            MPI_OP_MINMAX_TEST(max, mpi_op, MPI_INT32_T, int32_t,
                                               init_in_int32, in_int32,
                                               init_inout_int32, inout_int32,
                                               inout_int32_for_check, count, PRId32);
                        }
                        if (0 == strcmp(op, "min")) { // intentionally reversed in and out
                            MPI_OP_MINMAX_TEST(min, mpi_op, MPI_INT32_T, int32_t,
                                               init_in_int32, in_int32,
                                               init_inout_int32, inout_int32,
                                               inout_int32_for_check, count, PRId32);
                        }
                    }
                    if (64 == type_size) {
                        int64_t *in_int64 = (int64_t *) ((char *) in_buf
                                                         + op1_alignment * sizeof(int64_t)),
                                *inout_int64 = (int64_t *) ((char *) inout_buf
                                                            + res_alignment * sizeof(int64_t)),
                                *inout_int64_for_check = (int64_t *) inout_check_buf,
                                *init_inout_int64 = (int64_t *)init_inout_buf,
                                *init_in_int64 = (int64_t *)init_in_buf;
                        for (i = 0; i < count; i++) {
                            init_in_int64[i] = 5;
                            init_inout_int64[i] = 3;
                        }
                        mpi_type = "MPI_INT64_T";

                        if (0 == strcmp(op, "sum")) {
                            MPI_OP_TEST(+, mpi_op, MPI_INT64_T, int64_t,
                                        init_in_int64, in_int64,
                                        init_inout_int64, inout_int64,
                                        inout_int64_for_check, count, PRId64);
                        }
                        if (0 == strcmp(op, "bor")) {
                            MPI_OP_TEST(|, mpi_op, MPI_INT64_T, int64_t,
                                        init_in_int64, in_int64,
                                        init_inout_int64, inout_int64,
                                        inout_int64_for_check, count, PRId64);
                        }
                        if (0 == strcmp(op, "bxor")) {
                            MPI_OP_TEST(^, mpi_op, MPI_INT64_T, int64_t,
                                        init_in_int64, in_int64,
                                        init_inout_int64, inout_int64,
                                        inout_int64_for_check, count, PRId64);
                        }
                        if (0 == strcmp(op, "prod")) {
                            MPI_OP_TEST(*, mpi_op, MPI_INT64_T, int64_t,
                                        init_in_int64, in_int64,
                                        init_inout_int64, inout_int64,
                                        inout_int64_for_check, count, PRId64);
                        }
                        if (0 == strcmp(op, "band")) {
                            MPI_OP_TEST(&, mpi_op, MPI_INT64_T, int64_t,
                                        init_in_int64, in_int64,
                                        init_inout_int64, inout_int64,
                                        inout_int64_for_check, count, PRId64);
                        }
                        if (0 == strcmp(op, "max")) {
                            MPI_OP_MINMAX_TEST(max, mpi_op, MPI_INT64_T, int64_t,
                                               init_in_int64, in_int64,
                                               init_inout_int64, inout_int64,
                                               inout_int64_for_check, count, PRId64);
                        }
                        if (0 == strcmp(op, "min")) { // intentionally reversed in and out
                            MPI_OP_MINMAX_TEST(min, mpi_op, MPI_INT64_T, int64_t,
                                               init_in_int64, in_int64,
                                               init_inout_int64, inout_int64,
                                               inout_int64_for_check, count, PRId64);
                        }
                    }
                }

                if ('u' == type[type_idx]) {
                    if (8 == type_size) {
                        uint8_t *in_uint8 = (uint8_t *) ((char *) in_buf
                                                         + op1_alignment * sizeof(uint8_t)),
                                *inout_uint8 = (uint8_t *) ((char *) inout_buf
                                                            + res_alignment * sizeof(uint8_t)),
                                *inout_uint8_for_check = (uint8_t *) inout_check_buf,
                                *init_inout_uint8 = (uint8_t *)init_inout_buf,
                                *init_in_uint8 = (uint8_t *)init_in_buf;
                        for (i = 0; i < count; i++) {
                            init_in_uint8[i] = 5;
                            init_inout_uint8[i] = 2;
                        }
                        mpi_type = "MPI_UINT8_T";

                        if (0 == strcmp(op, "sum")) {
                            MPI_OP_TEST(+, mpi_op, MPI_UINT8_T, uint8_t,
                                        init_in_uint8, in_uint8,
                                        init_inout_uint8, inout_uint8,
                                        inout_uint8_for_check, count, PRIu8);
                        }
                        if (0 == strcmp(op, "bor")) {
                            MPI_OP_TEST(|, mpi_op, MPI_UINT8_T, uint8_t,
                                        init_in_uint8, in_uint8,
                                        init_inout_uint8, inout_uint8,
                                        inout_uint8_for_check, count, PRIu8);
                        }
                        if (0 == strcmp(op, "bxor")) {
                            MPI_OP_TEST(^, mpi_op, MPI_UINT8_T, uint8_t,
                                        init_in_uint8, in_uint8,
                                        init_inout_uint8, inout_uint8,
                                        inout_uint8_for_check, count, PRIu8);
                        }
                        if (0 == strcmp(op, "prod")) {
                            MPI_OP_TEST(*, mpi_op, MPI_UINT8_T, uint8_t,
                                        init_in_uint8, in_uint8,
                                        init_inout_uint8, inout_uint8,
                                        inout_uint8_for_check, count, PRIu8);
                        }
                        if (0 == strcmp(op, "band")) {
                            MPI_OP_TEST(&, mpi_op, MPI_UINT8_T, uint8_t,
                                        init_in_uint8, in_uint8,
                                        init_inout_uint8, inout_uint8,
                                        inout_uint8_for_check, count, PRIu8);
                        }
                        if (0 == strcmp(op, "max")) {
                            MPI_OP_MINMAX_TEST(max, mpi_op, MPI_UINT8_T, uint8_t,
                                               init_in_uint8, in_uint8,
                                               init_inout_uint8, inout_uint8,
                                               inout_uint8_for_check, count, PRIu8);
                        }
                        if (0 == strcmp(op, "min")) { // intentionally reversed in and out
                            MPI_OP_MINMAX_TEST(min, mpi_op, MPI_UINT8_T, uint8_t,
                                               init_in_uint8, in_uint8,
                                               init_inout_uint8, inout_uint8,
                                               inout_uint8_for_check, count, PRIu8);
                        }
                    }
                    if (16 == type_size) {
                        uint16_t *in_uint16 = (uint16_t *) ((char *) in_buf
                                                            + op1_alignment * sizeof(uint16_t)),
                                 *inout_uint16 = (uint16_t *) ((char *) inout_buf
                                                               + res_alignment * sizeof(uint16_t)),
                                 *inout_uint16_for_check = (uint16_t *) inout_check_buf,
                                 *init_inout_uint16 = (uint16_t *)init_inout_buf,
                                 *init_in_uint16 = (uint16_t *)init_in_buf;
                        for (i = 0; i < count; i++) {
                            init_in_uint16[i] = 5;
                            init_inout_uint16[i] = 1234;
                        }
                        mpi_type = "MPI_UINT16_T";

                        if (0 == strcmp(op, "sum")) {
                            MPI_OP_TEST(+, mpi_op, MPI_UINT16_T, uint16_t,
                                        init_in_uint16, in_uint16,
                                        init_inout_uint16, inout_uint16,
                                        inout_uint16_for_check, count, PRIu16);
                        }
                        if (0 == strcmp(op, "bor")) {
                            MPI_OP_TEST(|, mpi_op, MPI_UINT16_T, uint16_t,
                                        init_in_uint16, in_uint16,
                                        init_inout_uint16, inout_uint16,
                                        inout_uint16_for_check, count, PRIu16);
                        }
                        if (0 == strcmp(op, "bxor")) {
                            MPI_OP_TEST(^, mpi_op, MPI_UINT16_T, uint16_t,
                                        init_in_uint16, in_uint16,
                                        init_inout_uint16, inout_uint16,
                                        inout_uint16_for_check, count, PRIu16);
                        }
                        if (0 == strcmp(op, "prod")) {
                            MPI_OP_TEST(*, mpi_op, MPI_UINT16_T, uint16_t,
                                        init_in_uint16, in_uint16,
                                        init_inout_uint16, inout_uint16,
                                        inout_uint16_for_check, count, PRIu16);
                        }
                        if (0 == strcmp(op, "band")) {
                            MPI_OP_TEST(&, mpi_op, MPI_UINT16_T, uint16_t,
                                        init_in_uint16, in_uint16,
                                        init_inout_uint16, inout_uint16,
                                        inout_uint16_for_check, count, PRIu16);
                        }
                        if (0 == strcmp(op, "max")) {
                            MPI_OP_MINMAX_TEST(max, mpi_op, MPI_UINT16_T, uint16_t,
                                               init_in_uint16, in_uint16,
                                               init_inout_uint16, inout_uint16,
                                               inout_uint16_for_check, count, PRIu16);
                        }
                        if (0 == strcmp(op, "min")) { // intentionally reversed in and out
                            MPI_OP_MINMAX_TEST(min, mpi_op, MPI_UINT16_T, uint16_t,
                                               init_in_uint16, in_uint16,
                                               init_inout_uint16, inout_uint16,
                                               inout_uint16_for_check, count, PRIu16);
                        }
                    }
                    if (32 == type_size) {
                        uint32_t *in_uint32 = (uint32_t *) ((char *) in_buf
                                                            + op1_alignment * sizeof(uint32_t)),
                                 *inout_uint32 = (uint32_t *) ((char *) inout_buf
                                                               + res_alignment * sizeof(uint32_t)),
                                 *inout_uint32_for_check = (uint32_t *) inout_check_buf,
                                 *init_inout_uint32 = (uint32_t *)init_inout_buf,
                                 *init_in_uint32 = (uint32_t *)init_in_buf;
                        for (i = 0; i < count; i++) {
                            init_in_uint32[i] = 5;
                            init_inout_uint32[i] = 3;
                        }
                        mpi_type = "MPI_UINT32_T";

                        if (0 == strcmp(op, "sum")) {
                            MPI_OP_TEST(+, mpi_op, MPI_UINT32_T, uint32_t,
                                        init_in_uint32, in_uint32,
                                        init_inout_uint32, inout_uint32,
                                        inout_uint32_for_check, count, PRIu32);
                        }
                        if (0 == strcmp(op, "bor")) {
                            MPI_OP_TEST(|, mpi_op, MPI_UINT32_T, uint32_t,
                                        init_in_uint32, in_uint32,
                                        init_inout_uint32, inout_uint32,
                                        inout_uint32_for_check, count, PRIu32);
                        }
                        if (0 == strcmp(op, "bxor")) {
                            MPI_OP_TEST(^, mpi_op, MPI_UINT32_T, uint32_t,
                                        init_in_uint32, in_uint32,
                                        init_inout_uint32, inout_uint32,
                                        inout_uint32_for_check, count, PRIu32);
                        }
                        if (0 == strcmp(op, "prod")) {
                            MPI_OP_TEST(*, mpi_op, MPI_UINT32_T, uint32_t,
                                        init_in_uint32, in_uint32,
                                        init_inout_uint32, inout_uint32,
                                        inout_uint32_for_check, count, PRIu32);
                        }
                        if (0 == strcmp(op, "band")) {
                            MPI_OP_TEST(&, mpi_op, MPI_UINT32_T, uint32_t,
                                        init_in_uint32, in_uint32,
                                        init_inout_uint32, inout_uint32,
                                        inout_uint32_for_check, count, PRIu32);
                        }
                        if (0 == strcmp(op, "max")) {
                            MPI_OP_MINMAX_TEST(max, mpi_op, MPI_UINT32_T, uint32_t,
                                               init_in_uint32, in_uint32,
                                               init_inout_uint32, inout_uint32,
                                               inout_uint32_for_check, count, PRIu32);
                        }
                        if (0 == strcmp(op, "min")) { // intentionally reversed in and out
                            MPI_OP_MINMAX_TEST(min, mpi_op, MPI_UINT32_T, uint32_t,
                                               init_in_uint32, in_uint32,
                                               init_inout_uint32, inout_uint32,
                                               inout_uint32_for_check, count, PRIu32);
                        }
                    }
                    if (64 == type_size) {
                        uint64_t *in_uint64 = (uint64_t *) ((char *) in_buf
                                                            + op1_alignment * sizeof(uint64_t)),
                                 *inout_uint64 = (uint64_t *) ((char *) inout_buf
                                                               + res_alignment * sizeof(uint64_t)),
                                 *inout_uint64_for_check = (uint64_t *) inout_check_buf,
                                 *init_inout_uint64 = (uint64_t *)init_inout_buf,
                                 *init_in_uint64 = (uint64_t *)init_in_buf;
                        for (i = 0; i < count; i++) {
                            init_in_uint64[i] = 5;
                            init_inout_uint64[i] = 32433;
                        }
                        mpi_type = "MPI_UINT64_T";

                        if (0 == strcmp(op, "sum")) {
                            MPI_OP_TEST(+, mpi_op, MPI_UINT64_T, uint64_t,
                                        init_in_uint64, in_uint64,
                                        init_inout_uint64, inout_uint64,
                                        inout_uint64_for_check, count, PRIu64);
                        }
                        if (0 == strcmp(op, "bor")) {
                            MPI_OP_TEST(|, mpi_op, MPI_UINT64_T, uint64_t,
                                        init_in_uint64, in_uint64,
                                        init_inout_uint64, inout_uint64,
                                        inout_uint64_for_check, count, PRIu64);
                        }
                        if (0 == strcmp(op, "bxor")) {
                            MPI_OP_TEST(^, mpi_op, MPI_UINT64_T, uint64_t,
                                        init_in_uint64, in_uint64,
                                        init_inout_uint64, inout_uint64,
                                        inout_uint64_for_check, count, PRIu64);
                        }
                        if (0 == strcmp(op, "prod")) {
                            MPI_OP_TEST(*, mpi_op, MPI_UINT64_T, uint64_t,
                                        init_in_uint64, in_uint64,
                                        init_inout_uint64, inout_uint64,
                                        inout_uint64_for_check, count, PRIu64);
                        }
                        if (0 == strcmp(op, "band")) {
                            MPI_OP_TEST(&, mpi_op, MPI_UINT64_T, uint64_t,
                                        init_in_uint64, in_uint64,
                                        init_inout_uint64, inout_uint64,
                                        inout_uint64_for_check, count, PRIu64);
                        }
                        if (0 == strcmp(op, "max")) {
                            MPI_OP_MINMAX_TEST(max, mpi_op, MPI_UINT64_T, uint64_t,
                                               init_in_uint64, in_uint64,
                                               init_inout_uint64, inout_uint64,
                                               inout_uint64_for_check, count, PRIu64);
                        }
                        if (0 == strcmp(op, "min")) {
                            MPI_OP_MINMAX_TEST(min, mpi_op, MPI_UINT64_T, uint64_t,
                                               init_in_uint64, in_uint64,
                                               init_inout_uint64, inout_uint64,
                                               inout_uint64_for_check, count, PRIu64);
                        }
                    }
                }

                if ('f' == type[type_idx]) {
                    float *in_float = (float *) ((char *) in_buf + op1_alignment * sizeof(float)),
                          *inout_float = (float *) ((char *) inout_buf
                                                    + res_alignment * sizeof(float)),
                          *inout_float_for_check = (float *) inout_check_buf,
                          *init_inout_float = (float *)init_inout_buf,
                          *init_in_float = (float *)init_in_buf;
                    for (i = 0; i < count; i++) {
                        init_in_float[i] = 1000.0 + 1;
                        init_inout_float[i] = 100.0 + 2;
                    }
                    mpi_type = "MPI_FLOAT";

                    if (0 == strcmp(op, "sum")) {
                        MPI_OP_TEST(+, mpi_op, MPI_FLOAT, float,
                                    init_in_float, in_float,
                                    init_inout_float, inout_float,
                                    inout_float_for_check, count, "f");
                    }
                    if (0 == strcmp(op, "prod")) {
                        MPI_OP_TEST(*, mpi_op, MPI_FLOAT, float,
                                    init_in_float, in_float,
                                    init_inout_float, inout_float,
                                    inout_float_for_check, count, "f");
                    }
                    if (0 == strcmp(op, "max")) {
                        MPI_OP_MINMAX_TEST(max, mpi_op, MPI_FLOAT, float,
                                           init_in_float, in_float,
                                           init_inout_float, inout_float,
                                           inout_float_for_check, count, "f");
                    }
                    if (0 == strcmp(op, "min")) {
                        MPI_OP_MINMAX_TEST(min, mpi_op, MPI_FLOAT, float,
                                           init_in_float, in_float,
                                           init_inout_float, inout_float,
                                           inout_float_for_check, count, "f");
                    }
                }

                if ('d' == type[type_idx]) {
                    double *in_double = (double *) ((char *) in_buf
                                                    + op1_alignment * sizeof(double)),
                           *inout_double = (double *) ((char *) inout_buf
                                                       + res_alignment * sizeof(double)),
                           *inout_double_for_check = (double *) inout_check_buf,
                           *init_inout_double = (double *)init_inout_buf,
                           *init_in_double = (double *)init_in_buf;
                    for (i = 0; i < count; i++) {
                        init_in_double[i] = 10.0 + 1;
                        init_inout_double[i] = 1.0 + 2;
                    }
                    mpi_type = "MPI_DOUBLE";

                    if (0 == strcmp(op, "sum")) {
                        MPI_OP_TEST(+, mpi_op, MPI_DOUBLE, double,
                                    init_in_double, in_double,
                                    init_inout_double, inout_double,
                                    inout_double_for_check, count, "g");
                    }
                    if (0 == strcmp(op, "prod")) {
                        MPI_OP_TEST(*, mpi_op, MPI_DOUBLE, double,
                                    init_in_double, in_double,
                                    init_inout_double, inout_double,
                                    inout_double_for_check, count, "f");
                    }
                    if (0 == strcmp(op, "max")) {
                        MPI_OP_MINMAX_TEST(max, mpi_op, MPI_DOUBLE, double,
                                           init_in_double, in_double,
                                           init_inout_double, inout_double,
                                           inout_double_for_check, count, "f");
                    }
                    if (0 == strcmp(op, "min")) {
                        MPI_OP_MINMAX_TEST(min, mpi_op, MPI_DOUBLE, double,
                                           init_in_double, in_double,
                                           init_inout_double, inout_double,
                                           inout_double_for_check, count, "f");
                    }
                }
            check_and_continue:
                if (!skip_op_type)
                    print_status(array_of_ops[do_ops[op_idx]].mpi_op_name, mpi_type, type_size,
                                 count, max_shift, duration, repeats, correctness);
            }
            if (!skip_op_type)
                printf("\n");
        }
    }

    /* clean up allocator */
    allocator->free(in_buf);
    allocator->free(inout_buf);
    allocator->free(inout_check_buf);
    allocator->fini();

    if (allocator->flags & ALLOCATOR_DISCRETE) {
        free(init_in_buf);
        free(init_inout_buf);
    }

    ompi_mpi_finalize();

    return (0 == total_errors) ? 0 : -1;
}
