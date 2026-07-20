/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype_prototypes.h"

/*
 * The reference pack/unpack interpreters at the bottom of this file were relocated here from the
 * production datatype engine (opal_datatype_pack.c / opal_datatype_unpack.c) so the shipped library
 * no longer carries an A/B baseline that only this benchmark uses. They lean on the same internal
 * descriptor macros and inline helpers as the production interpreters, which pulls in the
 * convertor-internal, memcpy, and accelerator headers below. DO_DEBUG must be defined before the
 * pack/unpack headers because their inline helpers expand it; the benchmark does not want the debug
 * tracing, so it is a no-op.
 */
#include <assert.h>
#include <string.h>

#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_memcpy.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/accelerator/base/base.h"

#ifndef DO_DEBUG
#    define DO_DEBUG(INST)
#endif

#include "opal/datatype/opal_datatype_pack.h"
#include "opal/datatype/opal_datatype_unpack.h"

#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Keep implementation-specific descriptor experiments out of to_self, whose datatypes are valid
 * public MPI constructions. This benchmark intentionally replaces opt_desc so exact DATA and loop
 * shapes can be compared without relying on the commit-time optimizer to produce either shape.
 */
typedef enum {
    BENCHMARK_BACKEND_MPI,
    BENCHMARK_BACKEND_CURRENT,
    BENCHMARK_BACKEND_REFERENCE,
    BENCHMARK_BACKEND_ACCELERATOR,
    BENCHMARK_BACKEND_GENERAL,
} benchmark_backend_t;

typedef struct {
    int data_count;
    int blocklen;
    int element_size;
    int block_gap;
    int item_gap;
    int total_items;
    int loop_items;
    int datatype_count;
    int cycles;
    int trials;
    int warmups;
    int repetitions;
    size_t min_work_bytes;
    size_t fragment_bytes;
    benchmark_backend_t backend;
    int commit_description;
    int dump;
    int unpack;
} benchmark_config_t;

static void print_usage(const char *name)
{
    fprintf(stderr,
            "Usage: %s [--data-count=N] [--blocklen=N] [--element-size=4|8]\n"
            "          [--block-gap=N] [--item-gap=N]\n"
            "          [--total-items=N] [--loop-items=N]\n"
            "          [--datatype-count=N] [--cycles=N] [--trials=N] [--warmups=N]\n"
            "          [--repetitions=N] [--min-work-bytes=N] [--operation=pack|unpack]\n"
            "          [--backend=mpi|current|reference|accelerator|general]\n"
            "          [--fragment-bytes=N]\n"
            "          [--commit-description] [--dump]\n",
            name);
}

/* Parse one positive integer option while rejecting overflow and trailing characters. */
static int parse_int(const char *option, const char *value, int minimum, int *result)
{
    char *end = NULL;
    long parsed;

    errno = 0;
    parsed = strtol(value, &end, 10);
    if ((0 != errno) || (value == end) || ('\0' != *end) || (minimum > parsed) || (INT_MAX < parsed)) {
        fprintf(stderr, "%s requires an integer greater than or equal to %d\n", option, minimum);
        return -1;
    }
    *result = (int) parsed;
    return 0;
}

/* Parse a byte count independently from int-sized MPI count arguments. */
static int parse_size(const char *option, const char *value, size_t *result)
{
    char *end = NULL;
    unsigned long long parsed;

    errno = 0;
    parsed = strtoull(value, &end, 10);
    if ((0 != errno) || ('-' == value[0]) || (value == end) || ('\0' != *end) || (SIZE_MAX < parsed)) {
        fprintf(stderr, "%s requires a nonnegative byte count\n", option);
        return -1;
    }
    *result = (size_t) parsed;
    return 0;
}

/* Parse benchmark controls without allowing ambiguous positional arguments. */
static int parse_options(int argc, char **argv, benchmark_config_t *config)
{
    for (int i = 1; i < argc; ++i) {
#define PARSE_INT_OPTION(NAME, FIELD, MINIMUM)                                                   \
    if (0 == strncmp(argv[i], "--" NAME "=", sizeof("--" NAME "=") - 1)) {                    \
        if (0 != parse_int("--" NAME, argv[i] + sizeof("--" NAME "=") - 1, MINIMUM, &config->FIELD)) { \
            return -1;                                                                           \
        }                                                                                         \
        continue;                                                                                 \
    }

        PARSE_INT_OPTION("data-count", data_count, 1)
        PARSE_INT_OPTION("blocklen", blocklen, 1)
        PARSE_INT_OPTION("element-size", element_size, 1)
        PARSE_INT_OPTION("block-gap", block_gap, 0)
        PARSE_INT_OPTION("item-gap", item_gap, 0)
        PARSE_INT_OPTION("total-items", total_items, 1)
        PARSE_INT_OPTION("loop-items", loop_items, 1)
        PARSE_INT_OPTION("datatype-count", datatype_count, 1)
        PARSE_INT_OPTION("cycles", cycles, 1)
        PARSE_INT_OPTION("trials", trials, 2)
        PARSE_INT_OPTION("warmups", warmups, 0)
        PARSE_INT_OPTION("repetitions", repetitions, 1)

#undef PARSE_INT_OPTION

        if (0 == strncmp(argv[i], "--min-work-bytes=", sizeof("--min-work-bytes=") - 1)) {
            if (0 != parse_size("--min-work-bytes", argv[i] + sizeof("--min-work-bytes=") - 1,
                                &config->min_work_bytes)) {
                return -1;
            }
            continue;
        }
        if (0 == strncmp(argv[i], "--fragment-bytes=", sizeof("--fragment-bytes=") - 1)) {
            if (0 != parse_size("--fragment-bytes", argv[i] + sizeof("--fragment-bytes=") - 1,
                                &config->fragment_bytes)) {
                return -1;
            }
            continue;
        }
        if (0 == strcmp(argv[i], "--dump")) {
            config->dump = 1;
            continue;
        }
        if (0 == strcmp(argv[i], "--commit-description")) {
            config->commit_description = 1;
            continue;
        }
        if (0 == strcmp(argv[i], "--operation=pack")) {
            config->unpack = 0;
            continue;
        }
        if (0 == strcmp(argv[i], "--operation=unpack")) {
            config->unpack = 1;
            continue;
        }
        if (0 == strcmp(argv[i], "--backend=mpi")) {
            config->backend = BENCHMARK_BACKEND_MPI;
            continue;
        }
        if (0 == strcmp(argv[i], "--backend=current")) {
            config->backend = BENCHMARK_BACKEND_CURRENT;
            continue;
        }
        if (0 == strcmp(argv[i], "--backend=reference")) {
            config->backend = BENCHMARK_BACKEND_REFERENCE;
            continue;
        }
        if (0 == strcmp(argv[i], "--backend=accelerator")) {
            config->backend = BENCHMARK_BACKEND_ACCELERATOR;
            continue;
        }
        if (0 == strcmp(argv[i], "--backend=general")) {
            config->backend = BENCHMARK_BACKEND_GENERAL;
            continue;
        }
        if ((0 == strcmp(argv[i], "--help")) || (0 == strcmp(argv[i], "-h"))) {
            print_usage(argv[0]);
            return 1;
        }
        fprintf(stderr, "Unknown option: %s\n", argv[i]);
        return -1;
    }

    if (config->total_items < config->loop_items) {
        fprintf(stderr, "--loop-items cannot exceed --total-items\n");
        return -1;
    }
    if ((4 != config->element_size) && (8 != config->element_size)) {
        fprintf(stderr, "--element-size must be 4 or 8\n");
        return -1;
    }
    if (config->commit_description && (0 != config->total_items % config->loop_items)) {
        fprintf(stderr, "--loop-items must divide --total-items with --commit-description\n");
        return -1;
    }
    if ((BENCHMARK_BACKEND_MPI == config->backend) && (0 != config->fragment_bytes)) {
        fprintf(stderr,
                "--fragment-bytes requires the current, reference, accelerator, or general "
                "backend\n");
        return -1;
    }
    if (!config->unpack && (0 != config->fragment_bytes)
        && (config->fragment_bytes < (size_t) config->element_size)) {
        /* The packer controls its own output boundaries and never splits a predefined element: it
         * packs whole elements and reports a short length for the caller to handle. An output
         * fragment smaller than a single element therefore cannot make progress (max_data would be
         * 0), which no real PML/BTL ever requests. Reject it up front with a clear message rather
         * than letting the run fail later as a generic validation error. Unpack has no such limit:
         * the network chooses the boundaries, so a partial element is expected and handled. */
        fprintf(stderr,
                "--fragment-bytes for pack must be at least one element (%d bytes); the packer "
                "never splits a predefined element\n",
                config->element_size);
        return -1;
    }
    return 0;
}

/* Install one exact synthetic DATA entry without CREATE_ELEM's contiguous-count normalization. */
static void create_synthetic_data(dt_elem_desc_t *description, size_t blocklen, uint32_t count,
                                  ptrdiff_t displacement, ptrdiff_t extent, uint16_t type)
{
    description->elem.common.flags = OPAL_DATATYPE_FLAG_BASIC | OPAL_DATATYPE_FLAG_DATA;
    description->elem.common.type = type;
    description->elem.blocklen = blocklen;
    description->elem.count = count;
    description->elem.extent = extent;
    description->elem.disp = displacement;
}

/*
 * Replace the constructor-generated optimized description with the exact requested loop shape.
 * Complete groups are placed inside the loop; any group that is too short for the requested
 * unrolling factor remains as straight-line DATA entries after the loop.
 */
static int install_synthetic_description(MPI_Datatype datatype, const benchmark_config_t *config,
                                         ptrdiff_t block_stride, ptrdiff_t item_extent)
{
    opal_datatype_t *opal_type = &datatype->super;
    const uint32_t iterations = (uint32_t) (config->total_items / config->loop_items);
    const int tail_items = config->total_items % config->loop_items;
    const size_t loop_end = (size_t) config->loop_items + 1;
    const size_t tail_start = loop_end + 1;
    const size_t used = tail_start + (size_t) tail_items;
    const size_t loop_size = (size_t) config->loop_items * config->data_count * config->blocklen
                             * (size_t) config->element_size;
    const size_t datatype_size = (size_t) config->total_items * config->data_count
                                 * config->blocklen * (size_t) config->element_size;
    const ptrdiff_t loop_extent = item_extent * config->loop_items;
    const uint16_t type = (8 == config->element_size) ? OPAL_DATATYPE_FLOAT8 : OPAL_DATATYPE_FLOAT4;
    dt_elem_desc_t *description = (dt_elem_desc_t *) calloc(used + 1, sizeof(*description));

    if (NULL == description) {
        return MPI_ERR_NO_MEM;
    }

    CREATE_LOOP_START(&description[0], iterations, config->loop_items + 1, loop_extent, 0);
    for (int item = 0; item < config->loop_items; ++item) {
        create_synthetic_data(&description[item + 1], config->blocklen, config->data_count,
                              item_extent * item, block_stride, type);
    }
    CREATE_LOOP_END(&description[loop_end], config->loop_items + 1, 0, loop_size, 0);
    for (int item = 0; item < tail_items; ++item) {
        const ptrdiff_t displacement = loop_extent * iterations + item_extent * item;

        create_synthetic_data(&description[tail_start + item], config->blocklen, config->data_count,
                              displacement, block_stride, type);
    }
    CREATE_LOOP_END(&description[used], used, 0, datatype_size, 0);

    if ((NULL != opal_type->opt_desc.desc) && (opal_type->opt_desc.desc != opal_type->desc.desc)) {
        free(opal_type->opt_desc.desc);
    }
    opal_type->opt_desc.desc = description;
    opal_type->opt_desc.length = used;
    opal_type->opt_desc.used = used;
    if (2 > opal_type->loops) {
        opal_type->loops = 2;
    }
    return MPI_SUCCESS;
}

/*
 * Build the regular FLOAT4 typemap. Block and item gaps are independent so equivalent contiguous
 * regions can be represented by different count/blocklen pairs while a trailing item gap keeps
 * the datatype noncontiguous. The commit-description mode retains the optimizer's result; the
 * synthetic mode replaces it with the exact requested shape.
 */
static int create_synthetic_datatype(const benchmark_config_t *config, MPI_Datatype *datatype)
{
    int block_stride_elements;
    ptrdiff_t block_stride, item_extent;
    MPI_Datatype base_type = (8 == config->element_size) ? MPI_DOUBLE : MPI_FLOAT;
    MPI_Datatype vector = MPI_DATATYPE_NULL;
    MPI_Datatype record = MPI_DATATYPE_NULL;
    MPI_Datatype resized_record = MPI_DATATYPE_NULL;
    int rc;

    /*
     * The commit-description path forces a trailing gap (below) so construction cannot fold every
     * item into a single vector. The synthetic path has no such protection: a fully contiguous
     * datatype makes OPAL_CONVERTOR_PREPARE short-circuit to the memcpy fast path so the synthetic
     * opt_desc installed later is never interpreted. Reject that degenerate shape rather than
     * silently benchmarking a memcpy while claiming a loop/tail shape.
     *
     * The gap must be *effective*: block_gap sits between the blocks of an item, so it only breaks
     * contiguity when there is more than one block (data_count > 1) -- with data_count == 1 the
     * block gap is multiplied by (data_count - 1) == 0 below and has no effect. Contiguity is
     * therefore broken only by a nonzero item_gap, or by a nonzero block_gap with data_count > 1.
     */
    int fully_contiguous = (0 == config->item_gap)
                           && ((config->data_count <= 1) || (0 == config->block_gap));
    if (!config->commit_description && fully_contiguous) {
        fprintf(stderr, "the synthetic backend requires an effective gap (--item-gap, or "
                        "--block-gap with --data-count > 1) so the datatype is not fully "
                        "contiguous\n");
        return MPI_ERR_ARG;
    }

    if ((INT_MAX - config->block_gap < config->blocklen)
        || ((0 != config->data_count) && (INT_MAX / config->data_count < config->total_items))) {
        fprintf(stderr, "The requested signature exceeds MPI int count limits\n");
        return MPI_ERR_COUNT;
    }
    block_stride_elements = config->blocklen + config->block_gap;
#if PTRDIFF_MAX <= INT_MAX
    if ((PTRDIFF_MAX / config->element_size < block_stride_elements)
        || (PTRDIFF_MAX / config->element_size < config->blocklen)
        || (PTRDIFF_MAX / config->element_size < config->item_gap)) {
        fprintf(stderr, "The requested signature exceeds address displacement limits\n");
        return MPI_ERR_COUNT;
    }
#endif
    block_stride = (ptrdiff_t) block_stride_elements * config->element_size;
    item_extent = (ptrdiff_t) config->blocklen * config->element_size;
    if ((1 < config->data_count)
        && (PTRDIFF_MAX - item_extent) / block_stride < (ptrdiff_t) config->data_count - 1) {
        fprintf(stderr, "The requested signature exceeds address displacement limits\n");
        return MPI_ERR_COUNT;
    }
    item_extent += ((ptrdiff_t) config->data_count - 1) * block_stride;
    if (PTRDIFF_MAX - item_extent
        < (ptrdiff_t) config->item_gap * config->element_size) {
        fprintf(stderr, "The requested signature exceeds address displacement limits\n");
        return MPI_ERR_COUNT;
    }
    item_extent += (ptrdiff_t) config->item_gap * config->element_size;
    if (config->commit_description) {
        /* Keep a trailing gap so datatype construction cannot fold all items into one vector. */
        if (PTRDIFF_MAX - item_extent < config->element_size) {
            fprintf(stderr, "The requested signature exceeds address displacement limits\n");
            return MPI_ERR_COUNT;
        }
        item_extent += config->element_size;
    }
    if (PTRDIFF_MAX / item_extent < config->total_items) {
        fprintf(stderr, "The requested datatype extent is too large\n");
        return MPI_ERR_COUNT;
    }
    rc = MPI_Type_vector(config->data_count, config->blocklen, block_stride_elements, base_type,
                         &vector);
    if (MPI_SUCCESS != rc) {
        return rc;
    }
    if (config->commit_description) {
        int *blocklengths = (int *) malloc((size_t) config->loop_items * sizeof(*blocklengths));
        MPI_Aint *displacements =
            (MPI_Aint *) malloc((size_t) config->loop_items * sizeof(*displacements));
        MPI_Datatype *types = (MPI_Datatype *) malloc((size_t) config->loop_items * sizeof(*types));

        if ((NULL == blocklengths) || (NULL == displacements) || (NULL == types)) {
            free(types);
            free(displacements);
            free(blocklengths);
            MPI_Type_free(&vector);
            return MPI_ERR_NO_MEM;
        }
        for (int item = 0; item < config->loop_items; ++item) {
            blocklengths[item] = 1;
            displacements[item] = (MPI_Aint) item * item_extent;
            types[item] = vector;
        }
        rc = MPI_Type_create_struct(config->loop_items, blocklengths, displacements, types, &record);
        free(types);
        free(displacements);
        free(blocklengths);
        MPI_Type_free(&vector);
        if (MPI_SUCCESS != rc) {
            return rc;
        }
        rc = MPI_Type_create_resized(record, 0, (MPI_Aint) config->loop_items * item_extent,
                                     &resized_record);
        MPI_Type_free(&record);
        if (MPI_SUCCESS != rc) {
            return rc;
        }
        rc = MPI_Type_contiguous(config->total_items / config->loop_items, resized_record, datatype);
        MPI_Type_free(&resized_record);
    } else {
        rc = MPI_Type_create_resized(vector, 0, item_extent, &resized_record);
        MPI_Type_free(&vector);
        if (MPI_SUCCESS != rc) {
            return rc;
        }
        rc = MPI_Type_contiguous(config->total_items, resized_record, datatype);
        MPI_Type_free(&resized_record);
    }
    if (MPI_SUCCESS != rc) {
        return rc;
    }
    rc = MPI_Type_commit(datatype);
    if (MPI_SUCCESS != rc) {
        MPI_Type_free(datatype);
        return rc;
    }
    if (!config->commit_description) {
        rc = install_synthetic_description(*datatype, config, block_stride, item_extent);
        if (MPI_SUCCESS != rc) {
            MPI_Type_free(datatype);
        }
    }
    return rc;
}

/* Produce the expected packed byte stream independently of the convertor descriptor traversal. */
static void pack_reference(unsigned char *packed, const unsigned char *source,
                           const benchmark_config_t *config, MPI_Aint datatype_extent)
{
    const size_t block_bytes = (size_t) config->blocklen * config->element_size;
    const size_t block_stride = (size_t) (config->blocklen + config->block_gap)
                                * config->element_size;
    const size_t item_extent = (size_t) datatype_extent / config->total_items;
    size_t packed_offset = 0;

    for (int datatype_index = 0; datatype_index < config->datatype_count; ++datatype_index) {
        const unsigned char *base = source + datatype_index * datatype_extent;

        for (int item = 0; item < config->total_items; ++item) {
            for (int block = 0; block < config->data_count; ++block) {
                const size_t source_offset = (size_t) item * item_extent + block * block_stride;

                memcpy(packed + packed_offset, base + source_offset, block_bytes);
                packed_offset += block_bytes;
            }
        }
    }
}

/* Compute stable summary statistics without discarding variability needed by the sweep analysis. */
static void summarize(const double *samples, int count, double *mean, double *standard_deviation,
                      double *minimum, double *maximum)
{
    double square_sum = 0.0;

    *mean = 0.0;
    *minimum = samples[0];
    *maximum = samples[0];
    for (int i = 0; i < count; ++i) {
        *mean += samples[i];
        if (samples[i] < *minimum) {
            *minimum = samples[i];
        }
        if (samples[i] > *maximum) {
            *maximum = samples[i];
        }
    }
    *mean /= count;
    for (int i = 0; i < count; ++i) {
        const double difference = samples[i] - *mean;

        square_sum += difference * difference;
    }
    *standard_deviation = sqrt(square_sum / (count - 1));
}

/* Return the stable output name used by the tuning driver. */
static const char *benchmark_backend_name(benchmark_backend_t backend)
{
    switch (backend) {
    case BENCHMARK_BACKEND_MPI:
        return "mpi";
    case BENCHMARK_BACKEND_CURRENT:
        return "current";
    case BENCHMARK_BACKEND_REFERENCE:
        return "reference";
    case BENCHMARK_BACKEND_ACCELERATOR:
        return "accelerator";
    case BENCHMARK_BACKEND_GENERAL:
        return "general";
    }
    return "unknown";
}

/*
 * Reference homogeneous pack interpreter. This is the pre-typed-mover production packer, preserved
 * here purely as an A/B baseline for the "reference" backend. It is intentionally byte-for-byte the
 * old descriptor interpreter and must not diverge from opal_generic_inlined_pack's semantics.
 */
static int32_t sweep_reference_pack(opal_convertor_t *pConvertor, struct iovec *iov,
                                    uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack;      /* pointer to the position on the stack */
    uint32_t pos_desc;       /* actual position in the description of the derived datatype */
    size_t count_desc;       /* the number of items already done in the actual pos_desc */
    size_t total_packed = 0; /* total amount packed this time */
    dt_elem_desc_t *description;
    dt_elem_desc_t *pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    size_t iov_len_local;
    ptrdiff_t local_disp;
    uint32_t iov_count;

    DO_DEBUG(opal_output(0, "opal_convertor_generic_inlined_pack( %p:%p, {%p, %lu}, %d )\n",
                         (void *) pConvertor, (void *) pConvertor->pBaseBuf,
                         (void *) iov[0].iov_base, (unsigned long) iov[0].iov_len, *out_size););

    description = pConvertor->use_desc->desc;

    /* The first step adds both displacements to the source. Subsequent descriptor transitions
     * restore conv_ptr from the stack because conversion can stop in the middle of a DATA entry. */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc = pStack->index;
    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    DO_DEBUG(opal_output(0,
                         "pack start pos_desc %d count_desc %" PRIsize_t " disp %ld\n"
                         "stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pos_desc, count_desc, (long) (conv_ptr - pConvertor->pBaseBuf),
                         pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp););

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;

        if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            if (((size_t) pElem->elem.count * pElem->elem.blocklen) != count_desc) {
                /* we have a partial (less than blocklen) basic datatype */
                int rc = PACK_PARTIAL_BLOCKLEN(pConvertor, pElem, count_desc, conv_ptr, iov_ptr,
                                               iov_len_local);
                if (0 == rc) { /* not done */
                    goto complete_loop;
                }
                if (0 == count_desc) {
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++; /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                             process_loop, process_end_loop);
                    goto process_data;
                }
            }
        }

        /* Keep the per-iovec entry structured. Direct jumps here cause the compiler to reshape
         * the complete interpreter and regress otherwise unrelated datatype layouts. */
        while (1) {
            if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            process_data:
                /* Pack one DATA descriptor. A partial output buffer exits with count_desc
                 * preserving the exact position. */
                PACK_PREDEFINED_DATATYPE(pConvertor, pElem, count_desc, conv_ptr, iov_ptr,
                                         iov_len_local);
                if (0 != count_desc) {
                    goto complete_loop;
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;
                /* Keep dispatch local to each transition. Sharing it through another label would
                 * add an unconditional branch before the descriptor-type branch. */
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) {
            process_end_loop:
                DO_DEBUG(opal_output(0,
                                     "pack end_loop count %" PRIsize_t " stack_pos %d"
                                     " pos_desc %d disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
                if (--(pStack->count) == 0) { /* end of loop */
                    if (0 == pConvertor->stack_pos) {
                        /* we're done. Force the exit of the main for loop (around iovec) */
                        *out_size = iov_count;
                        goto complete_loop;
                    }
                    pConvertor->stack_pos--; /* go one position up on the stack */
                    pStack--;
                    pos_desc++; /* and move to the next element */
                } else {
                    pos_desc = pStack->index + 1; /* jump back to the beginning of the loop */
                    if (pStack->index == -1) {    /* If it's the datatype count loop */
                        pStack->disp += (pData->ub - pData->lb); /* jump by the datatype extent */
                    } else {
                        assert(OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type);
                        pStack->disp += description[pStack->index].loop.extent; /* jump by the loop extent */
                    }
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DO_DEBUG(opal_output(0,
                                     "pack new_loop count %" PRIsize_t " stack_pos %d pos_desc %d"
                                     " disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
            process_loop:
                local_disp = (ptrdiff_t) conv_ptr;
                if (pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                    PACK_CONTIGUOUS_LOOP(pConvertor, pElem, count_desc, conv_ptr, iov_ptr,
                                         iov_len_local);
                    if (0 == count_desc) { /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                local_disp = (ptrdiff_t) conv_ptr - local_disp;
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                           pStack->disp + local_disp);
                pos_desc++;
            update_loop_description:
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos, &description[pos_desc],
                               "advance loop");
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
        }

    complete_loop:
        iov[iov_count].iov_len -= iov_len_local; /* update the amount of valid data */
        total_packed += iov[iov_count].iov_len;
    }
    *max_data = total_packed;
    pConvertor->bConverted += total_packed; /* update the already converted bytes */
    *out_size = iov_count;
    if (pConvertor->bConverted == pConvertor->remote_size) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* Save the global position for the next round */
    PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
               conv_ptr - pConvertor->pBaseBuf);
    DO_DEBUG(opal_output(0,
                         "pack save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp););
    return 0;
}

/*
 * Reference homogeneous unpack interpreter. This is the pre-typed-mover production unpacker,
 * preserved here purely as an A/B baseline for the "reference" backend. It must not diverge from
 * opal_generic_inlined_unpack's semantics; it shares opal_unpack_partial_predefined (now in
 * opal_datatype_unpack.h) with the production unpacker.
 */
static int32_t sweep_reference_unpack(opal_convertor_t *pConvertor, struct iovec *iov,
                                      uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack;        /* pointer to the position on the stack */
    uint32_t pos_desc;         /* actual position in the description of the derived datatype */
    size_t count_desc;         /* the number of items already done in the actual pos_desc */
    size_t total_unpacked = 0; /* total size unpacked this time */
    dt_elem_desc_t *description;
    dt_elem_desc_t *pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    size_t iov_len_local;
    ptrdiff_t local_disp;
    uint32_t iov_count;

    DO_DEBUG( opal_output( 0, "opal_convertor_generic_inlined_unpack( %p, iov[%u] = {%p, %lu} )\n",
                           (void*)pConvertor, *out_size, (void*)iov[0].iov_base, (unsigned long)iov[0].iov_len ); );

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the source_base to the correct value. This is
     * due to the fact that the convertor can stop in the middle of a data with a count
     */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc = pStack->index;
    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    DO_DEBUG(opal_output(0,
                         "unpack start pos_desc %d count_desc %" PRIsize_t " disp %ld\n"
                         "stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pos_desc, count_desc, (long) (conv_ptr - pConvertor->pBaseBuf),
                         pConvertor->stack_pos, pStack->index, pStack->count,
                         (long) (pStack->disp)););

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;

        /* Deal with all types of partial predefined datatype unpacking, including when
         * unpacking a partial predefined element and when unpacking a part smaller than
         * the blocklen.
         */
        if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            if (0 != pConvertor->partial_length) {  /* partial predefined element */
                assert( pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA );
                opal_unpack_partial_predefined( pConvertor, pElem, &count_desc,
                                                &iov_ptr, &conv_ptr, &iov_len_local );
                if (0 == count_desc) {  /* the end of the vector ? */
                    assert( 0 == pConvertor->partial_length );
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++; /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                             process_loop, process_end_loop);
                    goto process_data;
                }
                if( 0 == iov_len_local )
                    goto complete_loop;
            }
            if (((size_t) pElem->elem.count * pElem->elem.blocklen) != count_desc) {
                /* we have a partial (less than blocklen) basic datatype */
                int rc = UNPACK_PARTIAL_BLOCKLEN(pConvertor, pElem, count_desc, iov_ptr, conv_ptr,
                                                 iov_len_local);
                if (0 == rc) { /* not done */
                    goto complete_loop;
                }
                if (0 == count_desc) {
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++; /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                             process_loop, process_end_loop);
                    goto process_data;
                }
            }
        }

        while (1) {
            while (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            process_data:
                /* we have a basic datatype (working on full blocks) */
                UNPACK_PREDEFINED_DATATYPE(pConvertor, pElem, count_desc, iov_ptr, conv_ptr,
                                           iov_len_local);
                if (0 != count_desc) { /* completed? */
                    goto complete_loop;
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++; /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) { /* end of the current loop */
            process_end_loop:
                DO_DEBUG(opal_output(0,
                                     "unpack end_loop count %" PRIsize_t
                                     " stack_pos %d pos_desc %d disp %ld space %" PRIsize_t "\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local););
                if (--(pStack->count) == 0) { /* end of loop */
                    if (0 == pConvertor->stack_pos) {
                        /* we're done. Force the exit of the main for loop (around iovec) */
                        *out_size = iov_count;
                        goto complete_loop;
                    }
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if (pStack->index == -1) {
                        pStack->disp += (pData->ub - pData->lb);
                    } else {
                        assert(OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type);
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DO_DEBUG(opal_output(0,
                                     "unpack new_loop count %" PRIsize_t
                                     " stack_pos %d pos_desc %d disp %ld space %" PRIsize_t "\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local););
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
            process_loop:
                local_disp = (ptrdiff_t) conv_ptr;
                if (pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                    UNPACK_CONTIGUOUS_LOOP(pConvertor, pElem, count_desc, iov_ptr, conv_ptr,
                                           iov_len_local);
                    if (0 == count_desc) { /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                local_disp = (ptrdiff_t) conv_ptr - local_disp;
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                           pStack->disp + local_disp);
                pos_desc++;
            update_loop_description: /* update the current state */
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos,
                               &description[pos_desc], "advance loop");
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
        }
    complete_loop:
        assert( pElem->elem.common.type < OPAL_DATATYPE_MAX_PREDEFINED );
        if( (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) && (0 != iov_len_local) ) {
            unsigned char* temp = conv_ptr;
            /* We have some partial data here. Let's copy it into the convertor
             * and keep it hot until the next round.
             */
            assert( iov_len_local < opal_datatype_basicDatatypes[pElem->elem.common.type]->size );
            opal_unpack_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr, &temp, &iov_len_local);
        }

        iov[iov_count].iov_len -= iov_len_local; /* update the amount of valid data */
        total_unpacked += iov[iov_count].iov_len;
    }
    *max_data = total_unpacked;
    pConvertor->bConverted += total_unpacked; /* update the already converted bytes */
    *out_size = iov_count;
    if (pConvertor->bConverted == pConvertor->local_size) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* Save the global position for the next round */
    PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
               conv_ptr - pConvertor->pBaseBuf);
    DO_DEBUG(opal_output(0,
                         "unpack save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pConvertor->stack_pos, pStack->index, pStack->count, (long) pStack->disp););
    return 0;
}

/*
 * CPU memcpy shim carrying the convertor cbmemcpy signature. The accelerator pack/unpack
 * interpreters move every byte through pConvertor->cbmemcpy, so pointing it at plain memcpy runs
 * their descriptor traversal entirely on host memory, without any device transfer or accelerator
 * runtime. This is what lets the "accelerator" backend be exercised and validated on the CPU.
 */
static void *sweep_host_memcpy(void *dest, const void *src, size_t n, opal_convertor_t *pConvertor)
{
    (void) pConvertor;
    return memcpy(dest, src, n);
}

/*
 * Execute one complete conversion through a prepared convertor. The caller may bound each call to
 * a fragment so the benchmark covers the same resumable interpreter used by transports. Selecting
 * the reference or accelerator backend changes only fAdvance (and, for the accelerator backend,
 * cbmemcpy) after normal convertor preparation.
 */
static int run_convertor_once(MPI_Datatype datatype, const benchmark_config_t *config,
                              const unsigned char *source, const unsigned char *packed,
                              unsigned char *packed_output, unsigned char *unpacked_output,
                              size_t packed_size)
{
    opal_convertor_t convertor;
    size_t converted = 0;
    int complete = 0;
    int rc;

    OBJ_CONSTRUCT(&convertor, opal_convertor_t);
    if (config->unpack) {
        rc = opal_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor, &datatype->super,
                                                       config->datatype_count, unpacked_output, 0,
                                                       &convertor);
        if (0 != rc) {
            rc = MPI_ERR_INTERN;
            goto out;
        }
        if (BENCHMARK_BACKEND_REFERENCE == config->backend) {
            if (opal_generic_inlined_unpack != convertor.fAdvance) {
                rc = MPI_ERR_TYPE;
                goto out;
            }
            convertor.fAdvance = sweep_reference_unpack;
        } else if (BENCHMARK_BACKEND_ACCELERATOR == config->backend) {
            if (opal_generic_inlined_unpack != convertor.fAdvance) {
                rc = MPI_ERR_TYPE;
                goto out;
            }
            convertor.fAdvance = opal_unpack_accelerator_simple;
            convertor.cbmemcpy = sweep_host_memcpy;
        } else if (BENCHMARK_BACKEND_GENERAL == config->backend) {
            if (opal_generic_inlined_unpack != convertor.fAdvance) {
                rc = MPI_ERR_TYPE;
                goto out;
            }
            /* Run the general (heterogeneous) interpreter without triggering a real byte-swap
             * conversion: keep the local (homogeneous) master, whose copy_TYPE movers already carry
             * the full heterogeneous signature (blocklen, extents, and the partial-blocklen
             * "leftover" path) and simply skip the swap when the two architectures match. Forcing
             * fAdvance and clearing CONVERTOR_HOMOGENEOUS selects opal_unpack_general so the fragment
             * loop exercises the general-path partial-blocklen re-alignment while round-trip stays a
             * plain byte compare. */
            convertor.fAdvance = opal_unpack_general;
            convertor.flags &= ~CONVERTOR_HOMOGENEOUS;
        }
    } else {
        rc = opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor, &datatype->super,
                                                       config->datatype_count, source, 0, &convertor);
        if (0 != rc) {
            rc = MPI_ERR_INTERN;
            goto out;
        }
        if (BENCHMARK_BACKEND_REFERENCE == config->backend) {
            if (opal_generic_inlined_pack != convertor.fAdvance) {
                rc = MPI_ERR_TYPE;
                goto out;
            }
            convertor.fAdvance = sweep_reference_pack;
        } else if (BENCHMARK_BACKEND_ACCELERATOR == config->backend) {
            if (opal_generic_inlined_pack != convertor.fAdvance) {
                rc = MPI_ERR_TYPE;
                goto out;
            }
            convertor.fAdvance = opal_pack_accelerator_simple;
            convertor.cbmemcpy = sweep_host_memcpy;
        } else if (BENCHMARK_BACKEND_GENERAL == config->backend) {
            if (opal_generic_inlined_pack != convertor.fAdvance) {
                rc = MPI_ERR_TYPE;
                goto out;
            }
            /* See the unpack side: keep the local (homogeneous) master for identity conversion but
             * force opal_pack_general and mark the convertor as needing send-side conversion, so the
             * general (heterogeneous) packer -- including the partial-blocklen re-alignment across
             * fragments -- is exercised without a real byte swap. */
            convertor.fAdvance = opal_pack_general;
            convertor.flags &= ~CONVERTOR_HOMOGENEOUS;
            convertor.flags |= CONVERTOR_SEND_CONVERSION;
        }
    }
    while (converted < packed_size) {
        struct iovec iov;
        size_t fragment = packed_size - converted;
        size_t max_data;
        uint32_t iov_count = 1;

        if ((0 != config->fragment_bytes) && (config->fragment_bytes < fragment)) {
            fragment = config->fragment_bytes;
        }
        iov.iov_base = config->unpack ? (void *) (packed + converted)
                                      : (void *) (packed_output + converted);
        iov.iov_len = fragment;
        max_data = fragment;
        complete = config->unpack ? opal_convertor_unpack(&convertor, &iov, &iov_count, &max_data)
                                  : opal_convertor_pack(&convertor, &iov, &iov_count, &max_data);
        if ((0 > complete) || (0 == max_data) || (fragment < max_data)) {
            rc = MPI_ERR_INTERN;
            goto out;
        }
        converted += max_data;
    }
    rc = (complete && (packed_size == converted)) ? MPI_SUCCESS : MPI_ERR_INTERN;

out:
    OBJ_DESTRUCT(&convertor);
    return rc;
}

/* Sum the base-type elements moved by the DATA entries in desc[start, end), and
 * flag any nested LOOP/END_LOOP encountered.  The element total (elem.count *
 * elem.blocklen) is conserved under the optimizer's boundary fusion, so dividing
 * it by the per-item element count recovers the committed item count even when
 * entries have been fused or split -- something a raw entry count cannot do. */
static long descriptor_data_elements(const dt_elem_desc_t *desc, size_t start, size_t end,
                                     int *nested_loop)
{
    long elements = 0;
    for (size_t i = start; i < end; ++i) {
        const uint16_t type = desc[i].elem.common.type;
        if ((OPAL_DATATYPE_LOOP == type) || (OPAL_DATATYPE_END_LOOP == type)) {
            *nested_loop = 1;
            continue;
        }
        if ((OPAL_DATATYPE_LB == type) || (OPAL_DATATYPE_UB == type)) {
            continue;
        }
        elements += (long) desc[i].elem.count * desc[i].elem.blocklen;
    }
    return elements;
}

/* Dispatch one benchmark iteration while keeping the existing MPI path as the default. */
static int run_operation_once(MPI_Datatype datatype, const benchmark_config_t *config,
                              const unsigned char *source, const unsigned char *reference,
                              unsigned char *packed, unsigned char *unpacked, size_t packed_size)
{
    if (BENCHMARK_BACKEND_MPI == config->backend) {
        int capacity = (int) packed_size;
        int position = 0;

        if (config->unpack) {
            return MPI_Unpack(reference, capacity, &position, unpacked, config->datatype_count,
                              datatype, MPI_COMM_SELF);
        }
        return MPI_Pack(source, config->datatype_count, datatype, packed, capacity, &position,
                        MPI_COMM_SELF);
    }
    return run_convertor_once(datatype, config, source, reference, packed, unpacked, packed_size);
}

/* Validate both directions once, then time the selected pack or unpack backend. */
static int run_benchmark(MPI_Datatype datatype, const benchmark_config_t *config)
{
    MPI_Count datatype_size;
    MPI_Aint lb, datatype_extent;
    unsigned char *source = NULL, *packed = NULL, *reference = NULL, *unpacked = NULL;
    double *samples = NULL;
    size_t source_size, packed_size, required_cycles;
    int pack_capacity, position, actual_cycles, reported_loop_items, reported_loop_iterations;
    int reported_tail_items, rc = MPI_SUCCESS;

    MPI_Type_size_x(datatype, &datatype_size);
    MPI_Type_get_extent(datatype, &lb, &datatype_extent);
    if ((0 != lb) || (0 >= datatype_extent) || (0 >= datatype_size)
        || ((MPI_Count) INT_MAX / config->datatype_count < datatype_size)
        || ((size_t) datatype_extent > SIZE_MAX / config->datatype_count)) {
        fprintf(stderr, "The requested signature has unsupported size or extent values\n");
        return MPI_ERR_TYPE;
    }

    packed_size = (size_t) datatype_size * config->datatype_count;
    source_size = (size_t) datatype_extent * config->datatype_count;
    pack_capacity = (int) packed_size;
    source = (unsigned char *) malloc(source_size);
    packed = (unsigned char *) malloc(packed_size);
    reference = (unsigned char *) malloc(packed_size);
    unpacked = (unsigned char *) malloc(source_size);
    samples = (double *) malloc((size_t) config->trials * sizeof(*samples));
    if ((NULL == source) || (NULL == packed) || (NULL == reference) || (NULL == unpacked)
        || (NULL == samples)) {
        rc = MPI_ERR_NO_MEM;
        goto out;
    }

    for (size_t i = 0; i < source_size; ++i) {
        source[i] = (unsigned char) (i % 251);
    }
    pack_reference(reference, source, config, datatype_extent);
    position = 0;
    rc = MPI_Pack(source, config->datatype_count, datatype, packed, pack_capacity, &position, MPI_COMM_SELF);
    if ((MPI_SUCCESS != rc) || ((size_t) position != packed_size)
        || (0 != memcmp(packed, reference, packed_size))) {
        fprintf(stderr, "Synthetic datatype failed packed-data validation\n");
        rc = MPI_ERR_TYPE;
        goto out;
    }
    memset(unpacked, 0, source_size);
    position = 0;
    rc = MPI_Unpack(reference, pack_capacity, &position, unpacked, config->datatype_count, datatype,
                    MPI_COMM_SELF);
    pack_reference(packed, unpacked, config, datatype_extent);
    if ((MPI_SUCCESS != rc) || ((size_t) position != packed_size)
        || (0 != memcmp(packed, reference, packed_size))) {
        fprintf(stderr, "Synthetic datatype failed unpacked-data validation\n");
        rc = MPI_ERR_TYPE;
        goto out;
    }
    if (BENCHMARK_BACKEND_MPI != config->backend) {
        memset(packed, 0, packed_size);
        memset(unpacked, 0, source_size);
        rc = run_operation_once(datatype, config, source, reference, packed, unpacked, packed_size);
        if (config->unpack) {
            pack_reference(packed, unpacked, config, datatype_extent);
        }
        if ((MPI_SUCCESS != rc) || (0 != memcmp(packed, reference, packed_size))) {
            fprintf(stderr, "%s backend failed %s validation\n",
                    benchmark_backend_name(config->backend), config->unpack ? "unpack" : "pack");
            rc = MPI_ERR_TYPE;
            goto out;
        }
    }

    required_cycles = (0 == config->min_work_bytes)
                          ? 1
                          : 1 + (config->min_work_bytes - 1) / packed_size;
    if (INT_MAX < required_cycles) {
        fprintf(stderr, "--min-work-bytes requires more than INT_MAX cycles\n");
        rc = MPI_ERR_COUNT;
        goto out;
    }
    actual_cycles = (required_cycles > (size_t) config->cycles) ? (int) required_cycles : config->cycles;

    /* In --synthetic mode the descriptor is hand-built exactly as requested, so the
     * requested values describe it faithfully.  In --commit-description mode the
     * optimizer decides the shape, so derive the reported values by walking the
     * committed descriptor instead of assuming one entry per requested item. */
    reported_loop_items = config->loop_items;
    reported_loop_iterations = config->total_items / config->loop_items;
    reported_tail_items = config->total_items % config->loop_items;
    if (config->commit_description) {
        const dt_elem_desc_t *desc = datatype->super.opt_desc.desc;
        const size_t used = datatype->super.opt_desc.used;
        const long per_item = (long) config->data_count * config->blocklen;
        if ((0 != used) && (OPAL_DATATYPE_LOOP == desc[0].elem.common.type)
            && ((size_t) desc[0].loop.items < used)) {
            /* Top-level LOOP: body is desc[1 .. loop.items-1], END_LOOP sits at
             * desc[loop.items], and any leftover items follow it. */
            const size_t body_end = desc[0].loop.items;
            int nested = 0;
            const long body_elements = descriptor_data_elements(desc, 1, body_end, &nested);
            const long tail_elements = descriptor_data_elements(desc, body_end + 1, used, &nested);
            reported_loop_iterations = (int) desc[0].loop.loops;
            /* Report -1 when the committed shape cannot be expressed in whole items
             * (nested loops, or an element total that is not a multiple of one item)
             * rather than printing a mixed-unit or negative count. */
            reported_loop_items = (!nested && (0 < per_item) && (0 == body_elements % per_item))
                                      ? (int) (body_elements / per_item) : -1;
            reported_tail_items = (!nested && (0 < per_item) && (0 == tail_elements % per_item))
                                      ? (int) (tail_elements / per_item) : -1;
        } else {
            /* The optimizer flattened the top-level loop away (e.g. full unroll to
             * straight DATA entries), so a loop/iteration/tail decomposition no
             * longer describes the committed descriptor: emit sentinels rather than
             * the now-inapplicable requested values. */
            reported_loop_items = -1;
            reported_loop_iterations = -1;
            reported_tail_items = -1;
        }
    }

    printf("SIGNATURE operation=%s backend=%s fragment_bytes=%zu element_size=%d data_count=%d "
           "blocklen=%d block_gap=%d "
           "item_gap=%d total_items=%d "
           "loop_items=%d loop_iterations=%d "
           "tail_items=%d description=%s datatype_count=%d packed_bytes=%zu cycles=%d trials=%d "
           "warmups=%d\n",
           config->unpack ? "unpack" : "pack", benchmark_backend_name(config->backend),
           config->fragment_bytes, config->element_size, config->data_count, config->blocklen,
           config->block_gap, config->item_gap,
           config->total_items, reported_loop_items, reported_loop_iterations, reported_tail_items,
           config->commit_description ? "commit" : "synthetic", config->datatype_count, packed_size,
           actual_cycles, config->trials, config->warmups);

    /* Print the MCA variables governing this run as bare "name = value" lines that can be
     * pasted straight into an MCA parameter file (or passed via --mca) to reproduce or
     * change the measured policy: the mover knobs for the operation under test, plus the
     * optimizer knobs when the committed (optimized) descriptor is the one being measured. */
    if (config->unpack) {
        printf("opal_datatype_unpack_max_vectorized_block_bytes = %zu\n",
               opal_datatype_config.unpack.max_vectorized_block_bytes);
        printf("opal_datatype_unpack_always_typed_block_bytes = %zu\n",
               opal_datatype_config.unpack.always_typed_block_bytes);
        printf("opal_datatype_unpack_compact_memcpy_max_bytes = %zu\n",
               opal_datatype_config.unpack.compact_memcpy_max_bytes);
        printf("opal_datatype_unpack_min_scatter_gap_bytes = %zu\n",
               opal_datatype_config.unpack.min_scatter_gap_bytes);
        printf("opal_datatype_unpack_small_fragment_bytes = %zu\n",
               opal_datatype_config.unpack.small_fragment_bytes);
        printf("opal_datatype_unpack_large_fragment_bytes = %zu\n",
               opal_datatype_config.unpack.large_fragment_bytes);
    } else {
        printf("opal_datatype_pack_max_vectorized_blocklen = %zu\n",
               opal_datatype_config.pack.max_vectorized_blocklen);
        printf("opal_datatype_pack_l1_cache_lines = %zu\n",
               opal_datatype_config.pack.l1_cache_lines);
    }
    if (config->commit_description) {
        printf("opal_datatype_optimize_max_desc_growth = %zu\n",
               opal_datatype_config.optimize.max_desc_growth);
        printf("opal_datatype_optimize_loop_unroll_max_items = %zu\n",
               opal_datatype_config.optimize.loop_unroll_max_items);
        printf("opal_datatype_optimize_loop_unroll_max_data_bytes = %zu\n",
               opal_datatype_config.optimize.loop_unroll_max_data_bytes);
    }

    for (int repetition = 0; repetition < config->repetitions; ++repetition) {
        double mean, standard_deviation, minimum, maximum;

        for (int warmup = 0; warmup < config->warmups; ++warmup) {
            for (int cycle = 0; cycle < actual_cycles; ++cycle) {
                rc = run_operation_once(datatype, config, source, reference, packed, unpacked,
                                        packed_size);
                if (MPI_SUCCESS != rc) {
                    fprintf(stderr, "%s backend operation failed during warmup\n",
                            benchmark_backend_name(config->backend));
                    goto out;
                }
            }
        }
        for (int trial = 0; trial < config->trials; ++trial) {
            const double start = MPI_Wtime();

            for (int cycle = 0; cycle < actual_cycles; ++cycle) {
                rc = run_operation_once(datatype, config, source, reference, packed, unpacked,
                                        packed_size);
                if (MPI_SUCCESS != rc) {
                    fprintf(stderr, "%s backend operation failed during timing\n",
                            benchmark_backend_name(config->backend));
                    goto out;
                }
            }
            samples[trial] = (MPI_Wtime() - start) / actual_cycles;
        }
        summarize(samples, config->trials, &mean, &standard_deviation, &minimum, &maximum);
        printf("RESULT repetition=%d mean_seconds=%.17g stddev_seconds=%.17g stddev_pct=%.9g "
               "min_seconds=%.17g max_seconds=%.17g bandwidth_mib_s=%.9g\n",
               repetition, mean, standard_deviation, 100.0 * standard_deviation / mean, minimum,
               maximum, packed_size / mean / (1024.0 * 1024.0));
    }

out:
    free(samples);
    free(unpacked);
    free(reference);
    free(packed);
    free(source);
    return rc;
}

int main(int argc, char **argv)
{
    /* Exercise the post-loop DATA tail when the tester is run without explicit arguments. */
    benchmark_config_t config = {
        .data_count = 2,
        .blocklen = 2,
        .element_size = 4,
        .block_gap = 1,
        .item_gap = 1,
        .total_items = 17,
        .loop_items = 5,
        .datatype_count = 1,
        .cycles = 100,
        .trials = 20,
        .warmups = 2,
        .repetitions = 5,
        .min_work_bytes = 1024 * 1024,
        .fragment_bytes = 0,
        .backend = BENCHMARK_BACKEND_MPI,
        .commit_description = 0,
        .dump = 0,
        .unpack = 0,
    };
    MPI_Datatype datatype = MPI_DATATYPE_NULL;
    int parse_result, rc;

    parse_result = parse_options(argc, argv, &config);
    if (0 != parse_result) {
        if (0 > parse_result) {
            print_usage(argv[0]);
        }
        return (0 > parse_result) ? EXIT_FAILURE : EXIT_SUCCESS;
    }

    rc = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != rc) {
        fprintf(stderr, "MPI_Init failed: %d\n", rc);
        return EXIT_FAILURE;
    }
    rc = create_synthetic_datatype(&config, &datatype);
    if (MPI_SUCCESS == rc) {
        if (config.dump) {
            ompi_datatype_dump(datatype);
        }
        rc = run_benchmark(datatype, &config);
        MPI_Type_free(&datatype);
    }
    MPI_Finalize();
    return (MPI_SUCCESS == rc) ? EXIT_SUCCESS : EXIT_FAILURE;
}
