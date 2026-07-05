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
            "          [--backend=mpi|current|reference] [--fragment-bytes=N]\n"
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
        fprintf(stderr, "--fragment-bytes requires the current or reference backend\n");
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
    }
    return "unknown";
}

/*
 * Execute one complete conversion through a prepared convertor. The caller may bound each call to
 * a fragment so the benchmark covers the same resumable interpreter used by transports. Selecting
 * the reference backend changes only fAdvance after normal convertor preparation.
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
            convertor.fAdvance = opal_generic_inlined_unpack_reference;
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
            convertor.fAdvance = opal_generic_inlined_pack_reference;
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

    reported_loop_items = config->loop_items;
    reported_loop_iterations = config->total_items / config->loop_items;
    reported_tail_items = config->total_items % config->loop_items;
    if (config->commit_description && (0 != datatype->super.opt_desc.used)
        && (OPAL_DATATYPE_LOOP == datatype->super.opt_desc.desc[0].elem.common.type)) {
        reported_loop_items = datatype->super.opt_desc.desc[0].loop.items - 1;
        reported_loop_iterations = datatype->super.opt_desc.desc[0].loop.loops;
        reported_tail_items = config->total_items - reported_loop_items * reported_loop_iterations;
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

    for (int repetition = 0; repetition < config->repetitions; ++repetition) {
        double mean, standard_deviation, minimum, maximum;

        for (int warmup = 0; warmup < config->warmups; ++warmup) {
            for (int cycle = 0; cycle < actual_cycles; ++cycle) {
                run_operation_once(datatype, config, source, reference, packed, unpacked,
                                   packed_size);
            }
        }
        for (int trial = 0; trial < config->trials; ++trial) {
            const double start = MPI_Wtime();

            for (int cycle = 0; cycle < actual_cycles; ++cycle) {
                run_operation_once(datatype, config, source, reference, packed, unpacked,
                                   packed_size);
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
