/*
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "include/pmix_common.h"

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "src/mca/psquash/base/base.h"
#include "psquash_flex128.h"

/* Flexible packing constants */
#define FLEX_BASE7_MAX_BUF_SIZE (SIZEOF_SIZE_T+1)
#define FLEX_BASE7_MASK ((1<<7) - 1)
#define FLEX_BASE7_SHIFT 7
#define FLEX_BASE7_CONT_FLAG (1<<7)

/**
 * Packing conversion of a signed integer value to a flexible representation.
 * The main idea is to split a signed negative value onto an absolute value
 * and a sign bit stored in the special location.
 * This allows efficient representetion of negative values in the
 * flexible form.
 *
 * type - type (pmix_data_type_t) of integer value
 * ptr - pointer to the signed integer value
 *       with the type defined as (type)
 * out - flexible representation of *ptr,
 *       extended to uint64_t if needed
 * (see a comment to `pmix_bfrops_pack_flex` for additional details)
 */
#define FLEX128_PACK_CONVERT_SIGNED(type, ptr, out)         \
do {                                                        \
    type __tbuf = 0;                                        \
    size_t __tmp;                                           \
    int __sign = 0;                                         \
    memcpy(&__tbuf, (ptr), sizeof(type));                   \
    __tmp = __tbuf;                                         \
    (out) = (size_t)__tmp;                                  \
    if (__tmp & (1UL << (sizeof(__tmp)*CHAR_BIT-1))) {      \
        __sign = 1;                                         \
        out = ~(out);                                       \
    }                                                       \
    (out) = ((out) << 1) + __sign;                          \
} while (0)

/**
 * Packing conversion of a signed integer value to a flexible representation.
 * For unsigned types it is reduced to a memcopy.
 *
 * type - usual integer C-type of integer value
 * ptr - pointer to the signed integer value
 *       with the type defined as (type)
 * out - flexible representation of *ptr,
 *       extended to uint64_t if needed
 * (see a comment to `pmix_bfrops_pack_flex` for additional details)
 */
#define FLEX128_PACK_CONVERT_UNSIGNED(type, ptr, out)       \
do {                                                        \
    type __tbuf = 0;                                        \
    memcpy(&__tbuf, (ptr), sizeof(type));                   \
    out = __tbuf;                                           \
} while (0)

/**
 * Packing conversion from integer value to a flexible representation.
 *
 * r - return status code
 * t - type (pmix_data_type_t) of integer value, it is determines
 *     which type of integer is converted
 * s - pointer to the integer value with the type defined as (t)
 * d - flexible representation output value (uin64_t)
 * (see a comment to `pmix_bfrops_pack_flex` for additional details)
 */
#define FLEX128_PACK_CONVERT(r, t, s, d)                            \
do {                                                                \
    (r) = PMIX_SUCCESS;                                             \
    switch (t) {                                                    \
        case PMIX_INT16:                                            \
            FLEX128_PACK_CONVERT_SIGNED(int16_t, s, d);             \
            break;                                                  \
        case PMIX_UINT16:                                           \
            FLEX128_PACK_CONVERT_UNSIGNED(uint16_t, s, d);          \
            break;                                                  \
        case PMIX_INT:                                              \
        case PMIX_INT32:                                            \
            FLEX128_PACK_CONVERT_SIGNED(int32_t, s, d);             \
            break;                                                  \
        case PMIX_UINT:                                             \
        case PMIX_UINT32:                                           \
            FLEX128_PACK_CONVERT_UNSIGNED(uint32_t, s, d);          \
            break;                                                  \
        case PMIX_INT64:                                            \
            FLEX128_PACK_CONVERT_SIGNED(int64_t, s, d);             \
            break;                                                  \
        case PMIX_SIZE:                                             \
            FLEX128_PACK_CONVERT_UNSIGNED(size_t, s, d);            \
            break;                                                  \
        case PMIX_UINT64:                                           \
            FLEX128_PACK_CONVERT_UNSIGNED(uint64_t, s, d);          \
            break;                                                  \
        default:                                                    \
            (r) = PMIX_ERR_BAD_PARAM;                               \
    }                                                               \
} while(0)

/**
 * Unpacking conversion from a flexible representation to a
 * signed integer value.
 *
 * type - C-type of a signed integer value
 * val - flexible representation (uint64_t)
 * ptr - pointer to a 64-bit output buffer for the upacked value
 * (see a comment to `pmix_bfrops_pack_flex` for additional details)
 */
#define FLEX128_UNPACK_CONVERT_SIGNED(type, val, ptr)           \
do {                                                            \
    type __tbuf = 0;                                            \
    size_t __tmp = val;                                         \
    int sign = (__tmp) & 1;                                     \
    __tmp >>= 1;                                                \
    if (sign) {                                                 \
        __tmp = ~__tmp;                                         \
    }                                                           \
    __tbuf = (type)__tmp;                                       \
    memcpy(ptr, &__tbuf, sizeof(type));                         \
} while (0)

/**
 * Unpacking conversion of a flexible representation value
 * to an unsigned integer.
 *
 * type - C-type of unsigned integer value
 * val - flexible representation value (uint64_t)
 * ptr - pointer to a 64-bit output buffer for the upacked value
 * (see a comment to `pmix_bfrops_pack_flex` for additional details)
 */
#define FLEX128_UNPACK_CONVERT_UNSIGNED(type, val, ptr)         \
do {                                                            \
    type __tbuf = 0;                                            \
    __tbuf = (type)val;                                         \
    memcpy(ptr, &__tbuf, sizeof(type));                         \
} while (0)

/**
 * Unpacking conversion of a flexible representation value
 * to an integer.
 *
 * r - return status code
 * t - type (pmix_data_type_t) of integer value, it is determines
 *     which type of integer is converted
 * s - flex-representation value (uin64_t)
 * d - pointer to a 64-bit output buffer for the upacked value
 * (see a comment to `pmix_bfrops_pack_flex` for additional details)
 */
#define FLEX128_UNPACK_CONVERT(r, t, s, d)                              \
do {                                                                    \
    (r) = PMIX_SUCCESS;                                                 \
    switch (t) {                                                        \
        case PMIX_INT16:                                                \
            FLEX128_UNPACK_CONVERT_SIGNED(int16_t, s, d);               \
            break;                                                      \
        case PMIX_UINT16:                                               \
            FLEX128_UNPACK_CONVERT_UNSIGNED(uint16_t, s, d);            \
            break;                                                      \
        case PMIX_INT:                                                  \
        case PMIX_INT32:                                                \
            FLEX128_UNPACK_CONVERT_SIGNED(int32_t, s, d);               \
            break;                                                      \
        case PMIX_UINT:                                                 \
        case PMIX_UINT32:                                               \
            FLEX128_UNPACK_CONVERT_UNSIGNED(uint32_t, s, d);            \
            break;                                                      \
        case PMIX_INT64:                                                \
            FLEX128_UNPACK_CONVERT_SIGNED(int64_t, s, d);               \
            break;                                                      \
        case PMIX_SIZE:                                                 \
            FLEX128_UNPACK_CONVERT_UNSIGNED(size_t, s, d);              \
            break;                                                      \
        case PMIX_UINT64:                                               \
            FLEX128_UNPACK_CONVERT_UNSIGNED(uint64_t, s, d);            \
            break;                                                      \
        default:                                                        \
            (r) = PMIX_ERR_BAD_PARAM;                                   \
    }                                                                   \
} while(0)

static pmix_status_t flex128_init(void);

static void flex128_finalize(void);

static pmix_status_t flex128_get_max_size(pmix_data_type_t type, size_t *size);

static pmix_status_t flex128_encode_int(pmix_data_type_t type, void *src,
                                        void *dst, size_t *size);

static pmix_status_t flex128_decode_int(pmix_data_type_t type, void *src,
                                        size_t src_len, void *dest,
                                        size_t *dst_size);

static size_t flex_pack_integer(size_t val,
                                uint8_t out_buf[FLEX_BASE7_MAX_BUF_SIZE]);

static size_t flex_unpack_integer(const uint8_t in_buf[], size_t buf_size,
                                  size_t *out_val, size_t *out_val_size);

pmix_psquash_base_module_t pmix_flex128_module = {
    .name = "flex128",
    .int_type_is_encoded = true,
    .init = flex128_init,
    .finalize = flex128_finalize,
    .get_max_size = flex128_get_max_size,
    .encode_int = flex128_encode_int,
    .decode_int = flex128_decode_int
};


static pmix_status_t flex128_init(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psquash: flex128 init");
    return PMIX_SUCCESS;
}

static void flex128_finalize(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psquash: flex128 finalize");
}

static pmix_status_t flex128_get_max_size(pmix_data_type_t type, size_t *size)
 {
    pmix_status_t rc;
    PMIX_SQUASH_TYPE_SIZEOF(rc, type, *size);
    /* the size of the packed value can be 1B larger
     * because of continuation flags */
    *size += 1;
    return rc;
}

static pmix_status_t flex128_encode_int(pmix_data_type_t type, void *src,
                                        void *dst, size_t *size)
{
    pmix_status_t rc = PMIX_SUCCESS;
    uint8_t tmp_buf[FLEX_BASE7_MAX_BUF_SIZE];
    uint64_t tmp;

    FLEX128_PACK_CONVERT(rc, type, (uint8_t*)src, tmp);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    *size = flex_pack_integer(tmp, tmp_buf);
    memcpy(dst, tmp_buf, *size);

    return rc;
}

static pmix_status_t flex128_decode_int(pmix_data_type_t type, void *src,
                                        size_t src_len, void *dest, size_t *dst_size)
{
    pmix_status_t rc = PMIX_SUCCESS;
    size_t tmp;
    size_t val_size, unpack_val_size;

    PMIX_SQUASH_TYPE_SIZEOF(rc, type, val_size);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    *dst_size = flex_unpack_integer(src, src_len, &tmp, &unpack_val_size);

    if( val_size < unpack_val_size ) { // sanity check
        rc = PMIX_ERR_UNPACK_FAILURE;
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    FLEX128_UNPACK_CONVERT(rc, type, tmp, (uint8_t*)dest);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return rc;
}

/*
 * Typical representation of a number in computer systems is:
 * A[0]*B^0 + A[1]*B^1 + A[2]*B^2 + ... + A[n]*B^n
 * where B called a base and B == 256 (one byte)
 *
 * This encoding changes the default representation by introducing an additional
 * bit per each byte to store a "continuation flag". So integers are now encoded
 * with the same representation, but the base B = 128 and the remaning bit is
 * used to indicate whether or not the next byte contains more bits of this value.
 */
static size_t flex_pack_integer(size_t val,
                                uint8_t out_buf[FLEX_BASE7_MAX_BUF_SIZE])
{
    size_t tmp = val;
    size_t idx = 0;

    do {
        uint8_t val = tmp & FLEX_BASE7_MASK;
        tmp >>= FLEX_BASE7_SHIFT;
        if (PMIX_UNLIKELY(tmp)) {
            val |= FLEX_BASE7_CONT_FLAG;
        }
        out_buf[idx++] = val;
    } while(tmp && idx < SIZEOF_SIZE_T);

    /* If we have leftover (VERY unlikely) */
    if (PMIX_UNLIKELY(SIZEOF_SIZE_T == idx && tmp)) {
        out_buf[idx++] = tmp;
    }

    return idx;
}

/*
 * See a comment to `pmix_bfrops_pack_flex` for additional details.
 */
static size_t flex_unpack_integer(const uint8_t in_buf[], size_t buf_size,
                                  size_t *out_val, size_t *out_val_size)
{
    size_t value = 0, shift = 0, shift_last = 0;
    size_t idx = 0;
    uint8_t val = 0, val_last = 0;
    uint8_t hi_bit = 0;
    size_t flex_size = buf_size;

    /* restrict the buf size to max flex size */
    if (buf_size > FLEX_BASE7_MAX_BUF_SIZE) {
        flex_size = FLEX_BASE7_MAX_BUF_SIZE;
    }

    do {
        val = in_buf[idx++];
        val_last = val;
        shift_last = shift;
        value = value + (((uint64_t)val & FLEX_BASE7_MASK) << shift);
        shift += FLEX_BASE7_SHIFT;
    } while(PMIX_UNLIKELY((val & FLEX_BASE7_CONT_FLAG) &&
                          (idx < (flex_size-1))));
    /* If we have leftover (VERY unlikely) */
    if (PMIX_UNLIKELY((flex_size-1) == idx &&
                      (val & FLEX_BASE7_CONT_FLAG))) {
        val = in_buf[idx++];
        val_last = val;
        value = value + ((uint64_t)val << shift);
        shift_last = shift;
    }
    /* compute the most significant bit of val */
    while (val_last != 0) {
        val_last >>= 1;
        hi_bit++;
    }
    /* compute the real val size */
    *out_val_size = (hi_bit + shift_last)/CHAR_BIT +
            !!((hi_bit + shift_last) & (CHAR_BIT - 1));
    *out_val = value;

    return idx;
}
