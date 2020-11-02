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
#include "psquash_native.h"

static pmix_status_t native_init(void);

static void native_finalize(void);

static pmix_status_t native_get_max_size(pmix_data_type_t type, size_t *size);

static pmix_status_t native_encode_int(pmix_data_type_t type, void *src,
                                       void *dst, size_t *size);

static pmix_status_t native_decode_int(pmix_data_type_t type, void *src,
                                       size_t src_len, void *dest,
                                       size_t *dst_size);

pmix_psquash_base_module_t pmix_psquash_native_module = {
    .name = "native",
    .int_type_is_encoded = false,
    .init = native_init,
    .finalize = native_finalize,
    .get_max_size = native_get_max_size,
    .encode_int = native_encode_int,
    .decode_int = native_decode_int
};

#define NATIVE_PACK_CONVERT(ret, type, val)         \
do {                                                \
    (ret) = PMIX_SUCCESS;                           \
    switch(type) {                                  \
        case PMIX_INT16:                            \
        case PMIX_UINT16:{                          \
            uint16_t __tmp = (uint16_t)val;         \
            val = pmix_htons(__tmp);                \
            break;                                  \
        }                                           \
        case PMIX_INT:                              \
        case PMIX_UINT:                             \
        case PMIX_INT32:                            \
        case PMIX_UINT32:{                          \
            uint32_t __tmp = (uint32_t)val;         \
            val = htonl(__tmp);                     \
            break;                                  \
        }                                           \
        case PMIX_SIZE:                             \
        case PMIX_INT64:                            \
        case PMIX_UINT64:{                          \
            uint64_t __tmp = (uint64_t)val;         \
            val = pmix_hton64(__tmp);               \
            break;                                  \
        }                                           \
        default:                                    \
            (ret) = PMIX_ERR_BAD_PARAM;             \
    }                                               \
} while (0)

#define NATIVE_UNPACK_CONVERT(ret, type, val)       \
do {                                                \
    (ret) = PMIX_SUCCESS;                           \
    switch(type) {                                  \
        case PMIX_INT16:                            \
        case PMIX_UINT16:{                          \
            uint16_t __tmp = (uint16_t)val;         \
            val = pmix_ntohs(__tmp);                \
            break;                                  \
        }                                           \
        case PMIX_INT:                              \
        case PMIX_UINT:                             \
        case PMIX_INT32:                            \
        case PMIX_UINT32:{                          \
            uint32_t __tmp = (uint32_t)val;         \
            val = ntohl(__tmp);                     \
            break;                                  \
        }                                           \
        case PMIX_INT64:                            \
        case PMIX_SIZE:                             \
        case PMIX_UINT64:{                          \
            uint64_t __tmp = (uint64_t)val;         \
            val = pmix_ntoh64(__tmp);               \
            break;                                  \
        }                                           \
        default:                                    \
            (ret) = PMIX_ERR_BAD_PARAM;             \
    }                                               \
} while (0)

static pmix_status_t native_init(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psquash: native init");
    return PMIX_SUCCESS;
}

static void native_finalize(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psquash: native finalize");
}

static pmix_status_t native_get_max_size(pmix_data_type_t type, size_t *size)

{
    pmix_status_t rc;
    PMIX_SQUASH_TYPE_SIZEOF(rc, type, *size);
    return rc;
}

static pmix_status_t native_encode_int(pmix_data_type_t type, void *src,
                                       void *dst, size_t *size)
{
    pmix_status_t rc;
    uint64_t tmp = 0;
    size_t val_size;

    PMIX_SQUASH_TYPE_SIZEOF(rc, type, val_size);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    memcpy(&tmp, src, val_size);
    NATIVE_PACK_CONVERT(rc, type, tmp);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    memcpy(dst, &tmp, val_size);
    *size = val_size;

    return PMIX_SUCCESS;
}

static pmix_status_t native_decode_int(pmix_data_type_t type, void *src,
                                       size_t src_len, void *dst,
                                       size_t *dst_size)
{
    pmix_status_t rc;
    uint64_t tmp = 0;
    size_t val_size;

    PMIX_SQUASH_TYPE_SIZEOF(rc, type, val_size);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* sanity check */
    if (src_len != val_size) {
        rc = PMIX_ERR_UNPACK_FAILURE;
    }

    memcpy(&tmp, src, val_size);
    NATIVE_UNPACK_CONVERT(rc, type, tmp);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    memcpy(dst, &tmp, val_size);
    *dst_size = val_size;

    return PMIX_SUCCESS;
}
