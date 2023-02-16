/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>
#include <stdlib.h>

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_checksum.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/mca/accelerator/accelerator.h"

#if OPAL_ENABLE_DEBUG
#    define DO_DEBUG(INST)         \
        if (opal_ddt_copy_debug) { \
            INST                   \
        }
#else
#    define DO_DEBUG(INST)
#endif /* OPAL_ENABLE_DEBUG */

#define MEMMOVE(d, s, l)                                                                    \
    do {                                                                                    \
        if ((((d) < (s)) && (((d) + (l)) > (s))) || (((s) < (d)) && (((s) + (l)) > (d)))) { \
            memmove((d), (s), (l));                                                         \
        } else {                                                                            \
            MEMCPY((d), (s), (l));                                                          \
        }                                                                                   \
    } while (0)

static void *opal_datatype_accelerator_memcpy(void *dest, const void *src, size_t size)
{
    int res;
    int dev_id;
    uint64_t flags;
    /* If accelerator check addr returns an error, we can only
     * assume it is a host buffer. If device buffer checking fails,
     * it's also highly likely that a device copy will fail. The best
     * we can do is fail as this is not a recoverable/ignorable failure
     * and retries are also unlikely to succeed. We identify these
     * buffers as host buffers as attempting a memcpy would provide
     * a chance to succeed. */
    if (0 >= opal_accelerator.check_addr(dest, &dev_id, &flags) &&
        0 >= opal_accelerator.check_addr(src, &dev_id, &flags)) {
        return memcpy(dest, src, size);
    }
    res = opal_accelerator.mem_copy(MCA_ACCELERATOR_NO_DEVICE_ID, MCA_ACCELERATOR_NO_DEVICE_ID,
                                  dest, src, size, MCA_ACCELERATOR_TRANSFER_UNSPEC);
    if (OPAL_SUCCESS != res) {
        opal_output(0, "Error in accelerator memcpy");
        abort();
    }
    return dest;
}

static void *opal_datatype_accelerator_memmove(void *dest, const void *src, size_t size)
{
    int res;
    int dev_id;
    uint64_t flags;
    /* If accelerator check addr returns an error, we can only
     * assume it is a host buffer. If device buffer checking fails,
     * it's also highly likely that a device copy will fail. The best
     * we can do is fail as this is not a recoverable/ignorable failure
     * and retries are also unlikely to succeed. We identify these
     * buffers as host buffers as attempting a memmove would provide
     * a chance to succeed. */
    if (0 >= opal_accelerator.check_addr(dest, &dev_id, &flags) &&
        0 >= opal_accelerator.check_addr(src, &dev_id, &flags)) {
        return memmove(dest, src, size);
    }
    res = opal_accelerator.mem_move(MCA_ACCELERATOR_NO_DEVICE_ID, MCA_ACCELERATOR_NO_DEVICE_ID,
                                    dest, src, size, MCA_ACCELERATOR_TRANSFER_UNSPEC);
    if (OPAL_SUCCESS != res) {
        opal_output(0, "Error in accelerator memmove");
        abort();
    }
    return dest;
}

#undef MEM_OP_BLOCK_SIZE
#define MEM_OP_BLOCK_SIZE total_length
#undef MEM_OP_NAME
#define MEM_OP_NAME non_overlap_accelerator
#undef MEM_OP
#define MEM_OP opal_datatype_accelerator_memcpy
#include "opal_datatype_copy.h"

#undef MEM_OP_BLOCK_SIZE
#define MEM_OP_BLOCK_SIZE total_length
#undef MEM_OP_NAME
#define MEM_OP_NAME overlap_accelerator
#undef MEM_OP
#define MEM_OP opal_datatype_accelerator_memmove
#include "opal_datatype_copy.h"

int32_t opal_datatype_copy_content_same_ddt(const opal_datatype_t *datatype, int32_t count,
                                            char *destination_base, char *source_base)
{
    ptrdiff_t extent;
    int32_t (*fct)(const opal_datatype_t *, int32_t, char *, char *);

    DO_DEBUG(opal_output(0, "opal_datatype_copy_content_same_ddt( %p, %d, dst %p, src %p )\n",
                         (void *) datatype, count, (void *) destination_base,
                         (void *) source_base););

    /* empty data ? then do nothing. This should normally be trapped
     * at a higher level.
     */
    if (0 == count) {
        return 1;
    }

    /**
     * see discussion in coll_basic_reduce.c for the computation of extent when
     * count != 1. Short version of the story:
     * (true_extent + ((count - 1) * extent))
     */
    extent = (datatype->true_ub - datatype->true_lb) + (count - 1) * (datatype->ub - datatype->lb);

    fct = non_overlap_accelerator_copy_content_same_ddt;
    if (destination_base < source_base) {
        if ((destination_base + extent) > source_base) {
            /* memmove */
            fct = overlap_accelerator_copy_content_same_ddt;
        }
    } else {
        if ((source_base + extent) > destination_base) {
            /* memmove */
            fct = overlap_accelerator_copy_content_same_ddt;
        }
    }
    return fct(datatype, count, destination_base, source_base);
}
