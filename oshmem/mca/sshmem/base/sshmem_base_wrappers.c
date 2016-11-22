/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/constants.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

/* ////////////////////////////////////////////////////////////////////////// */
int
mca_sshmem_segment_create(map_segment_t *ds_buf,
                          const char *file_name,
                          size_t size)
{
    if (!mca_sshmem_base_selected) {
        return OSHMEM_ERROR;
    }

    return mca_sshmem_base_module->segment_create(ds_buf, file_name, size);
}

/* ////////////////////////////////////////////////////////////////////////// */
int
mca_sshmem_ds_copy(const map_segment_t *from,
                   map_segment_t *to)
{
    if (!mca_sshmem_base_selected) {
        return OSHMEM_ERROR;
    }

    return mca_sshmem_base_module->ds_copy(from, to);
}

/* ////////////////////////////////////////////////////////////////////////// */
void *
mca_sshmem_segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    if (!mca_sshmem_base_selected) {
        return NULL;
    }

    return mca_sshmem_base_module->segment_attach(ds_buf, mkey);
}

/* ////////////////////////////////////////////////////////////////////////// */
int
mca_sshmem_segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    if (!mca_sshmem_base_selected) {
        return OSHMEM_ERROR;
    }

    return mca_sshmem_base_module->segment_detach(ds_buf, mkey);
}

/* ////////////////////////////////////////////////////////////////////////// */
int
mca_sshmem_unlink(map_segment_t *ds_buf)
{
    if (!mca_sshmem_base_selected) {
        return OSHMEM_ERROR;
    }

    return mca_sshmem_base_module->unlink(ds_buf);
}

