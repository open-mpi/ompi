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

int
mca_sshmem_segment_create(map_segment_t *ds_buf,
                          const char *file_name,
                          size_t size, long hint)
{
    if (!mca_sshmem_base_selected) {
        return OSHMEM_ERROR;
    }

    return mca_sshmem_base_module->segment_create(ds_buf, file_name, size, hint);
}

void *
mca_sshmem_segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    if (!mca_sshmem_base_selected) {
        return NULL;
    }

    return mca_sshmem_base_module->segment_attach(ds_buf, mkey);
}

int
mca_sshmem_segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    if (!mca_sshmem_base_selected) {
        return OSHMEM_ERROR;
    }

    return mca_sshmem_base_module->segment_detach(ds_buf, mkey);
}

int
mca_sshmem_unlink(map_segment_t *ds_buf)
{
    if (!mca_sshmem_base_selected) {
        return OSHMEM_ERROR;
    }

    return mca_sshmem_base_module->unlink(ds_buf);
}


char * oshmem_get_unique_file_name(uint64_t pe)
{
    char *file_name = NULL;

    assert(mca_sshmem_base_backing_file_dir);

    if (NULL == (file_name = calloc(OPAL_PATH_MAX, sizeof(char)))) {
        return NULL;
    }

    snprintf(file_name, OPAL_PATH_MAX, "%s/shmem_job_%u_pe_%llu", mca_sshmem_base_backing_file_dir, ORTE_PROC_MY_NAME->jobid, (unsigned long long)pe);

    return file_name;
}


void
shmem_ds_reset(map_segment_t *ds_buf)
{
    MAP_SEGMENT_RESET_FLAGS(ds_buf);
    ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
    ds_buf->super.va_base = 0;
    ds_buf->super.va_end = 0;
    ds_buf->seg_size = 0;
    ds_buf->type = MAP_SEGMENT_UNKNOWN;
}

