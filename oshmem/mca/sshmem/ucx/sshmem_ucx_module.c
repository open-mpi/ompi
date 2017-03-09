/*
 * Copyright (c) 2017      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "orte/util/show_help.h"

#include "oshmem/proc/proc.h"
#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/spml/ucx/spml_ucx.h"

#include "sshmem_ucx.h"

/* ////////////////////////////////////////////////////////////////////////// */
/*local functions */
/* local functions */
static int
module_init(void);

static int
segment_create(map_segment_t *ds_buf,
               const char *file_name,
               size_t size);

static void *
segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey);

static int
segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey);

static int
segment_unlink(map_segment_t *ds_buf);

static int
module_finalize(void);

/*
 * ucx shmem module
 */
mca_sshmem_ucx_module_t mca_sshmem_ucx_module = {
    /* super */
    {
        module_init,
        segment_create,
        segment_attach,
        segment_detach,
        segment_unlink,
        module_finalize
    }
};

static int
module_init(void)
{
    /* nothing to do */
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
module_finalize(void)
{
    /* nothing to do */
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */

static int
segment_create(map_segment_t *ds_buf,
               const char *file_name,
               size_t size)
{
    int rc = OSHMEM_SUCCESS;
    mca_spml_ucx_t *spml = (mca_spml_ucx_t *)mca_spml.self;
    ucp_mem_map_params_t mem_map_params;
    ucp_mem_h mem_h;
    ucs_status_t status;

    assert(ds_buf);

    /* init the contents of map_segment_t */
    shmem_ds_reset(ds_buf);

    mem_map_params.field_mask = UCP_MEM_MAP_PARAM_FIELD_ADDRESS |
                                UCP_MEM_MAP_PARAM_FIELD_LENGTH |
                                UCP_MEM_MAP_PARAM_FIELD_FLAGS;

    mem_map_params.address    = (void *)mca_sshmem_base_start_address;
    mem_map_params.length     = size;
    mem_map_params.flags      = UCP_MEM_MAP_ALLOCATE|UCP_MEM_MAP_FIXED;

    if (spml->heap_reg_nb) {
        mem_map_params.flags |= UCP_MEM_MAP_NONBLOCK;
    }

    status = ucp_mem_map(spml->ucp_context, &mem_map_params, &mem_h);
    if (UCS_OK != status) {
        rc = OSHMEM_ERROR;
        goto out;
    }

    ds_buf->super.va_base = mem_map_params.address;
    ds_buf->seg_size      = size;
    ds_buf->super.va_end  = (void*)((uintptr_t)ds_buf->super.va_base + ds_buf->seg_size);
    ds_buf->context       = mem_h;
    ds_buf->type          = MAP_SEGMENT_ALLOC_UCX;

out:
    OPAL_OUTPUT_VERBOSE(
          (70, oshmem_sshmem_base_framework.framework_output,
           "%s: %s: create %s "
           "(id: %d, addr: %p size: %lu)\n",
           mca_sshmem_ucx_component.super.base_version.mca_type_name,
           mca_sshmem_ucx_component.super.base_version.mca_component_name,
           (rc ? "failure" : "successful"),
           ds_buf->seg_id, ds_buf->super.va_base, (unsigned long)ds_buf->seg_size)
      );
    return rc;
}

static void *
segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    assert(ds_buf);
    assert(mkey->va_base == 0);

    OPAL_OUTPUT((oshmem_sshmem_base_framework.framework_output,
                "can not attach to ucx segment"));
    oshmem_shmem_abort(-1);
    return NULL;
}

static int
segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: detaching "
            "(id: %d, addr: %p size: %lu)\n",
            mca_sshmem_ucx_component.super.base_version.mca_type_name,
            mca_sshmem_ucx_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->super.va_base, (unsigned long)ds_buf->seg_size)
    );

    /* reset the contents of the map_segment_t associated with this
     * shared memory segment.
     */
    shmem_ds_reset(ds_buf);

    return OSHMEM_SUCCESS;
}

static int
segment_unlink(map_segment_t *ds_buf)
{
    mca_spml_ucx_t *spml = (mca_spml_ucx_t *)mca_spml.self;

    assert(ds_buf);

    ucp_mem_unmap(spml->ucp_context, (ucp_mem_h)ds_buf->context);

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: unlinking "
            "(id: %d, addr: %p size: %lu)\n",
            mca_sshmem_ucx_component.super.base_version.mca_type_name,
            mca_sshmem_ucx_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->super.va_base, (unsigned long)ds_buf->seg_size)
    );

    ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
    MAP_SEGMENT_INVALIDATE(ds_buf);

    return OSHMEM_SUCCESS;
}

