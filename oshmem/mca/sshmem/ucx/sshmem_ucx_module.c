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
#include "oshmem/include/shmemx.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/spml/ucx/spml_ucx.h"

#include "sshmem_ucx.h"

//#include <ucs/sys/math.h>

#if HAVE_UCX_DEVICE_MEM
#include <ucp/core/ucp_resource.h>
#include <uct/ib/base/ib_alloc.h>
#endif

#define ALLOC_ELEM_SIZE sizeof(uint64_t)
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

/* ////////////////////////////////////////////////////////////////////////// */
/*local functions */
/* local functions */
static int
module_init(void);

static int
segment_create(map_segment_t *ds_buf,
               const char *file_name,
               size_t size, long hint);

static void *
segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey);

static int
segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey);

static int
segment_unlink(map_segment_t *ds_buf);

static int
module_finalize(void);

static int sshmem_ucx_memheap_realloc(map_segment_t *s, size_t size,
                                      void* old_ptr, void** new_ptr);

static int sshmem_ucx_memheap_free(map_segment_t *s, void* ptr);

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

static segment_allocator_t sshmem_ucx_allocator = {
    .realloc = sshmem_ucx_memheap_realloc,
    .free    = sshmem_ucx_memheap_free
};

static int
segment_create_internal(map_segment_t *ds_buf, void *address, size_t size,
                        unsigned flags, long hint, void *dev_mem)
{
    mca_sshmem_ucx_segment_context_t *ctx;
    int rc = OSHMEM_SUCCESS;
    mca_spml_ucx_t *spml = (mca_spml_ucx_t*)mca_spml.self;
    ucp_mem_map_params_t mem_map_params;
    ucp_mem_h mem_h;
    ucs_status_t status;

    assert(ds_buf);

    /* init the contents of map_segment_t */
    shmem_ds_reset(ds_buf);

    mem_map_params.field_mask = UCP_MEM_MAP_PARAM_FIELD_ADDRESS |
                                UCP_MEM_MAP_PARAM_FIELD_LENGTH |
                                UCP_MEM_MAP_PARAM_FIELD_FLAGS;

    mem_map_params.address    = address;
    mem_map_params.length     = size;
    mem_map_params.flags      = flags;

    status = ucp_mem_map(spml->ucp_context, &mem_map_params, &mem_h);
    if (UCS_OK != status) {
        SSHMEM_ERROR("ucp_mem_map() failed: %s\n", ucs_status_string(status));
        rc = OSHMEM_ERROR;
        goto out;
    }

    if (!(flags & UCP_MEM_MAP_FIXED)) {
        /* Memory was allocated at an arbitrary address; obtain it */
        ucp_mem_attr_t mem_attr;
        mem_attr.field_mask = UCP_MEM_ATTR_FIELD_ADDRESS;
        status = ucp_mem_query(mem_h, &mem_attr);
        if (status != UCS_OK) {
            SSHMEM_ERROR("ucp_mem_query() failed: %s\n", ucs_status_string(status));
            ucp_mem_unmap(spml->ucp_context, mem_h);
            rc = OSHMEM_ERROR;
            goto out;
        }

        ds_buf->super.va_base = mem_attr.address;
    } else {
        ds_buf->super.va_base = mem_map_params.address;
    }

    ctx = calloc(1, sizeof(*ctx));
    if (!ctx) {
        ucp_mem_unmap(spml->ucp_context, mem_h);
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    ds_buf->seg_size      = size;
    ds_buf->super.va_end  = (void*)((uintptr_t)ds_buf->super.va_base + ds_buf->seg_size);
    ds_buf->context       = ctx;
    ds_buf->type          = MAP_SEGMENT_ALLOC_UCX;
    ds_buf->alloc_hints   = hint;
    ctx->ucp_memh         = mem_h;
    ctx->dev_mem          = dev_mem;
    if (hint) {
        ds_buf->allocator = &sshmem_ucx_allocator;
    }

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

#if HAVE_UCX_DEVICE_MEM
static uct_ib_device_mem_h alloc_device_mem(mca_spml_ucx_t *spml, size_t size,
                                            void **address_p)
{
    uct_ib_device_mem_h dev_mem = NULL;
    ucs_status_t status;
    uct_md_h uct_md;
    void *address;
    size_t length;

    uct_md = ucp_context_find_tl_md(spml->ucp_context, "mlx5");
    if (uct_md == NULL) {
        SSHMEM_VERBOSE(1, "ucp_context_find_tl_md() returned NULL\n");
        return NULL;
    }

    /* If found a matching memory domain, allocate device memory on it */
    length  = size;
    address = NULL;
    status = uct_ib_md_alloc_device_mem(uct_md, &length, &address,
                                        UCT_MD_MEM_ACCESS_ALL, "sshmem_seg",
                                        &dev_mem);
    if (status != UCS_OK) {
        /* If could not allocate device memory - fallback to mmap (since some
         * PEs in the job may succeed and while others failed */
        SSHMEM_VERBOSE(1, "uct_ib_md_alloc_dm() failed: %s\n",
                       ucs_status_string(status));
        return NULL;
    }

    SSHMEM_VERBOSE(3, "uct_ib_md_alloc_dm() returned address %p\n", address);
    *address_p = address;
    return dev_mem;
}
#endif

static int
segment_create(map_segment_t *ds_buf,
               const char *file_name,
               size_t size, long hint)
{
    mca_spml_ucx_t *spml = (mca_spml_ucx_t*)mca_spml.self;
    unsigned flags;
    int ret;

#if HAVE_UCX_DEVICE_MEM
    if (hint & SHMEM_HINT_DEVICE_NIC_MEM) {
        if (size > UINT_MAX) {
            return OSHMEM_ERR_BAD_PARAM;
        }

        void *dev_mem_address;
        uct_ib_device_mem_h dev_mem = alloc_device_mem(spml, size,
                                                       &dev_mem_address);
        if (dev_mem != NULL) {
            ret = segment_create_internal(ds_buf, dev_mem_address, size, 0,
                                          hint, dev_mem);
            if (ret == OSHMEM_SUCCESS) {
                return OSHMEM_SUCCESS;
            } else if (dev_mem != NULL) {
                uct_ib_md_release_device_mem(dev_mem);
                /* fallback to regular allocation */
            }
        }
    }
#endif

    flags = UCP_MEM_MAP_ALLOCATE | (spml->heap_reg_nb ? UCP_MEM_MAP_NONBLOCK : 0);
    if (hint) {
        return segment_create_internal(ds_buf, NULL, size, flags, hint, NULL);
    } else {
        return segment_create_internal(ds_buf, mca_sshmem_base_start_address,
                                       size, flags | UCP_MEM_MAP_FIXED, hint,
                                       NULL);
    }
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
    mca_sshmem_ucx_segment_context_t *ctx = ds_buf->context;

    if (ctx->shadow_allocator) {
        sshmem_ucx_shadow_destroy(ctx->shadow_allocator);
    }

    ucp_mem_unmap(spml->ucp_context, ctx->ucp_memh);

#if HAVE_UCX_DEVICE_MEM
    if (ctx->dev_mem) {
        uct_ib_md_release_device_mem(ctx->dev_mem);
    }
#endif

    ds_buf->context = NULL;
    free(ctx);

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

static void *sshmem_ucx_memheap_index2ptr(map_segment_t *s, unsigned index)
{
    return (char*)s->super.va_base + (index * ALLOC_ELEM_SIZE);
}

static unsigned sshmem_ucx_memheap_ptr2index(map_segment_t *s, void *ptr)
{
    return ((char*)ptr - (char*)s->super.va_base) / ALLOC_ELEM_SIZE;
}

static void sshmem_ucx_memheap_wordcopy(void *dst, void *src, size_t size)
{
    const size_t count = (size + sizeof(uint64_t) - 1) / sizeof(uint64_t);
    uint64_t *dst64 = (uint64_t*)dst;
    uint64_t *src64 = (uint64_t*)src;
    size_t i;

    for (i = 0; i < count; ++i) {
        *(dst64++) = *(src64++);
    }
    opal_atomic_wmb();
}

static int sshmem_ucx_memheap_realloc(map_segment_t *s, size_t size,
                                      void* old_ptr, void** new_ptr)
{
    mca_sshmem_ucx_segment_context_t *ctx = s->context;
    unsigned alloc_count, index, old_index, old_alloc_count;
    int res;
    int inplace;

    if (size > s->seg_size) {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    /* create allocator on demand */
    if (!ctx->shadow_allocator) {
        ctx->shadow_allocator = sshmem_ucx_shadow_create(s->seg_size);
        if (!ctx->shadow_allocator) {
            return OSHMEM_ERR_OUT_OF_RESOURCE;
        }
    }

    /* Allocate new element. Zero-size allocation should still return a unique
     * pointer, so allocate 1 byte */
    alloc_count = max((size + ALLOC_ELEM_SIZE - 1) / ALLOC_ELEM_SIZE, 1);

    if (!old_ptr) {
        res = sshmem_ucx_shadow_alloc(ctx->shadow_allocator, alloc_count, &index);
    } else {
        old_index = sshmem_ucx_memheap_ptr2index(s, old_ptr);
        res       = sshmem_ucx_shadow_realloc(ctx->shadow_allocator, alloc_count,
                                              old_index, &index, &inplace);
    }

    if (res != OSHMEM_SUCCESS) {
        return res;
    }

    *new_ptr = sshmem_ucx_memheap_index2ptr(s, index);

    /* Copy to new segment and release old*/
    if (old_ptr && !inplace) {
        old_alloc_count = sshmem_ucx_shadow_size(ctx->shadow_allocator, old_index);
        sshmem_ucx_memheap_wordcopy(*new_ptr, old_ptr,
                                    min(size, old_alloc_count * ALLOC_ELEM_SIZE));
        sshmem_ucx_shadow_free(ctx->shadow_allocator, old_index);
    }

    return OSHMEM_SUCCESS;
}

static int sshmem_ucx_memheap_free(map_segment_t *s, void* ptr)
{
    mca_sshmem_ucx_segment_context_t *ctx = s->context;

    if (!ptr) {
        return OSHMEM_SUCCESS;
    }

    return sshmem_ucx_shadow_free(ctx->shadow_allocator,
                                  sshmem_ucx_memheap_ptr2index(s, ptr));
}
