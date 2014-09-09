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

#include <errno.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif /* HAVE_NETDB_H */
#ifdef HAVE_TIME_H
#include <time.h>
#endif /* HAVE_NETDB_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

#include "sshmem_verbs.h"


static openib_device_t memheap_device;

/* ////////////////////////////////////////////////////////////////////////// */
/*local functions */
/* local functions */
static int
module_init(void);

static int
segment_create(map_segment_t *ds_buf,
               const char *file_name,
               size_t size);

static int
ds_copy(const map_segment_t *from,
        map_segment_t *to);

static void *
segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey);

static int
segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey);

static int
segment_unlink(map_segment_t *ds_buf);

static int
module_finalize(void);

/*
 * mmap shmem module
 */
mca_sshmem_verbs_module_t mca_sshmem_verbs_module = {
    /* super */
    {
        module_init,
        segment_create,
        ds_copy,
        segment_attach,
        segment_detach,
        segment_unlink,
        module_finalize
    }
};

/* ////////////////////////////////////////////////////////////////////////// */
/* private utility functions */
/* ////////////////////////////////////////////////////////////////////////// */

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * completely resets the contents of *ds_buf
 */
static inline void
shmem_ds_reset(map_segment_t *ds_buf)
{
  OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: shmem_ds_resetting "
         "(id: %d, size: %lu, name: %s)\n",
         mca_sshmem_verbs_component.super.base_version.mca_type_name,
         mca_sshmem_verbs_component.super.base_version.mca_component_name,
         ds_buf->seg_id, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
    );

    MAP_SEGMENT_RESET_FLAGS(ds_buf);
    ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
    ds_buf->seg_base_addr = 0;
    ds_buf->end = 0;
    ds_buf->seg_size = 0;
    ds_buf->type = MAP_SEGMENT_UNKNOWN;
    memset(ds_buf->seg_name, '\0', sizeof(ds_buf->seg_name));
}

/* ////////////////////////////////////////////////////////////////////////// */
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
ds_copy(const map_segment_t *from,
        map_segment_t *to)
{
    memcpy(to, from, sizeof(map_segment_t));

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: ds_copy complete "
         "from: (id: %d, size: %lu, "
         "name: %s flags: 0x%02x) "
         "to: (id: %d, size: %lu, "
         "name: %s flags: 0x%02x)\n",
         mca_sshmem_verbs_component.super.base_version.mca_type_name,
         mca_sshmem_verbs_component.super.base_version.mca_component_name,
         from->seg_id, (unsigned long)from->seg_size, from->seg_name,
         from->flags, to->seg_id, (unsigned long)to->seg_size, to->seg_name,
         to->flags)
    );

    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_create(map_segment_t *ds_buf,
               const char *file_name,
               size_t size)
{
    int rc = OSHMEM_SUCCESS;
    void *addr = NULL;
    openib_device_t *device = &memheap_device;
    int num_devs = 0;
    int i = 0;

    assert(ds_buf);

    /* init the contents of map_segment_t */
    shmem_ds_reset(ds_buf);

    memset(device, 0, sizeof(*device));

#ifdef HAVE_IBV_GET_DEVICE_LIST
    device->ib_devs = ibv_get_device_list(&num_devs);
#else
#error unsupported ibv_get_device_list in infiniband/verbs.h
#endif

    if (num_devs == 0 || !device->ib_devs) {
        return OSHMEM_ERR_NOT_SUPPORTED;
    }

    /* Open device */
    if (NULL != mca_sshmem_verbs_component.hca_name) {
        for (i = 0; i < num_devs; i++) {
            if (0 == strcmp(mca_sshmem_verbs_component.hca_name, ibv_get_device_name(device->ib_devs[i]))) {
                device->ib_dev = device->ib_devs[i];
                break;
            }
        }
    } else {
        device->ib_dev = device->ib_devs[0];
    }

    if (NULL == device->ib_dev) {
        OPAL_OUTPUT_VERBOSE(
            (5, oshmem_sshmem_base_framework.framework_output,
            "error getting device says %d: %s",
            errno, strerror(errno))
            );
        return OSHMEM_ERR_NOT_FOUND;
    }

    if (NULL == (device->ib_dev_context = ibv_open_device(device->ib_dev))) {
        OPAL_OUTPUT_VERBOSE(
            (5, oshmem_sshmem_base_framework.framework_output,
            "error obtaining device context for %s errno says %d: %s",
            ibv_get_device_name(device->ib_dev), errno, strerror(errno))
            );
        return OSHMEM_ERR_RESOURCE_BUSY;
    }

    /* Obtain device attributes */
    if (ibv_query_device(device->ib_dev_context, &device->ib_dev_attr)) {
        OPAL_OUTPUT_VERBOSE(
            (5, oshmem_sshmem_base_framework.framework_output,
            "error obtaining device attributes for %s errno says %d: %s",
            ibv_get_device_name(device->ib_dev), errno, strerror(errno))
            );
        return OSHMEM_ERR_RESOURCE_BUSY;
    }

    /* Allocate the protection domain for the device */
    device->ib_pd = ibv_alloc_pd(device->ib_dev_context);
    if (NULL == device->ib_pd) {
        OPAL_OUTPUT_VERBOSE(
            (5, oshmem_sshmem_base_framework.framework_output,
            "error allocating protection domain for %s errno says %d: %s",
            ibv_get_device_name(device->ib_dev), errno, strerror(errno))
            );
        return OSHMEM_ERR_RESOURCE_BUSY;
    }

    /* Allocate memory */
    if (!rc) {
        void *addr = NULL;
        struct ibv_mr *ib_mr = NULL;
        uint64_t access_flag = IBV_ACCESS_LOCAL_WRITE |
                          IBV_ACCESS_REMOTE_WRITE |
                          IBV_ACCESS_REMOTE_READ;
        uint64_t exp_access_flag = 0;

        OBJ_CONSTRUCT(&device->ib_mr_array, opal_value_array_t);
        opal_value_array_init(&device->ib_mr_array, sizeof(struct ibv_mr *));

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
        exp_access_flag = IBV_EXP_ACCESS_ALLOCATE_MR |
                          IBV_EXP_ACCESS_SHARED_MR_USER_READ |
                          IBV_EXP_ACCESS_SHARED_MR_USER_WRITE;
#endif /* MPAGE_ENABLE */

        struct ibv_exp_reg_mr_in in = {device->ib_pd, addr, size, access_flag|exp_access_flag, 0};

#if MPAGE_HAVE_IBV_EXP_REG_MR_CREATE_FLAGS
        if (0 == mca_sshmem_verbs_component.has_shared_mr) {
            in.addr = (void *)mca_sshmem_base_start_address;
            in.comp_mask    = IBV_EXP_REG_MR_CREATE_FLAGS;
            in.create_flags = IBV_EXP_REG_MR_CREATE_CONTIG;
            in.exp_access   = access_flag;
        }
#endif
        ib_mr = ibv_exp_reg_mr(&in);
        if (NULL == ib_mr) {
            OPAL_OUTPUT_VERBOSE(
                (5, oshmem_sshmem_base_framework.framework_output,
                    "error to ibv_exp_reg_mr() %llu bytes errno says %d: %s",
                    (unsigned long long)size, errno, strerror(errno))
                );
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        } else {
            device->ib_mr_shared = ib_mr;
            opal_value_array_append_item(&device->ib_mr_array, &ib_mr);
        }

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
        if (!rc && mca_sshmem_verbs_component.has_shared_mr) {
            access_flag = IBV_ACCESS_LOCAL_WRITE |
                          IBV_ACCESS_REMOTE_WRITE |
                          IBV_ACCESS_REMOTE_READ|
                          IBV_EXP_ACCESS_NO_RDMA;

            addr = (void *)mca_sshmem_base_start_address;
            struct ibv_exp_reg_shared_mr_in in;
            mca_sshmem_verbs_fill_shared_mr(&in, device->ib_pd, device->ib_mr_shared->handle, addr, access_flag);
            ib_mr = ibv_exp_reg_shared_mr(&in);
            if (NULL == ib_mr) {
                OPAL_OUTPUT_VERBOSE(
                    (5, oshmem_sshmem_base_framework.framework_output,
                        "error to ibv_reg_shared_mr() %llu bytes errno says %d: %s has_shared_mr: %d",
                        (unsigned long long)size, errno, strerror(errno),
                        mca_sshmem_verbs_component.has_shared_mr
                        )
                    );
                rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            } else {
                opal_value_array_append_item(&device->ib_mr_array, &ib_mr);
            }
        }
#endif /* MPAGE_ENABLE */

        if (!rc) {
            OPAL_OUTPUT_VERBOSE(
                (70, oshmem_sshmem_base_framework.framework_output,
                "ibv device %s shared_mr: %d",
                ibv_get_device_name(device->ib_dev),
                mca_sshmem_verbs_component.has_shared_mr)
                );

            if (mca_sshmem_verbs_component.has_shared_mr) {
                assert(size == device->ib_mr_shared->length);
                ds_buf->type = MAP_SEGMENT_ALLOC_IBV;
                ds_buf->seg_id = device->ib_mr_shared->handle;
            } else {
                ds_buf->type = MAP_SEGMENT_ALLOC_IBV_NOSHMR;
                ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
            }
            ds_buf->seg_base_addr = ib_mr->addr;
            ds_buf->seg_size = size;
            ds_buf->end = (void*)((uintptr_t)ds_buf->seg_base_addr + ds_buf->seg_size);
        }
    }

    OPAL_OUTPUT_VERBOSE(
          (70, oshmem_sshmem_base_framework.framework_output,
           "%s: %s: create %s "
           "(id: %d, addr: %p size: %lu, name: %s)\n",
           mca_sshmem_verbs_component.super.base_version.mca_type_name,
           mca_sshmem_verbs_component.super.base_version.mca_component_name,
           (rc ? "failure" : "successful"),
           ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
      );

    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * segment_attach can only be called after a successful call to segment_create
 */
static void *
segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    openib_device_t *device = &memheap_device;
    static int mr_count = 0;
    void *addr = NULL;

    assert(ds_buf);
    assert(mkey->va_base == 0);

    if (MAP_SEGMENT_SHM_INVALID == (int)(mkey->u.key)) {
        return (mkey->va_base);
    }

    /* workaround mtt problem - request aligned addresses */
    ++mr_count;
    addr = (void *)((uintptr_t)mca_sshmem_base_start_address +
        mca_sshmem_verbs_component.mr_interleave_factor * 1024ULL * 1024ULL * 1024ULL * mr_count);
    {
        struct ibv_mr *ib_mr = NULL;
        uint64_t access_flag = IBV_ACCESS_LOCAL_WRITE |
            IBV_ACCESS_REMOTE_WRITE |
            IBV_ACCESS_REMOTE_READ |
            IBV_EXP_ACCESS_NO_RDMA;
        struct ibv_exp_reg_shared_mr_in in;

        mca_sshmem_verbs_fill_shared_mr(&in, device->ib_pd, mkey->u.key, addr, access_flag);
        ib_mr = ibv_exp_reg_shared_mr(&in);
        if (NULL == ib_mr) {
            mkey->va_base = (void *)-1;
            OPAL_OUTPUT_VERBOSE(
                (5, oshmem_sshmem_base_framework.framework_output,
                    "error to ibv_reg_shared_mr() %llu bytes errno says %d: %s",
                    (unsigned long long)ds_buf->seg_size, errno, strerror(errno))
                );
        } else {
            if (ib_mr->addr != addr) {
                OPAL_OUTPUT_VERBOSE(
                    (5, oshmem_sshmem_base_framework.framework_output,
                        "Failed to map shared region to address %p got addr %p. Try to increase 'memheap_mr_interleave_factor' from %d",
                        addr, ib_mr->addr, mca_sshmem_verbs_component.mr_interleave_factor)
                    );
            }

            opal_value_array_append_item(&device->ib_mr_array, &ib_mr);
            mkey->va_base = ib_mr->addr;
        }
    }

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: attach successful "
            "(id: %d, addr: %p size: %lu, name: %s | va_base: 0x%p len: %d key %llx)\n",
            mca_sshmem_verbs_component.super.base_version.mca_type_name,
            mca_sshmem_verbs_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name,
            mkey->va_base, mkey->len, (unsigned long long)mkey->u.key)
    );

    /* update returned base pointer with an offset that hides our stuff */
    return (mkey->va_base);
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    int rc = OSHMEM_SUCCESS;
    openib_device_t *device = &memheap_device;
    int i;

    assert(ds_buf);

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: detaching "
            "(id: %d, addr: %p size: %lu, name: %s)\n",
            mca_sshmem_verbs_component.super.base_version.mca_type_name,
            mca_sshmem_verbs_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
    );

    if (device) {
        if (0 < (i = opal_value_array_get_size(&device->ib_mr_array))) {
            struct ibv_mr** array;
            struct ibv_mr* ib_mr = NULL;
            array = OPAL_VALUE_ARRAY_GET_BASE(&device->ib_mr_array, struct ibv_mr *);
            for (i--;i >= 0; i--) {
                ib_mr = array[i];
                if(ibv_dereg_mr(ib_mr)) {
                    OPAL_OUTPUT_VERBOSE(
                        (5, oshmem_sshmem_base_framework.framework_output,
                            "error ibv_dereg_mr(): %d: %s",
                            errno, strerror(errno))
                        );
                    rc = OSHMEM_ERROR;
                }
                opal_value_array_remove_item(&device->ib_mr_array, i);
            }

            if (!rc && device->ib_mr_shared) {
                device->ib_mr_shared = NULL;
            }
            OBJ_DESTRUCT(&device->ib_mr_array);
        }

        if (!rc && device->ib_pd) {
            if (ibv_dealloc_pd(device->ib_pd)) {
                OPAL_OUTPUT_VERBOSE(
                    (5, oshmem_sshmem_base_framework.framework_output,
                        "error ibv_dealloc_pd(): %d: %s",
                        errno, strerror(errno))
                    );
                rc = OSHMEM_ERROR;
            } else {
                device->ib_pd = NULL;
            }
        }

        if(!rc && device->ib_dev_context) {
            if(ibv_close_device(device->ib_dev_context)) {
                OPAL_OUTPUT_VERBOSE(
                    (5, oshmem_sshmem_base_framework.framework_output,
                        "error ibv_close_device(): %d: %s",
                        errno, strerror(errno))
                    );
                rc = OSHMEM_ERROR;
            } else {
                device->ib_dev_context = NULL;
            }
        }

        if(!rc && device->ib_devs) {
            ibv_free_device_list(device->ib_devs);
            device->ib_devs = NULL;
        }
    }

    /* reset the contents of the map_segment_t associated with this
     * shared memory segment.
     */
    shmem_ds_reset(ds_buf);

    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_unlink(map_segment_t *ds_buf)
{
    /* not much unlink work needed for sysv */

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: unlinking "
            "(id: %d, addr: %p size: %lu, name: %s)\n",
            mca_sshmem_verbs_component.super.base_version.mca_type_name,
            mca_sshmem_verbs_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
    );

    /* don't completely reset.  in particular, only reset
     * the id and flip the invalid bit.  size and name values will remain valid
     * across unlinks. other information stored in flags will remain untouched.
     */
    ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
    /* note: this is only changing the valid bit to 0. */
    MAP_SEGMENT_INVALIDATE(ds_buf);

    return OSHMEM_SUCCESS;
}

