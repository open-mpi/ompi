/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include <sys/ipc.h>
#include <sys/shm.h>

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
#include <infiniband/verbs.h>
#endif /* MPAGE_ENABLE */

static int __shm_attach(map_segment_t *, size_t, int, int);
static void __shm_detach(map_segment_t *);

static int __mmap_attach(map_segment_t *, size_t);
static void __mmap_detach(map_segment_t *);

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
static int __ibv_attach(map_segment_t *, size_t);
static void __ibv_detach(map_segment_t *);
#endif /* MPAGE_ENABLE */

static int __adaptive_attach(map_segment_t *, size_t);


int mca_memheap_base_alloc_init(mca_memheap_map_t *map, size_t size)
{
    int ret = OSHMEM_SUCCESS;
    int value = mca_memheap_base_shmalloc_use_hugepages;

    assert(map);
    assert(HEAP_SEG_INDEX == map->n_segments);

    MEMHEAP_VERBOSE(5,"memheap method : %d",
            mca_memheap_base_shmalloc_use_hugepages);

    map_segment_t *s = &map->mem_segs[map->n_segments];
    memset(s, 0, sizeof(*s));
    s->is_active = 0;
    s->shmid = MEMHEAP_SHM_INVALID;
    s->start = 0;
    s->end = 0;
    s->size = 0;
    s->type = MAP_SEGMENT_UNKNOWN;
    s->context = NULL;

    switch(value) {
    case 0:
        /* use sysv alloc without hugepages */
        ret = __shm_attach(s, size, 0, 1);
        break;

    case 1:
        ret = __shm_attach(s, size, 1, 1);
        if (OSHMEM_SUCCESS != ret)
            ret = __shm_attach(s, size, 0, 1);
        break;    

    case 2:
        /* huge pages only */
        ret = __shm_attach(s, size, 1, 1);
        if (OSHMEM_SUCCESS != ret)
            MEMHEAP_ERROR("FAILED to allocated symmetric heap using hugepages fallback is disabled, errno=%d", errno);
        break;

    case 3:
        /* huge pages only + cleanup shmid */
        ret = __shm_attach(s, size, 1, 0);
        if (OSHMEM_SUCCESS != ret)
            MEMHEAP_ERROR("FAILED to allocated symmetric heap using hugepages fallback is disabled, errno=%d", errno);
        break;

    case 4:
        /* use sysv alloc without hugepages */
        ret = __shm_attach(s, size, 0, 0);
        break;

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
    case 5:
        /* use shared memory registration (mpages) */
        ret = __ibv_attach(s, size);
        break;
#endif /* MPAGE_ENABLE */

    case 100:
        /* use mmap. It will severaly impact performance of intra node communication */
        ret = __mmap_attach(s, size);
        MEMHEAP_VERBOSE(1, "mmap() memheap allocation will severely impact performance of intra node communication");
        break;

    case 101:
        ret = __shm_attach(s, size, 1, 1);
        if (OSHMEM_SUCCESS != ret) {
            MEMHEAP_ERROR("Failed to allocate hugepages. Falling back on regular allocation");
            ret = __mmap_attach(s, size);
        }
        else {
            s->shmid = MEMHEAP_SHM_INVALID;
        }
        MEMHEAP_VERBOSE(1, "SM BTL will be always used for intranode comm\n");
        break;

    case 102:
        ret = __shm_attach(s, size, 1, 1);
        if (OSHMEM_SUCCESS != ret) {
            MEMHEAP_ERROR("FAILED to allocated symmetric heap using hugepages fallback is disabled, errno=%d", errno);
        }
        else {
            s->shmid = MEMHEAP_SHM_INVALID;
        }
        break;

    default:
        ret = __adaptive_attach(s, size);
//        MEMHEAP_ERROR("Unknown memheap allocation method: %d", value);
//        ret = OSHMEM_ERR_BAD_PARAM;
    }

    if (OSHMEM_SUCCESS == ret)
    {
        map->n_segments++;
        MEMHEAP_VERBOSE(1, "Memheap alloc memory: %llu byte(s), %d segments by method: %d",
                (unsigned long long)size, map->n_segments, s->type);
    }

    return ret;
}


void mca_memheap_base_alloc_exit(mca_memheap_map_t *map)
{
    if (map)
    {
        map_segment_t *s = &map->mem_segs[HEAP_SEG_INDEX];

        assert(s);

        switch(s->type) {
        case MAP_SEGMENT_ALLOC_SHM:
            __shm_detach(s);
            break;

        case MAP_SEGMENT_ALLOC_MMAP:
            __mmap_detach(s);
            break;

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
        case MAP_SEGMENT_ALLOC_IBV:
            __ibv_detach(s);
            break;
#endif /* MPAGE_ENABLE */

        default:
            MEMHEAP_ERROR("Unknown segment type: %d", (int)s->type);
        }
    }
}


static int __adaptive_attach(map_segment_t *s, size_t size)
{
    int rc = OSHMEM_SUCCESS;

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
    rc = __ibv_attach(s, size);
#endif /* MPAGE_ENABLE */
    
    if (rc)
    {
        rc = __shm_attach(s, size, 1, 1);
    }

    if (rc)
    {
        rc = __shm_attach(s, size, 0, 1);
    }

    if (rc)
    {
        rc = __shm_attach(s, size, 0, 0);
    }

    if (rc)
    {
        rc = __mmap_attach(s, size);
    }

    return rc;
}


static int __shm_attach(map_segment_t *s, size_t size, int use_hp, int do_rmid)
{
    static int shm_context = 0;;
    void *addr = NULL;
    int shmid = MEMHEAP_SHM_INVALID;
    int flags;

    assert(s);

    shm_context = use_hp;

    flags = IPC_CREAT | IPC_EXCL | SHM_R | SHM_W;
    flags |= ( use_hp ? SHM_HUGETLB : 0 );

    /* Create a new shared memory segment and save the shmid. */
    shmid = shmget(IPC_PRIVATE, size, flags);
    if (shmid == MEMHEAP_SHM_INVALID) {
        MEMHEAP_VERBOSE(1, "Failed to get shm segment (errno=%d)", errno);
        return OSHMEM_ERROR; 
    }

    /* Attach to the sement */
    addr = shmat(shmid, (void *)MEMHEAP_BASE_START_ADDRESS, 0);
    if (addr == (void *)-1L) {
        MEMHEAP_VERBOSE(1, "Failed to attach to shm segment (errno=%d)", errno);

        shmctl(shmid, IPC_RMID, NULL);
        return OSHMEM_ERR_OUT_OF_RESOURCE; 
    }

	MEMHEAP_VERBOSE(5, "got shmid %d", shmid);

    if (do_rmid)
        shmctl(shmid, IPC_RMID, NULL);

    s->type = MAP_SEGMENT_ALLOC_SHM;
    s->shmid = shmid;
    s->start = (uintptr_t)addr;
    s->size = size;
    s->end = s->start + s->size;
    s->context = &shm_context;

    return OSHMEM_SUCCESS;
}


static void __shm_detach(map_segment_t *s)
{
    assert(s);

    if (s->shmid != MEMHEAP_SHM_INVALID)
    {
        shmctl(s->shmid, IPC_RMID, NULL);
    }

    if (s->context && (*((int *)(s->context))) > 0) {
	/**
	 *  Workaround kernel panic when detaching huge pages from user space simultanously from several processes
	 *  dont detach here instead let kernel do it during process cleanup
	 */
        //shmdt((void *)s->start);
    }
}


static int __mmap_attach(map_segment_t *s, size_t size)
{
    void *addr = NULL;

    assert(s);

    addr = mmap((void *)MEMHEAP_BASE_START_ADDRESS, 
            size, 
            PROT_READ|PROT_WRITE, 
            MAP_SHARED|MAP_ANONYMOUS|MAP_FIXED, 0, 0);

    if (MAP_FAILED == addr) {
        MEMHEAP_ERROR("Failed to mmap() %llu bytes (errno=%d)", (unsigned long long)size, errno);
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    s->type = MAP_SEGMENT_ALLOC_MMAP;
    s->shmid = MEMHEAP_SHM_INVALID;
    s->start = (uintptr_t)addr;
    s->size = size;
    s->end = s->start + s->size;
    s->context = NULL;

    return OSHMEM_SUCCESS;
}


static void __mmap_detach(map_segment_t *s)
{
    assert(s);

    munmap((void *)s->start, s->size);
}


#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)

static int __ibv_attach(map_segment_t *s, size_t size)
{
    int rc = OSHMEM_SUCCESS;
    static openib_device_t memheap_device;
    openib_device_t *device = &memheap_device;
    int num_devs = 0;

    assert(s);

    memset(device, 0, sizeof(*device));

#ifdef HAVE_IBV_GET_DEVICE_LIST
    device->ib_devs = ibv_get_device_list(&num_devs);
#else
    #error unsupported ibv_get_device_list in infiniband/verbs.h
#endif
    
    if (num_devs == 0 || !device->ib_devs)
    {
        rc = OSHMEM_ERR_NOT_SUPPORTED;
    }

    /* Open device */
    if (!rc)
    {
        int i = 0;

        if (num_devs > 1)
            MEMHEAP_VERBOSE(5, "found %d HCAs, choosing the first", num_devs);

        for (i = 0; i < num_devs; i++)
        {
            device->ib_dev = device->ib_devs[i];

            device->ib_dev_context = ibv_open_device(device->ib_dev);
            if (NULL == device->ib_dev_context)
            {
                MEMHEAP_ERROR("error obtaining device context for %s errno says %d: %s",
                            ibv_get_device_name(device->ib_dev), errno, strerror(errno));
                rc = OSHMEM_ERR_RESOURCE_BUSY;
            }
            else
            {
                MEMHEAP_VERBOSE(5, "selected %s as %d of %d", ibv_get_device_name(device->ib_dev), i, num_devs);
                rc = OSHMEM_SUCCESS;
                break;
            }
        }
    }

    /* Obtain device attributes */
    if (!rc)
    {
        if (ibv_query_device(device->ib_dev_context, &device->ib_dev_attr))
        {
            MEMHEAP_ERROR("error obtaining device attributes for %s errno says %d: %s",
                        ibv_get_device_name(device->ib_dev), errno, strerror(errno));
            rc = OSHMEM_ERR_RESOURCE_BUSY;
        }
        else
        {
            MEMHEAP_VERBOSE(5, "ibv device %s",
                        ibv_get_device_name(device->ib_dev));
        }
    }

    /* Allocate the protection domain for the device */
    if (!rc)
    {
        device->ib_pd = ibv_alloc_pd(device->ib_dev_context);
        if (NULL == device->ib_pd)
        {
            MEMHEAP_ERROR("error allocating protection domain for %s errno says %d: %s",
                        ibv_get_device_name(device->ib_dev), errno, strerror(errno));
            rc = OSHMEM_ERR_RESOURCE_BUSY;
        }
    }

    /* Allocate memory */
    if (!rc)
    {
        void *addr = NULL;
        struct ibv_mr *ib_mr = NULL;
        int access_flag = IBV_ACCESS_LOCAL_WRITE |
                          IBV_ACCESS_REMOTE_WRITE | 
                          IBV_ACCESS_REMOTE_READ;

        OBJ_CONSTRUCT(&device->ib_mr_array, opal_value_array_t);
        opal_value_array_init(&device->ib_mr_array, sizeof(struct ibv_mr *));

#if defined(MPAGE_ENABLE) && ((MPAGE_ENABLE == 1) || (MPAGE_ENABLE == 2))
		access_flag |= IBV_ACCESS_ALLOCATE_MR |
                               IBV_ACCESS_SHARED_MR_USER_READ |
                               IBV_ACCESS_SHARED_MR_USER_WRITE;
#endif /* MPAGE_ENABLE */

        ib_mr = ibv_reg_mr(device->ib_pd, addr, size, access_flag);
        if (NULL == ib_mr) 
        {
            MEMHEAP_ERROR("error to ibv_reg_mr() %llu bytes errno says %d: %s", 
                        (unsigned long long)size, errno, strerror(errno));
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        }
        else
        {
            device->ib_mr_shared = ib_mr;
            opal_value_array_append_item(&device->ib_mr_array, &ib_mr);
        }

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE == 2)
        if (!rc)
        {
			access_flag = IBV_ACCESS_LOCAL_WRITE |
                                      IBV_ACCESS_REMOTE_WRITE | 
                                      IBV_ACCESS_REMOTE_READ|
                                      IBV_ACCESS_NO_RDMA;

            addr = (void *)MEMHEAP_BASE_START_ADDRESS;
            ib_mr = ibv_reg_shared_mr(device->ib_mr_shared->handle,
                                   device->ib_pd, addr, access_flag);
            if (NULL == ib_mr) 
            {
                MEMHEAP_ERROR("error to ibv_reg_shared_mr() %llu bytes errno says %d: %s", 
                            (unsigned long long)size, errno, strerror(errno));
                rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            }
            else
            {
                opal_value_array_append_item(&device->ib_mr_array, &ib_mr);
            }
        }
#endif /* MPAGE_ENABLE */

        if (!rc)
        {
            assert(size == device->ib_mr_shared->length);

            s->type = MAP_SEGMENT_ALLOC_IBV;
            s->shmid = device->ib_mr_shared->handle;
            s->start = (intptr_t)ib_mr->addr;
            s->size = size;
            s->end = s->start + s->size;
            s->context = &memheap_device;
        }
    }

    return rc;
}


static void __ibv_detach(map_segment_t *s)
{
    int rc = OSHMEM_SUCCESS;
    openib_device_t *device = NULL;

    assert(s);

    device = (openib_device_t *)s->context;

    if (device)
    {
        if(!rc && opal_value_array_get_size(&device->ib_mr_array)) 
        {
            struct ibv_mr** array;
            struct ibv_mr* ib_mr = NULL;
            array = OPAL_VALUE_ARRAY_GET_BASE(&device->ib_mr_array, struct ibv_mr *);
            while (opal_value_array_get_size(&device->ib_mr_array) > 0) 
            {
                ib_mr = array[0];
                if(ibv_dereg_mr(ib_mr)) 
                {
                    MEMHEAP_ERROR("error ibv_dereg_mr(): %d: %s", errno, strerror(errno));
                    rc = OSHMEM_ERROR;
                }
                opal_value_array_remove_item(&device->ib_mr_array, 0);
            }

            if(!rc && device->ib_mr_shared) 
            {
                device->ib_mr_shared = NULL;
            }
            OBJ_DESTRUCT(&device->ib_mr_array);
        }

        if(!rc && device->ib_pd) 
        {
            if(ibv_dealloc_pd(device->ib_pd)) 
            {
                MEMHEAP_ERROR("error ibv_dealloc_pd(): %d: %s", errno, strerror(errno));
                rc = OSHMEM_ERROR;
            }
            else
            {
                device->ib_pd = NULL;
            }
        }

        if(!rc && device->ib_dev_context) 
        {
            if(ibv_close_device(device->ib_dev_context)) 
            {
                MEMHEAP_ERROR("error ibv_close_device(): %d: %s", errno, strerror(errno));
                rc = OSHMEM_ERROR;
            }
            else
            {
                device->ib_dev_context = NULL;
            }
        }

        if(!rc && device->ib_devs) 
        {
            ibv_free_device_list(device->ib_devs);
            device->ib_devs = NULL;
        }
    }
}

#endif /* MPAGE_ENABLE */
