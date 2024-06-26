/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mplconfig.h"
#ifdef MPL_HAVE_X86INTRIN_H
/* must come before mpl_trmem.h */
#include <x86intrin.h>
#endif
#include "mpl.h"
#include <assert.h>
#include <dlfcn.h>

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#ifdef MPL_HAVE_ZE
#include <dirent.h>
#if defined(MPL_HAVE_DRM_I915_DRM_H)
#include "drm/i915_drm.h"
#define MPL_ENABLE_DRMFD
#elif defined(MPL_HAVE_LIBDRM_I915_DRM_H)
#include "libdrm/i915_drm.h"
#define MPL_ENABLE_DRMFD
#endif
#include <sys/ioctl.h>
#include <sys/syscall.h>        /* Definition of SYS_* constants */
#include "uthash.h"
#include "utlist.h"

/* Latest Level-zero Specification:
 * http://spec.oneapi.com/level-zero/latest/index.html
 */
ze_context_handle_t ze_context;
ze_driver_handle_t ze_driver_handle;
/* ze_devices_handle contains all devices and subdevices. Indices [0, device_count) are
 * devices while the rest are subdevices. Keeping them all in the same array allows for easy
 * comparison of device handle when a device id is passed from the upper layer. The only time it
 * matters if we have a subdevice vs a device is when creating or mapping an ipc handle. In these
 * situations, we use the subdevice_map to find the upper device id for indexing into
 * shared_device_fds, since these are only opened on the upper devices. */
ze_device_handle_t *ze_devices_handle = NULL;

static int gpu_initialized = 0;
static uint32_t device_count;   /* Counts all local devices, does not include subdevices */
static uint32_t local_ze_device_count;  /* Counts all local devices and subdevices */
static uint32_t global_ze_device_count; /* Counts all global devices and subdevices */
static int max_dev_id;  /* Does not include subdevices */
static int max_subdev_id;
static char **device_list = NULL;
static int *engine_conversion = NULL;

#define MPL_ZE_EVENT_POOL_SIZE    (4096)

typedef struct MPL_ze_event_pool {
    ze_event_pool_handle_t pool;
    struct MPL_ze_event_pool *next, *prev;
} MPL_ze_event_pool;

static MPL_ze_event_pool *ze_event_pools = NULL;
static MPL_gpu_event *ze_events = NULL;

typedef struct {
    ze_command_queue_handle_t *cmdQueues;
    MPL_cmdlist_pool_t *cmdList_pool;
    ze_command_list_handle_t *cmdlists; /* immediate command lists */
    unsigned int numQueues, curQueue;
} MPL_ze_engine_entry_t;

typedef struct {
    int dev_id;
    unsigned int numQueueGroups;
    MPL_ze_engine_entry_t *engines;
    MPL_gpu_event *prev_event[MPL_GPU_COPY_DIRECTION_TYPES];    /* for imemcpy */
    MPL_cmdlist_pool_t *last_cmdList_entry[MPL_GPU_COPY_DIRECTION_TYPES];       /* for imemcopy */
#ifdef ZE_PCI_PROPERTIES_EXT_NAME
    ze_pci_address_ext_t pci;
    int pci_avail;
    int sys_device_index;
#endif
} MPL_ze_device_entry_t;

static MPL_ze_device_entry_t *device_states;

/* Affinity mask contents */
typedef struct {
    int num_dev;
    int *dev_id;
    int *subdev_id;
} affinity_mask_t;

static affinity_mask_t mask_contents;

#define MAX_GPU_STR_LEN 256
static char affinity_env[MAX_GPU_STR_LEN] = { 0 };

/* Mappings for translating between local and global device ids */
static int *local_to_global_map;        /* [local_ze_device_count] */
static int *global_to_local_map;        /* [global_ze_device_count] */

/* Maps a subdevice id to the upper device id, specifically for indexing into shared_device_fds */
static int *subdevice_map = NULL;
/* Keeps the subdevice count for all locally visible devices */
static uint32_t *subdevice_count = NULL;

/* For drmfd */
typedef struct _physical_device_state {
    int fd;
    int domain, bus, device, function;
} physical_device_state;

static int physical_device_count = 0;
static physical_device_state *physical_device_states = NULL;

typedef struct {
    const void *ptr;
    int dev_id;
    int handles[2];
    uint32_t nhandles;
    UT_hash_handle hh;
} MPL_ze_gem_hash_entry_t;

/*
this cache entry may cache two device pointers:
   1. from remote device which is obtained by zeOpenIpcHandle
   2. from a local device pointer which is obtained by zeGetIpchandle and mmap
*/
typedef struct {
    uint64_t mem_id;            /* key */
    MPL_gpu_ipc_mem_handle_t ipc_handle;
    int fds[2];
    uint32_t nfds;
    void *mapped_ptr;
    size_t mapped_size;         /* total size */
    bool handle_cached;
    UT_hash_handle hh;
} MPL_ze_ipc_handle_entry_t;

typedef struct {
    uint64_t remote_mem_id;
    int remote_dev_id;
    pid_t remote_pid;
} MPL_ze_mapped_buffer_lookup_t;

typedef struct {
    MPL_ze_mapped_buffer_lookup_t key;
    void *ipc_buf;
    void *mapped_ptr;
    size_t mapped_size;
    int fds[2];
    int nfds;                   /* used when doing mmap */
    UT_hash_handle hh;
} MPL_ze_mapped_buffer_entry_t;

static MPL_ze_gem_hash_entry_t *gem_hash = NULL;
static MPL_ze_ipc_handle_entry_t **ipc_cache_tracked = NULL;
static MPL_ze_mapped_buffer_entry_t **ipc_cache_mapped = NULL;
static MPL_ze_mapped_buffer_entry_t **ipc_cache_removal = NULL;
static MPL_ze_mapped_buffer_entry_t **mmap_cache_removal = NULL;

/* For pidfd */
#ifndef __NR_pidfd_open
#define __NR_pidfd_open 434     /* System call # on most architectures */
#endif
#ifndef __NR_pidfd_getfd
#define __NR_pidfd_getfd 438    /* System call # on most architectures */
#endif

typedef struct {
    void *ptr;
    uint64_t mem_id;
    UT_hash_handle hh;
} MPL_ze_mem_id_entry_t;

static MPL_ze_mem_id_entry_t *mem_id_cache = NULL;

typedef struct gpu_free_hook {
    void (*free_hook) (void *dptr);
    struct gpu_free_hook *next;
} gpu_free_hook_s;
static MPL_initlock_t free_hook_mutex = MPL_INITLOCK_INITIALIZER;

pid_t mypid;

/* *INDENT-OFF* */
typedef ze_result_t (*pFnzexMemGetIpcHandles)(ze_context_handle_t, const void *, uint32_t *, ze_ipc_mem_handle_t *);
typedef ze_result_t (*pFnzexMemOpenIpcHandles)(ze_context_handle_t, ze_device_handle_t, uint32_t, ze_ipc_mem_handle_t *, ze_ipc_memory_flags_t, void **);
static pFnzexMemGetIpcHandles zexMemGetIpcHandles = NULL;
static pFnzexMemOpenIpcHandles zexMemOpenIpcHandles = NULL;
/* *INDENT-ON* */

/* Backend-specific functions */
/* *INDENT-OFF* */
typedef ze_result_t (*pFnzexDriverImportExternalPointer)(ze_driver_handle_t, void *, size_t);
typedef ze_result_t (*pFnzexDriverReleaseImportedPointer)(ze_driver_handle_t, void *);
typedef ze_result_t (*pFnzexDriverGetHostPointerBaseAddress)(ze_driver_handle_t, void *, void **);
static pFnzexDriverImportExternalPointer zexDriverImportExternalPointer = NULL;
static pFnzexDriverReleaseImportedPointer zexDriverReleaseImportedPointer = NULL;
static pFnzexDriverGetHostPointerBaseAddress zexDriverGetHostPointerBaseAddress = NULL;
/* *INDENT-ON* */

static int gpu_ze_init_driver(void);
static int fd_to_handle(int dev_fd, int fd, int *handle);
static int handle_to_fd(int dev_fd, int handle, int *fd);
static int close_handle(int dev_fd, int handle);
static int parse_affinity_mask();
static void get_max_dev_id(int *max_dev_id, int *max_subdev_id);
static int gpu_mem_hook_init(void);
static int remove_ipc_handle_entry(MPL_ze_mapped_buffer_entry_t * cache_entry, int dev_id);
static int MPL_event_pool_add_new_pool(void);
static void MPL_event_pool_destroy(void);
#ifdef ZE_PCI_PROPERTIES_EXT_NAME
static int search_physical_devices(ze_pci_address_ext_t pci);
#endif

/* For zeMemFree callbacks */
static gpu_free_hook_s *free_hook_chain = NULL;
static ze_result_t ZE_APICALL(*sys_zeMemFree) (ze_context_handle_t hContext, void *dptr) = NULL;

#define ZE_ERR_CHECK(ret) \
    do { \
        if (unlikely((ret) != ZE_RESULT_SUCCESS)) \
            goto fn_fail; \
    } while (0)

int MPL_gpu_get_dev_count(int *dev_cnt, int *dev_id, int *subdev_id)
{
    int ret = MPL_SUCCESS;
    if (!gpu_initialized) {
        ret = MPL_gpu_init(0);
    }

    *dev_cnt = local_ze_device_count;
    *dev_id = max_dev_id;
    *subdev_id = max_subdev_id;
    return ret;
}

int MPL_gpu_get_dev_list(int *dev_count, char ***dev_list, bool is_subdev)
{
    int ret = MPL_SUCCESS;
    if (!gpu_initialized) {
        ret = MPL_gpu_init(0);
    }

    if (!is_subdev) {
        device_list = (char **) MPL_malloc(device_count * sizeof(char *), MPL_MEM_OTHER);
        assert(device_list != NULL);

        for (int i = 0; i < device_count; ++i) {
            int str_len = snprintf(NULL, 0, "%d", i);
            device_list[i] = (char *) MPL_malloc((str_len + 1) * sizeof(char *), MPL_MEM_OTHER);
            sprintf(device_list[i], "%d", i);
        }

        *dev_count = device_count;
        *dev_list = device_list;
    } else {
        uint32_t driver_count = 0;
        uint32_t *subdev_counts = NULL;
        int total_subdev_count = 0;
        ze_driver_handle_t *all_drivers = NULL;

        ret = zeDriverGet(&driver_count, NULL);
        assert(ret == ZE_RESULT_SUCCESS);
        assert(device_count);

        all_drivers = MPL_malloc(driver_count * sizeof(ze_driver_handle_t), MPL_MEM_OTHER);
        assert(all_drivers);
        ret = zeDriverGet(&driver_count, all_drivers);
        assert(ret == ZE_RESULT_SUCCESS);

        /* Find a driver instance with a GPU device */
        for (int i = 0; i < driver_count; ++i) {
            local_ze_device_count = 0;
            ret = zeDeviceGet(all_drivers[i], &local_ze_device_count, NULL);
            ze_devices_handle =
                MPL_malloc(local_ze_device_count * sizeof(ze_device_handle_t), MPL_MEM_OTHER);
            assert(ze_devices_handle);
            subdev_counts = MPL_malloc(local_ze_device_count * sizeof(int), MPL_MEM_OTHER);
            memset(subdev_counts, 0, local_ze_device_count * sizeof(int));
            assert(subdev_counts);

            ret = zeDeviceGet(all_drivers[i], &local_ze_device_count, ze_devices_handle);
            assert(ret == ZE_RESULT_SUCCESS);

            /* Check if the driver supports a gpu */
            for (int d = 0; d < local_ze_device_count; ++d) {
                ze_device_properties_t device_properties;
                ret = zeDeviceGetProperties(ze_devices_handle[d], &device_properties);
                assert(ret == ZE_RESULT_SUCCESS);

                if (!(device_properties.flags & ZE_DEVICE_PROPERTY_FLAG_SUBDEVICE)) {
                    zeDeviceGetSubDevices(ze_devices_handle[d], &subdev_counts[d], NULL);
                    if (subdev_counts[d] == 0) {
                        /* ZE reports no subdevice when there is only one subdevice */
                        subdev_counts[d] = 1;
                    }
                    total_subdev_count += subdev_counts[d];

                }
            }
            MPL_free(ze_devices_handle);
        }

        device_list = (char **) MPL_malloc(total_subdev_count * sizeof(char *), MPL_MEM_OTHER);
        assert(device_list != NULL);

        int idx = 0;
        for (int i = 0; i < device_count; ++i) {
            for (int j = 0; j < subdev_counts[i]; j++) {
                int str_len = snprintf(NULL, 0, "%d.%d", i, j);
                device_list[idx] =
                    (char *) MPL_malloc((str_len + 1) * sizeof(char *), MPL_MEM_OTHER);
                sprintf(device_list[idx], "%d.%d", i, j);
                device_list[idx][str_len] = 0;
                idx++;
            }
        }

        MPL_free(subdev_counts);

        *dev_count = total_subdev_count;
        *dev_list = device_list;
    }
    return ret;
}

int MPL_gpu_dev_affinity_to_env(int dev_count, char **dev_list, char **env)
{
    int ret = MPL_SUCCESS;
    memset(affinity_env, 0, MAX_GPU_STR_LEN);
    if (dev_count == 0) {
        snprintf(affinity_env, 3, "-1");
    } else {
        int str_offset = 0;
        for (int i = 0; i < dev_count; ++i) {
            if (i) {
                MPL_strncpy(affinity_env + str_offset, ",", MAX_GPU_STR_LEN - str_offset);
                str_offset++;
            }
            MPL_strncpy(affinity_env + str_offset, dev_list[i], MAX_GPU_STR_LEN - str_offset);
            str_offset += strlen(dev_list[i]);
        }
    }
    *env = affinity_env;
    return ret;
}

int MPL_gpu_init_device_mappings(int max_devid, int max_subdevid)
{
    int mpl_err = MPL_SUCCESS;
    int global_dev_count = max_devid + 1;
    int global_subdev_count = 0;

    /* If max_subdevid is 0, then all procs use tile 0 as root devices, so subdevices aren't
     * needed. */
    if (max_subdevid == 0) {
        global_ze_device_count = global_dev_count;
    } else {
        /* We can still have the situation where all procs use non-zero tile as root devices, but
         * this can't be detected unless we also reduce subdevice count. Thus, consider them as
         * subdevices in the global_to_local_map even if they are all root devices. */
        global_subdev_count = max_subdevid + 1;
        global_ze_device_count = global_dev_count * (global_subdev_count + 1);
    }

    /* Initialize local_to_global_map */
    local_to_global_map = MPL_malloc(local_ze_device_count * sizeof(int), MPL_MEM_OTHER);
    if (local_to_global_map == NULL) {
        mpl_err = MPL_ERR_GPU_NOMEM;
        goto fn_fail;
    }

    for (int i = 0; i < local_ze_device_count; ++i) {
        local_to_global_map[i] = -1;
    }

    /* Initialize global_to_local_map */
    global_to_local_map = MPL_malloc(global_ze_device_count * sizeof(int), MPL_MEM_OTHER);
    if (global_to_local_map == NULL) {
        mpl_err = MPL_ERR_GPU_NOMEM;
        goto fn_fail;
    }

    for (int i = 0; i < global_ze_device_count; ++i) {
        global_to_local_map[i] = -1;
    }

    if (mask_contents.num_dev > 0) {
        int device, subdevice;
        for (int i = 0; i < mask_contents.num_dev; ++i) {
            device = mask_contents.dev_id[i];
            subdevice = mask_contents.subdev_id[i];
            /* Temporarily mark the device as visible. It might only be a subdevice that is
             * visible. */
            global_to_local_map[device] = 1;

            /* Mark the subdevice(s) as visible. */
            if (subdevice != -1) {
                /* Handle special case where there are no subdevices among any device. */
                if (global_subdev_count > 0) {
                    int idx = global_dev_count + device * global_subdev_count + subdevice;
                    global_to_local_map[idx] = 1;
                }
            } else {
                int idx = global_dev_count + device * global_subdev_count;
                for (int j = 0; j < global_subdev_count; ++j) {
                    global_to_local_map[idx + j] = 1;
                }
            }
        }
    } else {
        for (int i = 0; i < global_dev_count; ++i) {
            /* Set device as visible */
            global_to_local_map[i] = 1;

            /* Set subdevices as visible */
            int idx = global_dev_count + i * global_subdev_count;
            for (int j = 0; j < subdevice_count[i]; ++j) {
                global_to_local_map[idx + j] = 1;
            }
        }
    }

    /* Setup global_to_local_map */
    int local_dev_id = 0;

    /* The root devices first */
    for (int i = 0; i < global_dev_count; ++i) {
        if (global_to_local_map[i] == 1) {
            /* Check if the device has subdevices before setting its index. If it does not, then
             * only the subdevice is visible. However, need to check for the special case that
             * there are no subdevices among any device. */
            if (subdevice_count[local_dev_id] || global_subdev_count == 0) {
                global_to_local_map[i] = local_dev_id;
            } else {
                /* Find which subdevice is visible and give it the local device id since it is the
                 * root device. */
                int idx = global_dev_count + i * global_subdev_count;
                for (int j = 0; j < global_subdev_count; ++j) {
                    if (global_to_local_map[idx + j] == 1) {
                        global_to_local_map[idx + j] = local_dev_id;
                    }
                }
                /* Unset the device as the root device, since its subdevice is the root. */
                global_to_local_map[i] = -1;
            }
            ++local_dev_id;
        }
    }

    /* The subdevices next */
    for (int i = 0; i < global_dev_count; ++i) {
        if (global_to_local_map[i] != -1) {
            int idx = global_dev_count + i * global_subdev_count;
            for (int j = 0; j < global_subdev_count; ++j) {
                if (global_to_local_map[idx + j] == 1) {
                    global_to_local_map[idx + j] = local_dev_id;
                    ++local_dev_id;
                }
            }
        }
    }

    assert(local_dev_id == local_ze_device_count);

    /* Setup local_to_global_map */
    local_dev_id = 0;
    for (int i = 0; i < global_ze_device_count; ++i) {
        int local_id = global_to_local_map[i];
        if (local_id != -1) {
            local_to_global_map[local_id] = i;
        }
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

int MPL_gpu_init(int debug_summary)
{
    int mpl_err = MPL_SUCCESS;
    if (gpu_initialized) {
        goto fn_exit;
    }

    MPL_gpu_info.debug_summary = debug_summary;
    MPL_gpu_info.enable_ipc = true;

    if (MPL_gpu_info.debug_summary) {
        printf("==== GPU Init (ZE) ====\n");
    }

    mpl_err = gpu_ze_init_driver();
    if (mpl_err != MPL_SUCCESS)
        goto fn_fail;

    MPL_gpu_info.debug_summary = debug_summary;
    MPL_gpu_info.enable_ipc = true;
    MPL_gpu_info.ipc_handle_type = MPL_GPU_IPC_HANDLE_SHAREABLE_FD;

    max_dev_id = 0;
    max_subdev_id = 0;

    if (local_ze_device_count <= 0) {
        gpu_initialized = 1;
        goto fn_exit;
    }

    mpl_err = parse_affinity_mask();
    if (mpl_err != MPL_SUCCESS)
        goto fn_fail;

    get_max_dev_id(&max_dev_id, &max_subdev_id);

    if (likely(MPL_gpu_info.specialized_cache)) {
        ipc_cache_tracked =
            MPL_malloc(local_ze_device_count * sizeof(MPL_ze_ipc_handle_entry_t *), MPL_MEM_OTHER);
        if (ipc_cache_tracked == NULL) {
            mpl_err = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }

        ipc_cache_mapped =
            MPL_malloc(local_ze_device_count * sizeof(MPL_ze_mapped_buffer_entry_t *),
                       MPL_MEM_OTHER);
        if (ipc_cache_mapped == NULL) {
            mpl_err = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }

        ipc_cache_removal =
            MPL_malloc(local_ze_device_count * sizeof(MPL_ze_mapped_buffer_entry_t *),
                       MPL_MEM_OTHER);
        if (ipc_cache_removal == NULL) {
            mpl_err = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }

        mmap_cache_removal =
            MPL_malloc(local_ze_device_count * sizeof(MPL_ze_mapped_buffer_entry_t *),
                       MPL_MEM_OTHER);
        if (mmap_cache_removal == NULL) {
            mpl_err = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }

        for (int i = 0; i < local_ze_device_count; ++i) {
            ipc_cache_tracked[i] = NULL;
            ipc_cache_mapped[i] = NULL;
            ipc_cache_removal[i] = NULL;
            mmap_cache_removal[i] = NULL;
        }

        MPL_gpu_free_hook_register(MPL_ze_ipc_remove_cache_handle);
    }

    /* Initialize gpu engine mapping */
    engine_conversion = (int *) MPL_malloc(sizeof(int) * MPL_GPU_ENGINE_NUM_TYPES, MPL_MEM_OTHER);
    engine_conversion[MPL_GPU_ENGINE_TYPE_COMPUTE] = 0; // Compute engine
    engine_conversion[MPL_GPU_ENGINE_TYPE_COPY_HIGH_BANDWIDTH] = 1;     // Main copy engine
    engine_conversion[MPL_GPU_ENGINE_TYPE_COPY_LOW_LATENCY] = 2;        // Link copy engine

    mypid = getpid();

    MPL_initlock_lock(&free_hook_mutex);
    gpu_mem_hook_init();
    MPL_initlock_unlock(&free_hook_mutex);
    gpu_initialized = 1;

    if (MPL_gpu_info.debug_summary) {
        printf("device_count: %d\n", device_count);
        printf("subdevice_count: %d\n", local_ze_device_count - device_count);
        printf("=========================\n");
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

/* Get dev_id for shared_device_fds from regular dev_id */
int MPL_gpu_get_root_device(int dev_id)
{
    return subdevice_map[dev_id];
}

/* Get dev_id for shared_device_fds from regular dev_id */
static int get_physical_device(int dev_id)
{
#ifdef ZE_PCI_PROPERTIES_EXT_NAME
    if (device_states[dev_id].sys_device_index != -1)
        return device_states[dev_id].sys_device_index;
#endif
    return subdevice_map[dev_id];
}

/* Get dev_id from device handle */
MPL_STATIC_INLINE_PREFIX int device_to_dev_id(MPL_gpu_device_handle_t device)
{
    int dev_id = -1;
    for (int d = 0; d < local_ze_device_count; d++) {
        if (ze_devices_handle[d] == device) {
            dev_id = d;
            break;
        }
    }

    return dev_id;
}

/* Get device from dev_id */
MPL_STATIC_INLINE_PREFIX int dev_id_to_device(int dev_id, MPL_gpu_device_handle_t * device)
{
    int mpl_err = MPL_SUCCESS;

    if (dev_id > local_ze_device_count) {
        goto fn_fail;
    }

    *device = ze_devices_handle[dev_id];

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/* Functions for managing shareable IPC handles */
static int fd_to_handle(int dev_fd, int fd, int *handle)
{
#ifdef MPL_ENABLE_DRMFD
    struct drm_prime_handle open_fd = { 0, 0, 0 };
    open_fd.fd = fd;

    int ret = ioctl(dev_fd, DRM_IOCTL_PRIME_FD_TO_HANDLE, &open_fd);
    assert(ret != -1);
    *handle = open_fd.handle;
    return ret;
#else
    return -1;
#endif
}

static int handle_to_fd(int dev_fd, int handle, int *fd)
{
#ifdef MPL_ENABLE_DRMFD
    struct drm_prime_handle open_fd = { 0, 0, 0 };
    open_fd.flags = DRM_CLOEXEC | DRM_RDWR;
    open_fd.handle = handle;

    int ret = ioctl(dev_fd, DRM_IOCTL_PRIME_HANDLE_TO_FD, &open_fd);
    if (ret == -1)
        perror("handle_to_fd");
    assert(ret != -1);
    *fd = open_fd.fd;
    return ret;
#else
    return -1;
#endif
}

static int close_handle(int dev_fd, int handle)
{
#ifdef MPL_ENABLE_DRMFD
    struct drm_gem_close close = { 0, 0 };
    close.handle = handle;

    int ret = ioctl(dev_fd, DRM_IOCTL_GEM_CLOSE, &close);
    assert(ret != -1);
    return ret;
#else
    return -1;
#endif
}

/* implicit scaling */
static inline void split_size(size_t size, size_t sizes[2])
{
    /* calculate sizes */
    const size_t alignment = 64 * 1024;;
    size_t mask = alignment - 1;
    size = (size + mask) & ~mask;
    size_t n = size >> 16;
    if (size < alignment) {
        sizes[0] = size;
        sizes[1] = 0;
    } else if (n % 2) {
        sizes[0] = (n + 1) / 2 * alignment;
        sizes[1] = size - sizes[0];
    } else {
        sizes[0] = n / 2 * alignment;
        sizes[1] = size - sizes[0];
    }
    assert(sizes[0]);
}

/* map two allocations into one contiguous buffer */
static int mmapFunction(int nfds, int *fds, size_t size, void **ptr)
{
    int mpl_err = MPL_SUCCESS;
    size_t split_sizes[2];

    if (nfds == 1) {
        *ptr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, fds[0], 0);
        if (*ptr == (void *) -1) {
            mpl_err = MPL_ERR_GPU_INTERNAL;
            perror("mmap device to host");
            printf("mmap failed fd: %d size: %ld\n", fds[0], size);
            goto fn_fail;
        }
    } else {
        split_size(size, split_sizes);
        void *buf = mmap(0, size, PROT_NONE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
        if (buf == (void *) -1) {
            mpl_err = MPL_ERR_GPU_INTERNAL;
            perror("mmap");
            printf("mmapFunction failed when reserving whole size\n");
            goto fn_fail;
        }
        void *p =
            mmap(buf, split_sizes[0], PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, fds[0], 0);
        if (p != buf) {
            mpl_err = MPL_ERR_GPU_INTERNAL;
            perror("mmap 1st tile");
            printf("mmapFunction failed when mapping first tile \n");
            goto fn_fail;
        }
        if (split_sizes[1]) {
            char *p2 = (char *) buf + split_sizes[0];
            p = mmap(p2, split_sizes[1], PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, fds[1], 0);
            if (p != (void *) p2) {
                mpl_err = MPL_ERR_GPU_INTERNAL;
                perror("mmap 2nd tile");
                printf("mmapFunction failed when mapping second tile \n");
                goto fn_fail;
            }
        }
        *ptr = buf;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    *ptr = NULL;
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/* munmap an implicit scaling buffer */
static int munmapFunction(int nfds, int *fds, void *ptr, size_t size)
{
    int mpl_err = MPL_SUCCESS;
    size_t split_sizes[2];
    int ret;

    if (nfds == 1) {
        ret = munmap(ptr, size);
        if (ret != 0) {
            goto fn_fail;
        }
        close(fds[0]);
    } else {
        split_size(size, split_sizes);
        void *ptr1 = ptr;
        ret = munmap(ptr1, split_sizes[0]);
        if (ret != 0) {
            goto fn_fail;
        }
        close(fds[0]);
        void *ptr2 = (char *) ptr + split_sizes[0];
        ret = munmap(ptr2, split_sizes[1]);
        if (ret != 0) {
            goto fn_fail;
        }
        close(fds[1]);
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/*
    cache utility functions for MPL_ze_ipc_handle_entry_t:
    used to cache local device pointer's ipc handle and mmap'ed pointer.
    mapped pointers can be from a remote IPC handle
*/

static inline void free_ipc_handle_cache(MPL_ze_ipc_handle_entry_t * cache_entry)
{
    if (cache_entry->mapped_ptr) {
        munmapFunction(cache_entry->nfds, cache_entry->fds, cache_entry->mapped_ptr,
                       cache_entry->mapped_size);
    }
}

static inline int new_ipc_handle_cache(MPL_ze_ipc_handle_entry_t ** entry, int mem_id)
{
    int mpl_err = MPL_SUCCESS;
    MPL_ze_ipc_handle_entry_t *cache_entry;

    cache_entry =
        (MPL_ze_ipc_handle_entry_t *) MPL_malloc(sizeof(MPL_ze_ipc_handle_entry_t), MPL_MEM_OTHER);
    if (cache_entry == NULL) {
        mpl_err = MPL_ERR_GPU_NOMEM;
        goto fn_fail;
    }

    memset(cache_entry, 0, sizeof(MPL_ze_ipc_handle_entry_t));
    cache_entry->mem_id = mem_id;
    cache_entry->handle_cached = false;
    *entry = cache_entry;

  fn_exit:
    return mpl_err;
  fn_fail:
    *entry = NULL;
    goto fn_exit;
}

/* Loads a ze driver */
static int gpu_ze_init_driver(void)
{
    uint32_t driver_count = 0;
    ze_result_t ret;
    int ret_error = MPL_SUCCESS;
    ze_driver_handle_t *all_drivers = NULL;

    ze_init_flag_t flags = ZE_INIT_FLAG_GPU_ONLY;
    ret = zeInit(flags);
    ZE_ERR_CHECK(ret);

    ret = zeDriverGet(&driver_count, NULL);
    ZE_ERR_CHECK(ret);
    if (driver_count == 0) {
        if (MPL_gpu_info.debug_summary) {
            printf("No Intel GPU library driver found.\n");
        }
        goto fn_fail;
    }

    all_drivers = MPL_malloc(driver_count * sizeof(ze_driver_handle_t), MPL_MEM_OTHER);
    if (all_drivers == NULL) {
        ret_error = MPL_ERR_GPU_NOMEM;
        goto fn_fail;
    }
    ret = zeDriverGet(&driver_count, all_drivers);
    ZE_ERR_CHECK(ret);

    int i, d;
    /* Find a driver instance with a GPU device */
    for (i = 0; i < driver_count; ++i) {
        device_count = 0;
        ret = zeDeviceGet(all_drivers[i], &device_count, NULL);
        ZE_ERR_CHECK(ret);
        ze_devices_handle = MPL_malloc(device_count * sizeof(ze_device_handle_t), MPL_MEM_OTHER);
        if (ze_devices_handle == NULL) {
            ret_error = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }
        ret = zeDeviceGet(all_drivers[i], &device_count, ze_devices_handle);
        ZE_ERR_CHECK(ret);
        /* Check if the driver supports a gpu */
        for (d = 0; d < device_count; ++d) {
            ze_device_properties_t device_properties;
            device_properties.stype = ZE_STRUCTURE_TYPE_DEVICE_PROPERTIES;
            device_properties.pNext = NULL;
            ret = zeDeviceGetProperties(ze_devices_handle[d], &device_properties);
            ZE_ERR_CHECK(ret);

            if (ZE_DEVICE_TYPE_GPU == device_properties.type) {
                ze_driver_handle = all_drivers[i];
                break;
            }
        }

        if (NULL != ze_driver_handle) {
            break;
        } else {
            MPL_free(ze_devices_handle);
            ze_devices_handle = NULL;
        }
    }

    /* Setup subdevices */
    local_ze_device_count = device_count;
    if (ze_devices_handle != NULL) {
        /* Count the subdevices */
        subdevice_count = MPL_malloc(device_count * sizeof(uint32_t), MPL_MEM_OTHER);
        if (subdevice_count == NULL) {
            ret_error = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }

        for (d = 0; d < device_count; ++d) {
            subdevice_count[d] = 0;
            ret = zeDeviceGetSubDevices(ze_devices_handle[d], &subdevice_count[d], NULL);
            ZE_ERR_CHECK(ret);

            local_ze_device_count += subdevice_count[d];
        }

        subdevice_map = MPL_malloc(local_ze_device_count * sizeof(int), MPL_MEM_OTHER);
        if (subdevice_map == NULL) {
            ret_error = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }

        /* Add the subdevices to the device array */
        ze_devices_handle =
            MPL_realloc(ze_devices_handle,
                        local_ze_device_count * sizeof(ze_device_handle_t), MPL_MEM_OTHER);
        if (ze_devices_handle == NULL) {
            ret_error = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }

        int dev_id = device_count;
        for (d = 0; d < device_count; ++d) {
            ret =
                zeDeviceGetSubDevices(ze_devices_handle[d], &subdevice_count[d],
                                      &ze_devices_handle[dev_id]);
            ZE_ERR_CHECK(ret);

            /* Setup the subdevice map for shared_device_fds */
            subdevice_map[d] = d;
            for (i = 0; i < subdevice_count[d]; ++i) {
                subdevice_map[dev_id + i] = d;
            }

            dev_id += subdevice_count[d];
        }
    } else {
        if (MPL_gpu_info.debug_summary) {
            printf("No Intel GPU device found.\n");
        }
    }

    ze_context_desc_t contextDesc = {
        .stype = ZE_STRUCTURE_TYPE_CONTEXT_DESC,
        .pNext = NULL,
        .flags = 0,
    };
    ret = zeContextCreate(ze_driver_handle, &contextDesc, &ze_context);
    ZE_ERR_CHECK(ret);

    device_states =
        (MPL_ze_device_entry_t *) MPL_malloc(sizeof(MPL_ze_device_entry_t) * local_ze_device_count,
                                             MPL_MEM_OTHER);

    for (d = 0; d < local_ze_device_count; d++) {
        unsigned int numQueueGroups = 0;
        MPL_ze_device_entry_t *device_state = device_states + d;
        device_state->dev_id = d;
        memset(device_state->prev_event, 0,
               MPL_GPU_COPY_DIRECTION_TYPES * sizeof(ze_event_handle_t));
        memset(device_state->last_cmdList_entry, 0,
               MPL_GPU_COPY_DIRECTION_TYPES * sizeof(MPL_cmdlist_pool_t *));
        ret = zeDeviceGetCommandQueueGroupProperties(ze_devices_handle[d], &numQueueGroups, NULL);
        ZE_ERR_CHECK(ret);
        ze_command_queue_group_properties_t *queueProperties =
            (ze_command_queue_group_properties_t *)
            MPL_malloc(sizeof(ze_command_queue_group_properties_t) * numQueueGroups, MPL_MEM_OTHER);
        ret =
            zeDeviceGetCommandQueueGroupProperties(ze_devices_handle[d], &numQueueGroups,
                                                   queueProperties);
        device_state->engines =
            (MPL_ze_engine_entry_t *) MPL_malloc(sizeof(MPL_ze_engine_entry_t) * numQueueGroups,
                                                 MPL_MEM_OTHER);
        device_state->numQueueGroups = numQueueGroups;

        for (i = 0; i < numQueueGroups; i++) {
            int ordinal = -1;
            if (queueProperties[i].flags & ZE_COMMAND_QUEUE_GROUP_PROPERTY_FLAG_COMPUTE) {
                ordinal = i;
            } else if (queueProperties[i].flags & ZE_COMMAND_QUEUE_GROUP_PROPERTY_FLAG_COPY &&
                       queueProperties[i].numQueues >= 1 &&
                       !(queueProperties[i].flags & ZE_COMMAND_QUEUE_GROUP_PROPERTY_FLAG_COMPUTE)) {
                ordinal = i;
            }
            device_state->engines[i].cmdList_pool = NULL;
            device_state->engines[i].curQueue = 0;
            device_state->engines[i].numQueues = ordinal == -1 ? 0 : queueProperties[i].numQueues;
            device_state->engines[i].cmdQueues = NULL;
            if (device_state->engines[i].numQueues) {
                device_state->engines[i].cmdQueues =
                    (ze_command_queue_handle_t *) MPL_malloc(sizeof(ze_command_queue_handle_t) *
                                                             device_state->engines[i].numQueues,
                                                             MPL_MEM_OTHER);
                memset(device_state->engines[i].cmdQueues, 0,
                       sizeof(ze_command_queue_handle_t) * device_state->engines[i].numQueues);
                device_state->engines[i].cmdlists =
                    (ze_command_list_handle_t *) MPL_malloc(sizeof(ze_command_list_handle_t) *
                                                            device_state->engines[i].numQueues,
                                                            MPL_MEM_OTHER);
                memset(device_state->engines[i].cmdlists, 0,
                       device_state->engines[i].numQueues * sizeof(ze_command_list_handle_t));
            }
        }
#ifdef ZE_PCI_PROPERTIES_EXT_NAME
        device_state->sys_device_index = -1;
        ze_pci_ext_properties_t pci_property = {
            .stype = ZE_STRUCTURE_TYPE_PCI_EXT_PROPERTIES,
            .pNext = NULL,
        };
        ret = zeDevicePciGetPropertiesExt(ze_devices_handle[d], &pci_property);
        if (ret == ZE_RESULT_SUCCESS) {
            device_state->pci_avail = 1;
            device_state->pci = pci_property.address;
        } else {
            device_state->pci_avail = 0;
        }
#endif
        MPL_free(queueProperties);
    }

    MPL_event_pool_add_new_pool();

    /* driver extension */
    ret =
        zeDriverGetExtensionFunctionAddress(ze_driver_handle, "zexMemGetIpcHandles",
                                            (void **) &zexMemGetIpcHandles);
    if (ZE_RESULT_SUCCESS != ret)
        zexMemGetIpcHandles = NULL;

    ret =
        zeDriverGetExtensionFunctionAddress(ze_driver_handle, "zexMemOpenIpcHandles",
                                            (void **) &zexMemOpenIpcHandles);
    if (ZE_RESULT_SUCCESS != ret)
        zexMemOpenIpcHandles = NULL;

    if (MPL_gpu_info.debug_summary) {
        for (d = 0; d < local_ze_device_count; ++d) {
            MPL_ze_device_entry_t *device_state = device_states + d;
            ze_device_properties_t device_properties;
            memset(&device_properties, 0, sizeof(ze_device_properties_t));
            device_properties.stype = ZE_STRUCTURE_TYPE_DEVICE_PROPERTIES;
            device_properties.pNext = NULL;
            ret = zeDeviceGetProperties(ze_devices_handle[d], &device_properties);
            fprintf(stderr, "==== Device %d ====\n", d);
            if (d < device_count) {
                fprintf(stderr, "\troot device: %d tiles\n", subdevice_count[d]);
            }
            fprintf(stderr, "  device name: %s\n", device_properties.name);
            fprintf(stderr, "  deviceId: %d\n", device_properties.deviceId);
            fprintf(stderr, "  subdeviceId: %d\n", device_properties.subdeviceId);
            fprintf(stderr, "  coreClockRate: %d\n", device_properties.coreClockRate);
            fprintf(stderr, "  maxMemAllocSize: %ld\n", device_properties.maxMemAllocSize);
            fprintf(stderr, "  maxHardwareContexts: %d\n", device_properties.maxHardwareContexts);
            fprintf(stderr, "  numThreadsPerEU: %d\n", device_properties.numThreadsPerEU);
            char uuid_str[1024];
            strcpy(uuid_str, "uuid: ");
            for (int j = 0; j < ZE_MAX_DRIVER_UUID_SIZE; j++) {
                char s[128];
                sprintf(s, "%x ", device_properties.uuid.id[j]);
                strcat(uuid_str, s);
            }
            fprintf(stderr, "  %s\n", uuid_str);
#ifdef ZE_PCI_PROPERTIES_EXT_NAME
            if (device_state->pci_avail)
                fprintf(stderr, "  BDF: domain:%x B:%x D:%x F:%x\n", device_state->pci.domain,
                        device_state->pci.bus, device_state->pci.device,
                        device_state->pci.function);
#endif
            fprintf(stderr, "  Command Queue (engine) types: %d \n", device_state->numQueueGroups);
            for (i = 0; i < device_state->numQueueGroups; i++) {
                fprintf(stderr, "    Engine: %d  numQueues: %d \n", i,
                        device_state->engines[i].numQueues);
            }
        }
    }

    /* driver extension */
    /* Get pointers to host pointer interfaces */
    ret =
        zeDriverGetExtensionFunctionAddress(ze_driver_handle, "zexDriverImportExternalPointer",
                                            (void **) &zexDriverImportExternalPointer);
    ZE_ERR_CHECK(ret);

    ret =
        zeDriverGetExtensionFunctionAddress(ze_driver_handle, "zexDriverReleaseImportedPointer",
                                            (void **) &zexDriverReleaseImportedPointer);
    ZE_ERR_CHECK(ret);

    ret =
        zeDriverGetExtensionFunctionAddress(ze_driver_handle, "zexDriverGetHostPointerBaseAddress",
                                            (void **) &zexDriverGetHostPointerBaseAddress);
    ZE_ERR_CHECK(ret);

  fn_exit:
    MPL_free(all_drivers);
    return ret_error;
  fn_fail:
    MPL_free(ze_devices_handle);
    /* If error code is already set, preserve it */
    if (ret_error == MPL_SUCCESS)
        ret_error = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/* Parses ZE_AFFINITY_MASK to populate mask_contents with corresponding data */
static int parse_affinity_mask()
{
    int i, curr_dev, num_dev = 0, mpl_err = MPL_SUCCESS;

    char *visible_devices = getenv("ZE_AFFINITY_MASK");
    if (visible_devices) {
        char *devices = MPL_strdup(visible_devices);
        char *tmp = devices;
        char *dev, *free_ptr = devices;

        /* Count the number of devices */
        for (i = 0; tmp[i]; ++i) {
            if (tmp[i] == ',')
                ++num_dev;
        }
        if (tmp[i - 1] != ',')
            ++num_dev;

        mask_contents.num_dev = num_dev;
        mask_contents.dev_id = (int *) MPL_malloc(num_dev * sizeof(int), MPL_MEM_OTHER);
        if (mask_contents.dev_id == NULL) {
            mpl_err = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }
        mask_contents.subdev_id = (int *) MPL_malloc(num_dev * sizeof(int), MPL_MEM_OTHER);
        if (mask_contents.subdev_id == NULL) {
            mpl_err = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }

        tmp = strtok_r(devices, ",", &dev);
        curr_dev = 0;

        while (tmp != NULL) {
            char *subdev;
            char *subdevices = strtok_r(tmp, ".", &subdev);
            int device = atoi(subdevices);
            tmp = NULL;

            mask_contents.dev_id[curr_dev] = device;

            subdevices = strtok_r(tmp, ".", &subdev);
            tmp = NULL;

            if (subdevices != NULL) {
                mask_contents.subdev_id[curr_dev] = atoi(subdevices);
            } else {
                mask_contents.subdev_id[curr_dev] = -1;
            }

            devices = NULL;
            tmp = strtok_r(devices, ",", &dev);

            ++curr_dev;
        }

        MPL_free(free_ptr);
    } else {
        mask_contents.num_dev = 0;
        mask_contents.dev_id = NULL;
        mask_contents.subdev_id = NULL;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

/* Get the max dev_id and subdev_id based on the environment */
static void get_max_dev_id(int *max_dev_id, int *max_subdev_id)
{
    /* This function assumes that parse_affinity_mask was previously called */
    int mpl_err = MPL_SUCCESS;

    *max_dev_id = *max_subdev_id = 0;

    /* Values based on ZE_AFFINITY_MASK */
    for (int i = 0; i < mask_contents.num_dev; ++i) {
        if (mask_contents.dev_id[i] > *max_dev_id)
            *max_dev_id = mask_contents.dev_id[i];
        if (mask_contents.subdev_id[i] > *max_subdev_id)
            *max_subdev_id = mask_contents.subdev_id[i];
    }

    /* If ZE_AFFINITY_MASK wasn't set */
    if (mask_contents.num_dev == 0) {
        *max_dev_id = device_count - 1;
    }

    /* Include subdevices that weren't detected in parse_affinity_mask */
    for (int i = 0; i < device_count; ++i) {
        if (subdevice_count[i] > 0 && (subdevice_count[i] - 1) > *max_subdev_id) {
            *max_subdev_id = subdevice_count[i] - 1;
        }
    }
}

int MPL_gpu_finalize(void)
{
    int i, j, k;

    if (likely(MPL_gpu_info.specialized_cache)) {
        for (i = 0; i < local_ze_device_count; ++i) {
            if (ipc_cache_removal[i]) {
                MPL_ze_mapped_buffer_entry_t *entry = NULL, *tmp = NULL;
                HASH_ITER(hh, ipc_cache_removal[i], entry, tmp) {
                    remove_ipc_handle_entry(entry, i);
                }
            }

            if (mmap_cache_removal[i]) {
                MPL_ze_mapped_buffer_entry_t *entry = NULL, *tmp = NULL;
                HASH_ITER(hh, mmap_cache_removal[i], entry, tmp) {
                    MPL_ze_mmap_handle_unmap(entry->mapped_ptr, i);
                }
            }
        }

        for (i = 0; i < local_ze_device_count; ++i) {
            MPL_ze_ipc_handle_entry_t *entry = NULL, *tmp = NULL;
            HASH_ITER(hh, ipc_cache_tracked[i], entry, tmp) {
                free_ipc_handle_cache(entry);
                HASH_DELETE(hh, ipc_cache_tracked[i], entry);
                MPL_free(entry);
            }
        }

        {
            MPL_ze_mem_id_entry_t *entry = NULL, *tmp = NULL;
            HASH_ITER(hh, mem_id_cache, entry, tmp) {
                HASH_DELETE(hh, mem_id_cache, entry);
                MPL_free(entry);
            }
        }

        MPL_free(ipc_cache_tracked);
        MPL_free(ipc_cache_removal);
        MPL_free(mmap_cache_removal);
        MPL_free(ipc_cache_mapped);
    }

    MPL_free(local_to_global_map);
    MPL_free(global_to_local_map);
    MPL_free(ze_devices_handle);
    MPL_free(subdevice_map);
    MPL_free(subdevice_count);

    MPL_ze_gem_hash_entry_t *entry = NULL, *tmp = NULL;
    HASH_ITER(hh, gem_hash, entry, tmp) {
        HASH_DELETE(hh, gem_hash, entry);
        MPL_free(entry);
    }

    for (i = 0; i < physical_device_count; ++i) {
        close(physical_device_states[i].fd);
    }

    MPL_free(physical_device_states);

    gpu_free_hook_s *prev;
    MPL_initlock_lock(&free_hook_mutex);
    while (free_hook_chain) {
        prev = free_hook_chain;
        free_hook_chain = free_hook_chain->next;
        MPL_free(prev);
    }
    free_hook_chain = NULL;
    MPL_initlock_unlock(&free_hook_mutex);

    for (i = 0; i < local_ze_device_count; i++) {
        MPL_ze_device_entry_t *device_state = device_states + i;
        for (j = 0; j < device_state->numQueueGroups; j++) {
            MPL_ze_engine_entry_t *engine = device_state->engines + j;
            for (k = 0; k < engine->numQueues; k++) {
                if (engine->cmdQueues[k])
                    zeCommandQueueDestroy(engine->cmdQueues[k]);
                if (engine->cmdlists[k])
                    zeCommandListDestroy(engine->cmdlists[k]);
            }
            MPL_free(engine->cmdQueues);
            MPL_free(engine->cmdlists);
            MPL_cmdlist_pool_t *cmdlist, *t, *pool = engine->cmdList_pool;
            DL_FOREACH_SAFE(pool, cmdlist, t) {
                zeCommandListDestroy(cmdlist->cmdList);
                DL_DELETE(pool, cmdlist);
                MPL_free(cmdlist);
            }
        }
        MPL_free(device_state->engines);
    }
    MPL_free(device_states);
    MPL_free(engine_conversion);

    MPL_event_pool_destroy();

    MPL_free(mask_contents.dev_id);
    MPL_free(mask_contents.subdev_id);

    /* Reset initialization state */
    gpu_initialized = 0;

    return MPL_SUCCESS;
}

int MPL_gpu_global_to_local_dev_id(int global_dev_id)
{
    assert(global_dev_id < global_ze_device_count);
    return global_to_local_map[global_dev_id];
}

int MPL_gpu_local_to_global_dev_id(int local_dev_id)
{
    assert(local_dev_id < local_ze_device_count);
    return local_to_global_map[local_dev_id];
}

static int MPL_event_pool_add_new_pool(void)
{
    int mpl_err = MPL_SUCCESS;
    int ret;
    ze_event_pool_handle_t event_pool;

    ze_event_pool_desc_t pool_desc;
    pool_desc.stype = ZE_STRUCTURE_TYPE_EVENT_POOL_DESC;
    pool_desc.pNext = NULL;
    pool_desc.flags = 0;
    pool_desc.count = MPL_ZE_EVENT_POOL_SIZE;
    ret = zeEventPoolCreate(ze_context, &pool_desc, 0, NULL, &event_pool);
    ZE_ERR_CHECK(ret);

    MPL_ze_event_pool *ep =
        (MPL_ze_event_pool *) MPL_calloc(1, sizeof(MPL_ze_event_pool), MPL_MEM_OTHER);
    if (ep == NULL) {
        mpl_err = MPL_ERR_GPU_NOMEM;
        goto fn_exit;
    }
    ep->pool = event_pool;
    DL_APPEND(ze_event_pools, ep);

    for (int i = 0; i < MPL_ZE_EVENT_POOL_SIZE; i++) {
        ze_event_handle_t event = NULL;
        ze_event_desc_t event_desc = {
            .stype = ZE_STRUCTURE_TYPE_EVENT_DESC,
            .pNext = NULL,
            .index = i,
            .signal = ZE_EVENT_SCOPE_FLAG_HOST,
            .wait = ZE_EVENT_SCOPE_FLAG_HOST
        };
        ret = zeEventCreate(event_pool, &event_desc, &event);
        ZE_ERR_CHECK(ret);
        MPL_gpu_event *e = (MPL_gpu_event *) MPL_calloc(1, sizeof(MPL_gpu_event), MPL_MEM_OTHER);
        if (e == NULL) {
            mpl_err = MPL_ERR_GPU_NOMEM;
            goto fn_exit;
        }
        e->event = event;
        DL_APPEND(ze_events, e);
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

static inline int MPL_event_pool_get_event(MPL_gpu_event ** event)
{
    int mpl_err = MPL_SUCCESS;
    MPL_gpu_event *e = NULL;

    if (ze_events == NULL) {
        mpl_err = MPL_event_pool_add_new_pool();
        if (mpl_err != MPL_SUCCESS) {
            goto fn_fail;
        }
    }
    assert(ze_events);
    e = ze_events;
    DL_DELETE(ze_events, e);
    zeEventHostReset(e->event);
    *event = e;

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

static inline void MPL_event_pool_push_event(MPL_gpu_event * gpu_event)
{
    DL_APPEND(ze_events, gpu_event);
}

static void MPL_event_pool_destroy(void)
{
    MPL_gpu_event *e, *e_tmp;
    MPL_ze_event_pool *ep, *ep_tmp;

    DL_FOREACH_SAFE(ze_events, e, e_tmp) {
        zeEventDestroy(e->event);
        MPL_free(e);
    }
    DL_FOREACH_SAFE(ze_event_pools, ep, ep_tmp) {
        zeEventPoolDestroy(ep->pool);
        MPL_free(ep);
    }
}

/* given a local device pointer, create an IPC handle */
int MPL_gpu_ipc_handle_create(const void *ptr, MPL_gpu_device_attr * ptr_attr,
                              MPL_gpu_ipc_mem_handle_t * ipc_handle)
{
    int mpl_err = MPL_SUCCESS;
    int local_dev_id = -1;
    MPL_ze_ipc_handle_entry_t *cache_entry = NULL;
    MPL_ze_mem_id_entry_t *memid_entry = NULL;
    uint64_t mem_id = 0;
    void *pbase = NULL;
    uintptr_t len;

    local_dev_id = device_to_dev_id(ptr_attr->device);
    if (local_dev_id == -1) {
        goto fn_fail;
    }

    mpl_err = MPL_gpu_get_buffer_bounds(ptr, &pbase, &len);
    if (mpl_err != MPL_SUCCESS) {
        goto fn_fail;
    }

    /* First check if a removal from the cache is needed */
    if (likely(MPL_gpu_info.specialized_cache)) {
        mem_id = ptr_attr->prop.id;
        HASH_FIND(hh, mem_id_cache, &pbase, sizeof(void *), memid_entry);

        if (memid_entry && memid_entry->mem_id != mem_id) {
            HASH_FIND(hh, ipc_cache_tracked[local_dev_id], &memid_entry->mem_id, sizeof(uint64_t),
                      cache_entry);
            if (cache_entry) {
                free_ipc_handle_cache(cache_entry);
                HASH_DELETE(hh, ipc_cache_tracked[local_dev_id], cache_entry);
                MPL_free(cache_entry);
                cache_entry = NULL;
            }

            HASH_DELETE(hh, mem_id_cache, memid_entry);
            MPL_free(memid_entry);
            memid_entry = NULL;
        }

        /* Check if ipc_handle is already cached */
        HASH_FIND(hh, ipc_cache_tracked[local_dev_id], &mem_id, sizeof(uint64_t), cache_entry);
    }

    if (cache_entry && cache_entry->handle_cached) {
        memcpy(ipc_handle, &cache_entry->ipc_handle, sizeof(MPL_gpu_ipc_mem_handle_t));
    } else {
        mpl_err = MPL_ze_ipc_handle_create(ptr, ptr_attr, local_dev_id, true, ipc_handle);
        if (mpl_err != MPL_SUCCESS) {
            goto fn_fail;
        }

        if (likely(MPL_gpu_info.specialized_cache)) {
            if (cache_entry) {
                /* In the case where there was an entry, but no ipc handle yet */
                cache_entry->handle_cached = true;
                memcpy(&cache_entry->ipc_handle, ipc_handle, sizeof(MPL_gpu_ipc_mem_handle_t));
            } else {
                /* Insert into the cache */
                mpl_err = new_ipc_handle_cache(&cache_entry, mem_id);
                if (mpl_err != MPL_SUCCESS) {
                    mpl_err = MPL_ERR_GPU_NOMEM;
                    goto fn_fail;
                }

                cache_entry->handle_cached = true;
                memcpy(&cache_entry->ipc_handle, ipc_handle, sizeof(MPL_gpu_ipc_mem_handle_t));
                HASH_ADD(hh, ipc_cache_tracked[local_dev_id], mem_id, sizeof(uint64_t), cache_entry,
                         MPL_MEM_OTHER);

                memid_entry = (MPL_ze_mem_id_entry_t *) MPL_malloc(sizeof(MPL_ze_mem_id_entry_t),
                                                                   MPL_MEM_OTHER);
                if (memid_entry == NULL) {
                    mpl_err = MPL_ERR_GPU_NOMEM;
                    goto fn_fail;
                }
                memset(memid_entry, 0, sizeof(MPL_ze_mem_id_entry_t));
                memid_entry->ptr = pbase;
                memid_entry->mem_id = mem_id;
                HASH_ADD(hh, mem_id_cache, ptr, sizeof(void *), memid_entry, MPL_MEM_OTHER);
            }
        }
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_ipc_handle_destroy(const void *ptr, MPL_pointer_attr_t * gpu_attr)
{
    int status, mpl_err = MPL_SUCCESS;
    MPL_ze_ipc_handle_entry_t *cache_entry = NULL;
    int dev_id;
    uint64_t mem_id;

    if (physical_device_states != NULL) {
        MPL_ze_gem_hash_entry_t *entry = NULL;
        HASH_FIND_PTR(gem_hash, &ptr, entry);
        if (entry == NULL) {
            /* This might get called for pointers that didn't have IPC handles created */
            goto fn_exit;
        }

        HASH_DEL(gem_hash, entry);

        /* close GEM handle */
        for (int i = 0; i < entry->nhandles; i++) {
            status = close_handle(physical_device_states[entry->dev_id].fd, entry->handles[i]);
            if (status) {
                goto fn_fail;
            }
        }

        MPL_free(entry);
    }

    if (likely(MPL_gpu_info.specialized_cache)) {
        dev_id = device_to_dev_id(gpu_attr->device);
        if (dev_id == -1) {
            goto fn_fail;
        }

        mem_id = gpu_attr->device_attr.prop.id;
        HASH_FIND(hh, ipc_cache_tracked[dev_id], &mem_id, sizeof(uint64_t), cache_entry);

        if (cache_entry != NULL) {
            free_ipc_handle_cache(cache_entry);
            HASH_DELETE(hh, ipc_cache_tracked[dev_id], cache_entry);
            MPL_free(cache_entry);
        }
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_ipc_handle_map(MPL_gpu_ipc_mem_handle_t * mpl_ipc_handle, int dev_id, void **ptr)
{
    int mpl_err = MPL_SUCCESS;
    MPL_ze_mapped_buffer_entry_t *cache_entry = NULL;
    MPL_ze_mapped_buffer_entry_t *removal_entry = NULL;
    MPL_ze_mapped_buffer_lookup_t lookup_entry;
    unsigned keylen = 0;

    fd_pid_t h;
    h = mpl_ipc_handle->data;

    if (likely(MPL_gpu_info.specialized_cache)) {
        /* Check if ipc-mapped buffer is already cached */
        lookup_entry.remote_mem_id = h.mem_id;
        lookup_entry.remote_dev_id = h.dev_id;
        lookup_entry.remote_pid = h.pid;

        keylen = sizeof(MPL_ze_mapped_buffer_lookup_t);

        HASH_FIND(hh, ipc_cache_mapped[dev_id], &lookup_entry.remote_mem_id, keylen, cache_entry);
    }

    if (cache_entry && cache_entry->ipc_buf) {
        *ptr = cache_entry->ipc_buf;
    } else {
        mpl_err = MPL_ze_ipc_handle_map(mpl_ipc_handle, true, dev_id, false, 0, ptr);
        if (mpl_err != MPL_SUCCESS) {
            goto fn_fail;
        }

        if (likely(MPL_gpu_info.specialized_cache)) {
            if (cache_entry) {
                cache_entry->ipc_buf = *ptr;

                removal_entry = (MPL_ze_mapped_buffer_entry_t *)
                    MPL_malloc(sizeof(MPL_ze_mapped_buffer_entry_t), MPL_MEM_OTHER);
                if (removal_entry == NULL) {
                    mpl_err = MPL_ERR_GPU_NOMEM;
                    goto fn_fail;
                }
                memcpy(removal_entry, cache_entry, sizeof(MPL_ze_mapped_buffer_entry_t));
                removal_entry->mapped_ptr = NULL;

                HASH_ADD(hh, ipc_cache_removal[dev_id], ipc_buf, sizeof(void *), removal_entry,
                         MPL_MEM_OTHER);
            } else {
                /* Insert into the cache */
                cache_entry = (MPL_ze_mapped_buffer_entry_t *)
                    MPL_malloc(sizeof(MPL_ze_mapped_buffer_entry_t), MPL_MEM_OTHER);
                if (cache_entry == NULL) {
                    mpl_err = MPL_ERR_GPU_NOMEM;
                    goto fn_fail;
                }
                memset(cache_entry, 0, sizeof(MPL_ze_mapped_buffer_entry_t));
                cache_entry->key = lookup_entry;
                cache_entry->ipc_buf = *ptr;

                removal_entry = (MPL_ze_mapped_buffer_entry_t *)
                    MPL_malloc(sizeof(MPL_ze_mapped_buffer_entry_t), MPL_MEM_OTHER);
                if (removal_entry == NULL) {
                    mpl_err = MPL_ERR_GPU_NOMEM;
                    goto fn_fail;
                }
                memcpy(removal_entry, cache_entry, sizeof(MPL_ze_mapped_buffer_entry_t));

                HASH_ADD(hh, ipc_cache_mapped[dev_id], key, keylen, cache_entry, MPL_MEM_OTHER);
                HASH_ADD(hh, ipc_cache_removal[dev_id], ipc_buf, sizeof(void *), removal_entry,
                         MPL_MEM_OTHER);
            }
        }
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

/* free a cache entry in mmap_cache_removal */
int MPL_ze_mmap_handle_unmap(void *ptr, int dev_id)
{
    int mpl_err = MPL_SUCCESS;
    unsigned keylen;
    MPL_ze_mapped_buffer_entry_t *cache_entry = NULL;
    MPL_ze_mapped_buffer_lookup_t lookup_entry;

    if (likely(MPL_gpu_info.specialized_cache)) {
        HASH_FIND(hh, mmap_cache_removal[dev_id], &ptr, sizeof(void *), cache_entry);
        if (cache_entry != NULL) {
            lookup_entry = cache_entry->key;
            keylen = sizeof(MPL_ze_mapped_buffer_lookup_t);

            HASH_DEL(mmap_cache_removal[dev_id], cache_entry);
            MPL_free(cache_entry);
            cache_entry = NULL;

            HASH_FIND(hh, ipc_cache_mapped[dev_id], &lookup_entry, keylen, cache_entry);

            /* since ipc_cache_removal is removed first, only mapped_ptr
             * need to be considered */
            if (cache_entry != NULL) {
                if (cache_entry->mapped_ptr) {
                    munmapFunction(cache_entry->nfds, cache_entry->fds, cache_entry->mapped_ptr,
                                   cache_entry->mapped_size);
                }
                HASH_DEL(ipc_cache_mapped[dev_id], cache_entry);
                MPL_free(cache_entry);
                cache_entry = NULL;
            }
        }
    } else {
        goto fn_fail;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_ipc_handle_unmap(void *ptr)
{
    int mpl_err = MPL_SUCCESS;
    int dev_id;
    unsigned keylen;
    ze_result_t ret;
    ze_device_handle_t device = NULL;
    MPL_ze_mapped_buffer_entry_t *cache_entry = NULL;
    MPL_ze_mapped_buffer_lookup_t lookup_entry;

    ze_memory_allocation_properties_t ptr_attr = {
        .stype = ZE_STRUCTURE_TYPE_MEMORY_ALLOCATION_PROPERTIES,
        .pNext = NULL,
        .type = 0,
        .id = 0,
        .pageSize = 0,
    };

    ret = zeMemGetAllocProperties(ze_context, ptr, &ptr_attr, &device);
    ZE_ERR_CHECK(ret);

    dev_id = device_to_dev_id(device);
    if (dev_id == -1) {
        goto fn_fail;
    }

    if (likely(MPL_gpu_info.specialized_cache)) {
        /* Remove from the caches */
        HASH_FIND(hh, ipc_cache_removal[dev_id], &ptr, sizeof(void *), cache_entry);

        if (cache_entry != NULL) {
            lookup_entry = cache_entry->key;
            keylen = sizeof(MPL_ze_mapped_buffer_lookup_t);

            HASH_DEL(ipc_cache_removal[dev_id], cache_entry);
            MPL_free(cache_entry);
            cache_entry = NULL;

            HASH_FIND(hh, ipc_cache_mapped[dev_id], &lookup_entry.remote_mem_id, keylen,
                      cache_entry);

            if (cache_entry != NULL) {
                if (cache_entry->mapped_ptr) {
                    munmapFunction(cache_entry->nfds, cache_entry->fds, cache_entry->mapped_ptr,
                                   cache_entry->mapped_size);
                }
                HASH_DEL(ipc_cache_mapped[dev_id], cache_entry);
                MPL_free(cache_entry);
                cache_entry = NULL;
            }
        }
    }

    /* Unmap the buffer */
    ret = zeMemCloseIpcHandle(ze_context, ptr);
    ZE_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/* at finalize, to free a cache entry in ipc_cache_removal cache */
static int remove_ipc_handle_entry(MPL_ze_mapped_buffer_entry_t * cache_entry, int dev_id)
{
    int mpl_err = MPL_SUCCESS;
    ze_result_t ret;
    unsigned keylen;
    MPL_ze_mapped_buffer_lookup_t lookup_entry;

    lookup_entry = cache_entry->key;
    keylen = sizeof(MPL_ze_mapped_buffer_lookup_t);

    HASH_DEL(ipc_cache_removal[dev_id], cache_entry);
    MPL_free(cache_entry);
    cache_entry = NULL;

    HASH_FIND(hh, ipc_cache_mapped[dev_id], &lookup_entry.remote_mem_id, keylen, cache_entry);

    if (cache_entry != NULL) {
        if (cache_entry->ipc_buf) {
            ret = zeMemCloseIpcHandle(ze_context, cache_entry->ipc_buf);
            ZE_ERR_CHECK(ret);
        }
        if (cache_entry->mapped_ptr) {
            munmapFunction(cache_entry->nfds, cache_entry->fds, cache_entry->mapped_ptr,
                           cache_entry->mapped_size);
        }
        HASH_DEL(ipc_cache_mapped[dev_id], cache_entry);
        MPL_free(cache_entry);
        cache_entry = NULL;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_query_pointer_attr(const void *ptr, MPL_pointer_attr_t * attr)
{
    int mpl_err = MPL_SUCCESS;

    ze_result_t ret;
    memset(&attr->device_attr.prop, 0, sizeof(ze_memory_allocation_properties_t));
    ret = zeMemGetAllocProperties(ze_context, ptr,
                                  &attr->device_attr.prop, &attr->device_attr.device);
    ZE_ERR_CHECK(ret);
    attr->device = attr->device_attr.device;
    switch (attr->device_attr.prop.type) {
        case ZE_MEMORY_TYPE_UNKNOWN:
            attr->type = MPL_GPU_POINTER_UNREGISTERED_HOST;
            break;
        case ZE_MEMORY_TYPE_HOST:
            attr->type = MPL_GPU_POINTER_REGISTERED_HOST;
            break;
        case ZE_MEMORY_TYPE_DEVICE:
            attr->type = MPL_GPU_POINTER_DEV;
            break;
        case ZE_MEMORY_TYPE_SHARED:
            attr->type = MPL_GPU_POINTER_MANAGED;
            break;
        default:
            goto fn_fail;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_query_pointer_is_dev(const void *ptr, MPL_pointer_attr_t * attr)
{
    ze_result_t ret ATTRIBUTE((unused));
    ze_memory_type_t type;

    if (attr == NULL) {
        ze_memory_allocation_properties_t prop = {
            .stype = ZE_STRUCTURE_TYPE_MEMORY_ALLOCATION_PROPERTIES,
            .pNext = NULL,
            .type = 0,
            .id = 0,
            .pageSize = 0,
        };
        ze_device_handle_t device = NULL;

        ret = zeMemGetAllocProperties(ze_context, ptr, &prop, &device);
        assert(ret == ZE_RESULT_SUCCESS);
        type = prop.type;
    } else {
        type = attr->device_attr.prop.type;
    }

    /* Treat all ZE allocations as device objects. This is because even host-registered memory
     * are implemented as device objects in the driver. As such, these allocations don't work
     * properly with XPMEM. */
    return type != ZE_MEMORY_TYPE_UNKNOWN;
}

int MPL_gpu_query_pointer_is_strict_dev(const void *ptr, MPL_pointer_attr_t * attr)
{
    ze_result_t ret ATTRIBUTE((unused));
    ze_memory_type_t type;

    if (attr == NULL) {
        ze_memory_allocation_properties_t prop = {
            .stype = ZE_STRUCTURE_TYPE_MEMORY_ALLOCATION_PROPERTIES,
            .pNext = NULL,
            .type = 0,
            .id = 0,
            .pageSize = 0,
        };
        ze_device_handle_t device = NULL;

        ret = zeMemGetAllocProperties(ze_context, ptr, &prop, &device);
        assert(ret == ZE_RESULT_SUCCESS);
        type = prop.type;
    } else {
        type = attr->device_attr.prop.type;
    }
    return type == ZE_MEMORY_TYPE_DEVICE;
}

int MPL_gpu_query_is_same_dev(int global_dev1, int global_dev2)
{
    MPL_ze_device_entry_t *device_state1, *device_state2;
    int local_dev1, local_dev2;

    assert(global_dev1 >= 0 && global_dev1 < global_ze_device_count);
    assert(global_dev2 >= 0 && global_dev2 < global_ze_device_count);

    local_dev1 = MPL_gpu_global_to_local_dev_id(global_dev1);
    local_dev2 = MPL_gpu_global_to_local_dev_id(global_dev2);
    /* check if invisible devices */
    if (local_dev1 == -1 || local_dev2 == -1)
        return 0;

#ifdef ZE_PCI_PROPERTIES_EXT_NAME
    if (get_physical_device(local_dev1) != -1 && get_physical_device(local_dev2) != -1 &&
        get_physical_device(local_dev1) == get_physical_device(local_dev2))
        return 1;

    device_state1 = device_states + local_dev1;
    device_state2 = device_states + local_dev2;
    if (device_state1->pci_avail && device_state2->pci_avail)
        return device_state1->pci.domain == device_state2->pci.domain &&
            device_state1->pci.bus == device_state2->pci.bus &&
            device_state1->pci.device == device_state2->pci.device &&
            device_state1->pci.function == device_state2->pci.function;
    else
        return 0;
#else
    return MPL_gpu_get_root_device(local_dev1) == MPL_gpu_get_root_device(local_dev2);
#endif
}

int MPL_gpu_malloc(void **ptr, size_t size, MPL_gpu_device_handle_t h_device)
{
    int mpl_err = MPL_SUCCESS;
    int ret;
    size_t mem_alignment;
    ze_device_mem_alloc_desc_t device_desc = {
        .stype = ZE_STRUCTURE_TYPE_DEVICE_MEM_ALLOC_DESC,
        .pNext = NULL,
        .flags = 0,
        .ordinal = 0,   /* We currently support a single memory type */
    };
    /* Currently ZE ignores this argument and uses an internal alignment
     * value. However, this behavior can change in the future. */
    mem_alignment = 1;
    ret = zeMemAllocDevice(ze_context, &device_desc, size, mem_alignment, h_device, ptr);

    ZE_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_malloc_host(void **ptr, size_t size)
{
    int mpl_err = MPL_SUCCESS;
    int ret;
    size_t mem_alignment;
    ze_host_mem_alloc_desc_t host_desc = {
        .stype = ZE_STRUCTURE_TYPE_HOST_MEM_ALLOC_DESC,
        .pNext = NULL,
        .flags = 0,
    };

    /* Currently ZE ignores this argument and uses an internal alignment
     * value. However, this behavior can change in the future. */
    mem_alignment = 1;
    ret = zeMemAllocHost(ze_context, &host_desc, size, mem_alignment, ptr);
    ZE_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_free(void *ptr)
{
    int mpl_err = MPL_SUCCESS;
    mpl_err = zeMemFree(ze_context, ptr);
    ZE_ERR_CHECK(mpl_err);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_free_host(void *ptr)
{
    int mpl_err;
    mpl_err = zeMemFree(ze_context, ptr);
    ZE_ERR_CHECK(mpl_err);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_register_host(const void *ptr, size_t size)
{
    int mpl_err = MPL_SUCCESS;
    ze_result_t ret;

    assert(zexDriverImportExternalPointer);
    ret = zexDriverImportExternalPointer(ze_driver_handle, (void *) ptr, size);
    assert(ret == ZE_RESULT_SUCCESS);
    ZE_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_unregister_host(const void *ptr)
{
    int mpl_err = MPL_SUCCESS;
    ze_result_t ret;

    assert(zexDriverReleaseImportedPointer);
    ret = zexDriverReleaseImportedPointer(ze_driver_handle, (void *) ptr);
    assert(ret == ZE_RESULT_SUCCESS);
    ZE_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_get_dev_id_from_attr(MPL_pointer_attr_t * attr)
{
    int dev_id = -1;

    dev_id = device_to_dev_id(attr->device);

    return dev_id;
}

int MPL_gpu_get_buffer_bounds(const void *ptr, void **pbase, uintptr_t * len)
{
    int mpl_err = MPL_SUCCESS;
    int ret;
    ret = zeMemGetAddressRange(ze_context, ptr, pbase, len);
    ZE_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/* command list utility functions - used for MPL_gpu_imemcpy */
/* create complete set of command queues for an engine type */
static int create_cmdqueue(int dev, int engine)
{
    int mpl_err = MPL_SUCCESS;
    int ret;

    MPL_ze_device_entry_t *device_state = device_states + dev;
    assert(engine < device_state->numQueueGroups);
    MPL_ze_engine_entry_t *engine_state = device_state->engines + engine;
    assert(engine_state->numQueues);

    for (int i = 0; i < engine_state->numQueues; i++) {
        ze_command_queue_desc_t cmdQueueDesc = {
            .stype = ZE_STRUCTURE_TYPE_COMMAND_QUEUE_DESC,
            .pNext = NULL,
            .index = i,
            .flags = 0,
            .ordinal = engine,
            .mode = ZE_COMMAND_QUEUE_MODE_ASYNCHRONOUS,
            .priority = ZE_COMMAND_QUEUE_PRIORITY_NORMAL,
        };
        ret =
            zeCommandQueueCreate(ze_context, ze_devices_handle[dev], &cmdQueueDesc,
                                 &engine_state->cmdQueues[i]);
        ZE_ERR_CHECK(ret);
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

static int get_cmdlist(int dev, int engine, MPL_cmdlist_pool_t ** cl_entry)
{
    int mpl_err = MPL_SUCCESS;
    int ret;
    MPL_cmdlist_pool_t *cmdList_entry = NULL;
    ze_command_list_handle_t cmdList = NULL;
    MPL_ze_device_entry_t *device_state;

    *cl_entry = NULL;
    if (engine > device_states[dev].numQueueGroups - 1 ||
        device_states[dev].engines[engine].numQueues == 0) {
        /* certain type of engine may not be available on the subdevices */
        int parent_dev = MPL_gpu_get_root_device(dev);
        if (parent_dev == dev || device_states[dev].engines[engine].numQueues == 0)
            goto fn_fail;
        dev = parent_dev;
    }
    assert(dev < local_ze_device_count);
    device_state = device_states + dev;
    if (device_state->engines[engine].cmdList_pool) {
        cmdList_entry = device_state->engines[engine].cmdList_pool;
        DL_DELETE(device_state->engines[engine].cmdList_pool, cmdList_entry);
        cmdList = cmdList_entry->cmdList;
        ret = zeCommandListReset(cmdList);
        ZE_ERR_CHECK(ret);
    } else {
        ze_command_list_desc_t cmdListDesc = {
            .stype = ZE_STRUCTURE_TYPE_COMMAND_LIST_DESC,
            .pNext = NULL,
            .commandQueueGroupOrdinal = engine,
            .flags = 0,
        };
        ret = zeCommandListCreate(ze_context, ze_devices_handle[dev], &cmdListDesc, &cmdList);
        ZE_ERR_CHECK(ret);
        cmdList_entry =
            (MPL_cmdlist_pool_t *) MPL_malloc(sizeof(MPL_cmdlist_pool_t), MPL_MEM_OTHER);
        if (cmdList_entry == NULL) {
            goto fn_fail;
        }
        cmdList_entry->cmdList = cmdList;
        cmdList_entry->dev = dev;
        cmdList_entry->engine = engine;
    }
    *cl_entry = cmdList_entry;

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/* immediate command list utility functions */
static inline int get_immediate_cmdlist(int *dev, MPL_gpu_copy_direction_t dir, int engine,
                                        ze_command_list_handle_t * cl)
{
    int mpl_err = MPL_SUCCESS;
    int ret, index;
    MPL_ze_device_entry_t *device_state;

    if (engine > device_states[*dev].numQueueGroups - 1 ||
        device_states[*dev].engines[engine].numQueues == 0) {
        /* certain type of engine may not be available on the subdevices */
        *dev = MPL_gpu_get_root_device(*dev);
        if (device_states[*dev].engines[engine].numQueues == 0)
            goto fn_fail;
    }
    assert(*dev < local_ze_device_count);
    device_state = device_states + *dev;
    assert(engine < device_state->numQueueGroups);
    MPL_ze_engine_entry_t *engine_state = device_state->engines + engine;

    if (dir == MPL_GPU_COPY_DIRECTION_NONE) {
        if (MPL_gpu_info.roundrobin_cmdq) {
            index = engine_state->curQueue;
            /* move to next queue */
            engine_state->curQueue++;
            if (engine_state->curQueue == engine_state->numQueues)
                engine_state->curQueue = 0;
        } else {
            index = 0;
        }
    } else {
        if (MPL_gpu_info.roundrobin_cmdq)
            index = dir % engine_state->numQueues;
        else
            index = 0;
    }

    if (!engine_state->cmdlists[index]) {
        assert(engine_state->numQueues);

        for (int i = 0; i < engine_state->numQueues; i++) {
            ze_command_queue_desc_t cmdQueueDesc = {
                .stype = ZE_STRUCTURE_TYPE_COMMAND_QUEUE_DESC,
                .pNext = NULL,
                .index = i,
                .flags = 0,
                .ordinal = engine,
                .mode = ZE_COMMAND_QUEUE_MODE_ASYNCHRONOUS,
                .priority = ZE_COMMAND_QUEUE_PRIORITY_NORMAL,
            };
            ret = zeCommandListCreateImmediate(ze_context, ze_devices_handle[*dev], &cmdQueueDesc,
                                               &engine_state->cmdlists[i]);
            ZE_ERR_CHECK(ret);
        }
    }
    assert(engine_state->cmdlists[index]);

    *cl = engine_state->cmdlists[index];

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/* no safety check in this function, call this function with safety protection.
   commit = false: append to command list only
   commit = true:  close the command list and submit to command queue
   direction can be 0, 1:  for device to host   d2h h2d
                    2, 3:  for device to device    local to remote, remote to local
                    4:     no direction, can use random command list and queue
*/
static int MPL_gpu_imemcpy_normal(void *dest_ptr, void *src_ptr, size_t size, int dev,
                                  MPL_gpu_copy_direction_t dir, MPL_gpu_engine_type_t engine_type,
                                  MPL_gpu_request * req, bool commit)
{
    int mpl_err = MPL_SUCCESS;
    MPL_ze_device_entry_t *device_state = NULL;
    MPL_gpu_event *mpl_event;
    ze_command_list_handle_t cmdList = NULL;
    MPL_cmdlist_pool_t *cmdList_entry;
    int ret;
    int orig_dev = dev;
    int engine = engine_conversion[engine_type];

    assert(dev >= 0 && dev < local_ze_device_count);

    if (dest_ptr && src_ptr) {
        MPL_gpu_event *pre_event = NULL;
        int nevent = 0;
        ret = MPL_event_pool_get_event(&mpl_event);
        ZE_ERR_CHECK(ret);
        device_state = device_states + dev;
        if (dir == MPL_GPU_COPY_DIRECTION_NONE) {
            ret = get_cmdlist(orig_dev, engine, &cmdList_entry);
            ZE_ERR_CHECK(ret);
            cmdList = cmdList_entry->cmdList;
        } else {
            if (!device_states[orig_dev].last_cmdList_entry[dir]) {
                ret = get_cmdlist(orig_dev, engine, &cmdList_entry);
                ZE_ERR_CHECK(ret);
                device_states[orig_dev].last_cmdList_entry[dir] = cmdList_entry;
                dev = cmdList_entry->dev;
                device_state = device_states + dev;
            }
            cmdList_entry = device_states[orig_dev].last_cmdList_entry[dir];
            cmdList = cmdList_entry->cmdList;
            if (device_state->prev_event[dir]) {
                pre_event = device_state->prev_event[dir];
                nevent = 1;
            }
            if (device_states[orig_dev].last_cmdList_entry[dir]->dev != dev)
                goto fn_fail;
            device_state->prev_event[dir] = mpl_event;
        }
        assert(dev < local_ze_device_count);
        ret =
            zeCommandListAppendMemoryCopy(cmdList, dest_ptr, src_ptr, size, mpl_event->event,
                                          nevent, pre_event ? &pre_event->event : NULL);
        ZE_ERR_CHECK(ret);
        req->gpu_event = mpl_event;
    } else {
        /* unlikely */
        device_state = device_states + dev;
        assert(device_state->prev_event[dir]);
        req->gpu_event = device_state->prev_event[dir];
        assert(dir < MPL_GPU_COPY_DIRECTION_TYPES);
        cmdList_entry = device_states[orig_dev].last_cmdList_entry[dir];
        assert(cmdList_entry);
        cmdList = cmdList_entry->cmdList;
    }
    if (commit && cmdList) {
        MPL_ze_engine_entry_t *engine_state = device_state->engines + engine;
        ret = zeCommandListClose(cmdList);
        ZE_ERR_CHECK(ret);
        int q_index;
        if (dir == MPL_GPU_COPY_DIRECTION_NONE) {
            if (MPL_gpu_info.roundrobin_cmdq) {
                q_index = engine_state->curQueue;
                /* move to next queue */
                engine_state->curQueue++;
                if (engine_state->curQueue == engine_state->numQueues)
                    engine_state->curQueue = 0;
            } else {
                q_index = 0;
            }
        } else {
            if (MPL_gpu_info.roundrobin_cmdq)
                q_index = dir % engine_state->numQueues;
            else
                q_index = 0;
        }
        assert(engine_state->cmdQueues);
        ze_command_queue_handle_t cmdq = engine_state->cmdQueues[q_index];
        if (cmdq == NULL) {
            mpl_err = create_cmdqueue(device_state->dev_id, engine);
            if (mpl_err != MPL_SUCCESS)
                goto fn_fail;
            cmdq = engine_state->cmdQueues[q_index];
            assert(cmdq);
        }
        ret = zeCommandQueueExecuteCommandLists(cmdq, 1, &cmdList, NULL);
        ZE_ERR_CHECK(ret);
        req->cmdList = cmdList_entry;
        if (dir != MPL_GPU_COPY_DIRECTION_NONE)
            device_states[orig_dev].last_cmdList_entry[dir] = NULL;
    } else {
        /* continue building the command list till done */
        req->cmdList = NULL;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

/* use immediate command list, commit is ignored */
int MPL_gpu_imemcpy(void *dest_ptr, void *src_ptr, size_t size, int dev,
                    MPL_gpu_copy_direction_t dir, MPL_gpu_engine_type_t engine_type,
                    MPL_gpu_request * req, bool commit)
{
    int mpl_err = MPL_SUCCESS;
    int ret;
    MPL_ze_device_entry_t *device_state = NULL;
    MPL_gpu_event *mpl_event, *pre_event = NULL;
    ze_command_list_handle_t cmdl;
    int nevent = 0;
    int engine = -1;

    assert(dev >= 0 && dev < local_ze_device_count);
    /* if engine type does not exist, e.g. link copy does not exist on ATS
     * use the highest engine number */
    assert(engine_type >= 0 && engine_type < MPL_GPU_ENGINE_TYPE_LAST);
    engine = engine_conversion[engine_type];
    if (engine >= device_states[dev].numQueueGroups) {
        engine_type = device_states[dev].numQueueGroups - 1;
        engine = engine_conversion[engine_type];
    }

    if (!MPL_gpu_info.use_immediate_cmdlist) {
        return MPL_gpu_imemcpy_normal(dest_ptr, src_ptr, size, dev, dir, engine_type, req, commit);
    }

    assert(dest_ptr && src_ptr);

    ret = MPL_event_pool_get_event(&mpl_event);
    ZE_ERR_CHECK(ret);
    ret = get_immediate_cmdlist(&dev, dir, engine, &cmdl);
    ZE_ERR_CHECK(ret);
    assert(cmdl);
    device_state = device_states + dev;
    if (dir != MPL_GPU_COPY_DIRECTION_NONE) {
        if (device_state->prev_event[dir]) {
            nevent = 1;
            pre_event = device_state->prev_event[dir];
        }
        device_state->prev_event[dir] = mpl_event;
    }
    ret =
        zeCommandListAppendMemoryCopy(cmdl, dest_ptr, src_ptr, size, mpl_event->event, nevent,
                                      nevent ? &pre_event->event : NULL);
    ZE_ERR_CHECK(ret);
    req->cmdList = NULL;
    req->gpu_event = mpl_event;

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_test(MPL_gpu_request * req, int *completed)
{
    int mpl_err = MPL_SUCCESS;
    ze_result_t ret;

    assert(req->gpu_event->event);
    ret = zeEventQueryStatus(req->gpu_event->event);
    if (ret == ZE_RESULT_SUCCESS) {
        *completed = 1;
        if (req->cmdList) {
            DL_APPEND(device_states[req->cmdList->dev].engines[req->cmdList->engine].cmdList_pool,
                      req->cmdList);
        }
        MPL_event_pool_push_event(req->gpu_event);
    } else if (ret != ZE_RESULT_NOT_READY) {
        assert(0);
        goto fn_fail;
    } else {
        *completed = 0;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

static void gpu_free_hooks_cb(void *dptr)
{
    gpu_free_hook_s *current = free_hook_chain;
    if (dptr != NULL) {
        /* we call gpu hook only when dptr != NULL */
        while (current) {
            current->free_hook(dptr);
            current = current->next;
        }
    }
    return;
}

MPL_STATIC_INLINE_PREFIX int gpu_mem_hook_init(void)
{
    void *libze_handle = dlopen("libze_loader.so", RTLD_LAZY | RTLD_GLOBAL);
    assert(libze_handle);

    sys_zeMemFree = (void *) dlsym(libze_handle, "zeMemFree");
    assert(sys_zeMemFree);

    return MPL_SUCCESS;
}

int MPL_gpu_free_hook_register(void (*free_hook) (void *dptr))
{
    gpu_free_hook_s *hook_obj = MPL_malloc(sizeof(gpu_free_hook_s), MPL_MEM_OTHER);
    assert(hook_obj);
    hook_obj->free_hook = free_hook;
    hook_obj->next = NULL;

    MPL_initlock_lock(&free_hook_mutex);
    if (!free_hook_chain)
        free_hook_chain = hook_obj;
    else {
        hook_obj->next = free_hook_chain;
        free_hook_chain = hook_obj;
    }
    MPL_initlock_unlock(&free_hook_mutex);

    return MPL_SUCCESS;
}

int MPL_gpu_launch_hostfn(int stream, MPL_gpu_hostfn fn, void *data)
{
    return -1;
}

bool MPL_gpu_stream_is_valid(MPL_gpu_stream_t stream)
{
    return false;
}

void MPL_gpu_enqueue_trigger(MPL_gpu_event_t * var, MPL_gpu_stream_t stream)
{
    assert(0);
}

void MPL_gpu_enqueue_wait(MPL_gpu_event_t * var, MPL_gpu_stream_t stream)
{
    assert(0);
}

void MPL_gpu_event_init_count(MPL_gpu_event_t * var, int count)
{
    *var = count;
}

void MPL_gpu_event_complete(MPL_gpu_event_t * var)
{
    *var -= 1;
}

bool MPL_gpu_event_is_complete(MPL_gpu_event_t * var)
{
    return (*var) <= 0;
}

#ifdef HAVE_VISIBILITY
__attribute__ ((visibility("default")))
#endif
ze_result_t ZE_APICALL zeMemFree(ze_context_handle_t hContext, void *dptr)
{
    ze_result_t result;
    MPL_initlock_lock(&free_hook_mutex);
    if (!sys_zeMemFree) {
        gpu_mem_hook_init();
    }

    gpu_free_hooks_cb(dptr);
    result = sys_zeMemFree(hContext, dptr);

    MPL_initlock_unlock(&free_hook_mutex);
    return (result);
}

/* ZE-Specific Functions */

#ifdef ZE_PCI_PROPERTIES_EXT_NAME
static void queryBDF(char *pcipath, int *domain, int *b, int *d, int *f)
{
    char *endptr = NULL;
    const char *device_suffix = "-render";

    char *rdsPos = strstr(pcipath, device_suffix);
    assert(rdsPos);

    char *bdfstr = rdsPos - 12;

    *domain = strtol(bdfstr, &endptr, 16);
    bdfstr = endptr + 1;
    *b = strtol(bdfstr, &endptr, 16);
    bdfstr = endptr + 1;
    *d = strtol(bdfstr, &endptr, 16);
    bdfstr = endptr + 1;
    *f = strtol(bdfstr, &endptr, 16);
}

static int get_bdfs(int nfds, int *bdfs)
{
    const char *device_directory = "/dev/dri/by-path";
    const char *device_suffix = "-render";
    struct dirent *ent = NULL;
    int n = 0;

#ifndef MPL_ENABLE_DRMFD
    printf("Error> drmfd is not supported!");
    goto fn_fail;
#endif

    DIR *dir = opendir(device_directory);
    if (dir == NULL) {
        goto fn_fail;
    }

    /* Search for all devices in the device directory */
    while ((ent = readdir(dir)) != NULL) {
        char dev_name[128];

        if (ent->d_name[0] == '.') {
            continue;
        }

        /* They must contain the device suffix */
        if (strstr(ent->d_name, device_suffix) == NULL) {
            continue;
        }

        strcpy(dev_name, device_directory);
        strcat(dev_name, "/");
        strcat(dev_name, ent->d_name);

        int domain, b, d, f;
        queryBDF(dev_name, &domain, &b, &d, &f);
        bdfs[n++] = domain;
        bdfs[n++] = b;
        bdfs[n++] = d;
        bdfs[n++] = f;
    }

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

static int search_physical_devices(ze_pci_address_ext_t pci)
{
    for (int i = 0; i < physical_device_count; i++) {
        if (physical_device_states[i].domain == pci.domain &&
            physical_device_states[i].bus == pci.bus &&
            physical_device_states[i].device == pci.device &&
            physical_device_states[i].function == pci.function)
            return i;
    }
    return -1;
}
#endif

static int compareBDFFn(const void *_a, const void *_b)
{
    int *a = (int *) _a;
    int *b = (int *) _b;

    if (a[0] != b[0])
        return a[0] - b[0];

    if (a[1] != b[1])
        return a[1] - b[1];

    if (a[2] != b[2])
        return a[2] - b[2];

    return a[3] - b[3];
}

static void sort_bdfs(int n, int *bdfs)
{
    qsort(bdfs, n, sizeof(int) * 4, compareBDFFn);
}

int MPL_ze_init_device_fds(int *num_fds, int *device_fds, int *bdfs)
{
    const char *device_directory = "/dev/dri/by-path";
    const char *device_suffix = "-render";
    struct dirent *ent = NULL;
    int n = 0;

#ifndef MPL_ENABLE_DRMFD
    printf("Error> drmfd is not supported!");
    goto fn_fail;
#endif

#ifdef ZE_PCI_PROPERTIES_EXT_NAME
    if (device_fds) {
        get_bdfs(*num_fds, bdfs);
        /* sort bdf */
        sort_bdfs(*num_fds, bdfs);
    }
#endif

    DIR *dir = opendir(device_directory);
    if (dir == NULL) {
        goto fn_fail;
    }

    /* Search for all devices in the device directory */
    while ((ent = readdir(dir)) != NULL) {
        char dev_name[128];

        if (ent->d_name[0] == '.') {
            continue;
        }

        /* They must contain the device suffix */
        if (strstr(ent->d_name, device_suffix) == NULL) {
            continue;
        }

        strcpy(dev_name, device_directory);
        strcat(dev_name, "/");
        strcat(dev_name, ent->d_name);

        /* Open the device */
        if (device_fds) {
#ifdef ZE_PCI_PROPERTIES_EXT_NAME
            int domain, b, d, f;
            queryBDF(dev_name, &domain, &b, &d, &f);
            /* find in bdfs */
            n = -1;
            for (int i = 0; i < *num_fds; i++) {
                if (domain == bdfs[i * 4] && b == bdfs[i * 4 + 1] && d == bdfs[i * 4 + 2] &&
                    f == bdfs[i * 4 + 3])
                    n = i;
            }
            assert(n != -1);
#endif
            device_fds[n] = open(dev_name, O_RDWR);
        }

        n++;
    }

    if (device_fds == NULL)
        *num_fds = n;

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

void MPL_ze_set_fds(int num_fds, int *fds, int *bdfs)
{
    physical_device_count = num_fds;
    physical_device_states =
        (physical_device_state *) MPL_malloc(num_fds * sizeof(physical_device_state),
                                             MPL_MEM_OTHER);
    for (int i = 0; i < num_fds; i++) {
        physical_device_states[i].fd = fds[i];
#ifdef ZE_PCI_PROPERTIES_EXT_NAME
        physical_device_states[i].domain = bdfs[4 * i];
        physical_device_states[i].bus = bdfs[4 * i + 1];
        physical_device_states[i].device = bdfs[4 * i + 2];
        physical_device_states[i].function = bdfs[4 * i + 3];
#endif
    }
    MPL_free(fds);
    MPL_free(bdfs);

#ifdef ZE_PCI_PROPERTIES_EXT_NAME
    /* update sys_device_index for each logical device */
    for (int d = 0; d < local_ze_device_count; d++) {
        MPL_ze_device_entry_t *device_state = device_states + d;
        device_state->sys_device_index = search_physical_devices(device_state->pci);
        assert(device_state->sys_device_index != -1);
    }
#endif
}

void MPL_ze_ipc_remove_cache_handle(void *dptr)
{
    ze_result_t ret;
    ze_device_handle_t device = NULL;
    int local_dev_id = -1;
    uint64_t mem_id = 0;
    MPL_ze_ipc_handle_entry_t *cache_entry = NULL;

    ze_memory_allocation_properties_t ptr_attr = {
        .stype = ZE_STRUCTURE_TYPE_MEMORY_ALLOCATION_PROPERTIES,
        .pNext = NULL,
        .type = 0,
        .id = 0,
        .pageSize = 0,
    };

    ret = zeMemGetAllocProperties(ze_context, dptr, &ptr_attr, &device);
    ZE_ERR_CHECK(ret);

    if (ptr_attr.type == ZE_MEMORY_TYPE_DEVICE) {
        local_dev_id = device_to_dev_id(device);
        if (local_dev_id == -1) {
            goto fn_fail;
        }

        /* Remove entry if mem_id is cached */
        mem_id = ptr_attr.id;
        HASH_FIND(hh, ipc_cache_tracked[local_dev_id], &mem_id, sizeof(uint64_t), cache_entry);

        if (cache_entry) {
            HASH_DEL(ipc_cache_tracked[local_dev_id], cache_entry);
            MPL_free(cache_entry);
        }
    }

  fn_exit:
    return;
  fn_fail:
    fprintf(stderr, "Error > failed to complete MPL_ze_ipc_remove_cache_handle\n");
    goto fn_exit;
}

int MPL_ze_ipc_handle_create(const void *ptr, MPL_gpu_device_attr * ptr_attr, int local_dev_id,
                             int use_shared_fd, MPL_gpu_ipc_mem_handle_t * mpl_ipc_handle)
{
    int mpl_err = MPL_SUCCESS;
    ze_result_t ret;
    int fds[2], handles[2], status;
    uint32_t nfds;
    ze_ipc_mem_handle_t ze_ipc_handle[2];
    fd_pid_t h;
    uint64_t mem_id = 0;

    mem_id = ptr_attr->prop.id;

    nfds = 0;   /* must initialized to 0 */
    if (zexMemGetIpcHandles) {
        ret = zexMemGetIpcHandles(ze_context, ptr, &nfds, NULL);
        ZE_ERR_CHECK(ret);
        if (nfds) {
            assert(nfds <= 2);
            ret = zexMemGetIpcHandles(ze_context, ptr, &nfds, ze_ipc_handle);
        }
    }
    if (!nfds) {
        ret = zeMemGetIpcHandle(ze_context, ptr, &ze_ipc_handle[0]);
        nfds = 1;
    }
    ZE_ERR_CHECK(ret);

    h.nfds = nfds;
    if (physical_device_states != NULL) {
        if (use_shared_fd) {
            int shared_dev_id = get_physical_device(local_dev_id);
            for (int i = 0; i < nfds; i++) {
                /* convert dma_buf fd to GEM handle */
                memcpy(&fds[i], &ze_ipc_handle[i], sizeof(int));
                status =
                    fd_to_handle(physical_device_states[shared_dev_id].fd, fds[i], &handles[i]);
                if (status) {
                    goto fn_fail;
                }
            }

            /* Hash (ptr, dev_id, handle) to close later */
            MPL_ze_gem_hash_entry_t *entry = NULL;
            HASH_FIND_PTR(gem_hash, &ptr, entry);

            if (entry == NULL) {
                entry =
                    (MPL_ze_gem_hash_entry_t *) MPL_malloc(sizeof(MPL_ze_gem_hash_entry_t),
                                                           MPL_MEM_OTHER);
                if (entry == NULL) {
                    goto fn_fail;
                }

                entry->ptr = ptr;
                entry->dev_id = shared_dev_id;
                for (int i = 0; i < nfds; i++)
                    entry->handles[i] = handles[i];
                entry->nhandles = nfds;
                HASH_ADD_PTR(gem_hash, ptr, entry, MPL_MEM_OTHER);
            }

            for (int i = 0; i < nfds; i++)
                h.fds[i] = handles[i];
            h.dev_id = shared_dev_id;
        } else {
            for (int i = 0; i < nfds; i++) {
                memcpy(&h.fds[i], &ze_ipc_handle[i], sizeof(int));
            }
            h.dev_id = MPL_gpu_local_to_global_dev_id(local_dev_id);
        }
    } else {
        for (int i = 0; i < nfds; i++) {
            memcpy(&h.fds[i], &ze_ipc_handle[i], sizeof(int));
        }
        h.dev_id = MPL_gpu_local_to_global_dev_id(local_dev_id);
        assert(h.dev_id != -1);
    }

    h.pid = mypid;
    h.mem_id = mem_id;

    for (int i = 0; i < nfds; i++) {
        memcpy(&mpl_ipc_handle->ipc_handles[i], &ze_ipc_handle[i], sizeof(ze_ipc_mem_handle_t));
    }
    mpl_ipc_handle->data = h;

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_ze_ipc_handle_map(MPL_gpu_ipc_mem_handle_t * mpl_ipc_handle, int is_shared_handle,
                          int dev_id, int is_mmap, size_t size, void **ptr)
{
    int mpl_err = MPL_SUCCESS;
    ze_result_t ret;
    int fds[2], status;
    uint32_t nfds;
    MPL_gpu_device_handle_t dev_handle;

    fd_pid_t h;
    h = mpl_ipc_handle->data;
    nfds = h.nfds;

    if (physical_device_states != NULL) {
        /* convert GEM handle to fd */
        for (int i = 0; i < nfds; i++) {
            status = handle_to_fd(physical_device_states[h.dev_id].fd, h.fds[i], &fds[i]);
            if (status) {
                goto fn_fail;
            }
        }
    } else {
        /* pidfd_getfd */
        if (h.pid != mypid) {
            int pid_fd = syscall(__NR_pidfd_open, h.pid, 0);
            if (pid_fd == -1) {
                printf("pidfd_open error: %s (%d %d %d)\n", strerror(errno), h.pid, h.fds[0],
                       h.dev_id);
            }
            assert(pid_fd != -1);
            for (int i = 0; i < nfds; i++) {
                fds[i] = syscall(__NR_pidfd_getfd, pid_fd, h.fds[i], 0);
                if (fds[i] == -1) {
                    printf("Error> pidfd_getfd is not implemented!");
                    mpl_err = MPL_ERR_GPU_INTERNAL;
                    goto fn_fail;
                }
            }
            close(pid_fd);
        } else {
            fds[0] = h.fds[0];
            fds[1] = h.fds[1];
        }
    }

    if (is_mmap) {
        mpl_err = mmapFunction(nfds, fds, size, ptr);
        if (mpl_err != MPL_SUCCESS) {
            goto fn_fail;
        }
    } else {
        ze_ipc_mem_handle_t ze_ipc_handle[2];

        mpl_err = dev_id_to_device(dev_id, &dev_handle);
        if (mpl_err != MPL_SUCCESS) {
            goto fn_fail;
        }
        for (int i = 0; i < nfds; i++) {
            memcpy(&ze_ipc_handle[i], &mpl_ipc_handle->ipc_handles[i], sizeof(ze_ipc_mem_handle_t));
            memcpy(&ze_ipc_handle[i], &fds[i], sizeof(int));
        }

        if (zexMemOpenIpcHandles) {
            ret = zexMemOpenIpcHandles(ze_context, dev_handle, nfds, ze_ipc_handle, 0, ptr);
        } else {
            ret = zeMemOpenIpcHandle(ze_context, dev_handle, ze_ipc_handle[0], 0, ptr);
        }
        ZE_ERR_CHECK(ret);
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

/* given an remote IPC handle, mmap it to host */
int MPL_ze_ipc_handle_mmap_host(MPL_gpu_ipc_mem_handle_t * mpl_ipc_handle, int is_shared_handle,
                                int dev_id, size_t size, void **ptr)
{
    int mpl_err = MPL_SUCCESS;
    unsigned keylen;
    MPL_ze_mapped_buffer_entry_t *cache_entry = NULL;
    MPL_ze_mapped_buffer_entry_t *removal_entry = NULL;
    MPL_ze_mapped_buffer_lookup_t lookup_entry;

    fd_pid_t h;
    h = mpl_ipc_handle->data;

    if (likely(MPL_gpu_info.specialized_cache)) {
        /* Check if ipc-mapped buffer is already cached */
        memset(&lookup_entry, 0, sizeof(MPL_ze_mapped_buffer_lookup_t));
        lookup_entry.remote_mem_id = h.mem_id;
        lookup_entry.remote_dev_id = h.dev_id;
        lookup_entry.remote_pid = h.pid;

        keylen = sizeof(MPL_ze_mapped_buffer_lookup_t);

        HASH_FIND(hh, ipc_cache_mapped[dev_id], &lookup_entry.remote_mem_id, keylen, cache_entry);
    }

    *ptr = NULL;
    if (cache_entry) {
        if (cache_entry->mapped_ptr) {
            *ptr = cache_entry->mapped_ptr;
        }
    } else {
        if (likely(MPL_gpu_info.specialized_cache)) {
            /* Insert into the cache */
            cache_entry =
                (MPL_ze_mapped_buffer_entry_t *) MPL_malloc(sizeof(MPL_ze_mapped_buffer_entry_t),
                                                            MPL_MEM_OTHER);
            if (cache_entry == NULL) {
                mpl_err = MPL_ERR_GPU_NOMEM;
                goto fn_fail;
            }
            memset(cache_entry, 0, sizeof(MPL_ze_mapped_buffer_entry_t));
            cache_entry->key = lookup_entry;
            HASH_ADD(hh, ipc_cache_mapped[dev_id], key, keylen, cache_entry, MPL_MEM_OTHER);
        }
    }

    if (*ptr == NULL) {
        mpl_err = MPL_ze_ipc_handle_map(mpl_ipc_handle, is_shared_handle, dev_id, true, size, ptr);
        if (mpl_err != MPL_SUCCESS) {
            goto fn_fail;
        }

        if (likely(MPL_gpu_info.specialized_cache)) {
            cache_entry->mapped_ptr = *ptr;
            cache_entry->mapped_size = size;
            cache_entry->nfds = h.nfds;
            for (int i = 0; i < h.nfds; i++) {
                cache_entry->fds[i] = h.fds[1];
            }

            removal_entry =
                (MPL_ze_mapped_buffer_entry_t *) MPL_malloc(sizeof(MPL_ze_mapped_buffer_entry_t),
                                                            MPL_MEM_OTHER);
            if (removal_entry == NULL) {
                mpl_err = MPL_ERR_GPU_NOMEM;
                goto fn_fail;
            }
            memcpy(removal_entry, cache_entry, sizeof(MPL_ze_mapped_buffer_entry_t));

            HASH_ADD(hh, mmap_cache_removal[dev_id], mapped_ptr, sizeof(void *), removal_entry,
                     MPL_MEM_OTHER);
        }
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

/* this function takes a local device pointer and mmap to host */
int MPL_ze_mmap_device_pointer(void *dptr, MPL_gpu_device_attr * attr,
                               MPL_gpu_device_handle_t device, void **mmaped_ptr)
{
    ze_result_t ret;
    int mpl_err = MPL_SUCCESS;
    ze_ipc_mem_handle_t ze_ipc_handle[2];
    int fds[2], local_dev_id = -1;
    uint32_t nfds;
    uint64_t mem_id, offset, len;
    void *pbase, *base;
    MPL_ze_ipc_handle_entry_t *cache_entry = NULL;
    MPL_ze_mem_id_entry_t *memid_entry = NULL;

    ret = zeMemGetAddressRange(ze_context, dptr, &pbase, &len);
    ZE_ERR_CHECK(ret);

    offset = (char *) dptr - (char *) pbase;

    mem_id = attr->prop.id;
    local_dev_id = device_to_dev_id(device);
    if (local_dev_id == -1) {
        goto fn_fail;
    }

    /* First check if a removal from the cache is needed */
    if (likely(MPL_gpu_info.specialized_cache)) {
        HASH_FIND(hh, mem_id_cache, &pbase, sizeof(void *), memid_entry);

        if (memid_entry && memid_entry->mem_id != mem_id) {
            HASH_FIND(hh, ipc_cache_tracked[local_dev_id], &memid_entry->mem_id, sizeof(uint64_t),
                      cache_entry);
            if (cache_entry) {
                free_ipc_handle_cache(cache_entry);
                HASH_DELETE(hh, ipc_cache_tracked[local_dev_id], cache_entry);
                MPL_free(cache_entry);
                cache_entry = NULL;
            }

            HASH_DELETE(hh, mem_id_cache, memid_entry);
            MPL_free(memid_entry);
            memid_entry = NULL;
        }

        /* Search cache for mapped base address */
        HASH_FIND(hh, ipc_cache_tracked[local_dev_id], &mem_id, sizeof(uint64_t), cache_entry);
    }

    if (cache_entry && cache_entry->mapped_ptr) {
        base = cache_entry->mapped_ptr;
    } else {
        nfds = 0;       /* must be initialized to 0 */
        if (zexMemGetIpcHandles) {
            ret = zexMemGetIpcHandles(ze_context, pbase, &nfds, NULL);
            ZE_ERR_CHECK(ret);
            if (nfds) {
                assert(nfds <= 2);
                ret = zexMemGetIpcHandles(ze_context, pbase, &nfds, ze_ipc_handle);
            }
        }
        if (!nfds) {
            ret = zeMemGetIpcHandle(ze_context, pbase, &ze_ipc_handle[0]);
            nfds = 1;
        }
        ZE_ERR_CHECK(ret);

        for (int i = 0; i < nfds; i++)
            memcpy(&fds[i], &ze_ipc_handle[i], sizeof(int));
        mpl_err = mmapFunction(nfds, fds, len, &base);
        if (mpl_err != MPL_SUCCESS) {
            goto fn_fail;
        }

        if (likely(MPL_gpu_info.specialized_cache)) {
            if (cache_entry) {
                /* This could have been cached already, but via the ipc path */
                cache_entry->mapped_ptr = base;
                cache_entry->mapped_size = len;
                cache_entry->nfds = nfds;
                for (int i = 0; i < nfds; i++)
                    cache_entry->fds[i] = fds[i];
            } else {
                /* Insert into the cache */
                mpl_err = new_ipc_handle_cache(&cache_entry, mem_id);
                if (mpl_err != MPL_SUCCESS) {
                    goto fn_fail;
                }
                cache_entry->mapped_ptr = base;
                cache_entry->mapped_size = len;
                cache_entry->nfds = nfds;
                for (int i = 0; i < nfds; i++)
                    cache_entry->fds[i] = fds[i];
                HASH_ADD(hh, ipc_cache_tracked[local_dev_id], mem_id, sizeof(uint64_t), cache_entry,
                         MPL_MEM_OTHER);

                memid_entry = (MPL_ze_mem_id_entry_t *) MPL_malloc(sizeof(MPL_ze_mem_id_entry_t),
                                                                   MPL_MEM_OTHER);
                if (memid_entry == NULL) {
                    mpl_err = MPL_ERR_GPU_NOMEM;
                    goto fn_fail;
                }
                memset(memid_entry, 0, sizeof(MPL_ze_mem_id_entry_t));
                memid_entry->ptr = pbase;
                memid_entry->mem_id = mem_id;
                HASH_ADD(hh, mem_id_cache, ptr, sizeof(void *), memid_entry, MPL_MEM_OTHER);
            }
        }
    }

    *mmaped_ptr = (char *) base + offset;

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

int MPL_gpu_fast_memcpy(void *src, MPL_pointer_attr_t * src_attr, void *dest,
                        MPL_pointer_attr_t * dest_attr, size_t size)
{
    int mpl_err = MPL_SUCCESS;
    char *d = (char *) dest;
    const char *s = (const char *) src;
    size_t n = size;

    if (src_attr && src_attr->type == MPL_GPU_POINTER_DEV) {
        mpl_err =
            MPL_ze_mmap_device_pointer(src, &src_attr->device_attr, src_attr->device, (void **) &s);
        if (mpl_err != MPL_SUCCESS)
            goto fn_fail;
    }

    if (dest_attr && dest_attr->type == MPL_GPU_POINTER_DEV) {
        mpl_err =
            MPL_ze_mmap_device_pointer(dest, &dest_attr->device_attr, dest_attr->device,
                                       (void **) &d);
        if (mpl_err != MPL_SUCCESS)
            goto fn_fail;
    }
#if defined(MPL_HAVE_MM512_STOREU_SI512)
    while (n >= 64) {
        _mm512_storeu_si512((__m512i *) d, _mm512_loadu_si512((__m512i const *) s));
        d += 64;
        s += 64;
        n -= 64;
    }
    if (n & 32) {
        _mm256_storeu_si256((__m256i *) d, _mm256_loadu_si256((__m256i const *) s));
        d += 32;
        s += 32;
        n -= 32;
    }
#elif defined(MPL_HAVE_MM256_STOREU_SI256)
    while (n >= 32) {
        _mm256_storeu_si256((__m256i *) d, _mm256_loadu_si256((__m256i const *) s));
        d += 32;
        s += 32;
        n -= 32;
    }
#else
    goto fallback;
#endif
    if (n & 16) {
        _mm_storeu_si128((__m128i *) d, _mm_loadu_si128((__m128i const *) s));
        d += 16;
        s += 16;
        n -= 16;
    }
    if (n & 8) {
        *(int64_t *) d = *(int64_t *) s;
        d += 8;
        s += 8;
        n -= 8;
    }
    if (n & 4) {
        *(int *) d = *(int *) s;
        d += 4;
        s += 4;
        n -= 4;
    }
    if (n & 2) {
        *(int16_t *) d = *(int16_t *) s;
        d += 2;
        s += 2;
        n -= 2;
    }
    if (n == 1) {
        *(char *) d = *(char *) s;
    }
#if defined(MPL_HAVE_MM256_STOREU_SI256)
    _mm_sfence();
#endif
    goto fn_exit;

  fallback:
    memcpy(d, s, n);

  fn_exit:
    return mpl_err;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

#endif /* MPL_HAVE_ZE */
