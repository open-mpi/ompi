/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/*
 * kdreg2 is a kernel module that watches the virtual address ranges a
 * process tells it about, and reports when their mappings change.
 * grdma tells this component about each cached registration through
 * memoryc_register.  The kernel keeps a generation number for every
 * watched region in memory that is mapped read-only into the process,
 * so memoryc_valid can check a registration before it is reused with
 * no system call.  The kernel also queues an event for each changed
 * region; memoryc_process reads the queued events on the calling
 * thread and passes them to the memory hooks callbacks, which evict
 * the stale registrations.
 *
 * Unlike patcher, this component does not intercept munmap or free,
 * so it does not report every release of memory, and it sets no memory
 * hooks support level.  rcache recognizes it by memoryc_valid.
 */

#include "opal_config.h"

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

/* AC_CHECK_HEADERS in configure.m4 defines HAVE_KDREG2_H if it found
   kdreg2.h, and HAVE_LINUX_KDREG2_H if it found only linux/kdreg2.h */
#if defined(HAVE_KDREG2_H)
#    include <kdreg2.h>
#else
#    include <linux/kdreg2.h>
#endif

#include "memory_kdreg2.h"

#include "opal/class/opal_hash_table.h"
#include "opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/memory/base/empty.h"
#include "opal/mca/threads/mutex.h"
#include "opal/memoryhooks/memory_internal.h"
#include "opal/runtime/opal.h"
#include "opal/sys/atomic.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

static int kdreg2_register(void);
static int kdreg2_open(void);
static int kdreg2_close(void);
static int kdreg2_query(int *priority);
static int kdreg2_process(void);
static int kdreg2_monitor(void *base, size_t len, uint64_t cookie);
static int kdreg2_unmonitor(void *base, size_t len, uint64_t cookie);
static bool kdreg2_valid(void *base, size_t len, uint64_t cookie);

opal_memory_kdreg2_component_t mca_memory_kdreg2_component = {
    .super =
        {
            .memoryc_version =
                {
                    OPAL_MEMORY_BASE_VERSION_2_0_0,

                    /* Component name and version */
                    .mca_component_name = "kdreg2",
                    MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                          OPAL_RELEASE_VERSION),

                    /* Component open and close functions */
                    .mca_open_component = kdreg2_open,
                    .mca_close_component = kdreg2_close,
                    .mca_register_component_params = kdreg2_register,
                },
            .memoryc_data =
                {/* The component is checkpoint ready */
                 MCA_BASE_METADATA_PARAM_CHECKPOINT},

            /* Memory framework functions. */
            .memoryc_query = kdreg2_query,
            .memoryc_process = kdreg2_process,
            .memoryc_register = kdreg2_monitor,
            .memoryc_deregister = kdreg2_unmonitor,
            .memoryc_set_alignment = opal_memory_base_component_set_alignment_empty,
            .memoryc_valid = kdreg2_valid,
        },
};
MCA_BASE_COMPONENT_INIT(opal, memory, kdreg2)

static int kdreg2_priority;
static int kdreg2_max_regions;

static struct {
    /* kdreg2 device, or -1 if the component is not open */
    int fd;
    /* read-only kernel status, including each region's generation */
    const struct kdreg2_status_data *status_data;
    /* cookie -> struct kdreg2_monitoring_params * */
    opal_hash_table_t regions;
    opal_mutex_t lock;
    /* set while one thread reads events */
    opal_atomic_int32_t reading;
    /* do not trust the generations any more */
    bool failed;
    bool full_reported;
} kdreg2 = {.fd = -1};

/* The child of a fork() cannot use the parent's kdreg2 context */
static volatile bool kdreg2_forked = false;

static void kdreg2_atfork_child(void)
{
    kdreg2_forked = true;
}

static int kdreg2_register(void)
{
    /* patcher's default priority is 80; a higher value makes kdreg2 the
       default when its device can be opened */
    kdreg2_priority = 90;
    (void) mca_base_component_var_register(&mca_memory_kdreg2_component.super.memoryc_version,
                                           "priority",
                                           "Priority of the kdreg2 memory hook component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_CONSTANT, &kdreg2_priority);

    kdreg2_max_regions = 16384;
    (void) mca_base_component_var_register(
        &mca_memory_kdreg2_component.super.memoryc_version, "max_regions",
        "Maximum number of registered memory regions that kdreg2 watches at once.  Open MPI "
        "does not reuse registrations beyond this number.  The kernel reserves memory for "
        "this many regions in each process.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
        &kdreg2_max_regions);

    return OPAL_SUCCESS;
}

static int kdreg2_query(int *priority)
{
    /* kdreg2_open checks the device, and the base falls back to the
       next component if it fails */
    *priority = kdreg2_priority;
    return OPAL_SUCCESS;
}

static int kdreg2_device_error(const char *operation, int err)
{
    opal_show_help("help-opal-memory-kdreg2.txt", "device error", true, opal_gethostname(),
                   KDREG2_DEVICE_NAME, operation, strerror(err));
    return OPAL_ERR_NOT_AVAILABLE;
}

static int kdreg2_open(void)
{
    struct kdreg2_config_data config = {.max_regions = 0};
    static bool atfork_registered = false;
    int err;

    if (kdreg2.fd >= 0) {
        return OPAL_SUCCESS;
    }

    if (kdreg2_max_regions <= 0) {
        opal_show_help("help-opal-memory-kdreg2.txt", "bad max regions", true,
                       opal_gethostname(), kdreg2_max_regions);
        return OPAL_ERR_BAD_PARAM;
    }

    kdreg2.fd = open(KDREG2_DEVICE_NAME, O_RDWR | O_CLOEXEC | O_NONBLOCK);
    if (kdreg2.fd < 0) {
        err = errno;
        if (ENOENT == err || ENODEV == err || ENXIO == err) {
            /* the module is not loaded: fall back quietly */
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
                                opal_memory_base_framework.framework_output,
                                "memory:kdreg2: cannot open %s: %s", KDREG2_DEVICE_NAME,
                                strerror(err));
            return OPAL_ERR_NOT_AVAILABLE;
        }
        return kdreg2_device_error("open", err);
    }

    config.max_regions = (size_t) kdreg2_max_regions;
    if (0 != ioctl(kdreg2.fd, KDREG2_IOCTL_CONFIG_DATA, &config)) {
        err = errno;
        close(kdreg2.fd);
        kdreg2.fd = -1;
        return kdreg2_device_error("KDREG2_IOCTL_CONFIG_DATA", err);
    }
    kdreg2.status_data = config.status_data;

    OBJ_CONSTRUCT(&kdreg2.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&kdreg2.regions, opal_hash_table_t);
    if (OPAL_SUCCESS != opal_hash_table_init(&kdreg2.regions, 1024)) {
        OBJ_DESTRUCT(&kdreg2.regions);
        OBJ_DESTRUCT(&kdreg2.lock);
        close(kdreg2.fd);
        kdreg2.fd = -1;
        kdreg2.status_data = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (!atfork_registered) {
        (void) pthread_atfork(NULL, NULL, kdreg2_atfork_child);
        atfork_registered = true;
    }

    kdreg2.reading = 0;
    kdreg2.failed = false;
    kdreg2.full_reported = false;

    opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, opal_memory_base_framework.framework_output,
                        "memory:kdreg2: watching up to %d regions", kdreg2_max_regions);
    return OPAL_SUCCESS;
}

static int kdreg2_close(void)
{
    uint64_t key;
    void *value, *node = NULL;

    if (kdreg2.fd < 0) {
        return OPAL_SUCCESS;
    }

    /* closing the device stops watching every region */
    close(kdreg2.fd);
    kdreg2.fd = -1;
    kdreg2.status_data = NULL;

    if (OPAL_SUCCESS == opal_hash_table_get_first_key_uint64(&kdreg2.regions, &key, &value, &node)) {
        do {
            free(value);
        } while (OPAL_SUCCESS
                 == opal_hash_table_get_next_key_uint64(&kdreg2.regions, &key, &value, node,
                                                        &node));
    }
    OBJ_DESTRUCT(&kdreg2.regions);
    OBJ_DESTRUCT(&kdreg2.lock);

    return OPAL_SUCCESS;
}

static int kdreg2_monitor(void *base, size_t len, uint64_t cookie)
{
    struct kdreg2_ioctl_monitor arg = {.addr = base, .length = len, .cookie = cookie};
    struct kdreg2_monitoring_params *params, *old = NULL;
    bool report_full = false;
    int rc = OPAL_SUCCESS, err = 0;

    if (kdreg2.fd < 0 || kdreg2_forked) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    params = malloc(sizeof(*params));
    if (NULL == params) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    OPAL_THREAD_LOCK(&kdreg2.lock);
    if (0 != ioctl(kdreg2.fd, KDREG2_IOCTL_MONITOR, &arg)) {
        err = errno;
        rc = OPAL_ERROR;
        report_full = ENOSPC == err && !kdreg2.full_reported;
        kdreg2.full_reported |= report_full;
    } else {
        *params = arg.monitoring_params;
        (void) opal_hash_table_get_value_uint64(&kdreg2.regions, cookie, (void **) &old);
        rc = opal_hash_table_set_value_uint64(&kdreg2.regions, cookie, params);
        if (OPAL_SUCCESS != rc) {
            struct kdreg2_ioctl_unmonitor unarg = {.cookie = cookie,
                                                   .monitoring_params = arg.monitoring_params};
            (void) ioctl(kdreg2.fd, KDREG2_IOCTL_UNMONITOR, &unarg);
            old = NULL;
        }
    }
    OPAL_THREAD_UNLOCK(&kdreg2.lock);

    free(old);
    if (OPAL_SUCCESS != rc) {
        free(params);
        if (report_full) {
            opal_show_help("help-opal-memory-kdreg2.txt", "too many regions", true,
                           opal_gethostname(), kdreg2_max_regions);
        } else if (0 != err) {
            opal_output_verbose(MCA_BASE_VERBOSE_INFO,
                                opal_memory_base_framework.framework_output,
                                "memory:kdreg2: cannot watch %p (%zu bytes): %s", base, len,
                                strerror(err));
        }
    }
    return rc;
}

static int kdreg2_unmonitor(void *base, size_t len, uint64_t cookie)
{
    struct kdreg2_monitoring_params *params = NULL;

    /* the child of a fork cannot use the device, and must not take a lock
       that another thread of the parent may have held at the fork */
    if (kdreg2.fd < 0 || kdreg2_forked) {
        return OPAL_SUCCESS;
    }

    OPAL_THREAD_LOCK(&kdreg2.lock);
    if (OPAL_SUCCESS == opal_hash_table_get_value_uint64(&kdreg2.regions, cookie, (void **) &params)) {
        /* the kernel stops watching a region by itself when its mapping
           changes */
        if (!kdreg2_mapping_changed(kdreg2.status_data, params)) {
            struct kdreg2_ioctl_unmonitor arg = {.cookie = cookie, .monitoring_params = *params};
            (void) ioctl(kdreg2.fd, KDREG2_IOCTL_UNMONITOR, &arg);
        }
        (void) opal_hash_table_remove_value_uint64(&kdreg2.regions, cookie);
    }
    OPAL_THREAD_UNLOCK(&kdreg2.lock);

    free(params);
    return OPAL_SUCCESS;
}

static bool kdreg2_valid(void *base, size_t len, uint64_t cookie)
{
    struct kdreg2_monitoring_params *params;
    bool valid = false;

    if (kdreg2.fd < 0 || kdreg2_forked || kdreg2.failed) {
        return false;
    }

    OPAL_THREAD_LOCK(&kdreg2.lock);
    if (OPAL_SUCCESS == opal_hash_table_get_value_uint64(&kdreg2.regions, cookie, (void **) &params)) {
        valid = !kdreg2_mapping_changed(kdreg2.status_data, params);
    }
    OPAL_THREAD_UNLOCK(&kdreg2.lock);

    return valid;
}

static int kdreg2_process(void)
{
    struct kdreg2_event event;
    int32_t idle = 0;
    ssize_t bytes;

    if (kdreg2.fd < 0 || kdreg2_forked
        || 0 == kdreg2_read_counter(&kdreg2.status_data->pending_events)) {
        return OPAL_SUCCESS;
    }

    /* if another thread is reading the events, let it */
    if (!opal_atomic_compare_exchange_strong_32(&kdreg2.reading, &idle, 1)) {
        return OPAL_SUCCESS;
    }

    while (1) {
        int err = 0;

        bytes = read(kdreg2.fd, &event, sizeof(event));
        if (bytes < 0) {
            err = errno;
            if (EINTR == err) {
                continue;
            }
            if (EAGAIN == err || EWOULDBLOCK == err) {
                break;
            }
        } else if (sizeof(event) != (size_t) bytes) {
            err = EIO;
        }

        if (0 != err) {
            /* events may have been lost, so stop trusting the cache */
            if (!kdreg2.failed) {
                kdreg2.failed = true;
                opal_show_help("help-opal-memory-kdreg2.txt", "read error", true,
                               opal_gethostname(), strerror(err));
            }
            break;
        }

        if (KDREG2_EVENT_MAPPING_CHANGE == event.type) {
            opal_mem_hooks_release_hook((void *) event.u.mapping_change.addr,
                                        event.u.mapping_change.len, false);
        }
    }

    (void) opal_atomic_swap_32(&kdreg2.reading, 0);
    return OPAL_SUCCESS;
}
