/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/*
 * Memory hooks from the UCX memory event layer (UCM).
 *
 * When the UCX libraries are loaded in a process, UCM patches the memory management functions of
 * libc (mmap, munmap, mremap, madvise, brk, ...). The memory patcher component would patch the
 * very same functions: it would overwrite the UCM hooks, and which of the two is installed last
 * then decides which one silently stops working. Instead of fighting for the functions, this
 * component uses the hooks UCM already installed and feeds its VM_UNMAPPED events into the OPAL
 * memory hooks (opal_mem_hooks_release_hook()), so that everything that depends on the OPAL
 * hooks (registration caches, ...) keeps working and the functions are patched only once.
 *
 * The component never links against the UCX libraries: it only looks for UCM in the process, and
 * declines to run if UCM is not loaded, has no hooks in libc functions, or is not delivering
 * events. In this case the other memory components (e.g., patcher) are used.
 */

#include "memory_ucm.h"

#include "opal/mca/memory/base/base.h"
#include "opal/mca/memory/base/empty.h"
#include "opal/memoryhooks/memory.h"
#include "opal/util/output.h"

#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <ucm/api/ucm.h>

static int memory_ucm_open(void);
static int memory_ucm_close(void);
static int memory_ucm_register(void);
static int memory_ucm_query(int *priority);

/* Above the patcher (80) and the kdreg2 component (90): when UCM has already hooked the libc
 * memory functions, this is the only component that can report every release of memory without
 * patching the same functions a second time. Keep it strictly above kdreg2 so that the selection
 * does not depend on the order of the components. */
static int mca_memory_ucm_priority = 91;

opal_memory_ucm_component_t mca_memory_ucm_component = {
    .super =
        {
            .memoryc_version =
                {
                    OPAL_MEMORY_BASE_VERSION_2_0_0,

                    /* Component name and version */
                    .mca_component_name = "ucm",
                    MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                          OPAL_RELEASE_VERSION),

                    /* Component open and close functions */
                    .mca_open_component = memory_ucm_open,
                    .mca_close_component = memory_ucm_close,
                    .mca_register_component_params = memory_ucm_register,
                },
            .memoryc_data = {/* The component is checkpoint ready */
                             MCA_BASE_METADATA_PARAM_CHECKPOINT},

            /* Memory framework functions. */
            .memoryc_query = memory_ucm_query,
            .memoryc_register = opal_memory_base_component_register_empty,
            .memoryc_deregister = opal_memory_base_component_deregister_empty,
            .memoryc_set_alignment = opal_memory_base_component_set_alignment_empty,
        },
};
MCA_BASE_COMPONENT_INIT(opal, memory, ucm)

/* The UCM entry points, resolved at run time */
typedef ucs_status_t (*memory_ucm_set_handler_fn_t)(int events, int priority,
                                                    ucm_event_callback_t cb, void *arg);
typedef void (*memory_ucm_unset_handler_fn_t)(int events, ucm_event_callback_t cb, void *arg);

static struct {
    void *library;
    memory_ucm_set_handler_fn_t set_handler;
    memory_ucm_unset_handler_fn_t unset_handler;
} memory_ucm;

/** an event handler is installed */
static bool memory_ucm_handler_installed = false;

static void memory_ucm_event_cb(ucm_event_type_t event_type, ucm_event_t *event, void *arg)
{
    (void) event_type;
    (void) arg;

    /* UCM calls the handlers before the memory is released, which is what the hooks expect */
    opal_mem_hooks_release_hook(event->vm_unmapped.address, event->vm_unmapped.size, true);
}

/**
 * Look a function up in the process (or in a library handle); ISO C does not allow to convert
 * the object pointer returned by dlsym() into a function pointer directly.
 */
static void memory_ucm_lookup(void *handle, const char *name, void *function_pointer)
{
    void *symbol = dlsym(handle, name);

    memcpy(function_pointer, &symbol, sizeof(symbol));
}

/**
 * Find the UCM entry points if the UCM library is already loaded in this process.
 */
static bool memory_ucm_resolve(void)
{
    if (NULL != memory_ucm.set_handler) {
        return true;
    }

    /* The common case: the library is a dependency of something that is loaded */
    memory_ucm_lookup(RTLD_DEFAULT, "ucm_set_event_handler", &memory_ucm.set_handler);
    memory_ucm_lookup(RTLD_DEFAULT, "ucm_unset_event_handler", &memory_ucm.unset_handler);

    if (NULL == memory_ucm.set_handler || NULL == memory_ucm.unset_handler) {
        /* It might be loaded, but not visible to the whole process. Never load it. */
        memory_ucm.library = dlopen("libucm.so.0", RTLD_LAZY | RTLD_NOLOAD);
        if (NULL != memory_ucm.library) {
            memory_ucm_lookup(memory_ucm.library, "ucm_set_event_handler",
                              &memory_ucm.set_handler);
            memory_ucm_lookup(memory_ucm.library, "ucm_unset_event_handler",
                              &memory_ucm.unset_handler);
        }
    }

    if (NULL == memory_ucm.set_handler || NULL == memory_ucm.unset_handler) {
        if (NULL != memory_ucm.library) {
            dlclose(memory_ucm.library);
            memory_ucm.library = NULL;
        }
        memory_ucm.set_handler = NULL;
        memory_ucm.unset_handler = NULL;
        return false;
    }

    return true;
}

/*
 * Check that UCM reports the release of memory the way the hooks need it. The test maps a page and
 * unmaps it through the munmap() of libc itself, resolved from the libc handle with dlsym(). This
 * does not depend on the allocator in use (a preloaded jemalloc or tcmalloc might retain or defer
 * the release of the memory that free() is given back), and a call made through the address
 * returned by dlsym() does not go through the relocation tables: the event is reported only if the
 * code of the libc function itself is hooked, which is what makes UCM and the memory patcher
 * compete.
 */
typedef int (*memory_ucm_munmap_fn_t)(void *address, size_t length);

static size_t memory_ucm_test_length = 0;
static void *memory_ucm_test_buffer = NULL;
static int memory_ucm_test_events = 0;

static void memory_ucm_test_cb(ucm_event_type_t event_type, ucm_event_t *event, void *arg)
{
    (void) event_type;
    (void) arg;

    if ((char *) event->vm_unmapped.address <= (char *) memory_ucm_test_buffer
        && (char *) memory_ucm_test_buffer + memory_ucm_test_length
               <= (char *) event->vm_unmapped.address + event->vm_unmapped.size) {
        ++memory_ucm_test_events;
    }
}

/**
 * Find the munmap() implemented by libc, as opposed to whatever the symbol resolves to in this
 * object. The libc is identified as the object that provides getpid(), a function that is not
 * hooked.
 * Returns NULL if it cannot be found; the caller must not guess in this case.
 */
static memory_ucm_munmap_fn_t memory_ucm_libc_munmap(void)
{
    memory_ucm_munmap_fn_t libc_munmap = NULL;
    Dl_info info;
    void *libc;

    if (0 == dladdr((void *) (uintptr_t) getpid, &info) || NULL == info.dli_fname) {
        return NULL;
    }

    /* Never load anything: libc is necessarily loaded already */
    libc = dlopen(info.dli_fname, RTLD_LAZY | RTLD_NOLOAD);
    if (NULL == libc) {
        return NULL;
    }
    memory_ucm_lookup(libc, "munmap", &libc_munmap);
    dlclose(libc);

    return libc_munmap;
}

static bool memory_ucm_events_work(void)
{
    memory_ucm_munmap_fn_t libc_munmap = memory_ucm_libc_munmap();
    long page_size = sysconf(_SC_PAGESIZE);
    int flags = MAP_PRIVATE;
    bool work = false;

    if (NULL == libc_munmap || 0 >= page_size) {
        return false;
    }
#if defined(MAP_ANONYMOUS)
    flags |= MAP_ANONYMOUS;
#elif defined(MAP_ANON)
    flags |= MAP_ANON;
#else
    return false;
#endif

    if (UCS_OK != memory_ucm.set_handler(UCM_EVENT_VM_UNMAPPED, 0, memory_ucm_test_cb, NULL)) {
        return false;
    }

    /* Never touched, so it costs nothing */
    memory_ucm_test_buffer = mmap(NULL, (size_t) page_size, PROT_NONE, flags, -1, 0);
    if (MAP_FAILED != memory_ucm_test_buffer) {
        memory_ucm_test_length = (size_t) page_size;
        memory_ucm_test_events = 0;
        libc_munmap(memory_ucm_test_buffer, memory_ucm_test_length);
        work = (0 < memory_ucm_test_events);
    }
    memory_ucm_test_buffer = NULL;

    memory_ucm.unset_handler(UCM_EVENT_VM_UNMAPPED, memory_ucm_test_cb, NULL);

    return work;
}

static int memory_ucm_register(void)
{
    mca_base_component_var_register(&mca_memory_ucm_component.super.memoryc_version, "priority",
                                    "Priority of the UCM memory hook component. It is used only "
                                    "if UCM is loaded in the process and its hooks are installed "
                                    "in the libc memory functions",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_CONSTANT, &mca_memory_ucm_priority);

    return OPAL_SUCCESS;
}

static int memory_ucm_query(int *priority)
{
    *priority = -1;

    if (!memory_ucm_resolve()) {
        return OPAL_SUCCESS;
    }

    if (!memory_ucm_events_work()) {
        opal_output_verbose(10, opal_memory_base_framework.framework_output,
                            "UCM is loaded but does not report unmapped memory through the libc "
                            "functions, not using it");
        return OPAL_SUCCESS;
    }

    *priority = mca_memory_ucm_priority;

    return OPAL_SUCCESS;
}

static int memory_ucm_open(void)
{
    if (memory_ucm_handler_installed) {
        return OPAL_SUCCESS;
    }

    if (!memory_ucm_resolve()
        || UCS_OK
               != memory_ucm.set_handler(UCM_EVENT_VM_UNMAPPED, 0, memory_ucm_event_cb, NULL)) {
        return OPAL_ERR_NOT_AVAILABLE;
    }
    memory_ucm_handler_installed = true;

    /* UCM reports the release of memory by free(), munmap(), mremap(), madvise(), brk(),
     * shmdt() and mmap() over an existing mapping */
    opal_mem_hooks_set_support(OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT);

    return OPAL_SUCCESS;
}

static int memory_ucm_close(void)
{
    if (memory_ucm_handler_installed) {
        memory_ucm.unset_handler(UCM_EVENT_VM_UNMAPPED, memory_ucm_event_cb, NULL);
        memory_ucm_handler_installed = false;
        /* Nothing reports the releases anymore. Only reset it if this component set it: close
         * is also called for components that were not selected. */
        opal_mem_hooks_set_support(0);
    }

    if (NULL != memory_ucm.library) {
        dlclose(memory_ucm.library);
        memory_ucm.library = NULL;
    }
    memory_ucm.set_handler = NULL;
    memory_ucm.unset_handler = NULL;

    return OPAL_SUCCESS;
}
