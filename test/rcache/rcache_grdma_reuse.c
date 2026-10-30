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
 * Check that grdma does not reuse a cached registration after the
 * memory behind it was unmapped and mapped again at the same address,
 * and that the stale registration is released on the next
 * registration.  Runs with whichever memory component the memory
 * framework selects, and is skipped if none provides memory hooks.
 *
 * Fails if kdreg2 was built and its device can be opened but the
 * memory framework did not select kdreg2, unless MCA variables choose
 * the memory component or set kdreg2's priority.  Otherwise a kdreg2
 * component that cannot be loaded or opened would fall back to
 * patcher without notice.
 */

#include "opal_config.h"

#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/memory/memory.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/memoryhooks/memory.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_params.h"

#define SKIP 77

/* Makefile.am sets this to the memory components that configure built */
#if !defined(TEST_MEMORY_COMPONENTS)
#    define TEST_MEMORY_COMPONENTS ""
#endif

static int registered, deregistered;

static int test_register_mem(void *reg_data, void *base, size_t size,
                             mca_rcache_base_registration_t *reg)
{
    (void) reg_data;
    (void) base;
    (void) size;
    (void) reg;
    ++registered;
    return OPAL_SUCCESS;
}

static int test_deregister_mem(void *reg_data, mca_rcache_base_registration_t *reg)
{
    (void) reg_data;
    (void) reg;
    ++deregistered;
    return OPAL_SUCCESS;
}

static void *map(void *addr, size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | (NULL == addr ? 0 : MAP_FIXED);
    void *buf = mmap(addr, len, PROT_READ | PROT_WRITE, flags, -1, 0);

    if (MAP_FAILED == buf) {
        return NULL;
    }
    memset(buf, 1, len);
    return buf;
}

static int check(const char *what, bool ok)
{
    printf("%s: %s\n", ok ? "PASS" : "FAIL", what);
    return ok ? 0 : 1;
}

static bool have_memory_hooks(void)
{
    int support = opal_mem_hooks_support_level();

    return NULL != opal_memory->memoryc_valid
           || (OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)
                  == ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & support);
}

/* Must the memory framework select kdreg2?  Call this after the
   framework registers its variables and before it opens, which unloads
   the components that it does not select. */
static bool kdreg2_required(void)
{
    const char *selection = opal_memory_base_framework.framework_selection;
    mca_base_var_source_t source;
    int fd, vari;

    if (NULL == strstr(TEST_MEMORY_COMPONENTS, " kdreg2 ")) {
        return false;
    }
    if (NULL != selection && '\0' != selection[0]) {
        return false;
    }

    fd = open("/dev/kdreg2", O_RDWR | O_CLOEXEC);
    if (fd < 0) {
        return false;
    }
    close(fd);

    /* a kdreg2 component that was not found has no variables */
    vari = mca_base_var_find("opal", "memory", "kdreg2", "priority");
    if (vari < 0) {
        return true;
    }
    if (OPAL_SUCCESS != mca_base_var_get_value(vari, NULL, &source, NULL)) {
        return true;
    }
    return MCA_BASE_VAR_SOURCE_DEFAULT == source;
}

int main(int argc, char *argv[])
{
    static char cache_name[] = "rcache_grdma_reuse";
    mca_rcache_base_resources_t resources = {
        .cache_name = cache_name,
        .reg_data = NULL,
        .sizeof_reg = sizeof(mca_rcache_base_registration_t),
        .register_mem = test_register_mem,
        .deregister_mem = test_deregister_mem,
    };
    size_t len = 4 * (size_t) getpagesize();
    mca_rcache_base_registration_t *reg;
    mca_rcache_base_module_t *rcache;
    const char *component;
    void *buf, *other;
    bool need_kdreg2;
    int failures = 0, rc;

#if !defined(__linux__)
    /* patcher's mmap interception is only reliable on Linux.  On
       FreeBSD, for example, it falls back to syscall(2), which returns
       an int and truncates the mapped address. */
    printf("SKIP: only runs on Linux\n");
    return SKIP;
#endif

    if (OPAL_SUCCESS != opal_init(&argc, &argv)) {
        fprintf(stderr, "opal_init failed\n");
        return 1;
    }
    if (OPAL_SUCCESS
        != mca_base_framework_register(&opal_memory_base_framework, MCA_BASE_REGISTER_DEFAULT)) {
        fprintf(stderr, "cannot register the memory framework\n");
        return 1;
    }
    need_kdreg2 = kdreg2_required();

    rc = mca_base_framework_open(&opal_memory_base_framework, 0);
    component = opal_memory->memoryc_version.mca_component_name;
    printf("memory component: %s\n", '\0' == component[0] ? "none" : component);
    if (need_kdreg2 && 0 != strcmp("kdreg2", component)) {
        printf("FAIL: kdreg2 was built and /dev/kdreg2 can be opened, but the memory "
               "framework did not select kdreg2\n");
        opal_finalize();
        return 1;
    }
    if (OPAL_SUCCESS != rc || !have_memory_hooks()) {
        printf("SKIP: no memory hooks\n");
        opal_finalize();
        return SKIP;
    }

    if (OPAL_SUCCESS != mca_base_framework_open(&opal_rcache_base_framework, 0)) {
        fprintf(stderr, "cannot open the rcache framework\n");
        return 1;
    }

    /* cache registrations after they are released */
    opal_leave_pinned = 1;
    rcache = mca_rcache_base_module_create("grdma", NULL, &resources);
    if (NULL == rcache) {
        fprintf(stderr, "cannot create a grdma rcache\n");
        return 1;
    }

    buf = map(NULL, len);
    other = map(NULL, len);
    if (NULL == buf || NULL == other) {
        fprintf(stderr, "mmap failed\n");
        return 1;
    }

    (void) rcache->rcache_register(rcache, buf, len, 0, MCA_RCACHE_ACCESS_ANY, &reg);
    (void) rcache->rcache_deregister(rcache, reg);
    (void) rcache->rcache_register(rcache, buf, len, 0, MCA_RCACHE_ACCESS_ANY, &reg);
    (void) rcache->rcache_deregister(rcache, reg);
    failures += check("an unchanged region reuses its registration", 1 == registered);

    /* replace the mapping at the same address */
    munmap(buf, len);
    if (buf != map(buf, len)) {
        fprintf(stderr, "cannot map the same address again\n");
        return 1;
    }

    /* registering something else releases the stale registration */
    (void) rcache->rcache_register(rcache, other, len, 0, MCA_RCACHE_ACCESS_ANY, &reg);
    (void) rcache->rcache_deregister(rcache, reg);
    failures += check("a changed region's registration is released", 1 == deregistered);

    (void) rcache->rcache_register(rcache, buf, len, 0, MCA_RCACHE_ACCESS_ANY, &reg);
    (void) rcache->rcache_deregister(rcache, reg);
    failures += check("a changed region is registered again", 3 == registered);

    /* replace it once more, and look it up before anything else */
    munmap(buf, len);
    if (buf != map(buf, len)) {
        fprintf(stderr, "cannot map the same address again\n");
        return 1;
    }
    (void) rcache->rcache_register(rcache, buf, len, 0, MCA_RCACHE_ACCESS_ANY, &reg);
    (void) rcache->rcache_deregister(rcache, reg);
    failures += check("a lookup right after the change does not reuse it", 4 == registered);

    (void) mca_rcache_base_module_destroy(rcache);
    munmap(buf, len);
    munmap(other, len);
    (void) mca_base_framework_close(&opal_rcache_base_framework);
    (void) mca_base_framework_close(&opal_memory_base_framework);
    opal_finalize();

    return 0 == failures ? 0 : 1;
}
