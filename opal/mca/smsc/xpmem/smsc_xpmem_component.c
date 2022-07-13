/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/xpmem/smsc_xpmem_internal.h"
#include "opal/util/minmax.h"

#include <fcntl.h>
#include <stdio.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

static int mca_smsc_xpmem_component_register(void);
static int mca_smsc_xpmem_component_open(void);
static int mca_smsc_xpmem_component_close(void);
static int mca_smsc_xpmem_component_query(void);
static mca_smsc_module_t *mca_smsc_xpmem_component_enable(void);

#define MCA_SMSC_XPMEM_DEFAULT_PRIORITY 42
static const int mca_smsc_xpmem_default_priority = MCA_SMSC_XPMEM_DEFAULT_PRIORITY;

mca_smsc_xpmem_component_t mca_smsc_xpmem_component = {
    .super = {
        .smsc_version = {
            MCA_SMSC_DEFAULT_VERSION("xpmem"),
            .mca_open_component = mca_smsc_xpmem_component_open,
            .mca_close_component = mca_smsc_xpmem_component_close,
            .mca_register_component_params = mca_smsc_xpmem_component_register,
        },
        .priority = MCA_SMSC_XPMEM_DEFAULT_PRIORITY,
        .query = mca_smsc_xpmem_component_query,
        .enable = mca_smsc_xpmem_component_enable,
    },
};

static int mca_smsc_xpmem_component_register(void)
{
    mca_smsc_xpmem_component.log_attach_align = 23;
    (void) mca_base_component_var_register(&mca_smsc_xpmem_component.super.smsc_version,
                                           "log_align",
                                           "Log base 2 of the alignment to use for xpmem "
                                           "segments (default: 23, minimum: 12, maximum: 25)",
                                           MCA_BASE_VAR_TYPE_INT, /*enumerator=*/NULL, /*bind=*/0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_smsc_xpmem_component.log_attach_align);

    mca_smsc_xpmem_component.memcpy_chunk_size = 262144;
    (void) mca_base_component_var_register(
        &mca_smsc_xpmem_component.super.smsc_version, "memcpy_chunk_size",
        "Maximum size to copy with a single call to memcpy. On some systems a smaller or larger "
        "number may provide better performance (default: 256k)",
        MCA_BASE_VAR_TYPE_UINT64_T, /*enumerator=*/NULL, /*bind=*/0, MCA_BASE_VAR_FLAG_SETTABLE,
        OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL, &mca_smsc_xpmem_component.memcpy_chunk_size);

    mca_smsc_base_register_default_params(&mca_smsc_xpmem_component.super,
                                          mca_smsc_xpmem_default_priority);
    return OPAL_SUCCESS;
}

static int mca_smsc_xpmem_component_open(void)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

static int mca_smsc_xpmem_component_close(void)
{
    if (mca_smsc_xpmem_module.vma_module) {
        OBJ_RELEASE(mca_smsc_xpmem_module.vma_module);
    }

    return OPAL_SUCCESS;
}

static int mca_smsc_xpmem_send_modex(void)
{
    mca_smsc_xpmem_modex_t modex;

    modex.seg_id = mca_smsc_xpmem_component.my_seg_id;
    modex.address_max = mca_smsc_xpmem_component.my_address_max;

    int rc;
    OPAL_MODEX_SEND(rc, PMIX_LOCAL, &mca_smsc_xpmem_component.super.smsc_version, &modex,
                    sizeof(modex));
    return rc;
}

static int mca_smsc_xpmem_component_query(void)
{
    /* Any attachment that goes past the Linux TASK_SIZE will always fail. To prevent this we need
     * to determine the value of TASK_SIZE. On x86_64 the value was hard-coded in sm to be
     * 0x7ffffffffffful but this approach does not work with AARCH64 (and possibly other
     * architectures). Since there is really no way to directly determine the value we can (in all
     * cases?) look through the mapping for this process to determine what the largest address is.
     * This should be the top of the stack. No heap allocations should be larger than this value.
     * Since the largest address may differ between processes the value must be shared as part of
     * the modex and stored in the endpoint. */
    FILE *fh = fopen("/proc/self/maps", "r");
    if (NULL == fh) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, opal_smsc_base_framework.framework_output,
                            "mca_smsc_xpmem_component_query: could not open /proc/self/maps for "
                            "reading. disabling XPMEM");
        return OPAL_ERR_NOT_AVAILABLE;
    }

    char buffer[1024];
    uintptr_t address_max = 0;
    while (fgets(buffer, sizeof(buffer), fh)) {
        uintptr_t low, high;
        char *tmp;
        /* each line of /proc/self/maps starts with low-high in hexadecimal (without a 0x) */
        low = strtoul(buffer, &tmp, 16);
        high = strtoul(tmp + 1, NULL, 16);
        if (address_max < high) {
            address_max = high;
        }
    }

    fclose(fh);

    if (0 == address_max) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, opal_smsc_base_framework.framework_output,
                            "mca_smsc_xpmem_component_query: could not determine the address max");
        return OPAL_ERR_NOT_AVAILABLE;
    }

    /* save the calculated maximum */
    mca_smsc_xpmem_component.my_address_max = address_max - 1;

    /* it is safe to use XPMEM_MAXADDR_SIZE here (which is always (size_t)-1 even though
     * it is not safe for attach */
    mca_smsc_xpmem_component.my_seg_id = xpmem_make(0, XPMEM_MAXADDR_SIZE, XPMEM_PERMIT_MODE,
                                                    (void *) 0666);
    if (-1 == mca_smsc_xpmem_component.my_seg_id) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    mca_smsc_xpmem_send_modex();

    return OPAL_SUCCESS;
}

static mca_smsc_module_t *mca_smsc_xpmem_component_enable(void)
{
    if (0 > mca_smsc_xpmem_component.super.priority) {
        return NULL;
    }

    /* limit segment alignment to be between 4k and 16M */
    mca_smsc_xpmem_component.log_attach_align
        = opal_min(opal_max(mca_smsc_xpmem_component.log_attach_align, 12), 25);

    mca_smsc_xpmem_module.vma_module = mca_rcache_base_vma_module_alloc();

    return &mca_smsc_xpmem_module.super;
}
