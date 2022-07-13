/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/knem/smsc_knem_internal.h"
#include "opal/util/show_help.h"

#include <fcntl.h>
#include <stdio.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

static int mca_smsc_knem_component_register(void);
static int mca_smsc_knem_component_open(void);
static int mca_smsc_knem_component_close(void);
static int mca_smsc_knem_component_query(void);
static mca_smsc_module_t *mca_smsc_knem_component_enable(void);

static int mca_smsc_knem_fini(void);

#define MCA_SMSC_KNEM_DEFAULT_PRIORITY 23
static const int mca_smsc_knem_default_priority = MCA_SMSC_KNEM_DEFAULT_PRIORITY;

mca_smsc_knem_component_t mca_smsc_knem_component = {
    .super = {
        .smsc_version = {
            MCA_SMSC_DEFAULT_VERSION("knem"),
            .mca_open_component = mca_smsc_knem_component_open,
            .mca_close_component = mca_smsc_knem_component_close,
            .mca_register_component_params = mca_smsc_knem_component_register,
        },
        .priority = MCA_SMSC_KNEM_DEFAULT_PRIORITY,
        .query = mca_smsc_knem_component_query,
        .enable = mca_smsc_knem_component_enable,
    },
};

static int mca_smsc_knem_component_register(void)
{
    /* Currently disabling DMA mode by default; it's not clear that this is useful in all
     * applications and architectures. */
    mca_smsc_knem_component.dma_min = 0;
    (void) mca_base_component_var_register(
        &mca_smsc_knem_component.super.smsc_version, "dma_min",
        "Minimum message size (in bytes) to use the knem DMA mode; "
        "ignored if knem does not support DMA mode (0 = do not use the "
        "knem DMA mode, default: 0)",
        MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_smsc_knem_component.dma_min);

    mca_smsc_base_register_default_params(&mca_smsc_knem_component.super,
                                          mca_smsc_knem_default_priority);
    return OPAL_SUCCESS;
}

static int mca_smsc_knem_component_open(void)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

static int mca_smsc_knem_component_close(void)
{
    return mca_smsc_knem_fini();
}

static int mca_smsc_knem_get_info(struct knem_cmd_info *knem_info)
{
    /* Only show the help message if this is the only component. */
    bool show_help = (1 == opal_list_get_size(&opal_smsc_base_framework.framework_components));

    /* Check that the ABI if kernel module running is the same
     * as what we were compiled against. */
    memset(knem_info, 0, sizeof(*knem_info));
    int rc = ioctl(mca_smsc_knem_component.knem_fd, KNEM_CMD_GET_INFO, knem_info);
    if (rc < 0) {
        if (show_help) {
            opal_show_help("help-smsc-knem.txt", "knem get ABI fail", true,
                           opal_process_info.nodename, errno, strerror(errno));
        }
        return OPAL_ERR_NOT_AVAILABLE;
    }

    if (KNEM_ABI_VERSION != knem_info->abi) {
        if (show_help) {
            opal_show_help("help-smsc-knem.txt", "knem ABI mismatch", true,
                           opal_process_info.nodename, KNEM_ABI_VERSION, knem_info->abi);
        }
        return OPAL_ERR_NOT_AVAILABLE;
    }

    return OPAL_SUCCESS;
}
static int mca_smsc_knem_reg(void *reg_data, void *base, size_t size,
                             mca_rcache_base_registration_t *reg)
{
    mca_smsc_knem_registration_handle_t *knem_reg = (mca_smsc_knem_registration_handle_t *) reg;
    struct knem_cmd_create_region knem_cr;
    struct knem_cmd_param_iovec knem_iov;

    knem_iov.base = (uintptr_t) base;
    knem_iov.len = size;

    knem_cr.iovec_array = (uintptr_t) &knem_iov;
    knem_cr.iovec_nr = 1;
    knem_cr.protection = 0;

    if (reg->access_flags & (MCA_RCACHE_ACCESS_LOCAL_WRITE | MCA_RCACHE_ACCESS_REMOTE_WRITE)) {
        knem_cr.protection |= PROT_WRITE;
    }

    if (reg->access_flags & MCA_RCACHE_ACCESS_REMOTE_READ) {
        knem_cr.protection |= PROT_READ;
    }

    /* We will explicitly destroy this cookie. Do not use the single-use flag here. */
    knem_cr.flags = 0;
    if (OPAL_UNLIKELY(ioctl(mca_smsc_knem_component.knem_fd, KNEM_CMD_CREATE_REGION, &knem_cr)
                      < 0)) {
        return OPAL_ERROR;
    }

    knem_reg->data.cookie = knem_cr.cookie;
    knem_reg->data.base_addr = (intptr_t) base;

    return OPAL_SUCCESS;
}

static int mca_smsc_knem_dereg(void *reg_data, mca_rcache_base_registration_t *reg)
{
    mca_smsc_knem_registration_handle_t *knem_reg = (mca_smsc_knem_registration_handle_t *) reg;

    /* NTH: explicitly ignore the return code. Don't care about this cookie anymore anyway. */
    (void) ioctl(mca_smsc_knem_component.knem_fd, KNEM_CMD_DESTROY_REGION, &knem_reg->data.cookie);

    return OPAL_SUCCESS;
}

static int mca_smsc_knem_fini(void)
{
    if (-1 != mca_smsc_knem_component.knem_fd) {
        close(mca_smsc_knem_component.knem_fd);
        mca_smsc_knem_component.knem_fd = -1;
    }

    if (mca_smsc_knem_module.rcache) {
        (void) mca_rcache_base_module_destroy(mca_smsc_knem_module.rcache);
        mca_smsc_knem_module.rcache = NULL;
    }

    return OPAL_SUCCESS;
}

static int mca_smsc_knem_component_query(void)
{
    struct knem_cmd_info knem_info;
    int rc;

    /* Open the knem device.  Try to print a helpful message if we
       fail to open it. */
    mca_smsc_knem_component.knem_fd = open("/dev/knem", O_RDWR);
    if (mca_smsc_knem_component.knem_fd < 0) {
        if (EACCES == errno) {
            struct stat sbuf;
            if (0 != stat("/dev/knem", &sbuf)) {
                sbuf.st_mode = 0;
            }
            opal_show_help("help-smsc-knem.txt", "knem permission denied", true,
                           opal_process_info.nodename, sbuf.st_mode);
        } else {
            opal_show_help("help-smsc-knem.txt", "knem fail open", true, opal_process_info.nodename,
                           errno, strerror(errno));
        }

        return OPAL_ERR_NOT_AVAILABLE;
    }

    rc = mca_smsc_knem_get_info(&knem_info);
    if (OPAL_SUCCESS != rc) {
        mca_smsc_knem_fini();
        return rc;
    }

    if (!(mca_smsc_knem_component.dma_min && (knem_info.features & KNEM_FEATURE_DMA))) {
        /* disable DMA */
        mca_smsc_knem_component.dma_min = UINT_MAX;
    }

    return OPAL_SUCCESS;
}

static mca_smsc_module_t *mca_smsc_knem_component_enable(void)
{
    if (0 > mca_smsc_knem_component.super.priority) {
        return NULL;
    }

    mca_rcache_base_resources_t rcache_resources = {.cache_name = "smsc_knem",
                                                    .reg_data = NULL,
                                                    .sizeof_reg = sizeof(
                                                        mca_smsc_knem_registration_handle_t),
                                                    .register_mem = mca_smsc_knem_reg,
                                                    .deregister_mem = mca_smsc_knem_dereg};

    mca_smsc_knem_module.rcache = mca_rcache_base_module_create("grdma", NULL, &rcache_resources);
    if (NULL == mca_smsc_knem_module.rcache) {
        return NULL;
    }

    return &mca_smsc_knem_module.super;
}
