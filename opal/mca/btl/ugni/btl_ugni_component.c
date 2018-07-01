/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2017      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_rdma.h"
#include "btl_ugni_smsg.h"

#include "opal/util/sys_limits.h"

#include <stdlib.h>
#include <fcntl.h>
#include <ctype.h>

#include "opal/memoryhooks/memory.h"
#include "opal/runtime/opal_params.h"

#include "opal/mca/base/mca_base_pvar.h"

static int btl_ugni_component_register(void);
static int btl_ugni_component_open(void);
static int btl_ugni_component_close(void);
static mca_btl_base_module_t **mca_btl_ugni_component_init(int *, bool, bool);
static int mca_btl_ugni_component_progress(void);
static unsigned long mca_btl_ugni_ugni_page_size = 0;

mca_btl_ugni_component_t mca_btl_ugni_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        .btl_version = {
            MCA_BTL_DEFAULT_VERSION("ugni"),
            .mca_open_component = btl_ugni_component_open,
            .mca_close_component = btl_ugni_component_close,
            .mca_register_component_params = btl_ugni_component_register,
        },
        .btl_data = {
            .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        .btl_init = mca_btl_ugni_component_init,
        .btl_progress = mca_btl_ugni_component_progress,
    }
};

mca_base_var_enum_value_t rcache_values[] = {
    {MCA_BTL_UGNI_RCACHE_UDREG, "udreg"},
    {MCA_BTL_UGNI_RCACHE_GRDMA, "grdma"},
    {-1, NULL} /* sentinal */
};

mca_base_var_enum_value_flag_t cdm_flags[] = {
    {.flag = GNI_CDM_MODE_FORK_NOCOPY, .string = "fork-no-copy", .conflicting_flag = GNI_CDM_MODE_FORK_FULLCOPY | GNI_CDM_MODE_FORK_PARTCOPY},
    {.flag = GNI_CDM_MODE_FORK_FULLCOPY, .string = "fork-full-copy", .conflicting_flag = GNI_CDM_MODE_FORK_NOCOPY | GNI_CDM_MODE_FORK_PARTCOPY},
    {.flag = GNI_CDM_MODE_FORK_PARTCOPY, .string = "fork-part-copy", .conflicting_flag = GNI_CDM_MODE_FORK_NOCOPY | GNI_CDM_MODE_FORK_FULLCOPY},
    {.flag = GNI_CDM_MODE_ERR_NO_KILL, .string = "err-no-kill", .conflicting_flag = GNI_CDM_MODE_ERR_ALL_KILL},
    {.flag = GNI_CDM_MODE_ERR_ALL_KILL, .string = "err-all-kill", .conflicting_flag = GNI_CDM_MODE_ERR_NO_KILL},
    {.flag = GNI_CDM_MODE_FAST_DATAGRAM_POLL, .string = "fast-datagram-poll", .conflicting_flag = 0},
    {.flag = GNI_CDM_MODE_BTE_SINGLE_CHANNEL, .string = "bte-single-channel", .conflicting_flag = 0},
    {.flag = GNI_CDM_MODE_USE_PCI_IOMMU, .string = "use-pci-iommu", .conflicting_flag = 0},
    {.flag = GNI_CDM_MODE_MDD_DEDICATED, .string = "mdd-dedicated", .conflicting_flag = GNI_CDM_MODE_MDD_SHARED},
    {.flag = GNI_CDM_MODE_MDD_SHARED, .string = "mdd-shared", .conflicting_flag = GNI_CDM_MODE_MDD_DEDICATED},
    {.flag = GNI_CDM_MODE_FMA_DEDICATED, .string = "fma-dedicated", .conflicting_flag = GNI_CDM_MODE_FMA_SHARED},
    {.flag = GNI_CDM_MODE_FMA_SHARED, .string = "fma-shared", .conflicting_flag = GNI_CDM_MODE_FMA_DEDICATED},
    {.flag = GNI_CDM_MODE_CACHED_AMO_ENABLED, .string = "cached-amo-enabled", .conflicting_flag = 0},
    {.flag = GNI_CDM_MODE_CQ_NIC_LOCAL_PLACEMENT, .string = "cq-nic-placement", .conflicting_flag = 0},
    {.flag = GNI_CDM_MODE_FMA_SMALL_WINDOW, .string = "fma-small-window", .conflicting_flag = 0},
    {.string = NULL}
};

static inline int mca_btl_ugni_get_stat (const mca_base_pvar_t *pvar, void *value, void *obj)
{
    gni_statistic_t statistic = (gni_statistic_t) (intptr_t) pvar->ctx;
    gni_return_t rc = GNI_RC_SUCCESS;

    for (int i = 0 ; i < mca_btl_ugni_component.virtual_device_count ; ++i) {
        rc = GNI_GetNicStat (mca_btl_ugni_component.modules[0].devices[i].dev_handle, statistic,
                             ((unsigned int *) value) + i);
    }

    return mca_btl_rc_ugni_to_opal (rc);
}

static inline int mca_btl_ugni_notify_stat (mca_base_pvar_t *pvar, mca_base_pvar_event_t event, void *obj, int *count)
{
    if (MCA_BASE_PVAR_HANDLE_BIND == event) {
        /* one value for each virtual device handle */
        *count = mca_btl_ugni_component.virtual_device_count;
    }

    return OPAL_SUCCESS;
}

static int btl_ugni_component_register(void)
{
    mca_base_var_enum_t *new_enum;
    gni_nic_device_t device_type;
    char *mpool_hints_tmp = NULL;
    int rc;

    (void) mca_base_var_group_component_register(&mca_btl_ugni_component.super.btl_version,
                                                 "uGNI byte transport layer");

    mca_btl_ugni_component.ugni_free_list_num = 8;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "free_list_num", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_free_list_num);
    mca_btl_ugni_component.ugni_free_list_max = 4096;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "free_list_max", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_free_list_max);
    mca_btl_ugni_component.ugni_free_list_inc = 64;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "free_list_inc", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_free_list_inc);

    mca_btl_ugni_component.ugni_eager_num = 16;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "eager_num", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_eager_num);
    mca_btl_ugni_component.ugni_eager_max = 128;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "eager_max", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_eager_max);
    mca_btl_ugni_component.ugni_eager_inc = 16;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "eager_inc", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_eager_inc);

    mca_btl_ugni_component.remote_cq_size = 40000;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "remote_cq_size", "Remote SMSG completion queue "
                                           "size (default 40000)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.remote_cq_size);

    mca_btl_ugni_component.local_cq_size = 8192;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "local_cq_size", "Local SMSG completion queue size "
                                           "(default 8k)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.local_cq_size);

    mca_btl_ugni_component.local_rdma_cq_size = 1024;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "local_rdma_cq_size", "Local FMA/RDMA completion queue size "
                                           "(default: 1024)",MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.local_rdma_cq_size);

    mca_btl_ugni_component.ugni_smsg_limit = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "smsg_limit", "Maximum size message that "
                                           "will be sent using the SMSG/MSGQ protocol "
                                           "(0 - autoselect(default), 16k max)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_smsg_limit);

    mca_btl_ugni_component.smsg_max_credits = 32;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "smsg_max_credits", "Maximum number of "
                                           "outstanding SMSG/MSGQ message (default 32)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.smsg_max_credits);

#if OPAL_C_HAVE__THREAD_LOCAL
    mca_btl_ugni_component.bind_threads_to_devices = true;

    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "bind_devices", "Bind threads to virtual "
                                           "devices. In general this should improve "
                                           "RDMA performance (default: true)",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.bind_threads_to_devices);
#endif

    mca_btl_ugni_component.ugni_fma_limit = -1;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "fma_limit", "Default maximum size message that "
                                           "will be sent using the FMA (Fast Memory "
                                           "Access) protocol (default: -1 (don't use), 64k max)",
                                           MCA_BASE_VAR_TYPE_LONG, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE | MCA_BASE_VAR_FLAG_DEPRECATED,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_fma_limit);

    mca_btl_ugni_component.ugni_fma_get_limit = 2048;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "fma_get_limit", "Maximum size message that "
                                           "will be sent using the FMA (Fast Memory "
                                           "Access) protocol for get (default 2k, "
                                           "64k max)",
                                           MCA_BASE_VAR_TYPE_LONG, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_fma_get_limit);

    mca_btl_ugni_component.ugni_fma_put_limit = 4096;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "fma_put_limit", "Maximum size message that "
                                           "will be sent using the FMA (Fast Memory "
                                           "Access) protocol for put (default: 4k, "
                                           "64k max)",
                                           MCA_BASE_VAR_TYPE_LONG, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_fma_put_limit);

    mca_btl_ugni_component.rdma_max_retries = 16;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "rdma_max_retries", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.rdma_max_retries);

    mca_btl_ugni_component.smsg_max_retries = 16;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "smsg_max_retries", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.smsg_max_retries);

    mca_btl_ugni_component.max_mem_reg = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "max_mem_reg", "Maximum number of "
                                           "memory registrations a process can "
                                           "hold (0 - autoselect, -1 - unlimited)"
                                           " (default 0)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.max_mem_reg);

    mca_btl_ugni_component.mbox_increment = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "mbox_inc", "Number of SMSG mailboxes to "
                                           "allocate in each block (0 - autoselect(default))",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_ugni_component.mbox_increment);

    /* communication domain flags */
    rc = mca_base_var_enum_create_flag ("btl_ugni_cdm_flags", cdm_flags, (mca_base_var_enum_flag_t **) &new_enum);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    mca_btl_ugni_component.cdm_flags = GNI_CDM_MODE_FORK_PARTCOPY | GNI_CDM_MODE_ERR_NO_KILL | GNI_CDM_MODE_FAST_DATAGRAM_POLL |
        GNI_CDM_MODE_MDD_SHARED | GNI_CDM_MODE_FMA_SHARED | GNI_CDM_MODE_FMA_SMALL_WINDOW;
    mca_btl_ugni_component.cdm_flags_id = mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "cdm_flags", "Flags to set when creating a communication domain "
                                           " (default: fork-full-copy,cached-amo-enabled,err-no-kill,fast-datagram-poll,"
                                           "fma-shared,fma-small-window)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, new_enum, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_ugni_component.cdm_flags);
    OBJ_RELEASE(new_enum);

    mca_btl_ugni_component.virtual_device_count = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "virtual_device_count", "Number of virtual devices to create. Higher numbers may "
                                           "result in better performance when using threads. (default: 0 (auto), max: 128)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_ugni_component.virtual_device_count);

    /* determine if there are get alignment restrictions */
    GNI_GetDeviceType (&device_type);


    mca_btl_ugni_component.smsg_page_size = 2 << 20;
    if (GNI_DEVICE_GEMINI == device_type) {
        if (access ("/sys/class/gemini/ghal0/mrt", R_OK)) {
            int fd = open ("/sys/class/gemini/ghal0/mrt", O_RDONLY);
            char buffer[10];

            if (0 <= fd) {
                memset (buffer, 0, sizeof (buffer));
                read (fd, buffer, sizeof (buffer) - 1);
                close (fd);
                mca_btl_ugni_ugni_page_size = strtol (buffer, NULL, 10) * 1024;
                mca_btl_ugni_component.smsg_page_size = mca_btl_ugni_ugni_page_size;
            }
        }
    }

    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "smsg_page_size", "Page size to use for SMSG mailbox allocation (default: detect)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_ugni_component.smsg_page_size);

    mca_btl_ugni_component.progress_thread_requested = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "request_progress_thread",
                                           "Enable to request ugni btl progress thread - requires MPI_THREAD_MULTIPLE support",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.progress_thread_requested);

    /* performance variables */
    mca_btl_ugni_progress_thread_wakeups = 0;
    (void) mca_base_component_pvar_register(&mca_btl_ugni_component.super.btl_version,
                                            "progress_thread_wakeups", "Number of times the progress thread "
                                            "has been woken", OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_COUNTER,
                                            MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS, NULL,
                                            NULL, NULL, &mca_btl_ugni_progress_thread_wakeups);

    /* register network statistics as performance variables */
    for (int i = 0 ; i < GNI_NUM_STATS ; ++i) {
        char name[128], desc[128];
        size_t str_len = strlen (gni_statistic_str[i]);

        assert (str_len < sizeof (name));

        /* we can get an all-caps string for the variable from gni_statistic_str. need to make it lowercase
         * to match ompi standards */
        for (size_t j = 0 ; j < str_len ; ++j) {
            name[j] = tolower (gni_statistic_str[i][j]);
            desc[j] = ('_' == name[j]) ? ' ' : name[j];
        }

        name[str_len] = '\0';
        desc[str_len] = '\0';

        (void) mca_base_component_pvar_register (&mca_btl_ugni_component.super.btl_version, name, desc,
                                                 OPAL_INFO_LVL_4, MCA_BASE_PVAR_CLASS_COUNTER,
                                                 MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                                 MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                                 mca_btl_ugni_get_stat, NULL, mca_btl_ugni_notify_stat,
                                                 (void *) (intptr_t) i);
    }

    /* btl/ugni can only support only a fixed set of rcache components (these rcache components have compatible resource
     * structures) */
    rc = mca_base_var_enum_create ("btl_ugni_rcache", rcache_values, &new_enum);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* NTH: there are known *serious* performance issues with udreg. if they are ever resolved it is the preferred rcache */
    mca_btl_ugni_component.rcache_type = MCA_BTL_UGNI_RCACHE_GRDMA;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "rcache", "registration cache to use (default: grdma)", MCA_BASE_VAR_TYPE_INT, new_enum,
                                           0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_ugni_component.rcache_type);
    OBJ_RELEASE(new_enum);

    if (mca_btl_ugni_ugni_page_size) {
        rc = asprintf (&mpool_hints_tmp, "page_size=%lu", mca_btl_ugni_ugni_page_size);
        if (rc < 0) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        mca_btl_ugni_component.mpool_hints = mpool_hints_tmp;
    } else {
        mca_btl_ugni_component.mpool_hints = "page_size=2M";
    }

    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "mpool_hints", "hints to use when selecting a memory pool (default: "
                                           "\"page_size=2M\")", MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_ugni_component.mpool_hints);
    free (mpool_hints_tmp);

    /* ensure we loose send exclusivity to sm and vader if they are enabled */
    mca_btl_ugni_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH - 2;

    /* smsg threshold */
    mca_btl_ugni_module.super.btl_eager_limit               = 8 * 1024;
    mca_btl_ugni_module.super.btl_rndv_eager_limit          = 8 * 1024;
    mca_btl_ugni_module.super.btl_rdma_pipeline_frag_size   = 4 * 1024 * 1024;
    mca_btl_ugni_module.super.btl_max_send_size             = 8 * 1024;
    mca_btl_ugni_module.super.btl_rdma_pipeline_send_length = 8 * 1024;

    mca_btl_ugni_module.super.btl_get_limit = 1 * 1024 * 1024;

    /*
     * see def. of ALIGNMENT_MASK to figure this one out
     */
    /* both gemini and aries have a 4-byte alignment requirement on remote addresses */
    mca_btl_ugni_module.super.btl_get_alignment = 4;

    /* threshold for put */
    mca_btl_ugni_module.super.btl_min_rdma_pipeline_size    = 8 * 1024;

    mca_btl_ugni_module.super.btl_flags = MCA_BTL_FLAGS_SEND |
        MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_SEND_INPLACE | MCA_BTL_FLAGS_ATOMIC_OPS |
        MCA_BTL_FLAGS_ATOMIC_FOPS;
    mca_btl_ugni_module.super.btl_atomic_flags = MCA_BTL_ATOMIC_SUPPORTS_ADD |
        MCA_BTL_ATOMIC_SUPPORTS_AND | MCA_BTL_ATOMIC_SUPPORTS_OR | MCA_BTL_ATOMIC_SUPPORTS_XOR |
        MCA_BTL_ATOMIC_SUPPORTS_CSWAP;

    if (GNI_DEVICE_ARIES == device_type) {
        /* aries supports additional atomic operations */
        mca_btl_ugni_module.super.btl_atomic_flags |= MCA_BTL_ATOMIC_SUPPORTS_MIN | MCA_BTL_ATOMIC_SUPPORTS_MAX |
            MCA_BTL_ATOMIC_SUPPORTS_LAND | MCA_BTL_ATOMIC_SUPPORTS_LOR | MCA_BTL_ATOMIC_SUPPORTS_LXOR |
            MCA_BTL_ATOMIC_SUPPORTS_32BIT | MCA_BTL_ATOMIC_SUPPORTS_FLOAT;
    }

    mca_btl_ugni_module.super.btl_registration_handle_size = sizeof (mca_btl_base_registration_handle_t);

    mca_btl_ugni_module.super.btl_bandwidth = 40000; /* Mbs */
    mca_btl_ugni_module.super.btl_latency   = 2;     /* Microsecs */

    mca_btl_ugni_module.super.btl_get_local_registration_threshold = 0;
    mca_btl_ugni_module.super.btl_put_local_registration_threshold = mca_btl_ugni_component.ugni_fma_put_limit;

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_ugni_component.super.btl_version,
                                &mca_btl_ugni_module.super);

    return OPAL_SUCCESS;
}

static int
btl_ugni_component_open(void)
{
    mca_btl_ugni_component.ugni_num_btls = 0;
    mca_btl_ugni_component.modules = NULL;

    return OPAL_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */
static int
btl_ugni_component_close(void)
{
    mca_btl_ugni_fini ();

    free (mca_btl_ugni_component.modules);
    mca_btl_ugni_component.modules = NULL;

    return OPAL_SUCCESS;
}

static mca_btl_base_module_t **
mca_btl_ugni_component_init (int *num_btl_modules,
                             bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    struct mca_btl_base_module_t **base_modules;
    mca_btl_ugni_module_t *ugni_modules;
    int rc;

    if (16384 < mca_btl_ugni_component.ugni_smsg_limit) {
        mca_btl_ugni_component.ugni_smsg_limit = 16384;
    }

    if (65536 < mca_btl_ugni_component.ugni_fma_limit) {
        mca_btl_ugni_component.ugni_fma_limit = 65536;
    }

    if (-1 != mca_btl_ugni_component.ugni_fma_limit) {
        mca_btl_ugni_component.ugni_fma_get_limit = mca_btl_ugni_component.ugni_fma_limit;
    } else if (65536 < mca_btl_ugni_component.ugni_fma_get_limit) {
        mca_btl_ugni_component.ugni_fma_get_limit = 65536;
    }

    if (-1 != mca_btl_ugni_component.ugni_fma_limit) {
        mca_btl_ugni_component.ugni_fma_put_limit = mca_btl_ugni_component.ugni_fma_limit;
    } else if (65536 < mca_btl_ugni_component.ugni_fma_put_limit) {
        mca_btl_ugni_component.ugni_fma_put_limit = 65536;
    }

    mca_btl_ugni_module.super.btl_put_local_registration_threshold = mca_btl_ugni_component.ugni_fma_put_limit;

    /* limit the number of outstanding RDMA operations over all devices */
    mca_btl_ugni_component.active_rdma_threshold = mca_btl_ugni_component.local_rdma_cq_size;

    if (enable_mpi_threads && mca_btl_ugni_component.progress_thread_requested) {
        mca_btl_ugni_component.progress_thread_enabled = 1;
    }

    /* Initialize ugni library and create communication domain */
    rc = mca_btl_ugni_init();
    if (OPAL_SUCCESS != rc) {
        return NULL;
    }

    /* For now only create a single BTL module */
    mca_btl_ugni_component.ugni_num_btls = 1;

    BTL_VERBOSE(("btl/ugni initializing"));

    ugni_modules = mca_btl_ugni_component.modules = (mca_btl_ugni_module_t *)
        calloc (mca_btl_ugni_component.ugni_num_btls, sizeof (mca_btl_ugni_module_t));

    if (OPAL_UNLIKELY(NULL == mca_btl_ugni_component.modules)) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return NULL;
    }

    base_modules = (struct mca_btl_base_module_t **)
        calloc (mca_btl_ugni_component.ugni_num_btls,
                sizeof (struct mca_btl_base_module_t *));
    if (OPAL_UNLIKELY(NULL == base_modules)) {
        BTL_ERROR(("Malloc failed : %s:%d", __FILE__, __LINE__));
        return NULL;
    }

    if (mca_btl_ugni_component.smsg_page_size != (unsigned long) opal_getpagesize ()) {
        if (mca_btl_ugni_ugni_page_size > mca_btl_ugni_component.smsg_page_size) {
            mca_btl_ugni_component.smsg_page_size = mca_btl_ugni_ugni_page_size;
        }
    }

    mca_btl_ugni_module.super.btl_rdma_pipeline_send_length = mca_btl_ugni_module.super.btl_eager_limit;

    rc = mca_btl_ugni_module_init (ugni_modules);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("Failed to initialize uGNI module @ %s:%d", __FILE__,
                   __LINE__));
        return NULL;
    }

    *base_modules = (mca_btl_base_module_t *) ugni_modules;

    *num_btl_modules = mca_btl_ugni_component.ugni_num_btls;

    BTL_VERBOSE(("btl/ugni done initializing %d module(s)", *num_btl_modules));

    return base_modules;
}

int mca_btl_ugni_progress_datagram (mca_btl_ugni_device_t *device)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_component.modules;
    mca_btl_base_endpoint_t *ep = NULL;
    gni_ep_handle_t handle;
    int count = 0, rc;

    rc = mca_btl_ugni_get_datagram (ugni_module, device, &handle, &ep);
    if (1 != rc) {
        return rc;
    }

    BTL_VERBOSE(("remote datagram completion on handle %p", (void*)handle));

    /* if this is a wildcard endpoint lookup the remote peer by the proc id we received */
    if (handle == ugni_module->wildcard_ep) {
        struct opal_proc_t *remote_proc = opal_proc_for_name (ugni_module->wc_remote_attr.proc_name);

        BTL_VERBOSE(("received connection attempt on wildcard endpoint from proc: %s",
                     OPAL_NAME_PRINT(ugni_module->wc_remote_attr.proc_name)));

        ep = mca_btl_ugni_get_ep (&ugni_module->super, remote_proc);
        if (OPAL_UNLIKELY(NULL == ep)) {
            /* there is no way to recover from this error so just abort() */
            BTL_ERROR(("could not find/allocate a btl endpoint for peer %s",
                       OPAL_NAME_PRINT(ugni_module->wc_remote_attr.proc_name)));
            abort ();
            return OPAL_ERR_NOT_FOUND;
        }
    }

    /* should not have gotten a NULL endpoint */
    assert (NULL != ep);

    BTL_VERBOSE(("got a datagram completion: ep = %p. wc = %d", (void *) ep, handle == ugni_module->wildcard_ep));

    /* NTH: TODO -- error handling */
    opal_mutex_lock (&ep->lock);
    if (handle != ugni_module->wildcard_ep) {
        /* directed post complete */
        BTL_VERBOSE(("directed datagram complete for endpoint %p", (void *) ep));

        ep->dg_posted = false;
        (void) opal_atomic_add_fetch_32 (&ugni_module->active_datagrams, -1);
    }

    (void) mca_btl_ugni_ep_connect_progress (ep);
    opal_mutex_unlock (&ep->lock);

    if (MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state) {
        /*  process messages waiting in the endpoint's smsg mailbox */
        count = mca_btl_ugni_smsg_process (ep);
    }

    /* repost the wildcard datagram */
    if (handle == ugni_module->wildcard_ep) {
        mca_btl_ugni_wildcard_ep_post (ugni_module);
    }

    return count;
}

void mca_btl_ugni_handle_rdma_completions (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_device_t *device,
                                           struct mca_btl_ugni_post_descriptor_t *post_desc, const int count)
{
    int bte_complete = 0;

    for (int i = 0 ; i < count ; ++i) {
        BTL_VERBOSE(("post descriptor complete. status: %d", post_desc[i].rc));

        if (OPAL_UNLIKELY(OPAL_SUCCESS != post_desc[i].rc)) {
            /* dump the post descriptor if in a debug build */
            btl_ugni_dump_post_desc (post_desc + i);
        }

        bte_complete += post_desc[i].use_bte == true;

        mca_btl_ugni_post_desc_complete (ugni_module, post_desc + i, post_desc[i].rc);
    }

    if (bte_complete > 0)  {
        (void) OPAL_THREAD_FETCH_ADD32 (&ugni_module->active_rdma_count, -bte_complete);
    }
}

static inline int mca_btl_ugni_progress_rdma (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_device_t *device,
                                              mca_btl_ugni_cq_t *cq)
{
    mca_btl_ugni_post_descriptor_t post_desc[MCA_BTL_UGNI_COMPLETIONS_PER_LOOP];
    int rc;

    rc = mca_btl_ugni_cq_get_completed_desc (device, cq, post_desc, MCA_BTL_UGNI_COMPLETIONS_PER_LOOP);
    if (0 >= rc) {
        return rc;
    }

    BTL_VERBOSE(("got %d completed rdma descriptors", rc));

    mca_btl_ugni_handle_rdma_completions (ugni_module, device, post_desc, rc);

    return rc;
}

static inline int
mca_btl_ugni_progress_wait_list (mca_btl_ugni_module_t *ugni_module)
{
    int rc = OPAL_SUCCESS;
    mca_btl_base_endpoint_t *endpoint = NULL;
    int count;

    if (0 == opal_list_get_size(&ugni_module->ep_wait_list)) {
        return 0;
    }

    /* check the count before taking the lock to avoid unnecessary locking */
    count = opal_list_get_size(&ugni_module->ep_wait_list);
    if (0 == count) {
        return 0;
    }

    OPAL_THREAD_LOCK(&ugni_module->ep_wait_list_lock);
    count = opal_list_get_size(&ugni_module->ep_wait_list);
    do {
        endpoint = (mca_btl_base_endpoint_t *) opal_list_remove_first (&ugni_module->ep_wait_list);
        if (endpoint != NULL) {
            rc = mca_btl_ugni_progress_send_wait_list (endpoint);

            if (OPAL_SUCCESS != rc) {
                opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
            } else {
                endpoint->wait_listed = false;
            }
        }
    } while (endpoint != NULL && --count > 0) ;
    OPAL_THREAD_UNLOCK(&ugni_module->ep_wait_list_lock);

    return rc;
}

static int mca_btl_ugni_component_progress (void)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_component.modules;
    int count = 0;

    count += mca_btl_ugni_progress_remote_smsg (ugni_module);

    if (ugni_module->active_datagrams) {
        count += mca_btl_ugni_progress_datagram (ugni_module->devices);
    }

    for (int i = 0 ; i < mca_btl_ugni_component.virtual_device_count ; ++i) {
        mca_btl_ugni_device_t *device = ugni_module->devices + i;

        if (device->smsg_connections) {
            count += mca_btl_ugni_progress_local_smsg (ugni_module, device);
            mca_btl_ugni_progress_wait_list (ugni_module);
        }

        if (device->dev_rdma_local_cq.active_operations) {
            count += mca_btl_ugni_progress_rdma (ugni_module, device, &device->dev_rdma_local_cq);
        }

        if (mca_btl_ugni_component.progress_thread_enabled && device->dev_rdma_local_irq_cq.active_operations) {
            count += mca_btl_ugni_progress_rdma (ugni_module, device, &device->dev_rdma_local_irq_cq);
        }
    }

    return count;
}

int mca_btl_ugni_flush (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_component.modules;

    for (int i = 0 ; i < mca_btl_ugni_component.virtual_device_count ; ++i) {
        mca_btl_ugni_device_t *device = ugni_module->devices + i;
        /* spin on progress until all active operations are complete. it is tempting to
         * take an initial count then wait until that many operations have been completed
         * but it is impossible to tell if those are the operations the caller is waiting
         * on. */
        while (device->dev_rdma_local_cq.active_operations) {
            (void) mca_btl_ugni_progress_rdma (ugni_module, device, &device->dev_rdma_local_cq);
        }

        /* mark that the device was recently flushed */
        device->flushed = true;
    }

    return OPAL_SUCCESS;
}

void btl_ugni_dump_post_desc (mca_btl_ugni_post_descriptor_t *desc)
{

    fprintf (stderr, "desc->gni_desc.post_id          = %" PRIx64 "\n", desc->gni_desc.post_id);
    fprintf (stderr, "desc->gni_desc.status           = %" PRIx64 "\n", desc->gni_desc.status);
    fprintf (stderr, "desc->gni_desc.cq_mode_complete = %hu\n", desc->gni_desc.cq_mode_complete);
    fprintf (stderr, "desc->gni_desc.type             = %d\n", desc->gni_desc.type);
    fprintf (stderr, "desc->gni_desc.cq_mode          = %hu\n", desc->gni_desc.cq_mode);
    fprintf (stderr, "desc->gni_desc.dlvr_mode        = %hu\n", desc->gni_desc.dlvr_mode);
    fprintf (stderr, "desc->gni_desc.local_addr       = %" PRIx64 "\n", desc->gni_desc.local_addr);
    fprintf (stderr, "desc->gni_desc.local_mem_hndl   = {%" PRIx64 ", %" PRIx64 "}\n", desc->gni_desc.local_mem_hndl.qword1,
             desc->gni_desc.local_mem_hndl.qword2);
    fprintf (stderr, "desc->gni_desc.remote_addr      = %" PRIx64 "\n", desc->gni_desc.remote_addr);
    fprintf (stderr, "desc->gni_desc.remote_mem_hndl  = {%" PRIx64 ", %" PRIx64 "}\n", desc->gni_desc.remote_mem_hndl.qword1,
             desc->gni_desc.remote_mem_hndl.qword2);
    fprintf (stderr, "desc->gni_desc.length           = %" PRIu64 "\n", desc->gni_desc.length);
    fprintf (stderr, "desc->gni_desc.rdma_mode        = %hu\n", desc->gni_desc.rdma_mode);
    fprintf (stderr, "desc->gni_desc.amo_cmd          = %d\n", desc->gni_desc.amo_cmd);
}
