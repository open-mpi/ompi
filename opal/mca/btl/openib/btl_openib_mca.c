/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2013-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include "opal/util/bit_ops.h"
#include "opal/mca/common/verbs/common_verbs.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/proc.h"

#include "btl_openib.h"
#include "btl_openib_mca.h"
#include "btl_openib_ini.h"
#include "connect/base.h"

#ifdef HAVE_IBV_FORK_INIT
#define OPAL_HAVE_IBV_FORK_INIT 1
#else
#define OPAL_HAVE_IBV_FORK_INIT 0
#endif

/*
 * Local flags
 */
enum {
    REGINT_NEG_ONE_OK = 0x01,
    REGINT_GE_ZERO = 0x02,
    REGINT_GE_ONE = 0x04,
    REGINT_NONZERO = 0x08,

    REGINT_MAX = 0x88
};


enum {
    REGSTR_EMPTY_OK = 0x01,

    REGSTR_MAX = 0x88
};

static mca_base_var_enum_value_t ib_mtu_values[] = {
    {IBV_MTU_256, "256B"},
    {IBV_MTU_512, "512B"},
    {IBV_MTU_1024, "1k"},
    {IBV_MTU_2048, "2k"},
    {IBV_MTU_4096, "4k"},
    {0, NULL}
};

static mca_base_var_enum_value_t device_type_values[] = {
    {BTL_OPENIB_DT_IB,    "infiniband"},
    {BTL_OPENIB_DT_IB,    "ib"},
    {BTL_OPENIB_DT_IWARP, "iwarp"},
    {BTL_OPENIB_DT_IWARP, "iw"},
    {BTL_OPENIB_DT_ALL,   "all"},
    {0, NULL}
};

static int btl_openib_cq_size;
static bool btl_openib_have_fork_support = OPAL_HAVE_IBV_FORK_INIT;

#if BTL_OPENIB_FAILOVER_ENABLED
static int btl_openib_verbose_failover;
static bool btl_openib_failover_enabled = true;
#endif

/*
 * utility routine for string parameter registration
 */
static int reg_string(const char* param_name,
                      const char* deprecated_param_name,
                      const char* param_desc,
                      const char* default_value, char **storage,
                      int flags)
{
    int index;

    /* The MCA variable system will not change this pointer */
    *storage = (char *) default_value;
    index = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_STRING,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "btl", "openib",
                                             deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if (0 != (flags & REGSTR_EMPTY_OK) && (NULL == storage || 0 == strlen(*storage))) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
}


/*
 * utility routine for integer parameter registration
 */
static int reg_int(const char* param_name,
                   const char* deprecated_param_name,
                   const char* param_desc,
                   int default_value, int *storage, int flags)
{
    int index;

    *storage = default_value;
    index = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_INT,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "btl", "openib",
                                             deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if (0 != (flags & REGINT_NEG_ONE_OK) && -1 == *storage) {
        return OPAL_SUCCESS;
    }

    if ((0 != (flags & REGINT_GE_ZERO) && *storage < 0) ||
        (0 != (flags & REGINT_GE_ONE) && *storage < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == *storage)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
}

/*
 * utility routine for integer parameter registration
 */
static int reg_uint(const char* param_name,
                    const char* deprecated_param_name,
                    const char* param_desc,
                    unsigned int default_value, unsigned int *storage,
                    int flags)
{
    int index;

    *storage = default_value;
    index = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "btl", "openib",
                                             deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if ((0 != (flags & REGINT_GE_ONE) && *storage < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == *storage)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
}

/*
 * utility routine for integer parameter registration
 */
static int reg_bool(const char* param_name,
                    const char* deprecated_param_name,
                    const char* param_desc,
                    bool default_value, bool *storage)
{
    int index;

    *storage = default_value;
    index = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_BOOL,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "btl", "openib",
                                             deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    return OPAL_SUCCESS;
}

/*
 * Register and check all MCA parameters
 */
int btl_openib_register_mca_params(void)
{
    mca_base_var_enum_t *new_enum;
    char *default_qps;
    uint32_t mid_qp_size;
    char *msg, *str;
    int ret, tmp;

    ret = OPAL_SUCCESS;
#define CHECK(expr) do {\
        tmp = (expr); \
        if (OPAL_SUCCESS != tmp) ret = tmp; \
     } while (0)

    /* register openib component parameters */
    CHECK(reg_bool("verbose", NULL,
                  "Output some verbose OpenIB BTL information "
                  "(0 = no output, nonzero = output)", false,
                   &mca_btl_openib_component.verbose));

    CHECK(reg_bool("warn_no_device_params_found",
                  "warn_no_hca_params_found",
                  "Warn when no device-specific parameters are found in the INI file specified by the btl_openib_device_param_files MCA parameter "
                  "(0 = do not warn; any other value = warn)",
                  true, &mca_btl_openib_component.warn_no_device_params_found));

    CHECK(reg_bool("warn_default_gid_prefix", NULL,
                  "Warn when there is more than one active ports and at least one of them connected to the network with only default GID prefix configured "
                  "(0 = do not warn; any other value = warn)",
                  true, &mca_btl_openib_component.warn_default_gid_prefix));

    CHECK(reg_bool("warn_nonexistent_if", NULL,
                  "Warn if non-existent devices and/or ports are specified in the btl_openib_if_[in|ex]clude MCA parameters "
                  "(0 = do not warn; any other value = warn)",
                  true, &mca_btl_openib_component.warn_nonexistent_if));

    /* If we print a warning about not having enough registered memory
       available, do we want to abort? */
    CHECK(reg_bool("abort_not_enough_reg_mem", NULL,
                  "If there is not enough registered memory available on the system for Open MPI to function properly, Open MPI will issue a warning.  If this MCA parameter is set to true, then Open MPI will also abort all MPI jobs "
                  "(0 = warn, but do not abort; any other value = warn and abort)",
                  false, &mca_btl_openib_component.abort_not_enough_reg_mem));

    CHECK(reg_uint("poll_cq_batch", NULL,
                   "Retrieve up to poll_cq_batch completions from CQ",
                   MCA_BTL_OPENIB_CQ_POLL_BATCH_DEFAULT, &mca_btl_openib_component.cq_poll_batch,
                   REGINT_GE_ONE));

    asprintf(&str, "%s/mca-btl-openib-device-params.ini",
             opal_install_dirs.opaldatadir);
    if (NULL == str) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    CHECK(reg_string("device_param_files", "hca_param_files",
                     "Colon-delimited list of INI-style files that contain device vendor/part-specific parameters (use semicolon for Windows)",
                     str, &mca_btl_openib_component.device_params_file_names,
                     0));
    free(str);

    (void)mca_base_var_enum_create("btl_openib_device_types", device_type_values, &new_enum);
    mca_btl_openib_component.device_type = BTL_OPENIB_DT_ALL;
    tmp = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                          "device_type", "Specify to only use IB or iWARP "
                                          "network adapters (infiniband = only use InfiniBand "
                                          "HCAs; iwarp = only use iWARP NICs; all = use any "
                                          "available adapters)", MCA_BASE_VAR_TYPE_INT, new_enum,
                                          0, 0, OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_btl_openib_component.device_type);
    if (0 > tmp) ret = tmp;
    OBJ_RELEASE(new_enum);

    CHECK(reg_int("max_btls", NULL,
                  "Maximum number of device ports to use "
                  "(-1 = use all available, otherwise must be >= 1)",
                  -1, &mca_btl_openib_component.ib_max_btls,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_num", NULL,
                  "Initial size of free lists "
                  "(must be >= 1)",
                  8, &mca_btl_openib_component.ib_free_list_num,
                  REGINT_GE_ONE));
    CHECK(reg_int("free_list_max", NULL,
                  "Maximum size of free lists "
                  "(-1 = infinite, otherwise must be >= 0)",
                  -1, &mca_btl_openib_component.ib_free_list_max,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_inc", NULL,
                  "Increment size of free lists "
                  "(must be >= 1)",
                  32, &mca_btl_openib_component.ib_free_list_inc,
                  REGINT_GE_ONE));
    CHECK(reg_string("mpool", NULL,
                     "Name of the memory pool to be used (it is unlikely that you will ever want to change this)",
                     "grdma", &mca_btl_openib_component.ib_mpool_name,
                     0));
    CHECK(reg_int("reg_mru_len", NULL,
                  "Length of the registration cache most recently used list "
                  "(must be >= 1)",
                  16, (int*) &mca_btl_openib_component.reg_mru_len,
                  REGINT_GE_ONE));

    CHECK(reg_int("cq_size", "ib_cq_size",
                  "Minimum size of the OpenFabrics completion queue "
                  "(CQs are automatically sized based on the number "
                  "of peer MPI processes; this value determines the "
                  "*minimum* size of all CQs)",
                  8192, &btl_openib_cq_size, REGINT_GE_ONE));
    mca_btl_openib_component.ib_cq_size[BTL_OPENIB_LP_CQ] =
        mca_btl_openib_component.ib_cq_size[BTL_OPENIB_HP_CQ] = (uint32_t) btl_openib_cq_size;

    CHECK(reg_int("max_inline_data", "ib_max_inline_data",
                  "Maximum size of inline data segment "
                  "(-1 = run-time probe to discover max value, otherwise must be >= 0). "
                  "If not explicitly set, use max_inline_data from "
                  "the INI file containing device-specific parameters",
                  -1, &mca_btl_openib_component.ib_max_inline_data,
                  REGINT_NEG_ONE_OK | REGINT_GE_ZERO));

    CHECK(reg_uint("pkey", "ib_pkey_val",
                   "OpenFabrics partition key (pkey) value. "
                   "Unsigned integer decimal or hex values are allowed (e.g., \"3\" or \"0x3f\") and will be masked against the maximum allowable IB partition key value (0x7fff)",
                   0, &mca_btl_openib_component.ib_pkey_val, 0));

    CHECK(reg_uint("psn", "ib_psn",
                  "OpenFabrics packet sequence starting number "
                  "(must be >= 0)",
                  0, &mca_btl_openib_component.ib_psn, 0));

    CHECK(reg_uint("ib_qp_ous_rd_atom", NULL,
                   "InfiniBand outstanding atomic reads "
                   "(must be >= 0)",
                   4, &mca_btl_openib_component.ib_qp_ous_rd_atom, 0));
    
    asprintf(&msg, "OpenFabrics MTU, in bytes (if not specified in INI files).  Valid values are: %d=256 bytes, %d=512 bytes, %d=1024 bytes, %d=2048 bytes, %d=4096 bytes",
             IBV_MTU_256,
             IBV_MTU_512,
             IBV_MTU_1024,
             IBV_MTU_2048,
             IBV_MTU_4096);
    if (NULL == msg) {
        /* Don't try to recover from this */
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    mca_btl_openib_component.ib_mtu = IBV_MTU_1024;
    (void) mca_base_var_enum_create("btl_openib_mtus", ib_mtu_values, &new_enum);
    tmp = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                          "mtu", msg, MCA_BASE_VAR_TYPE_INT, new_enum,
                                          0, 0, OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_btl_openib_component.ib_mtu);
    if (0 <= tmp) {
        (void) mca_base_var_register_synonym(tmp, "ompi", "btl", "openib", "ib_mtu",
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    } else {
        ret = tmp;
    }

    OBJ_RELEASE(new_enum);
    free(msg);

    CHECK(reg_uint("ib_min_rnr_timer", NULL, "InfiniBand minimum "
                   "\"receiver not ready\" timer, in seconds "
                   "(must be >= 0 and <= 31)",
                   25, &mca_btl_openib_component.ib_min_rnr_timer, 0));

    CHECK(reg_uint("ib_timeout", NULL,
                  "InfiniBand transmit timeout, plugged into formula: 4.096 microseconds * (2^btl_openib_ib_timeout) "
                  "(must be >= 0 and <= 31)",
                  20, &mca_btl_openib_component.ib_timeout, 0));
    
    CHECK(reg_uint("ib_retry_count", NULL,
                  "InfiniBand transmit retry count "
                  "(must be >= 0 and <= 7)",
                  7, &mca_btl_openib_component.ib_retry_count, 0));

    CHECK(reg_uint("ib_rnr_retry", NULL,
                   "InfiniBand \"receiver not ready\" "
                   "retry count; applies *only* to SRQ/XRC queues.  PP queues "
                   "use RNR retry values of 0 because Open MPI performs "
                   "software flow control to guarantee that RNRs never occur "
                   "(must be >= 0 and <= 7; 7 = \"infinite\")",
                   7, &mca_btl_openib_component.ib_rnr_retry, 0));

    CHECK(reg_uint("ib_max_rdma_dst_ops", NULL, "InfiniBand maximum pending RDMA "
                  "destination operations "
                  "(must be >= 0)",
                  4, &mca_btl_openib_component.ib_max_rdma_dst_ops, 0));

    CHECK(reg_uint("ib_service_level", NULL, "InfiniBand service level "
                   "(must be >= 0 and <= 15)",
                   0, &mca_btl_openib_component.ib_service_level, 0));

#if (ENABLE_DYNAMIC_SL)
    CHECK(reg_uint("ib_path_record_service_level", NULL,
                   "Enable getting InfiniBand service level from PathRecord "
                   "(must be >= 0, 0 = disabled, positive = try to get the "
                   "service level from PathRecord)",
                   0, &mca_btl_openib_component.ib_path_record_service_level, 0));
#endif

    CHECK(reg_int("use_eager_rdma", NULL, "Use RDMA for eager messages "
                  "(-1 = use device default, 0 = do not use eager RDMA, "
                  "1 = use eager RDMA)",
                  -1, &mca_btl_openib_component.use_eager_rdma, 0));

    CHECK(reg_int("eager_rdma_threshold", NULL,
                  "Use RDMA for short messages after this number of "
                  "messages are received from a given peer "
                  "(must be >= 1)",
                  16, &mca_btl_openib_component.eager_rdma_threshold, REGINT_GE_ONE));

    CHECK(reg_int("max_eager_rdma", NULL, "Maximum number of peers allowed to use "
                  "RDMA for short messages (RDMA is used for all long "
                  "messages, except if explicitly disabled, such as "
                  "with the \"dr\" pml) "
                  "(must be >= 0)",
                  16, &mca_btl_openib_component.max_eager_rdma, REGINT_GE_ZERO));

    CHECK(reg_int("eager_rdma_num", NULL, "Number of RDMA buffers to allocate "
                  "for small messages "
                  "(must be >= 1)",
                  16, &mca_btl_openib_component.eager_rdma_num, REGINT_GE_ONE));
    mca_btl_openib_component.eager_rdma_num++;

    CHECK(reg_uint("btls_per_lid", NULL, "Number of BTLs to create for each "
                  "InfiniBand LID "
                  "(must be >= 1)",
                  1, &mca_btl_openib_component.btls_per_lid, REGINT_GE_ONE));

    CHECK(reg_uint("max_lmc", NULL, "Maximum number of LIDs to use for each device port "
                   "(must be >= 0, where 0 = use all available)",
                   1, &mca_btl_openib_component.max_lmc, 0));

    CHECK(reg_int("enable_apm_over_lmc", NULL, "Maximum number of alternative paths for each device port "
                  "(must be >= -1, where 0 = disable apm, -1 = all available alternative paths )",
                  0, &mca_btl_openib_component.apm_lmc, REGINT_NEG_ONE_OK|REGINT_GE_ZERO));

    CHECK(reg_int("enable_apm_over_ports", NULL, "Enable alternative path migration (APM) over different ports of the same device "
                  "(must be >= 0, where 0 = disable APM over ports, 1 = enable APM over ports of the same device)",
                  0, &mca_btl_openib_component.apm_ports, REGINT_GE_ZERO));

    CHECK(reg_bool("use_async_event_thread", NULL,
                   "If nonzero, use the thread that will handle InfiniBand asynchronous events",
                   true, &mca_btl_openib_component.use_async_event_thread));

#if BTL_OPENIB_FAILOVER_ENABLED
    /* failover specific output */
    CHECK(reg_int("verbose_failover", NULL,
                  "Output some verbose OpenIB BTL failover information "
                  "(0 = no output, nonzero = output)", 0, &btl_openib_verbose_failover, 0));
    mca_btl_openib_component.verbose_failover = opal_output_open(NULL);
    opal_output_set_verbosity(mca_btl_openib_component.verbose_failover, btl_openib_verbose_failover);

    CHECK(reg_bool("port_error_failover", NULL,
                   "If nonzero, asynchronous port errors will trigger failover",
                   0, &mca_btl_openib_component.port_error_failover));

    /* Make non writeable parameter that indicates failover is configured in. */
    tmp = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                          "failover_enabled",
                                          "openib failover is configured: run with bfo PML to support failover between openib BTLs",
                                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                          MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_CONSTANT,
                                          &btl_openib_failover_enabled);
    if (0 > tmp) ret = tmp;
#endif
    
    CHECK(reg_bool("enable_srq_resize", NULL,
                   "Enable/Disable on demand SRQ resize. "
                   "(0 = without resizing, nonzero = with resizing)", 1,
                   &mca_btl_openib_component.enable_srq_resize));

#if HAVE_DECL_IBV_LINK_LAYER_ETHERNET
    CHECK(reg_bool("rroce_enable", NULL,
                   "Enable/Disable routing between different subnets"
                   "(0 = disable, nonzero = enable)", false,
                   &mca_btl_openib_component.rroce_enable));
#endif

    CHECK(reg_uint("buffer_alignment", NULL,
                   "Preferred communication buffer alignment, in bytes "
                   "(must be > 0 and power of two)",
                   64, &mca_btl_openib_component.buffer_alignment, 0));

    CHECK(reg_bool("use_message_coalescing", NULL,
                   "If nonzero, use message coalescing", false,
                   &mca_btl_openib_component.use_message_coalescing));

    CHECK(reg_uint("cq_poll_ratio", NULL,
                   "How often to poll high priority CQ versus low priority CQ",
                   100, &mca_btl_openib_component.cq_poll_ratio, REGINT_GE_ONE));
    CHECK(reg_uint("eager_rdma_poll_ratio", NULL,
                   "How often to poll eager RDMA channel versus CQ",
                   100, &mca_btl_openib_component.eager_rdma_poll_ratio, REGINT_GE_ONE));
    CHECK(reg_uint("hp_cq_poll_per_progress", NULL,
                  "Max number of completion events to process for each call "
                  "of BTL progress engine",
                  10, &mca_btl_openib_component.cq_poll_progress, REGINT_GE_ONE));

    CHECK(reg_uint("max_hw_msg_size", NULL,
                   "Maximum size (in bytes) of a single fragment of a long message when using the RDMA protocols (must be > 0 and <= hw capabilities).",
                   0, &mca_btl_openib_component.max_hw_msg_size, 0));

    CHECK(reg_bool("allow_max_memory_registration", NULL,
                  "Allow maximum possible memory to register with HCA",
                   1, &mca_btl_openib_component.allow_max_memory_registration));

    /* Help debug memory registration issues */
    CHECK(reg_int("memory_registration_verbose", NULL,
                  "Output some verbose memory registration information "
                  "(0 = no output, nonzero = output)", 0,
		  &mca_btl_openib_component.memory_registration_verbose_level, 0));

    CHECK(reg_int("ignore_locality", NULL,
                  "Ignore any locality information and use all devices "
                  "(0 = use locality informaiton and use only close devices, nonzero = ignore locality information)", 0,
                  &mca_btl_openib_component.ignore_locality, REGINT_GE_ZERO));

    /* Info only */
    tmp = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                          "have_fork_support",
                                          "Whether the OpenFabrics stack supports applications that invoke the \"fork()\" system call or not (0 = no, 1 = yes). "
                                          "Note that this value does NOT indicate whether the system being run on supports \"fork()\" with OpenFabrics applications or not.",
                                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                          MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_CONSTANT,
                                          &btl_openib_have_fork_support);

    mca_btl_openib_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_DEFAULT;

    mca_btl_openib_module.super.btl_eager_limit = 12 * 1024;
    mca_btl_openib_module.super.btl_rndv_eager_limit = 12 * 1024;
    mca_btl_openib_module.super.btl_max_send_size = 64 * 1024;
    mca_btl_openib_module.super.btl_rdma_pipeline_send_length = 1024 * 1024;
    mca_btl_openib_module.super.btl_rdma_pipeline_frag_size = 1024 * 1024;
    mca_btl_openib_module.super.btl_min_rdma_pipeline_size = 256 * 1024;
    mca_btl_openib_module.super.btl_flags = MCA_BTL_FLAGS_RDMA |
	MCA_BTL_FLAGS_NEED_ACK | MCA_BTL_FLAGS_NEED_CSUM | MCA_BTL_FLAGS_HETEROGENEOUS_RDMA;
#if BTL_OPENIB_FAILOVER_ENABLED
    mca_btl_openib_module.super.btl_flags |= MCA_BTL_FLAGS_FAILOVER_SUPPORT;
#endif

#if HAVE_DECL_IBV_ATOMIC_HCA
    mca_btl_openib_module.super.btl_flags |= MCA_BTL_FLAGS_ATOMIC_FOPS;
    mca_btl_openib_module.super.btl_atomic_flags = MCA_BTL_ATOMIC_SUPPORTS_ADD | MCA_BTL_ATOMIC_SUPPORTS_CSWAP;
#endif

    /* Default to bandwidth auto-detection */
    mca_btl_openib_module.super.btl_bandwidth = 0;
    mca_btl_openib_module.super.btl_latency = 4;
#if OPAL_CUDA_SUPPORT /* CUDA_ASYNC_RECV */
    /* Default is enabling CUDA asynchronous send copies */
    CHECK(reg_bool("cuda_async_send", NULL,
                   "Enable or disable CUDA async send copies "
                   "(true = async; false = sync)",
                   true, &mca_btl_openib_component.cuda_async_send));

    /* Default is enabling CUDA asynchronous receive copies */
    CHECK(reg_bool("cuda_async_recv", NULL,
                   "Enable or disable CUDA async recv copies "
                   "(true = async; false = sync)",
                   true, &mca_btl_openib_component.cuda_async_recv));
    /* Also make the max send size larger for better GPU buffer performance */
    mca_btl_openib_module.super.btl_max_send_size = 128 * 1024;
    /* Turn of message coalescing - not sure if it works with GPU buffers */
    mca_btl_openib_component.use_message_coalescing = 0;

    /* Indicates if library was built with GPU Direct RDMA support.  Not changeable.  */
    mca_btl_openib_component.cuda_have_gdr = OPAL_INT_TO_BOOL(OPAL_CUDA_GDR_SUPPORT);
    (void) mca_base_component_var_register(&mca_btl_openib_component.super.btl_version, "have_cuda_gdr",
                                           "Whether CUDA GPU Direct RDMA support is built into library or not",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &mca_btl_openib_component.cuda_have_gdr);

    /* Indicates if driver has GPU Direct RDMA support.  Not changeable.  */
    if (OPAL_SUCCESS == opal_os_dirpath_access("/sys/kernel/mm/memory_peers/nv_mem/version", S_IRUSR)) {
        mca_btl_openib_component.driver_have_gdr = 1;
    } else {
        mca_btl_openib_component.driver_have_gdr = 0;
    }
    (void) mca_base_component_var_register(&mca_btl_openib_component.super.btl_version, "have_driver_gdr",
                                           "Whether Infiniband driver has GPU Direct RDMA support",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &mca_btl_openib_component.driver_have_gdr);

    /* Default for GPU Direct RDMA is off for now */
    CHECK(reg_bool("want_cuda_gdr", NULL,
                   "Enable or disable CUDA GPU Direct RDMA support "
                   "(true = enabled; false = disabled)",
                   false, &mca_btl_openib_component.cuda_want_gdr));

    if (mca_btl_openib_component.cuda_want_gdr && !mca_btl_openib_component.cuda_have_gdr) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "CUDA_no_gdr_support", true,
                       opal_process_info.nodename);
        return OPAL_ERROR;
    }
    if (mca_btl_openib_component.cuda_want_gdr && !mca_btl_openib_component.driver_have_gdr) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "driver_no_gdr_support", true,
                       opal_process_info.nodename);
        return OPAL_ERROR;
    }
#if OPAL_CUDA_GDR_SUPPORT
    if (mca_btl_openib_component.cuda_want_gdr) {
        mca_btl_openib_module.super.btl_flags |= MCA_BTL_FLAGS_CUDA_GET;
        mca_btl_openib_module.super.btl_cuda_eager_limit = SIZE_MAX; /* magic number - indicates set it to minimum */
        mca_btl_openib_module.super.btl_cuda_rdma_limit = 30000;  /* default switchover is 30,000 to pipeline */
    } else {
        mca_btl_openib_module.super.btl_cuda_eager_limit = 0; /* Turns off any of the GPU Direct RDMA code */
        mca_btl_openib_module.super.btl_cuda_rdma_limit = 0;  /* Unused */
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */
#endif /* OPAL_CUDA_SUPPORT */
    CHECK(mca_btl_base_param_register(
            &mca_btl_openib_component.super.btl_version,
            &mca_btl_openib_module.super));

    /* setup all the qp stuff */
    /* round mid_qp_size to smallest power of two */
    mid_qp_size = opal_next_poweroftwo (mca_btl_openib_module.super.btl_eager_limit / 4) >> 1;

    /* mid_qp_size = MAX (mid_qp_size, 1024); ?! */
    if(mid_qp_size <= 128) {
        mid_qp_size = 1024;
    }

    asprintf(&default_qps,
            "P,128,256,192,128:S,%u,1024,1008,64:S,%u,1024,1008,64:S,%u,1024,1008,64",
            mid_qp_size,
            (uint32_t)mca_btl_openib_module.super.btl_eager_limit,
            (uint32_t)mca_btl_openib_module.super.btl_max_send_size);
    if (NULL == default_qps) {
        /* Don't try to recover from this */
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    mca_btl_openib_component.default_recv_qps = default_qps;
    CHECK(reg_string("receive_queues", NULL,
                     "Colon-delimited, comma-delimited list of receive queues: P,4096,8,6,4:P,32768,8,6,4",
                     default_qps, &mca_btl_openib_component.receive_queues,
                     0
                ));

    CHECK(reg_string("if_include", NULL,
                     "Comma-delimited list of devices/ports to be used (e.g. \"mthca0,mthca1:2\"; empty value means to use all ports found).  Mutually exclusive with btl_openib_if_exclude.",
                     NULL, &mca_btl_openib_component.if_include,
                     0));

    CHECK(reg_string("if_exclude", NULL,
                     "Comma-delimited list of device/ports to be excluded (empty value means to not exclude any ports).  Mutually exclusive with btl_openib_if_include.",
                     NULL, &mca_btl_openib_component.if_exclude,
                     0));

    CHECK(reg_string("ipaddr_include", NULL,
                     "Comma-delimited list of IP Addresses to be used (e.g. \"192.168.1.0/24\").  Mutually exclusive with btl_openib_ipaddr_exclude.",
                     NULL, &mca_btl_openib_component.ipaddr_include,
                     0));

    CHECK(reg_string("ipaddr_exclude", NULL,
                     "Comma-delimited list of IP Addresses to be excluded (e.g. \"192.168.1.0/24\").  Mutually exclusive with btl_openib_ipaddr_include.",
                     NULL, &mca_btl_openib_component.ipaddr_exclude,
                     0));

    CHECK(reg_int("gid_index", NULL,
                  "GID index to use on verbs device ports",
                  0, &mca_btl_openib_component.gid_index,
                  REGINT_GE_ZERO));

#if BTL_OPENIB_MALLOC_HOOKS_ENABLED
    CHECK(reg_int("memalign", NULL,
                  "[64 | 32 | 0] - Enable (64bit or 32bit)/Disable(0) memory"
                  "alignment for all malloc calls if btl openib is used.",
                  32, &mca_btl_openib_component.use_memalign,
                  REGINT_GE_ZERO));

    mca_btl_openib_component.memalign_threshold = mca_btl_openib_component.eager_limit;
    tmp = mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                          "memalign_threshold",
                                          "Allocating memory more than btl_openib_memalign_threshhold"
                                          "bytes will automatically be algined to the value of btl_openib_memalign bytes."
                                          "memalign_threshhold defaults to the same value as mca_btl_openib_eager_limit.",
                                          MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_btl_openib_component.memalign_threshold);
    if (0 > tmp) ret = tmp;
#endif

    /* Register any MCA params for the connect pseudo-components */
    if (OPAL_SUCCESS == ret) {
        ret = opal_btl_openib_connect_base_register();
    }

    return btl_openib_verify_mca_params();
}

int btl_openib_verify_mca_params (void)
{
    if (mca_btl_openib_component.cq_poll_batch > MCA_BTL_OPENIB_CQ_POLL_BATCH_DEFAULT) {
        mca_btl_openib_component.cq_poll_batch = MCA_BTL_OPENIB_CQ_POLL_BATCH_DEFAULT;
    }

#if !HAVE_IBV_FORK_INIT
    if (1 == mca_btl_openib_component.want_fork_support) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "ibv_fork requested but not supported", true,
                       opal_process_info.nodename);
        return OPAL_ERR_BAD_PARAM;
    }
#endif

    mca_btl_openib_component.ib_pkey_val &= MCA_BTL_IB_PKEY_MASK;

    if (mca_btl_openib_component.ib_min_rnr_timer > 31) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_min_rnr_timer > 31",
                       "btl_openib_ib_min_rnr_timer reset to 31");
        mca_btl_openib_component.ib_min_rnr_timer = 31;
    }

    if (mca_btl_openib_component.ib_timeout > 31) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_timeout > 31",
                       "btl_openib_ib_timeout reset to 31");
        mca_btl_openib_component.ib_timeout = 31;
    }

    if (mca_btl_openib_component.ib_retry_count > 7) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_retry_count > 7",
                       "btl_openib_ib_retry_count reset to 7");
        mca_btl_openib_component.ib_retry_count = 7;
    }

    if (mca_btl_openib_component.ib_rnr_retry > 7) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_rnr_retry > 7",
                       "btl_openib_ib_rnr_retry reset to 7");
        mca_btl_openib_component.ib_rnr_retry = 7;
    }

    if (mca_btl_openib_component.ib_service_level > 15) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_service_level > 15",
                       "btl_openib_ib_service_level reset to 15");
        mca_btl_openib_component.ib_service_level = 15;
    }

    if(mca_btl_openib_component.buffer_alignment <= 1 ||
       (mca_btl_openib_component.buffer_alignment & (mca_btl_openib_component.buffer_alignment - 1))) {
        opal_show_help("help-mpi-btl-openib.txt", "wrong buffer alignment",
                true, mca_btl_openib_component.buffer_alignment, opal_process_info.nodename, 64);
        mca_btl_openib_component.buffer_alignment = 64;
    }

#if OPAL_CUDA_SUPPORT /* CUDA_ASYNC_RECV */
    if (mca_btl_openib_component.cuda_async_send) {
        mca_btl_openib_module.super.btl_flags |= MCA_BTL_FLAGS_CUDA_COPY_ASYNC_SEND;
    } else {
        mca_btl_openib_module.super.btl_flags &= ~MCA_BTL_FLAGS_CUDA_COPY_ASYNC_SEND;
    }

    if (mca_btl_openib_component.cuda_async_recv) {
        mca_btl_openib_module.super.btl_flags |= MCA_BTL_FLAGS_CUDA_COPY_ASYNC_RECV;
    } else {
        mca_btl_openib_module.super.btl_flags &= ~MCA_BTL_FLAGS_CUDA_COPY_ASYNC_RECV;
    }
#if 0 /* Disable this check for now while fork support code is worked out. */
    /* Cannot have fork support and GDR on at the same time.  If the user asks for both,
     * then print a message and return error.  If the user does not explicitly ask for
     * fork support, then turn it off in the presence of GDR.  */
    if (mca_btl_openib_component.cuda_want_gdr && mca_btl_openib_component.cuda_have_gdr &&
        mca_btl_openib_component.driver_have_gdr) {
        if (1 == opal_common_verbs_want_fork_support) {
              opal_show_help("help-mpi-btl-openib.txt", "no_fork_with_gdr",
                             true, opal_process_info.nodename);
              return OPAL_ERR_BAD_PARAM;
        }
    }
#endif /* Workaround */
#endif

#if BTL_OPENIB_MALLOC_HOOKS_ENABLED
    if (mca_btl_openib_component.use_memalign != 32  
        && mca_btl_openib_component.use_memalign != 64
        && mca_btl_openib_component.use_memalign != 0){ 
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "Wrong btl_openib_memalign parameter value. Allowed values: 64, 32, 0.",
                       "btl_openib_memalign is reset to 32");
        mca_btl_openib_component.use_memalign = 32; 
    }
#endif

    return OPAL_SUCCESS;
}
