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
 * Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/mca/installdirs/installdirs.h"
#include "orte/util/show_help.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "btl_wv.h"
#include "btl_wv_mca.h"
#include "btl_wv_ini.h"
#include "connect/base.h"

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


/*
 * utility routine for string parameter registration
 */
static int reg_string(const char* param_name, 
                      const char* deprecated_param_name,
                      const char* param_desc,
                      const char* default_value, char **out_value,
                      int flags)
{
    int index;
    char *value;
    index = mca_base_param_reg_string(&mca_btl_wv_component.super.btl_version,
                                      param_name, param_desc, false, false,
                                      default_value, &value);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index, 
                               &mca_btl_wv_component.super.btl_version, 
                               deprecated_param_name, true);
    }
    mca_base_param_lookup_string(index, &value);

    if (0 != (flags & REGSTR_EMPTY_OK) && 0 == strlen(value)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OMPI_ERR_BAD_PARAM;
    }
    *out_value = value;
    return OMPI_SUCCESS;
}


/*
 * utility routine for integer parameter registration
 */
static int reg_int(const char* param_name, 
                   const char* deprecated_param_name,
                   const char* param_desc,
                   int default_value, int *out_value, int flags)
{
    int index, value;
    index = mca_base_param_reg_int(&mca_btl_wv_component.super.btl_version,
                                   param_name, param_desc, false, false,
                                   default_value, NULL);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index, 
                               &mca_btl_wv_component.super.btl_version, 
                               deprecated_param_name, true);
    }
    mca_base_param_lookup_int(index, &value);
    
    if (0 != (flags & REGINT_NEG_ONE_OK) && -1 == value) {
        *out_value = value;
        return OMPI_SUCCESS;
    }
    if ((0 != (flags & REGINT_GE_ZERO) && value < 0) ||
        (0 != (flags & REGINT_GE_ONE) && value < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == value)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OMPI_ERR_BAD_PARAM;
    }
    *out_value = value;
    return OMPI_SUCCESS;
}

/*
 * Register and check all MCA parameters
 */
int btl_wv_register_mca_params(void)
{
    char default_qps[100];
    uint32_t mid_qp_size;
    int i;
    char *msg, *str, *pkey;
    int ival, ival2, ret, tmp;

    ret = OMPI_SUCCESS;
#define CHECK(expr) do {\
        tmp = (expr); \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    /* register wv component parameters */
    CHECK(reg_int("verbose", NULL,
                  "Output some verbose wv BTL information "
                  "(0 = no output, nonzero = output)", 0, &ival, 0));
    mca_btl_wv_component.verbose = (0 != ival);

    CHECK(reg_int("warn_no_device_params_found",
                  "warn_no_hca_params_found",
                  "Warn when no device-specific parameters are found in the INI file specified by the btl_wv_device_param_files MCA parameter "
                  "(0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_wv_component.warn_no_device_params_found = (0 != ival);
    CHECK(reg_int("warn_default_gid_prefix", NULL,
                  "Warn when there is more than one active ports and at least one of them connected to the network with only default GID prefix configured "
                  "(0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_wv_component.warn_default_gid_prefix = (0 != ival);
    CHECK(reg_int("warn_nonexistent_if", NULL,
                  "Warn if non-existent devices and/or ports are specified in the btl_wv_if_[in|ex]clude MCA parameters "
                  "(0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_wv_component.warn_nonexistent_if = (0 != ival);
    ival2 = 0;
    CHECK(reg_int("want_fork_support", NULL,
                  "Whether fork support is desired or not "
                  "(negative = try to enable fork support, but continue even if it is not available, 0 = do not enable fork support, positive = try to enable fork support and fail if it is not available)",
                  ival2, &ival, 0));
    if (0 != ival) {
        orte_show_help("help-mpi-btl-wv.txt",
                       "ib_fork requested but not supported", true,
                       orte_process_info.nodename);
        return OMPI_ERROR;
    }

    asprintf(&str, "%s/mca-btl-wv-device-params.ini",
             opal_install_dirs.pkgdatadir);
    if (NULL == str) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    CHECK(reg_string("device_param_files", "hca_param_files",
                     "Colon-delimited list of INI-style files that contain device vendor/part-specific parameters (use semicolon for Windows)",
                     str, &mca_btl_wv_component.device_params_file_names, 
                     0));
    free(str);

    CHECK(reg_string("device_type", NULL,
                     "Specify to only use IB or iWARP network adapters "
                     "(infiniband = only use InfiniBand HCAs; iwarp = only use iWARP NICs; all = use any available adapters)",
                     "all", &str, 0));
    if (0 == strcasecmp(str, "ib") ||
        0 == strcasecmp(str, "infiniband")) {
        mca_btl_wv_component.device_type = BTL_WV_DT_IB;
    } else if (0 == strcasecmp(str, "iw") ||
               0 == strcasecmp(str, "iwarp")) {
        mca_btl_wv_component.device_type = BTL_WV_DT_IWARP;
    } else if (0 == strcasecmp(str, "all")) {
        mca_btl_wv_component.device_type = BTL_WV_DT_ALL;
    } else {
        orte_show_help("help-mpi-btl-wv.txt",
                       "ib_fork requested but not supported", true,
                       orte_process_info.nodename);
        return OMPI_ERROR;
    }
    free(str);

    CHECK(reg_int("max_btls", NULL,
                  "Maximum number of device ports to use "
                  "(-1 = use all available, otherwise must be >= 1)",
                  -1, &mca_btl_wv_component.ib_max_btls,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_num", NULL,
                  "Initial size of free lists "
                  "(must be >= 1)",
                  8, &mca_btl_wv_component.ib_free_list_num,
                  REGINT_GE_ONE));
    CHECK(reg_int("free_list_max", NULL,
                  "Maximum size of free lists "
                  "(-1 = infinite, otherwise must be >= 0)",
                  -1, &mca_btl_wv_component.ib_free_list_max,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_inc", NULL,
                  "Increment size of free lists "
                  "(must be >= 1)",
                  32, &mca_btl_wv_component.ib_free_list_inc,
                  REGINT_GE_ONE));
    CHECK(reg_string("mpool", NULL,
                     "Name of the memory pool to be used (it is unlikely that you will ever want to change this)",
                     "rdma", &mca_btl_wv_component.ib_mpool_name,
                     0));
    CHECK(reg_int("reg_mru_len", NULL,
                  "Length of the registration cache most recently used list "
                  "(must be >= 1)",
                  16, (int*) &mca_btl_wv_component.reg_mru_len,
                  REGINT_GE_ONE));

    CHECK(reg_int("cq_size", "ib_cq_size",
                  "Size of the OpenFabrics completion "
                  "queue (will automatically be set to a minimum of "
                  "(2 * number_of_peers * btl_wv_rd_num))",
                  1000, &ival, REGINT_GE_ONE));
    mca_btl_wv_component.ib_cq_size[BTL_WV_LP_CQ] =
        mca_btl_wv_component.ib_cq_size[BTL_WV_HP_CQ] = (uint32_t) ival;

    CHECK(reg_int("max_inline_data", "ib_max_inline_data",
                  "Maximum size of inline data segment "
                  "(-1 = run-time probe to discover max value, otherwise must be >= 0). "
                  "If not explicitly set, use max_inline_data from "
                  "the INI file containing device-specific parameters",
                  -1, &ival, REGINT_NEG_ONE_OK | REGINT_GE_ZERO));
    mca_btl_wv_component.ib_max_inline_data = (int32_t) ival;

    CHECK(reg_string("pkey", "ib_pkey_val", 
                     "OpenFabrics partition key (pkey) value. "
                     "Unsigned integer decimal or hex values are allowed (e.g., \"3\" or \"0x3f\") and will be masked against the maximum allowable IB partition key value (0x7fff)",
                     "0", &pkey, 0));
    mca_btl_wv_component.ib_pkey_val = 
        ompi_btl_wv_ini_intify(pkey) & MCA_BTL_IB_PKEY_MASK;
    free(pkey);

    CHECK(reg_int("psn", "ib_psn",
                  "OpenFabrics packet sequence starting number "
                  "(must be >= 0)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_wv_component.ib_psn = (uint32_t) ival;

    CHECK(reg_int("ib_qp_ous_rd_atom", NULL, 
                  "InfiniBand outstanding atomic reads "
                  "(must be >= 0)",
                  4, &ival, REGINT_GE_ZERO));
    mca_btl_wv_component.ib_qp_ous_rd_atom = (uint32_t) ival;

    asprintf(&msg, "OpenFabrics MTU, in bytes (if not specified in INI files).  Valid values are: %d=256 bytes, %d=512 bytes, %d=1024 bytes, %d=2048 bytes, %d=4096 bytes",
             WV_MTU_256,
             WV_MTU_512,
             WV_MTU_1024,
             WV_MTU_2048,
             WV_MTU_4096);
    if (NULL == msg) {
        /* Don't try to recover from this */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    CHECK(reg_int("mtu", "ib_mtu", msg, WV_MTU_1024, &ival, 0));
    free(msg);
    if (ival < WV_MTU_1024 || ival > WV_MTU_4096) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                       true, "invalid value for btl_wv_ib_mtu",
                       "btl_wv_ib_mtu reset to 1024");
        mca_btl_wv_component.ib_mtu = WV_MTU_1024;
    } else {
        mca_btl_wv_component.ib_mtu = (uint32_t) ival;
    }

    CHECK(reg_int("ib_min_rnr_timer", NULL, "InfiniBand minimum "
                  "\"receiver not ready\" timer, in seconds "
                  "(must be >= 0 and <= 31)",
                  25, &ival, 0));
    if (ival > 31) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                       true, "btl_wv_ib_min_rnr_timer > 31",
                       "btl_wv_ib_min_rnr_timer reset to 31");
        ival = 31;
    } else if (ival < 0){
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                   true, "btl_wv_ib_min_rnr_timer < 0",
                   "btl_wv_ib_min_rnr_timer reset to 0");
        ival = 0;
    }
    mca_btl_wv_component.ib_min_rnr_timer = (uint32_t) ival;

    CHECK(reg_int("ib_timeout", NULL,
                  "InfiniBand transmit timeout, plugged into formula: 4.096 microseconds * (2^btl_wv_ib_timeout) "
                  "(must be >= 0 and <= 31)",
                  20, &ival, 0));
    if (ival > 31) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                       true, "btl_wv_ib_timeout > 31",
                       "btl_wv_ib_timeout reset to 31");
        ival = 31;
    } else if (ival < 0) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                   true, "btl_wv_ib_timeout < 0",
                   "btl_wv_ib_timeout reset to 0");
        ival = 0;
    }
    mca_btl_wv_component.ib_timeout = (uint32_t) ival;

    CHECK(reg_int("ib_retry_count", NULL,
                  "InfiniBand transmit retry count "
                  "(must be >= 0 and <= 7)",
                  7, &ival, 0));
    if (ival > 7) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                       true, "btl_wv_ib_retry_count > 7",
                       "btl_wv_ib_retry_count reset to 7");
        ival = 7;
    } else if (ival < 0) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                   true, "btl_wv_ib_retry_count < 0",
                   "btl_wv_ib_retry_count reset to 0");
        ival = 0;
    }
    mca_btl_wv_component.ib_retry_count = (uint32_t) ival;

    CHECK(reg_int("ib_rnr_retry", NULL,
                  "InfiniBand \"receiver not ready\" "
                  "retry count; applies *only* to SRQ/XRC queues.  PP queues "
                  "use RNR retry values of 0 because Open MPI performs "
                  "software flow control to guarantee that RNRs never occur "
                  "(must be >= 0 and <= 7; 7 = \"infinite\")",
                  7, &ival, 0));
    if (ival > 7) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                       true, "btl_wv_ib_rnr_retry > 7",
                       "btl_wv_ib_rnr_retry reset to 7");
        ival = 7;
    } else if (ival < 0) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                   true, "btl_wv_ib_rnr_retry < 0",
                   "btl_wv_ib_rnr_retry reset to 0");
        ival = 0;
    }
    mca_btl_wv_component.ib_rnr_retry = (uint32_t) ival;

    CHECK(reg_int("ib_service_level", NULL, "InfiniBand service level "
                  "(must be >= 0 and <= 15)",
                  0, &ival, 0));
    if (ival > 15) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                       true, "btl_wv_ib_service_level > 15",
                       "btl_wv_ib_service_level reset to 15");
        ival = 15;
    } else if (ival < 0) {
        orte_show_help("help-mpi-btl-wv.txt", "invalid mca param value",
                   true, "btl_wv_ib_service_level < 0",
                   "btl_wv_ib_service_level reset to 0");
        ival = 0;
    }
    mca_btl_wv_component.ib_service_level = (uint32_t) ival;

    CHECK(reg_int("ib_path_rec_service_level", NULL, "Enable getting InfiniBand service level from PathRecord " 
                  "(must be >= 0, 0 = disabled, positive = try to get the service level from PathRecord)", 
                  0, &ival, REGINT_GE_ZERO)); 
    mca_btl_wv_component.ib_path_rec_service_level = (uint32_t) ival;

    CHECK(reg_int("use_eager_rdma", NULL, "Use RDMA for eager messages "
                  "(-1 = use device default, 0 = do not use eager RDMA, "
                  "1 = use eager RDMA)",
                  -1, &ival, 0));
    mca_btl_wv_component.use_eager_rdma = (int32_t) ival;

    CHECK(reg_int("eager_rdma_threshold", NULL,
                  "Use RDMA for short messages after this number of "
                  "messages are received from a given peer "
                  "(must be >= 1)",
                  16, &ival, REGINT_GE_ONE));
    mca_btl_wv_component.eager_rdma_threshold = (int32_t) ival;

    CHECK(reg_int("max_eager_rdma", NULL, "Maximum number of peers allowed to use "
                  "RDMA for short messages (RDMA is used for all long "
                  "messages, except if explicitly disabled, such as "
                  "with the \"dr\" pml) "
                  "(must be >= 0)",
                  16, &ival, REGINT_GE_ZERO));
    mca_btl_wv_component.max_eager_rdma = (int32_t) ival;

    CHECK(reg_int("eager_rdma_num", NULL, "Number of RDMA buffers to allocate "
                  "for small messages "
                  "(must be >= 1)",
                  16, &ival, REGINT_GE_ONE));
    mca_btl_wv_component.eager_rdma_num = (int32_t) (ival + 1);

    CHECK(reg_int("btls_per_lid", NULL, "Number of BTLs to create for each "
                  "InfiniBand LID "
                  "(must be >= 1)",
                  1, &ival, REGINT_GE_ONE));
    mca_btl_wv_component.btls_per_lid = (uint32_t) ival;

    CHECK(reg_int("max_lmc", NULL, "Maximum number of LIDs to use for each device port "
                  "(must be >= 0, where 0 = use all available)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_wv_component.max_lmc = (uint32_t) ival;

    mca_btl_wv_component.enable_srq_resize = 0;

    CHECK(reg_int("buffer_alignment", NULL,
                  "Preferred communication buffer alignment, in bytes "
                  "(must be > 0 and power of two)",
                  64, &ival, REGINT_GE_ZERO));
    if(ival <= 1 || (ival & (ival - 1))) {
        orte_show_help("help-mpi-btl-wv.txt", "wrong buffer alignment",
                true, ival, orte_process_info.nodename, 64);
        mca_btl_wv_component.buffer_alignment = 64;
    } else {
        mca_btl_wv_component.buffer_alignment = (uint32_t) ival;
    }

    CHECK(reg_int("use_message_coalescing", NULL,
                  "If nonzero, use message coalescing", 1, &ival, 0));
    mca_btl_wv_component.use_message_coalescing = (0 != ival);

    CHECK(reg_int("cq_poll_ratio", NULL,
                  "How often to poll high priority CQ versus low priority CQ",
                  100, &ival, REGINT_GE_ONE));
    mca_btl_wv_component.cq_poll_ratio = (uint32_t)ival;
    CHECK(reg_int("eager_rdma_poll_ratio", NULL,
                  "How often to poll eager RDMA channel versus CQ",
                  100, &ival, REGINT_GE_ONE));
    mca_btl_wv_component.eager_rdma_poll_ratio = (uint32_t)ival;
    CHECK(reg_int("hp_cq_poll_per_progress", NULL,
                  "Max number of completion events to process for each call "
                  "of BTL progress engine",
                  10, &ival, REGINT_GE_ONE));
    mca_btl_wv_component.cq_poll_progress = (uint32_t)ival;

    mca_btl_wv_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_DEFAULT;

    mca_btl_wv_module.super.btl_eager_limit = 12 * 1024;
    mca_btl_wv_module.super.btl_rndv_eager_limit = 12 * 1024;
    mca_btl_wv_module.super.btl_max_send_size = 64 * 1024;
    mca_btl_wv_module.super.btl_rdma_pipeline_send_length = 1024 * 1024;
    mca_btl_wv_module.super.btl_rdma_pipeline_frag_size = 1024 * 1024;
    mca_btl_wv_module.super.btl_min_rdma_pipeline_size = 256 * 1024;
    mca_btl_wv_module.super.btl_flags = MCA_BTL_FLAGS_PUT |
        MCA_BTL_FLAGS_NEED_ACK | MCA_BTL_FLAGS_NEED_CSUM | MCA_BTL_FLAGS_HETEROGENEOUS_RDMA;
#if BTL_WV_FAILOVER_ENABLED
    mca_btl_wv_module.super.btl_flags |= MCA_BTL_FLAGS_FAILOVER_SUPPORT;
#endif
    mca_btl_wv_module.super.btl_bandwidth = 800;
    mca_btl_wv_module.super.btl_latency = 10;
    CHECK(mca_btl_base_param_register(
            &mca_btl_wv_component.super.btl_version,
            &mca_btl_wv_module.super));

    /* setup all the qp stuff */
    mid_qp_size = mca_btl_wv_module.super.btl_eager_limit / 4;
    /* round mid_qp_size to smallest power of two */
    for(i = 31; i > 0; i--) {
        if(!(mid_qp_size & (1<<i))) {
            continue;
        }
        mid_qp_size = (1<<i);
        break;
    }

    if(mid_qp_size <= 128) {
        mid_qp_size = 1024;
    }

    snprintf(default_qps, 100,
            "P,128,256,192,128:S,%u,256,128,32:S,%u,256,128,32:S,%u,256,128,32",
            mid_qp_size,
            (uint32_t)mca_btl_wv_module.super.btl_eager_limit,
            (uint32_t)mca_btl_wv_module.super.btl_max_send_size);

    mca_btl_wv_component.default_recv_qps = strdup(default_qps);
    if(NULL == mca_btl_wv_component.default_recv_qps) {
        BTL_ERROR(("Unable to allocate memory for default receive queues string.\n"));
        return OMPI_ERROR;
    }

    CHECK(reg_string("receive_queues", NULL,
                     "Colon-delimited, comma-delimited list of receive queues: P,4096,8,6,4:P,32768,8,6,4",
                     default_qps, &mca_btl_wv_component.receive_queues, 
                     0));
    mca_btl_wv_component.receive_queues_source = 
        (0 == strcmp(default_qps, 
                     mca_btl_wv_component.receive_queues)) ? 
        BTL_WV_RQ_SOURCE_DEFAULT : BTL_WV_RQ_SOURCE_MCA;

    CHECK(reg_string("if_include", NULL,
                     "Comma-delimited list of devices/ports to be used (e.g. \"mthca0,mthca1:2\"; empty value means to use all ports found).  Mutually exclusive with btl_wv_if_exclude.",
                     NULL, &mca_btl_wv_component.if_include,
                     0));

    CHECK(reg_string("if_exclude", NULL,
                     "Comma-delimited list of device/ports to be excluded (empty value means to not exclude any ports).  Mutually exclusive with btl_wv_if_include.",
                     NULL, &mca_btl_wv_component.if_exclude,
                     0));

    CHECK(reg_string("ipaddr_include", NULL,
                     "Comma-delimited list of IP Addresses to be used (e.g. \"192.168.1.0/24\").  Mutually exclusive with btl_wv_ipaddr_exclude.",
                     NULL, &mca_btl_wv_component.ipaddr_include,
                     0));

    CHECK(reg_string("ipaddr_exclude", NULL,
                     "Comma-delimited list of IP Addresses to be excluded (e.g. \"192.168.1.0/24\").  Mutually exclusive with btl_wv_ipaddr_include.",
                     NULL, &mca_btl_wv_component.ipaddr_exclude,
                     0));

    /* Register any MCA params for the connect pseudo-components */
    if (OMPI_SUCCESS == ret) {
        ret = ompi_btl_wv_connect_base_register();
    }

    return ret;
}

