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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2007 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "btl_openib.h"
#include "btl_openib_mca.h"

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
static inline int reg_string(const char* param_name, const char* param_desc,
                             const char* default_value, char **out_value,
                             int flags)
{
    char *value;
    mca_base_param_reg_string(&mca_btl_openib_component.super.btl_version, 
                              param_name, param_desc, false, false,
                              default_value, &value);

    if (0 != (flags & REGSTR_EMPTY_OK) && 0 == strlen(value)) {
        return OMPI_ERR_BAD_PARAM;
    }
    *out_value = value;
    return OMPI_SUCCESS;
}


/*
 * utility routine for integer parameter registration
 */
static inline int reg_int(const char* param_name, const char* param_desc,
                          int default_value, int *out_value, int flags)
{
    int value;
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version, 
                              param_name, param_desc, false, false,
                              default_value, &value);

    if (0 != (flags & REGINT_NEG_ONE_OK) && -1 == value) {
        *out_value = value;
        return OMPI_SUCCESS;
    }
    if ((0 != (flags & REGINT_GE_ZERO) && value < 0) ||
        (0 != (flags & REGINT_GE_ONE) && value < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == value)) {
        return OMPI_ERR_BAD_PARAM;
    }
    *out_value = value;
    return OMPI_SUCCESS;
}


/*
 * Register and check all MCA parameters
 */
int btl_openib_register_mca_params(void) 
{
    char *msg, *str;
    int ival, ival2, ret, tmp;

    ret = OMPI_SUCCESS;
#define CHECK(expr) \
        tmp = (expr); \
        if (OMPI_SUCCESS != tmp) ret = tmp;

    /* register IB component parameters */
    CHECK(reg_int("verbose", 
                  "Output some verbose OpenIB BTL information "
                  "(0 = no output, nonzero = output)", 0, &ival, 0));
    mca_btl_openib_component.verbose = (0 != ival);
    CHECK(reg_int("warn_no_hca_params_found", 
                  "Warn when no HCA-specific parameters are found in the INI file specified by the btl_openib_hca_param_files MCA parameter (0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_openib_component.warn_no_hca_params_found = (0 != ival);
    CHECK(reg_int("warn_default_gid_prefix", 
                  "Warn when there is more than one active ports and at least one of them connected to the network with only default GID prefix configured (0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_openib_component.warn_default_gid_prefix = (0 != ival);
    CHECK(reg_int("warn_nonexistent_if", 
                  "Warn if non-existent HCAs and/or ports are specified in the btl_openib_if_[in|ex]clude MCA parameters (0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_openib_component.warn_nonexistent_if = (0 != ival);

#ifdef HAVE_IBV_FORK_INIT
    ival2 = -1;
#else
    ival2 = 0;
#endif
    CHECK(reg_int("want_fork_support", 
                  "Whether fork support is desired or not "
                  "(negative = try to enable fork support, but continue even if it is not available, 0 = do not enable fork support, positive = try to enable fork support and fail if it is not available)", 
                  ival2, &ival, 0));
#ifdef HAVE_IBV_FORK_INIT
    mca_btl_openib_component.want_fork_support = ival;
#else
    if (0 != ival) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "ibv_fork requested but not supported", true,
                       orte_system_info.nodename);
        return OMPI_ERROR;
    }
#endif

    asprintf(&str, "%s/mca-btl-openib-hca-params.ini", 
             opal_install_dirs.pkgdatadir);
    if (NULL == str) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
#ifdef HAVE_IBV_FORK_INIT
    ival2 = -1;
#else
    ival2 = 0;
#endif
    CHECK(reg_int("want_fork_support", 
                  "Whether fork support is desired or not "
                  "(negative = try to enable fork support, but continue even if it is not available, 0 = do not enable fork support, positive = try to enable fork support and fail if it is not available)", 
                  ival2, &ival, 0));
#ifdef HAVE_IBV_FORK_INIT
    mca_btl_openib_component.want_fork_support = ival;
#else
    if (0 != ival) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "ibv_fork requested but not supported", true,
                       orte_system_info.nodename);
        return OMPI_ERROR;
    }
#endif

    CHECK(reg_string("hca_param_files",
                     "Colon-delimited list of INI-style files that contain HCA vendor/part-specific parameters",
                     str, &mca_btl_openib_component.hca_params_file_names, 0));
    free(str);
    
    CHECK(reg_int("max_btls", 
                  "Maximum number of HCA ports to use "
                  "(-1 = use all available, otherwise must be >= 1)",
                  -1, &mca_btl_openib_component.ib_max_btls,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_num", 
                  "Intial size of free lists (must be >= 1)", 
                  8, &mca_btl_openib_component.ib_free_list_num,
                  REGINT_GE_ONE));
    CHECK(reg_int("free_list_max",
                  "Maximum size of free lists "
                  "(-1 = infinite, otherwise must be >= 0)",
                  -1, &mca_btl_openib_component.ib_free_list_max,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_inc", 
                  "Increment size of free lists (must be >= 1)",
                  32, &mca_btl_openib_component.ib_free_list_inc,
                  REGINT_GE_ONE));
    CHECK(reg_string("mpool",
                     "Name of the memory pool to be used (it is unlikely that you will ever want to change this", 
                     "rdma", &mca_btl_openib_component.ib_mpool_name,
                     0));
    CHECK(reg_int("reg_mru_len",  
                  "Length of the registration cache most recently used list "
                  "(must be >= 1)", 
                  16, (int*) &mca_btl_openib_component.reg_mru_len,
                  REGINT_GE_ONE)); 
    
    CHECK(reg_int("ib_cq_size", "Size of the IB completion " 
                  "queue (will automatically be set to a minimum of "
                  "(2 * number_of_peers * btl_openib_rd_num))",
                  1000, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.ib_cq_size = (uint32_t) ival;
    
    CHECK(reg_int("ib_sg_list_size", "Size of IB segment list "
                  "(must be >= 1)", 
                  4, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.ib_sg_list_size = (uint32_t) ival;

    CHECK(reg_int("ib_pkey_ix", "InfiniBand pkey index "
                  "(must be >= 0)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_pkey_ix = (uint32_t) ival;

    CHECK(reg_int("ib_pkey_val", "InfiniBand pkey value"
                  "(must be > 0 and < 0xffff)",
                  0, &ival, REGINT_GE_ZERO));
    if (ival > 0xffff) {
        ret = OMPI_ERR_BAD_PARAM;
    }
    mca_btl_openib_component.ib_pkey_val = (uint32_t) ival;

    CHECK(reg_int("ib_psn", "InfiniBand packet sequence starting number "
                  "(must be >= 0)", 
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_psn = (uint32_t) ival;

    CHECK(reg_int("ib_qp_ous_rd_atom", "InfiniBand outstanding atomic reads "
                  "(must be >= 0)",
                  4, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_qp_ous_rd_atom = (uint32_t) ival;
    
    asprintf(&msg, "IB MTU, in bytes (if not specified in INI files).  Valid values are: %d=256 bytes, %d=512 bytes, %d=1024 bytes, %d=2048 bytes, %d=4096 bytes",
             IBV_MTU_256,
             IBV_MTU_512,
             IBV_MTU_1024,
             IBV_MTU_2048,
             IBV_MTU_4096);
    if (NULL == msg) {
        /* Don't try to recover from this */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    CHECK(reg_int("ib_mtu", msg, IBV_MTU_1024, &ival, 0));
    free(msg);
    if (ival < IBV_MTU_1024 || ival > IBV_MTU_4096) {
        ret = OMPI_ERR_BAD_PARAM;
        mca_btl_openib_component.ib_mtu = IBV_MTU_1024;
    } else {
        mca_btl_openib_component.ib_mtu = (uint32_t) ival;
    }

    /* JMS Is this really in seconds?  Is there a max? */
    CHECK(reg_int("ib_min_rnr_timer", "InfiniBand minimum "
                  "\"receiver not ready\" timer, in seconds "
                  "(must be >= 0 and <= 32)",
                  5, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_min_rnr_timer = (uint32_t) ival;

    CHECK(reg_int("ib_timeout", "InfiniBand transmit timeout, plugged into formula: 4.096 microseconds * (2^btl_openib_ib_timeout)"
                  "(must be >= 0 and <= 32)",
                  10, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_timeout = (uint32_t) ival;

    /* JMS What is the difference between these two counts? */
    CHECK(reg_int("ib_retry_count", "InfiniBand transmit retry count "
                  "(must be >= 0 and <= 7)",
                  7, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_retry_count = (uint32_t) ival;

    CHECK(reg_int("ib_rnr_retry", "InfiniBand \"receiver not ready\" "
                  "retry count "
                  "(must be >= 0 and <= 7)", 
                  7, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_rnr_retry = (uint32_t) ival;

    CHECK(reg_int("ib_max_rdma_dst_ops", "InfiniBand maximum pending RDMA "
                  "destination operations "
                  "(must be >= 0)",
                  4, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_max_rdma_dst_ops = (uint32_t) ival;

    /* JMS is there a max? */
    CHECK(reg_int("ib_service_level", "InfiniBand service level "
                  "(must be >= 0)", 
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_service_level = (uint32_t) ival;

    /* JMS what is this? */
    CHECK(reg_int("ib_static_rate", "InfiniBand static rate "
                  "(must be >= 0; default: %d)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_static_rate = (uint32_t) ival;

    CHECK(reg_int("rd_num", "Number of receive descriptors to post to a "
                  "per-peer queue pair (must be >= 1)",
                  8, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.rd_num = (uint32_t) ival;

    CHECK(reg_int("rd_low", "Low water mark before posting additional receive descriptors "
                  "(must be >= 1)",
                  6, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.rd_low = (uint32_t) ival;

    /* JMS meaning what? */
    CHECK(reg_int("rd_win", 
                  "Window size at which generate explicit credit message "
                  "(must be >= 1)",
                  4, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.rd_win = (uint32_t) ival;
    
    /* we only allow one outstanding ctrl message at a time  */
    mca_btl_openib_component.rd_rsv = 1;

    CHECK(reg_int("use_srq", 
                  "If nonzero, use the InfiniBand shared receive "
                  "queue (\"SRQ\")", 
                  0, &ival, 0));
    mca_btl_openib_component.use_srq = (0 != ival);
    CHECK(reg_int("srq_rd_max", "Total number of receive descriptors "
                  "posted per SRQ.  This value is only used if it is larger "
                  "than (rd_num + log2(num_MPI_processes) * srq_rd_per_peer), "
                  "and is only relevant if btl_openib_use_srq is "
                  "true (must be >= 1)",
                  1000, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.srq_rd_max = (uint32_t) ival;

    CHECK(reg_int("srq_rd_per_peer", 
                  "Number of receive descriptors posted per peer in the SRQ "
                  "(only relevant if btl_openib_use_srq is "
                  "true; must be >= 1)",
                  16, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.srq_rd_per_peer = ival;

    CHECK(reg_int("srq_sd_max",
                  "Maximum number of send descriptors posted "
                  "(only relevant if btl_openib_use_srq is "
                  "true; must be >= 1)",
                  8, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.srq_sd_max = (uint32_t) ival;

    CHECK(reg_int("use_eager_rdma", "Use RDMA for eager messages",
                  1, &ival, 0));
    mca_btl_openib_component.use_eager_rdma = (uint32_t) (ival != 0);
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* Fast rdma path isn't supported by PROGRESS_THREAD */
    mca_btl_openib_component.use_eager_rdma = 0;
#endif

    CHECK(reg_int("eager_rdma_threshold", 
                  "Use RDMA for short messages after this number of "
                  "messages are received from a given peer "
                  "(must be >= 1)", 
                  16, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.eager_rdma_threshold = (int32_t) ival;

    CHECK(reg_int("max_eager_rdma", "Maximum number of peers allowed to use "
                  "RDMA for short messages (RDMA is used for all long "
                  "messages, except if explicitly disabled, such as "
                  "with the \"dr\" pml) "
                  "(must be >= 0)",
                  16, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.max_eager_rdma = (int32_t) ival;

    CHECK(reg_int("eager_rdma_num", "Number of RDMA buffers to allocate "
                  "for small messages"
                  "(must be >= 1)",
                  16, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.eager_rdma_num = (uint32_t) (ival + 1);

    CHECK(reg_int("btls_per_lid", "Number of BTLs to create for each "
                  "InfiniBand LID "
                  "(must be >= 1)",
                  1, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.btls_per_lid = (uint32_t) ival;

    CHECK(reg_int("max_lmc", "Maximum number of LIDs to use for each HCA port "
                  "(must be >= 0, where 0 = use all available)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.max_lmc = (uint32_t) ival;

    CHECK(reg_int("buffer_alignment", 
                  "Prefered communication buffer alignment, in bytes "
                  "(must be > 0 and power of two)",
                  64, &ival, REGINT_GE_ZERO));
    if(ival <= 1 || (ival & (ival - 1))) {
        opal_show_help("help-mpi-btl-openib.txt", "wrong buffer alignment",
                true, ival, orte_system_info.nodename, 64);
        mca_btl_openib_component.buffer_alignment = 64;
    } else {
        mca_btl_openib_component.buffer_alignment = (uint32_t) ival;
    }

    /* Info only */

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version, 
                           "have_fork_support", 
                           "Whether the OpenFabrics stack supports applications that invoke the \"fork()\" system call or not (0 = no, 1 = yes).  Note that this value does NOT indicate whether the system being run on supports \"fork()\" with OpenFabrics applications or not.",
                           false, true,
#ifdef HAVE_IBV_FORK_INIT
                           1,
#else
                           0,
#endif
                           NULL);

    mca_btl_openib_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_DEFAULT;
    mca_btl_openib_module.super.btl_eager_limit = 12 * 1024;
    mca_btl_openib_module.super.btl_min_send_size = 32 * 1024;
    mca_btl_openib_module.super.btl_max_send_size = 64 * 1024;
    mca_btl_openib_module.super.btl_rdma_pipeline_send_length = 1024 * 1024;
    mca_btl_openib_module.super.btl_rdma_pipeline_frag_size = 1024 * 1024;    
    mca_btl_openib_module.super.btl_min_rdma_pipeline_size = 256 * 1024;
    mca_btl_openib_module.super.btl_flags = MCA_BTL_FLAGS_RDMA |
        MCA_BTL_FLAGS_NEED_ACK | MCA_BTL_FLAGS_NEED_CSUM;
    mca_btl_openib_module.super.btl_bandwidth = 800;
    mca_btl_openib_module.super.btl_latency = 10;
    mca_btl_base_param_register(&mca_btl_openib_component.super.btl_version,
            &mca_btl_openib_module.super);

    CHECK(reg_string("if_include",
                     "Comma-delimited list of HCAs/ports to be used (e.g. \"mthca0,mthca1:2\"; empty value means to use all ports found).  Mutually exclusive with btl_openib_if_exclude.",
                     NULL, &mca_btl_openib_component.if_include,
                     0));    

    CHECK(reg_string("if_exclude",
                     "Comma-delimited list of HCAs/ports to be excluded (empty value means to not exclude any ports).  Mutually exclusive with btl_openib_if_include.",
                     NULL, &mca_btl_openib_component.if_exclude,
                     0));    

    return ret;
}
