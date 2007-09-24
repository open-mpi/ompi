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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
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
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "btl_openib.h"
#include "btl_openib_mca.h"
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

static int mca_btl_openib_mca_setup_qps(void);


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
    mca_btl_openib_component.ib_lp_cq_size =
        mca_btl_openib_component.ib_hp_cq_size = (uint32_t) ival;
    
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
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "invalid value for btl_openib_ib_pkey_val",
                       "btl_openib_ib_pkey_val ignored");
    } else {
        mca_btl_openib_component.ib_pkey_val = (uint32_t) ival;
    }

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
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "invalid value for btl_openib_ib_mtu",
                       "btl_openib_ib_mtu reset to 1024");
        mca_btl_openib_component.ib_mtu = IBV_MTU_1024;
    } else {
        mca_btl_openib_component.ib_mtu = (uint32_t) ival;
    }

    CHECK(reg_int("ib_min_rnr_timer", "InfiniBand minimum "
                  "\"receiver not ready\" timer, in seconds "
                  "(must be >= 0 and <= 31)",
                  5, &ival, 0));
    if (ival > 31) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_min_rnr_timer > 31",
                       "btl_openib_ib_min_rnr_timer reset to 31");
        ival = 31;
    } else if (ival < 0){
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_min_rnr_timer < 0",
                   "btl_openib_ib_min_rnr_timer reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_min_rnr_timer = (uint32_t) ival;

    CHECK(reg_int("ib_timeout", "InfiniBand transmit timeout, plugged into formula: 4.096 microseconds * (2^btl_openib_ib_timeout)"
                  "(must be >= 0 and <= 31)",
                  10, &ival, 0));
    if (ival > 31) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_timeout > 31",
                       "btl_openib_ib_timeout reset to 31");
        ival = 31;
    } else if (ival < 0) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_timeout < 0",
                   "btl_openib_ib_timeout reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_timeout = (uint32_t) ival;

    CHECK(reg_int("ib_retry_count", "InfiniBand transmit retry count "
                  "(must be >= 0 and <= 7)",
                  7, &ival, 0));
    if (ival > 7) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_retry_count > 7",
                       "btl_openib_ib_retry_count reset to 7");
        ival = 7;
    } else if (ival < 0) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_retry_count < 0",
                   "btl_openib_ib_retry_count reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_retry_count = (uint32_t) ival;

    CHECK(reg_int("ib_rnr_retry", "InfiniBand \"receiver not ready\" "
                  "retry count "
                  "(must be >= 0 and <= 7)", 
                  7, &ival, 0));
    if (ival > 7) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_rnr_retry > 7",
                       "btl_openib_ib_rnr_retry reset to 7");
        ival = 7;
    } else if (ival < 0) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_rnr_retry < 0",
                   "btl_openib_ib_rnr_retry reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_rnr_retry = (uint32_t) ival;

    CHECK(reg_int("ib_max_rdma_dst_ops", "InfiniBand maximum pending RDMA "
                  "destination operations "
                  "(must be >= 0)",
                  4, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_max_rdma_dst_ops = (uint32_t) ival;

    CHECK(reg_int("ib_service_level", "InfiniBand service level "
                  "(must be >= 0 and <= 15)", 
                  0, &ival, 0));
    if (ival > 15) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_service_level > 15",
                       "btl_openib_ib_service_level reset to 15");
        ival = 15;
    } else if (ival < 0) {
        opal_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_service_level < 0",
                   "btl_openib_ib_service_level reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_service_level = (uint32_t) ival;

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
    mca_btl_openib_component.eager_rdma_num = (int32_t) (ival + 1);

    CHECK(reg_int("btls_per_lid", "Number of BTLs to create for each "
                  "InfiniBand LID "
                  "(must be >= 1)",
                  1, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.btls_per_lid = (uint32_t) ival;

    CHECK(reg_int("max_lmc", "Maximum number of LIDs to use for each HCA port "
                  "(must be >= 0, where 0 = use all available)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.max_lmc = (uint32_t) ival;

#if OMPI_HAVE_THREADS
    CHECK(reg_int("use_async_event_thread", 
                "If nonzero, use the thread that will handle InfiniBand asyncihronous events ",
                1, &ival, 0));
    mca_btl_openib_component.use_async_event_thread = (0 != ival);
#endif

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
        MCA_BTL_FLAGS_NEED_ACK | MCA_BTL_FLAGS_NEED_CSUM | MCA_BTL_FLAGS_HETEROGENEOUS_RDMA;
    mca_btl_openib_module.super.btl_bandwidth = 800;
    mca_btl_openib_module.super.btl_latency = 10;
    ret = mca_btl_base_param_register(
            &mca_btl_openib_component.super.btl_version,
            &mca_btl_openib_module.super);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* setup all the qp stuff */
    if (OMPI_SUCCESS != (ret = mca_btl_openib_mca_setup_qps())) {
        return ret;
    }

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

static int32_t atoi_param(char *param, int32_t dflt)
{
    if(NULL == param || '\0' == param[0])
        return dflt ? dflt : 1;

    return atoi(param);
}

static int mca_btl_openib_mca_setup_qps(void) 
{ 
    /* All the multi-qp stuff.. */
    char *str;
    char **queues, **params = NULL;
    int num_pp_qps = 0, num_srq_qps = 0, qp = 0, ret = OMPI_ERROR;
    char *default_qps = "P,128,256,128,16:S,1024,256,128,32:S,4096,256,128,32:S,65536,256,128,32";
    uint32_t max_qp_size, max_size_needed;
    int32_t min_freelist_size = 0;
    
    reg_string("receive_queues",
               "Colon-delimited, coma delimited list of receive queues: P,4096,8,6,4:P,32768,8,6,4",
               default_qps, &str, 0);
    queues = opal_argv_split(str, ':');

    if (0 == opal_argv_count(queues)) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "no qps in receive_queues", true,
                       orte_system_info.nodename, str);
        return OMPI_ERROR;
    }

    while (queues[qp] != NULL) {
        if (0 == strncmp("P,", queues[qp], 2)) { 
            num_pp_qps++;   
        } else if (0 == strncmp("S,", queues[qp], 2)) { 
            num_srq_qps++;
        } else {
            opal_show_help("help-mpi-btl-openib.txt",
                           "invalid qp type in receive_queues", true, 
                           orte_system_info.nodename, str, queues[qp]);
            goto error;
        }
        qp++;
    }
    mca_btl_openib_component.num_pp_qps = num_pp_qps;
    mca_btl_openib_component.num_srq_qps = num_srq_qps;
    mca_btl_openib_component.num_qps = num_pp_qps + num_srq_qps;

    mca_btl_openib_component.qp_infos = (mca_btl_openib_qp_info_t*)
        malloc(sizeof(mca_btl_openib_qp_info_t) *
                mca_btl_openib_component.num_qps);
    
    qp = 0;
#define P(N) (((N) > count)?NULL:params[(N)])
    while(queues[qp] != NULL) { 
        uint32_t tmp;
        int i = 0, count;
        params = opal_argv_split_with_empty(queues[qp], ',');
        count = opal_argv_count(params);

        if ('P' == params[0][0]) {
            if (count < 3 || count > 6) {
                opal_show_help("help-mpi-btl-openib.txt",
                               "invalid pp qp specification", true,
                               orte_system_info.nodename, queues[qp]);
                goto error;
            }
            mca_btl_openib_component.qp_infos[qp].size = atoi_param(P(1), 0);
            mca_btl_openib_component.qp_infos[qp].rd_num = atoi_param(P(2), 8);
            tmp = mca_btl_openib_component.qp_infos[qp].rd_num >> 1;
            mca_btl_openib_component.qp_infos[qp].rd_low =
                atoi_param(P(3), tmp);
            tmp = (mca_btl_openib_component.qp_infos[qp].rd_low >> 1);
            mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_win =
                atoi_param(P(4), tmp);
            tmp = ((mca_btl_openib_component.qp_infos[qp].rd_num << 1) - 1)/
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_win;
            mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv =
                atoi_param(P(5), tmp);
            opal_output(mca_btl_base_output, "pp: rd_num is %d\trd_low is %d\trd_win %d\trd_rsv %d \n", 
                        mca_btl_openib_component.qp_infos[qp].rd_num, 
                        mca_btl_openib_component.qp_infos[qp].rd_low,
                        mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_win, 
                        mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv
                        );
            
            mca_btl_openib_component.qp_infos[qp].type = MCA_BTL_OPENIB_PP_QP;

            /* Calculate the smallest freelist size that can be allowed */
            if (mca_btl_openib_component.qp_infos[qp].rd_num +
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv >
                min_freelist_size) {
                min_freelist_size = 
                    mca_btl_openib_component.qp_infos[qp].rd_num +
                    mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
            }
        } else if(params[0][0] =='S') { 
            if(count < 3 || count > 5) {
                opal_show_help("help-mpi-btl-openib.txt",
                               "invalid srq specification", true,
                               orte_system_info.nodename, queues[qp]);
                goto error;
            }
            mca_btl_openib_component.qp_infos[qp].size = atoi_param(P(1), 0);
            mca_btl_openib_component.qp_infos[qp].rd_num = atoi_param(P(2), 16);
            tmp = mca_btl_openib_component.qp_infos[qp].rd_num >> 1;
            mca_btl_openib_component.qp_infos[qp].rd_low =
                atoi_param(P(3), tmp);
            tmp = mca_btl_openib_component.qp_infos[qp].rd_low >> 2;
            mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max =
                atoi_param(P(4), tmp);
            opal_output(mca_btl_base_output, "srq: rd_num is %d\trd_low is %d\tsd_max is %d\n", 
                        mca_btl_openib_component.qp_infos[qp].rd_num, 
                        mca_btl_openib_component.qp_infos[qp].rd_low,
                        mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max);
            mca_btl_openib_component.qp_infos[qp].type = MCA_BTL_OPENIB_SRQ_QP;

            /* Calculate the smallest freelist size that can be allowed */
            if (mca_btl_openib_component.qp_infos[qp].rd_num >
                min_freelist_size) {
                min_freelist_size = 
                    mca_btl_openib_component.qp_infos[qp].rd_num;
            }
        }

        if (mca_btl_openib_component.qp_infos[qp].rd_num <=
            mca_btl_openib_component.qp_infos[qp].rd_low) {
            opal_show_help("help-mpi-btl-openib.txt",
                           "rd_num must be > rd_low", true,
                           orte_system_info.nodename, queues[qp]);
            goto error;
        }
        while (NULL != params[i]) {
            free(params[i++]);
        }
        free(params);
        qp++;
    }
    params = NULL;
   
    /* Sanity check some sizes */

    max_qp_size = mca_btl_openib_component.qp_infos[mca_btl_openib_component.num_qps - 1].size;
    max_size_needed = (mca_btl_openib_module.super.btl_eager_limit >
                       mca_btl_openib_module.super.btl_max_send_size) ?
        mca_btl_openib_module.super.btl_eager_limit :
        mca_btl_openib_module.super.btl_max_send_size;
    if (max_qp_size < max_size_needed) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "biggest qp size is too small", true,
                       orte_system_info.nodename, max_qp_size, 
                       max_size_needed);
        ret = OMPI_ERROR;
        goto error;
    } else if (max_qp_size > max_size_needed) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "biggest qp size is too big", true,
                       orte_system_info.nodename, max_qp_size, 
                       max_size_needed);
        opal_output(0, "The biggest QP size is bigger than maximum send size. "
                "This is not optimal configuration as memory will be waisted.\n");
    }

    if (mca_btl_openib_component.ib_free_list_max > 0 &&
        min_freelist_size > mca_btl_openib_component.ib_free_list_max) {
        opal_show_help("help-mpi-btl-openib.txt", "freelist too small", true,
                       orte_system_info.nodename, 
                       mca_btl_openib_component.ib_free_list_max,
                       min_freelist_size);
        goto error;
    }
    
    mca_btl_openib_component.rdma_qp = mca_btl_openib_component.num_qps - 1;
    mca_btl_openib_component.eager_rdma_qp = 0;

    /* Register any MCA params for the connect pseudo-components */

    ompi_btl_openib_connect_base_open();

    ret = MPI_SUCCESS;
error:
    if(params) {
        qp = 0;
        while(params[qp] != NULL)
            free(params[qp++]);
        free(params);
    }

    if(queues) {
        qp = 0;
        while(queues[qp] != NULL)
            free(queues[qp++]);
        free(queues);
    }

    return ret;
}

