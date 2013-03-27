/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>

#include "bcol_iboffload.h"
#include "bcol_iboffload_mca.h"

#include "ompi/constants.h"
#include "ompi/mca/common/ofacm/base.h"
#include "ompi/communicator/communicator.h"

#include "opal/util/show_help.h"

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

mca_base_var_enum_value_t mtu_values[] = {
    {IBV_MTU_256, "256B"},
    {IBV_MTU_512, "512B"},
    {IBV_MTU_1024, "1k"},
    {IBV_MTU_4096, "4k"},
    {0, NULL}
};

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

    *storage = default_value;
    index = mca_base_component_var_register(&mca_bcol_iboffload_component.super.bcol_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_STRING,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "bcol", "iboffload", deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if (0 != (flags & REGSTR_EMPTY_OK) && 0 == strlen(*storage)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS;
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
    index = mca_base_component_var_register(&mca_bcol_iboffload_component.super.bcol_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_INT,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "bcol", "iboffload", deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if (0 != (flags & REGINT_NEG_ONE_OK) && -1 == *storage) {
        return OMPI_SUCCESS;
    }

    if ((0 != (flags & REGINT_GE_ZERO) && *storage < 0) ||
        (0 != (flags & REGINT_GE_ONE) && *storage < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == *storage)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS;
}

int mca_bcol_iboffload_verify_params(void)
{
    if (mca_bcol_iboffload_component.min_rnr_timer > 31) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                       true, "bcol_iboffload_ib_min_rnr_timer > 31",
                       "bcol_iboffload_ib_min_rnr_timer reset to 31");
        mca_bcol_iboffload_component.min_rnr_timer = 31;
    } else if (ival < 0){
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                   true, "bcol_iboffload_ib_min_rnr_timer < 0",
                   "bcol_iboffload_ib_min_rnr_timer reset to 0");
        mca_bcol_iboffload_component.min_rnr_timer = 0;
    }

    if (mca_bcol_iboffload_component.timeout > 31) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                       true, "bcol_iboffload_ib_timeout > 31",
                       "bcol_iboffload_ib_timeout reset to 31");
        mca_bcol_iboffload_component.timeout = 31;
    } else if (ival < 0) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                   true, "bcol_iboffload_ib_timeout < 0",
                   "bcol_iboffload_ib_timeout reset to 0");
        mca_bcol_iboffload_component.timeout = 0;
    }

    if (mca_bcol_iboffload_component.retry_count > 7) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                       true, "bcol_iboffload_ib_retry_count > 7",
                       "bcol_iboffload_ib_retry_count reset to 7");
        mca_bcol_iboffload_component.retry_count = 7;
    } else if (mca_bcol_iboffload_component.retry_count < 0) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                   true, "bcol_iboffload_ib_retry_count < 0",
                   "bcol_iboffload_ib_retry_count reset to 0");
        mca_bcol_iboffload_component.retry_count = 0;
    }

    if (mca_bcol_iboffload_component.max_rdma_dst_ops > 7) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                       true, "bcol_iboffload_ib_rnr_retry > 7",
                       "bcol_iboffload_ib_rnr_retry reset to 7");
        mca_bcol_iboffload_component.max_rdma_dst_ops = 7;
    } else if (mca_bcol_iboffload_component.max_rdma_dst_ops < 0) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                   true, "bcol_iboffload_ib_rnr_retry < 0",
                   "bcol_iboffload_ib_rnr_retry reset to 0");
        mca_bcol_iboffload_component.max_rdma_dst_ops = 0;
    }

    if (mca_bcol_iboffload_component.service_level > 15) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                       true, "bcol_iboffload_ib_service_level > 15",
                       "bcol_iboffload_ib_service_level reset to 15");
        mca_bcol_iboffload_component.service_level = 15;
    } else if (mca_bcol_iboffload_component.service_level < 0) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                   true, "bcol_iboffload_ib_service_level < 0",
                   "bcol_iboffload_ib_service_level reset to 0");
        mca_bcol_iboffload_component.service_level = 0;
    }
 
    if(mca_bcol_iboffload_component.buffer_alignment <= 1 ||
       (mca_bcol_iboffload_component.buffer_alignment & (mca_bcol_iboffload_component.buffer_alignment - 1))) {
        opal_show_help("help-mpi-bcol-iboffload.txt", "wrong buffer alignment",
                true, ival, ompi_process_info.nodename, 64);
        mca_bcol_iboffload_component.buffer_alignment = 64;
    }

    return OMPI_SUCCESS;
}

int mca_bcol_iboffload_register_params(void)
{
    char *msg;
    int ret = OMPI_SUCCESS, tmp;

#define CHECK(expr) do {                    \
        tmp = (expr);                       \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    /* register openib component parameters */
    CHECK(reg_int("k_nomial_radix", NULL,
                  "The radix of the K-nomial tree for scatther-gather type algorithms"
                  "(starts from 2)", 2, &mca_bcol_iboffload_component.k_nomial_radix,
                  REGINT_GE_ONE));

    CHECK(reg_int("priority", NULL,
                  "IB offload component priority"
                  "(from 0(low) to 90 (high))", 90,
                  &mca_bcol_iboffload_component.super.priority, 0));

    CHECK(reg_int("verbose", NULL,
                  "Output some verbose IB offload BTL information "
                  "(0 = no output, nonzero = output)", 0,
                  &mca_bcol_iboffload_component.verbose, 0));

    CHECK(reg_bool("warn_default_gid_prefix", NULL,
                   "Warn when there is more than one active ports and at least one of them connected to the network with only default GID prefix configured (0 = do not warn; any other value = warn)",
                   true, &mca_bcol_iboffload_component.warn_default_gid_prefix, 0));

    CHECK(reg_bool("warn_nonexistent_if", NULL,
                   "Warn if non-existent devices and/or ports are specified in the bcol_iboffla_if_[in|ex]clude MCA parameters (0 = do not warn; any other value = warn)",
                   true, &mca_bcol_iboffload_component.warn_nonexistent_if, 0));

    CHECK(reg_int("max_pipeline_depth", NULL,
                  "The maximal number of fragments of the same collective request that can be transferred in parallel", 3,
                  &mca_bcol_iboffload_component.max_pipeline_depth, 0));

    CHECK(reg_int("max_mqe_tasks", NULL,
                  "Maximum number of MQEs for each iboffload module",
                  1024, &mca_bcol_iboffload_component.max_mqe_tasks, 0));
    CHECK(reg_int("max_mq_size", NULL,
                  "Maximum size of each MQ for each iboffload module",
                  1024, &mca_bcol_iboffload_component.max_mq_size, 0));
    CHECK(reg_int("free_list_num", NULL,
                  "Intial size of free lists (must be >= 1)",
                  256, &mca_bcol_iboffload_component.free_list_num,
                  REGINT_GE_ONE));
    CHECK(reg_int("free_list_max", NULL,
                  "Maximum size of free lists "
                  "(-1 = infinite, otherwise must be >= 0)",
                  -1, &mca_bcol_iboffload_component.free_list_max,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_inc", NULL,
                  "Increment size of free lists (must be >= 1)",
                  32, &mca_bcol_iboffload_component.free_list_inc,
                  REGINT_GE_ONE));
    CHECK(reg_string("mpool", NULL,
                     "Name of the memory pool to be used (it is unlikely that you will ever want to change this",
                     "rdma", &mca_bcol_iboffload_component.mpool_name,
                     0));
    CHECK(reg_int("cq_size", "cq_size",
                  "Size of the OpenFabrics completion "
                  "queue (will automatically be set to a minimum of "
                  "(2 * number_of_peers * bcol_iboffload_rd_num))",
                  1024, &mca_bcol_iboffload_component.cq_size, REGINT_GE_ONE));

    CHECK(reg_int("exchange_tree_order", NULL,
                  "The order of the exchange tree. "
                  "Must be power of two.",
                   2, &mca_bcol_iboffload_component.exchange_tree_order, REGINT_GE_ONE));

    CHECK(reg_int("knomial_tree_order", NULL,
                  "The order of the knomial exchange tree. ",
                   3, &mca_bcol_iboffload_component.knomial_tree_order, REGINT_GE_ONE));


    CHECK(reg_int("max_inline_data", "max_inline_data",
                  "Maximum size of inline data segment "
                  "(-1 = run-time probe to discover max value, "
                  "otherwise must be >= 0). "
                  "If not explicitly set, use max_inline_data from "
                  "the INI file containing device-specific parameters",
                  128, &mca_bcol_iboffload_component.max_inline_data,
                  REGINT_NEG_ONE_OK | REGINT_GE_ZERO));

#if 0
    CHECK(reg_string("pkey", "ib_pkey_val",
                     "OpenFabrics partition key (pkey) value. "
                     "Unsigned integer decimal or hex values are allowed (e.g., \"3\" or \"0x3f\") and will be masked against the maximum allowable IB paritition key value (0x7fff)",
                     "0", &pkey, 0));
    /* Pasha
    mca_bcol_iboffload_component.pkey_val =
        ompi_btl_openib_ini_intify(pkey) & MCA_BTL_IB_PKEY_MASK;
    free(pkey);
    */
#endif

    CHECK(reg_string("receive_queues", NULL,
                     "Colon-delimited, comma delimited list of receive queues: P,4096,8,6,4:P,32768,8,6,4",
                     "P,512,256,192,128", &mca_bcol_iboffload_component.receive_queues,
                     0));

    CHECK(reg_int("qp_ous_rd_atom", NULL,
                  "InfiniBand outstanding atomic reads "
                  "(must be >= 0)",
                  4, &ival, REGINT_GE_ZERO));
    mca_bcol_iboffload_component.qp_ous_rd_atom = (uint32_t) ival;

    asprintf(&msg, "OpenFabrics MTU, in bytes (if not specified in INI files).  Valid values are: %d=256 bytes, %d=512 bytes, %d=1024 bytes, %d=2048 bytes, %d=4096 bytes",
             IBV_MTU_256,
             IBV_MTU_512,
             IBV_MTU_1024,
             IBV_MTU_2048,
             IBV_MTU_4096);
    if (NULL == msg) {
        /* Don't try to recover from this */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    CHECK(mca_base_var_enum_create("infiniband mtu", mtu_values, &new_enum));
    mca_bcol_iboffload_component.mtu = IBV_MTU_1024;
    tmp = mca_base_component_var_register(&mca_bcol_iboffload_component.super.bcol_version,
                                          "mtu", MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_bcol_iboffload_component.mtu);
    OBJ_RELEASE(new_enum);
    free(msg);

    if (0 > tmp) ret = tmp;

    tmp = mca_base_var_register_synonym(tmp, "ompi", "bcol", "iboffload", "ib_mtu",
                                        MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    if (0 > tmp) ret = tmp;

    CHECK(reg_int("ib_min_rnr_timer", NULL, "InfiniBand minimum "
                  "\"receiver not ready\" timer, in seconds "
                  "(must be >= 0 and <= 31)",
                  1 , &mca_bcol_iboffload_component.min_rnr_timer, 0));

    CHECK(reg_int("ib_timeout", NULL, "InfiniBand transmit timeout, plugged into formula: 4.096 microseconds * "
                  "(2^bcol_iboffload_ib_timeout) (must be >= 0 and <= 31)",
                  20, &mca_bcol_iboffload_component.timeout, 0));

    CHECK(reg_int("ib_retry_count", NULL, "InfiniBand transmit retry count "
                  "(must be >= 0 and <= 7)",
                  7, &mca_bcol_iboffload_component.retry_count, 0));

    CHECK(reg_int("ib_rnr_retry", NULL, "InfiniBand \"receiver not ready\" "
                  "retry count; applies *only* to SRQ/XRC queues.  PP queues "
                  "use RNR retry values of 0 because Open MPI performs "
                  "software flow control to guarantee that RNRs never occur "
                  "(must be >= 0 and <= 7; 7 = \"infinite\")",
                  7, &mca_bcol_iboffload_component.rnr_retry, 0));

    CHECK(reg_int("ib_max_rdma_dst_ops", NULL, "InfiniBand maximum pending RDMA "
                  "destination operations "
                  "(must be >= 0)",
                  4, &mca_bcol_iboffload_component.max_rdma_dst_ops, REGINT_GE_ZERO));

    CHECK(reg_int("ib_service_level", NULL, "InfiniBand service level "
                  "(must be >= 0 and <= 15)",
                  0, &mca_bcol_iboffload_component.service_level, 0));

    CHECK(reg_int("buffer_alignment", NULL,
                  "Prefered communication buffer alignment, in bytes "
                  "(must be > 0 and power of two)",
                  64, &mca_bcol_iboffload_component.buffer_alignment, REGINT_GE_ZERO));

    /* register parmeters controlling message fragementation */
    CHECK(reg_int("min_frag_size", NULL,
                  "Minimum fragment size",
                  getpagesize(), &mca_bcol_iboffload_component.super.min_frag_size,
                  REGINT_GE_ONE));

    CHECK(reg_int("max_frag_size", NULL,
                  "Maximum fragment size",
                  FRAG_SIZE_NO_LIMIT, &mca_bcol_iboffload_component.super.max_frag_size,
                  REGINT_NONZERO));

    CHECK(reg_bool("can_use_user_buffers", NULL,
                   "User memory can be used by the collective algorithms",
                   true, &mca_bcol_iboffload_component.super.can_use_user_buffers));

    CHECK(reg_int("barrier_mode", NULL,
                "Barrier mode: 0 - Recursive doubling; 1 - Recursive K-ing",
                0, &mca_bcol_iboffload_component.barrier_mode, REGINT_GE_ZERO));

    CHECK(reg_int("max_progress_pull", NULL,
                "Max number of progress pull checks",
                8, &mca_bcol_iboffload_component.max_progress_pull, REGINT_GE_ZERO));

    CHECK(reg_int("use_brucks_smsg_alltoall_rdma", NULL,
                "Use brucks algorithm for smsg alltoall and RDMA semantics 1 = No Temp buffer recycling"
                "1 = Alg with no Temp Buffer Recycling (faster), 2 = Alg with temp Buffer Recycling (slower)",
                0, &mca_bcol_iboffload_component.use_brucks_smsg_alltoall_rdma, 0));

    CHECK(reg_int("use_brucks_smsg_alltoall_sr", NULL,
                "Use brucks algorithm for smsg alltoall and Send/Recv semantics "
                "1 = Alg with RTR (faster), 2 = Alg with RNR (slower)",
                0, &mca_bcol_iboffload_component.use_brucks_smsg_alltoall_sr, 0));

    CHECK(reg_int("alltoall_bruck_radix", NULL,
                "Radix for Bruck algorithm for smsg alltoall",
                3, &mca_bcol_iboffload_component.k_alltoall_bruck_radix, 0));

    CHECK(reg_int("k_alltoall_bruck_radix", NULL,
                "Temp Buffer alignment for Bruck algorithm for smsg alltoall",
                64, &mca_bcol_iboffload_component.tmp_buf_alignment, 0));

    /*
    CHECK(reg_string("if_include", NULL,
                     "Comma-delimited list of devices/ports to be used (e.g. \"mthca0,mthca1:2\"; empty value means to use all ports found).  Mutually exclusive with bcol_iboffload_if_exclude.",
                     NULL, &mca_bcol_iboffload_component.if_include,
                     0));

    CHECK(reg_string("if_exclude", NULL,
                     "Comma-delimited list of device/ports to be excluded (empty value means to not exclude any ports).  Mutually exclusive with bcol_iboffload_if_include.",
                     NULL, &mca_bcol_iboffload_component.if_exclude,
                     0));
    */

    CHECK(mca_bcol_iboffload_verify_params());

    /* Register any MCA params for the connect pseudo-components */
    if (OMPI_SUCCESS == ret) {
        ret = ompi_common_ofacm_base_register(&mca_bcol_iboffload_component.super.bcol_version);
    }

    return ret;
}
