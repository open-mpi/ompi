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
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "coll_ml.h"
#include "coll_ml_mca.h"
#include "coll_ml_lmngr.h"
#include "ompi/mca/common/netpatterns/common_netpatterns.h"
#include "opal/mca/installdirs/installdirs.h"

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

    index = mca_base_param_reg_string(&mca_coll_ml_component.super.collm_version,
                                      param_name, param_desc, false, false,
                                      default_value, &value);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index,
                               &mca_coll_ml_component.super.collm_version,
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
int reg_int(const char* param_name,
        const char* deprecated_param_name,
        const char* param_desc,
        int default_value, int *out_value, int flags)
{
    int index, value;
    index = mca_base_param_reg_int(&mca_coll_ml_component.super.collm_version,
            param_name, param_desc, false, false,
            default_value, NULL);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index,
                               &mca_coll_ml_component.super.collm_version,
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

int mca_coll_ml_register_params(void)
{
    int ival, ret, tmp, dummy;
    char *str = NULL;

    ret = OMPI_SUCCESS;
#define CHECK(expr) do {                    \
        tmp = (expr);                       \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    /* register openib component parameters */

    CHECK(reg_int("priority", NULL,
                  "ML component priority"
                  "(from 0(low) to 90 (high))", 0, &ival, 0));
    mca_coll_ml_component.ml_priority = ival;

    CHECK(reg_int("verbose", NULL,
                  "Output some verbose ML information "
                  "(0 = no output, nonzero = output)", 0, &ival, 0));
    mca_coll_ml_component.verbose = ival;

    CHECK(reg_int("n_levels", NULL,
                  "number of levels in the hierarchy ", 1, &ival, 0));
    mca_coll_ml_component.ml_n_levels = ival;

    CHECK(reg_int("max_comm", NULL,
                  "max of communicators available to run ML", 12, &ival, 0));
    mca_coll_ml_component.max_comm = ival;

    CHECK(reg_int("min_comm_size", NULL,
                  " min size of comm to be available to run ML", 0, &ival, 0));
    mca_coll_ml_component.min_comm_size = ival;

    CHECK(reg_int("n_payload_mem_banks", NULL,
                " number of payload memory banks", 2, &ival, 0));
    mca_coll_ml_component.n_payload_mem_banks = ival;

    /* Make sure that the the number of memory banks is a power of 2 */
    mca_coll_ml_component.n_payload_mem_banks =
        roundup_to_power_radix(2, mca_coll_ml_component.n_payload_mem_banks,
                &dummy);

    CHECK(reg_int("n_payload_buffs_per_bank", NULL,
                " number of payload buffers per bank", 16, &ival, 0));
    mca_coll_ml_component.n_payload_buffs_per_bank = ival;

    /* Make sure that the the number of buffers is a power of 2 */
    mca_coll_ml_component.n_payload_buffs_per_bank =
        roundup_to_power_radix(2, mca_coll_ml_component.n_payload_buffs_per_bank,
                &dummy);

    /* RLG: need to handle alignment and size */
    CHECK(reg_int("payload_buffer_size", NULL,
                  " size of payload buffer", 4*1024, &ival, 0));
    mca_coll_ml_component.payload_buffer_size = (size_t) ival;

    /* get the pipeline depth, default is 2 */
    CHECK(reg_int("pipeline_depth", NULL,
                  "size of fragmentation pipeline", 2, &ival, 0));
    mca_coll_ml_component.pipeline_depth = (int) ival;

    CHECK(reg_int("free_list_init_size", NULL,
                  " Initial size for free lists in ML", 128, &ival, 0));
    mca_coll_ml_component.free_list_init_size = (size_t) ival;

    CHECK(reg_int("free_list_grow_size", NULL,
                  " Initial size for free lists in ML", 64, &ival, 0));
    mca_coll_ml_component.free_list_grow_size = (size_t) ival;

    CHECK(reg_int("free_list_max_size", NULL,
                  " Initial size for free lists in ML", -1, &ival, 0));
    mca_coll_ml_component.free_list_max_size = (size_t) ival;

    CHECK(reg_int("use_knomial_allreduce", NULL,
                "Use k-nomial Allreduce supports only p2p currently"
                , 1, &ival, 0));
    mca_coll_ml_component.use_knomial_allreduce = ival;

    CHECK(reg_int("use_static_bcast", NULL,
                "Use new bcast static algorithm", 1, &ival, 0));
    mca_coll_ml_component.use_static_bcast = (0 != ival);

    CHECK(reg_int("use_sequential_bcast", NULL,
                  "Use new bcast static algorithm", 0, &ival, 0));
    mca_coll_ml_component.use_sequential_bcast = (0 != ival);

    CHECK(reg_int("disable_allgather", NULL,
                  "Allgather disabling",
                   0, &ival, 0));
    mca_coll_ml_component.disable_allgather = (0 != ival);

    CHECK(reg_int("disable_alltoall", NULL,
                  "Alltoall disabling",
                   0, &ival, 0));
    mca_coll_ml_component.disable_alltoall = (0 != ival);

    CHECK(reg_int("enable_fragmentation", NULL,
                "Disable/Enable fragmentation for large messages"
                , 0, &ival, 0));
    mca_coll_ml_component.enable_fragmentation = (0 != ival);

    CHECK(reg_int("use_brucks_smsg_alltoall", NULL,
                "Use Bruck's Algo for Small Msg Alltoall"
                "1 - Bruck's Algo with RDMA; 2 - Bruck's with Send Recv"
                , 0, &ival, 0));
    mca_coll_ml_component.use_brucks_smsg_alltoall = ival;

    asprintf(&str, "%s/mca-coll-ml.config",
            opal_install_dirs.pkgdatadir);
    if (NULL == str) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    CHECK(reg_string("config_file", NULL,
                "ML collectives configuration file",
                str, &mca_coll_ml_component.config_file_name,
                0));
    free(str);

    /* Reading parameters for list manager */
    CHECK(mca_coll_ml_lmngr_reg());

    return ret;
}
