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

#include "bcol_ptpcoll_mca.h"
#include "bcol_ptpcoll.h"

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

#if 0 /* Pasha: we will be need this function in future */
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
    index = mca_base_param_reg_string(&mca_bcol_ptpcoll_component.super.bcol_version,
                                     param_name, param_desc, false, false,
                                      default_value, &value);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index,
                               &mca_bcol_ptpcoll_component.super.bcol_version,
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
#endif

/*
 * utility routine for integer parameter registration
 */
static int reg_int(const char* param_name,
                   const char* deprecated_param_name,
                   const char* param_desc,
                   int default_value, int *out_value, int flags)
{
    int index, value;
    index = mca_base_param_reg_int(&mca_bcol_ptpcoll_component.super.bcol_version,
            param_name, param_desc, false, false,
            default_value, NULL);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index,
                               &mca_bcol_ptpcoll_component.super.bcol_version,
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

int mca_bcol_ptpcoll_register_mca_params(void)
{
    int ival, ret, tmp;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;

    ret = OMPI_SUCCESS;
#define CHECK(expr) do {\
        tmp = (expr); \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    CHECK(reg_int("priority", NULL,
                  "PTPCOLL component priority"
                  "(from 0(low) to 90 (high))", 90, &ival, 0));
    cm->super.priority = ival;
    CHECK(reg_int("verbose", NULL,
                  "Output some verbose PTPCOLL information "
                  "(0 = no output, nonzero = output)", 0, &ival, REGINT_GE_ZERO));
    cm->verbose = ival;
    CHECK(reg_int("k_nomial_radix", NULL,
                  "The radix of K-Nomial Tree "
                  "(starts from 2)", 2, &ival, REGINT_GE_ONE));
    cm->k_nomial_radix = ival;
    CHECK(reg_int("narray_radix", NULL,
                  "The radix of Narray Tree "
                  "(starts from 2)", 2, &ival, REGINT_GE_ONE));
    cm->narray_radix = ival;

    CHECK(reg_int("narray_knomial_radix", NULL,
                  "The radix of Narray/Knomial Tree for scatther-gather type algorithms"
                  "(starts from 2)", 2, &ival, REGINT_GE_ONE));
    cm->narray_knomial_radix = ival;

    CHECK(reg_int("num_to_probe", NULL,
                  "Number of probe operation in single source data check"
                  "(starts from 8)", 8, &ival, REGINT_GE_ONE));
    cm->num_to_probe = ival;

    CHECK(reg_int("bcast_small_msg_known_root_alg", NULL,
                "Algoritm selection for bcast small messages known root"
                "(1 - K-nomial, 2 - N-array)", 1, &ival, REGINT_GE_ZERO));
    cm->bcast_small_messages_known_root_alg = ival;

    CHECK(reg_int("bcast_large_msg_known_root_alg", NULL,
                  "Algoritm selection for bcast large messages known root"
                  "(1 - Binomial scatther-gather, 2 - N-array scather, K-nomial gather)",
                  1, &ival, REGINT_GE_ZERO));
    cm->bcast_large_messages_known_root_alg = ival;

    CHECK(reg_int("barrier_alg", NULL,
                  "Algoritm selection for Barrier"
                  "(1 - Recursive doubling, 2 - Recursive K-ing)",
                  1, &ival, REGINT_GE_ZERO));
    cm->barrier_alg = ival;

    /* register parmeters controlling message fragementation */
    CHECK(reg_int("min_frag_size", NULL,
                "Minimum fragment size",
                getpagesize(), &ival, REGINT_GE_ONE));
    cm->super.min_frag_size=ival;

    CHECK(reg_int("max_frag_size", NULL,
                "Maximum fragment size",
                FRAG_SIZE_NO_LIMIT, &ival, REGINT_NONZERO));
    cm->super.max_frag_size=ival;

    CHECK(reg_int("can_use_user_buffers", NULL,
                "User memory can be used by the collective algorithms",
                1, &ival, REGINT_GE_ZERO));
    cm->super.can_use_user_buffers=ival;

    CHECK(reg_int("use_pipeline", NULL,
                "Pipeline the algorithm",
                1, &ival, REGINT_GE_ZERO));
    cm->super.use_pipeline=ival;

    CHECK(reg_int("use_brucks_smsg_alltoall_rdma", NULL,
                "Use brucks algorithm for smsg alltoall and RDMA semantics 1 = No Temp buffer recycling"
                "1 = Alg with no Temp Buffer Recycling (faster), 2 = Alg with temp Buffer Recycling (slower)",
                0, &ival, 0));
    cm->use_brucks_smsg_alltoall_rdma = ival;

    return ret;
}
