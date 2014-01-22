/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
                      const char* default_value, char **storage,
                      int flags)
{
    int index;

    *storage = default_value;
    index = mca_base_component_var_register(&mca_bcol_ptpcoll_component.super.bcol_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_STRING,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "bcol", "ptpcoll",
                                             deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if (0 != (flags & REGSTR_EMPTY_OK) && (NULL == *storage || 0 == strlen(*storage))) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS;
}
#endif

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
    index = mca_base_component_var_register(&mca_bcol_ptpcoll_component.super.bcol_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_INT,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "bcol", "ptpcoll",
                                             deprecated_param_name,
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

static int reg_bool(const char* param_name,
                    const char* deprecated_param_name,
                    const char* param_desc,
                    bool default_value, bool *storage)
{
    int index;

    *storage = default_value;
    index = mca_base_component_var_register(&mca_bcol_ptpcoll_component.super.bcol_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_BOOL,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (0 > index) {
        return index;
    }

    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "bcol", "ptpcoll",
                                             deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    return OMPI_SUCCESS;
}

int mca_bcol_ptpcoll_register_mca_params(void)
{
    int ret, tmp;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;

    ret = OMPI_SUCCESS;
#define CHECK(expr) do {\
        tmp = (expr); \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    CHECK(reg_int("priority", NULL,
                  "PTPCOLL component priority"
                  "(from 0(low) to 90 (high))", 90, &cm->super.priority, 0));

    CHECK(reg_int("verbose", NULL,
                  "Output some verbose PTPCOLL information "
                  "(0 = no output, nonzero = output)", 0, &cm->verbose, REGINT_GE_ZERO));

    CHECK(reg_int("k_nomial_radix", NULL,
                  "The radix of K-Nomial Tree "
                  "(starts from 2)", 2, &cm->k_nomial_radix, REGINT_GE_ONE));

    CHECK(reg_int("narray_radix", NULL,
                  "The radix of Narray Tree "
                  "(starts from 2)", 2, &cm->narray_radix, REGINT_GE_ONE));

    CHECK(reg_int("narray_knomial_radix", NULL,
                  "The radix of Narray/Knomial Tree for scatther-gather type algorithms"
                  "(starts from 2)", 2, &cm->narray_knomial_radix, REGINT_GE_ONE));

    CHECK(reg_int("num_to_probe", NULL,
                  "Number of probe operation in single source data check"
                  "(starts from 8)", 8, &cm->num_to_probe, REGINT_GE_ONE));

    CHECK(reg_int("bcast_small_msg_known_root_alg", NULL,
                  "Algoritm selection for bcast small messages known root"
                  "(1 - K-nomial, 2 - N-array)", 1, &cm->bcast_small_messages_known_root_alg,
                  REGINT_GE_ZERO));

    CHECK(reg_int("bcast_large_msg_known_root_alg", NULL,
                  "Algoritm selection for bcast large messages known root"
                  "(1 - Binomial scatther-gather, 2 - N-array scather, K-nomial gather)",
                  1, &cm->bcast_large_messages_known_root_alg, REGINT_GE_ZERO));

    CHECK(reg_int("barrier_alg", NULL,
                  "Algoritm selection for Barrier"
                  "(1 - Recursive doubling, 2 - Recursive K-ing)",
                  1, &cm->barrier_alg, REGINT_GE_ZERO));

    /* register parmeters controlling message fragementation */
    CHECK(reg_int("min_frag_size", NULL,
                "Minimum fragment size",
                getpagesize(), &cm->super.min_frag_size, REGINT_GE_ONE));

    CHECK(reg_int("max_frag_size", NULL,
                "Maximum fragment size",
                FRAG_SIZE_NO_LIMIT, &cm->super.max_frag_size, REGINT_NONZERO));

    CHECK(reg_bool("can_use_user_buffers", NULL,
                "User memory can be used by the collective algorithms",
                1, &cm->super.can_use_user_buffers));

    return ret;
}
