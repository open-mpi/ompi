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

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "coll_ml.h"
#include "coll_ml_inlines.h"
#include "coll_ml_mca.h"
#include "coll_ml_lmngr.h"
#include "ompi/patterns/net/netpatterns.h"
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
 * Enumerators
 */
mca_base_var_enum_value_t fragmentation_enable_enum[] = {
    {0, "disable"},
    {1, "enable"},
    {2, "auto"},
    {-1, NULL}
};

mca_base_var_enum_value_t bcast_algorithms[] = {
    {COLL_ML_STATIC_BCAST, "static"},
    {COLL_ML_SEQ_BCAST, "sequential"},
    {COLL_ML_UNKNOWN_BCAST, "unknown-root"},
    {-1, NULL}
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

    *storage = (char *) default_value;
    index = mca_base_component_var_register(&mca_coll_ml_component.super.collm_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_STRING,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "coll", "ml", deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if (0 != (flags & REGSTR_EMPTY_OK) && (NULL == *storage || 0 == strlen(*storage))) {
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
    index = mca_base_component_var_register(&mca_coll_ml_component.super.collm_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_INT,
                                            NULL, 0, 0,OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "coll", "ml", deprecated_param_name,
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
    index = mca_base_component_var_register(&mca_coll_ml_component.super.collm_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_BOOL,
                                            NULL, 0, 0,OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "coll", "ml", deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    return OMPI_SUCCESS;
}

static int reg_ullint(const char* param_name,
        const char* deprecated_param_name,
        const char* param_desc,
        unsigned long long default_value, unsigned long long *storage, int flags)
{
    int index;

    *storage = default_value;
    index = mca_base_component_var_register(&mca_coll_ml_component.super.collm_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG,
                                            NULL, 0, 0,OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "coll", "ml", deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if ((0 != (flags & REGINT_GE_ONE) && *storage < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == *storage)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS;
}

static int mca_coll_ml_verify_params(void)
{
    int dummy;

    /* Make sure that the the number of memory banks is a power of 2 */
    mca_coll_ml_component.n_payload_mem_banks =
        roundup_to_power_radix(2, mca_coll_ml_component.n_payload_mem_banks,
                &dummy);

    /* Make sure that the the number of buffers is a power of 2 */
    mca_coll_ml_component.n_payload_buffs_per_bank =
        roundup_to_power_radix(2, mca_coll_ml_component.n_payload_buffs_per_bank,
                &dummy);

    return OMPI_SUCCESS;
}

int mca_coll_ml_register_params(void)
{
    mca_base_var_enum_t *new_enum;
    int ret, tmp;
    char *str = NULL;

    ret = OMPI_SUCCESS;
#define CHECK(expr) do {                    \
        tmp = (expr);                       \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    /* register openib component parameters */

    /*** RHC: per telecon agreement, temporarily set the default
     *** component priority to 0 so this won't be selected by default
     */
    CHECK(reg_int("priority", NULL, "ML component priority"
                  "(from 0(low) to 90 (high))", 0, &mca_coll_ml_component.ml_priority, 0));

    CHECK(reg_int("verbose", NULL, "Output some verbose ML information "
                  "(0 = no output, nonzero = output)", 0, &mca_coll_ml_component.verbose, 0));

    CHECK(reg_int("max_comm", NULL, "Maximum number of communicators that can use coll/ml", 24,
                  (int *) &mca_coll_ml_component.max_comm, 0));

    CHECK(reg_int("min_comm_size", NULL, "Minimum size of communicator to use coll/ml", 0,
                  &mca_coll_ml_component.min_comm_size, 0));

    CHECK(reg_int("n_payload_mem_banks", NULL, "Number of payload memory banks", 2,
                  &mca_coll_ml_component.n_payload_mem_banks, 0));

    CHECK(reg_int("n_payload_buffs_per_bank", NULL, "Number of payload buffers per bank", 16,
                  &mca_coll_ml_component.n_payload_buffs_per_bank, 0));

    /* RLG: need to handle alignment and size */
    CHECK(reg_ullint("payload_buffer_size", NULL, "Size of payload buffers", 4*1024,
                     &mca_coll_ml_component.payload_buffer_size, 0));

    /* get the pipeline depth, default is 2 */
    CHECK(reg_int("pipeline_depth", NULL, "Size of fragmentation pipeline", 2,
                  &mca_coll_ml_component.pipeline_depth, 0));

    CHECK(reg_int("free_list_init_size", NULL, "Initial size of free lists in coll/ml", 128,
                  &mca_coll_ml_component.free_list_init_size, 0));

    CHECK(reg_int("free_list_grow_size", NULL, "Initial size of free lists in coll/ml", 64,
                  &mca_coll_ml_component.free_list_grow_size, 0));

    CHECK(reg_int("free_list_max_size", NULL, "Initial size of free lists in coll/ml", -1,
                  &mca_coll_ml_component.free_list_max_size, 0));

    mca_coll_ml_component.use_knomial_allreduce = 1;

    tmp = mca_base_var_enum_create ("coll_ml_bcast_algorithm", bcast_algorithms, &new_enum);
    if (OPAL_SUCCESS != tmp) {
        return tmp;
    }

    mca_coll_ml_component.bcast_algorithm = COLL_ML_STATIC_BCAST;
    tmp = mca_base_component_var_register (&mca_coll_ml_component.super.collm_version, "bcast_algorithm",
                                           "Algorithm to use for broadcast", MCA_BASE_VAR_TYPE_INT,
                                           new_enum, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_ml_component.bcast_algorithm);
    OBJ_RELEASE(new_enum);
    if (0 > tmp) {
        ret = tmp;
    }

    CHECK(reg_bool("disable_allgather", NULL, "Disable Allgather", false,
                   &mca_coll_ml_component.disable_allgather));

    CHECK(reg_bool("disable_reduce", NULL, "Disable Reduce", false,
                   &mca_coll_ml_component.disable_reduce));

    tmp = mca_base_var_enum_create ("coll_ml_enable_fragmentation_enum", fragmentation_enable_enum, &new_enum);
    if (OPAL_SUCCESS != tmp) {
        return tmp;
    }

    /* default to auto-enable fragmentation */
    mca_coll_ml_component.enable_fragmentation = 2;
    tmp = mca_base_component_var_register (&mca_coll_ml_component.super.collm_version, "enable_fragmentation",
                                           "Disable/Enable fragmentation for large messages", MCA_BASE_VAR_TYPE_INT,
                                           new_enum, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_ml_component.enable_fragmentation);
    if (0 > tmp) {
        ret = tmp;
    }
    OBJ_RELEASE(new_enum);

    asprintf(&str, "%s/mca-coll-ml.config",
            opal_install_dirs.ompidatadir);
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

    /* Verify the parameters */
    CHECK(mca_coll_ml_verify_params());

    return ret;
}
