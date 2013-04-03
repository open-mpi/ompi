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
#include "ompi/mca/common/ofacm/base.h"

#include "sbgp_ibnet.h"
#include "sbgp_ibnet_mca.h"

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

static mca_base_var_enum_value_t mtu_values[] = {
    {IBV_MTU_512, "256B"},
    {IBV_MTU_512, "512B"},
    {IBV_MTU_1024, "1k"},
    {IBV_MTU_2048, "2k"},
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

    /* the MCA variable system will not change this value */
    *storage = (char *) default_value;
    index = mca_base_component_var_register(&mca_sbgp_ibnet_component.super.sbgp_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_STRING,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "sbgp", "ibnet", deprecated_param_name,
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
    index = mca_base_component_var_register(&mca_sbgp_ibnet_component.super.sbgp_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_INT,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "sbgp", "ibnet", deprecated_param_name,
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

/*
 * utility routine for boolean parameter registration
 */
static int reg_bool(const char* param_name,
                    const char* deprecated_param_name,
                    const char* param_desc,
                    bool default_value, bool *storage)
{
    int index;

    *storage = default_value;
    index = mca_base_component_var_register(&mca_sbgp_ibnet_component.super.sbgp_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_BOOL,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "sbgp", "ibnet", deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    return OMPI_SUCCESS;
}

int mca_sbgp_ibnet_register_params(void)
{
    mca_base_var_enum_t *new_enum;
    char *msg;
    int ret, tmp;

    ret = OMPI_SUCCESS;

#define CHECK(expr) do {                    \
        tmp = (expr);                       \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    /* register openib component parameters */

    CHECK(reg_int("priority", NULL,
                  "IB offload component priority"
                  "(from 0(low) to 90 (high))", 90, &mca_sbgp_ibnet_component.super.priority, 0));

    CHECK(reg_int("verbose", NULL,
                  "Output some verbose IB offload BTL information "
                  "(0 = no output, nonzero = output)", 0, &mca_sbgp_ibnet_component.verbose, 0));

    CHECK(reg_bool("warn_default_gid_prefix", NULL,
                   "Warn when there is more than one active ports and at least one of them connected to the network with only default GID prefix configured (0 = do not warn; any other value = warn)",
                   true, &mca_sbgp_ibnet_component.warn_default_gid_prefix));
    CHECK(reg_bool("warn_nonexistent_if", NULL,
                  "Warn if non-existent devices and/or ports are specified in the sbgp_ibnet_if_[in|ex]clude MCA parameters (0 = do not warn; any other value = warn)",
                  true, &mca_sbgp_ibnet_component.warn_nonexistent_if));

    CHECK(reg_int("max_sbgps", NULL,
                  "Maximum allowed number of subroups",
                  100, &mca_sbgp_ibnet_component.max_sbgps, 0));

    CHECK(reg_int("pkey", "ib_pkey_val",
                  "OpenFabrics partition key (pkey) value. "
                  "Unsigned integer decimal or hex values are allowed (e.g., \"3\" or \"0x3f\") and will be masked against the maximum allowable IB paritition key value (0x7fff)",
                  0, &mca_sbgp_ibnet_component.pkey_val, 0));
    mca_sbgp_ibnet_component.pkey_val &= SBGP_IBNET_IB_PKEY_MASK;

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

    CHECK(mca_base_var_enum_create("sbgp_ibnet_mtu", mtu_values, &new_enum));
    if (OPAL_SUCCESS != ret) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    ret = mca_base_component_var_register(&mca_sbgp_ibnet_component.super.sbgp_version,
                                          "mtu", msg, MCA_BASE_VAR_TYPE_INT, new_enum,
                                          0, 0, OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY, &mca_sbgp_ibnet_component.mtu);
    OBJ_RELEASE(new_enum);
    free(msg);

    if (0 > ret) {
        return ret;
    }

    (void) mca_base_var_register_synonym(ret, "ompi", "sbgp", "ibnet", "ib_mtu",
                                         MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    CHECK(reg_string("if_include", NULL,
                     "Comma-delimited list of devices/ports to be used (e.g. \"mthca0,mthca1:2\"; empty value means to use all ports found).  Mutually exclusive with sbgp_ibnet_if_exclude.",
                     NULL, &mca_sbgp_ibnet_component.if_include,
                     0));

    CHECK(reg_string("if_exclude", NULL,
                     "Comma-delimited list of device/ports to be excluded (empty value means to not exclude any ports).  Mutually exclusive with sbgp_ibnet_if_include.",
                     NULL, &mca_sbgp_ibnet_component.if_exclude,
                     0));

    /* Register any MCA params for the connect pseudo-components */
    if (OMPI_SUCCESS == ret) {
        ret = ompi_common_ofacm_base_register(&mca_sbgp_ibnet_component.super.sbgp_version);
    }

    return ret;
}
