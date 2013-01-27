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
    index = mca_base_param_reg_string(&mca_sbgp_ibnet_component.super.sbgp_version,
                                      param_name, param_desc, false, false,
                                      default_value, &value);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index,
                               &mca_sbgp_ibnet_component.super.sbgp_version,
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

static int mca_sbgp_ibnet_ini_intify(char *str)
{
    while (isspace(*str)) {
        ++str;
    }

    /* If it's hex, use sscanf() */
    if (strlen(str) > 3 && 0 == strncasecmp("0x", str, 2)) {
        unsigned int i;
        sscanf(str, "%X", &i);
        return (int) i;
    }

    /* Nope -- just decimal, so use atoi() */
    return atoi(str);
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
    index = mca_base_param_reg_int(&mca_sbgp_ibnet_component.super.sbgp_version,
            param_name, param_desc, false, false,
            default_value, NULL);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index,
                               &mca_sbgp_ibnet_component.super.sbgp_version,
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

int mca_sbgp_ibnet_register_params(void)
{
    char *msg, *pkey;
    int ival, ret, tmp;

    ret = OMPI_SUCCESS;

#define CHECK(expr) do {                    \
        tmp = (expr);                       \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    /* register openib component parameters */

    CHECK(reg_int("priority", NULL,
                  "IB offload component priority"
                  "(from 0(low) to 90 (high))", 90, &ival, 0));
    mca_sbgp_ibnet_component.super.priority = ival;

    CHECK(reg_int("verbose", NULL,
                  "Output some verbose IB offload BTL information "
                  "(0 = no output, nonzero = output)", 0, &ival, 0));
    mca_sbgp_ibnet_component.verbose = ival;

    CHECK(reg_int("warn_default_gid_prefix", NULL,
                  "Warn when there is more than one active ports and at least one of them connected to the network with only default GID prefix configured (0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_sbgp_ibnet_component.warn_default_gid_prefix = (0 != ival);
    CHECK(reg_int("warn_nonexistent_if", NULL,
                  "Warn if non-existent devices and/or ports are specified in the sbgp_ibnet_if_[in|ex]clude MCA parameters (0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_sbgp_ibnet_component.warn_nonexistent_if = (0 != ival);

    CHECK(reg_int("max_sbgps", NULL,
                  "Maximum allowed number of subroups",
                  100, &mca_sbgp_ibnet_component.max_sbgps, 0));

    CHECK(reg_string("pkey", "ib_pkey_val",
                     "OpenFabrics partition key (pkey) value. "
                     "Unsigned integer decimal or hex values are allowed (e.g., \"3\" or \"0x3f\") and will be masked against the maximum allowable IB paritition key value (0x7fff)",
                     "0", &pkey, 0));

    mca_sbgp_ibnet_component.pkey_val =
        mca_sbgp_ibnet_ini_intify(pkey) & SBGP_IBNET_IB_PKEY_MASK;
    free(pkey);

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

    CHECK(reg_int("mtu", "ib_mtu", msg, IBV_MTU_1024, &ival, 0));
    free(msg);

    if (ival < IBV_MTU_1024 || ival > IBV_MTU_4096) {
        ompi_show_help("help-mpi-bcol-iboffload.txt", "invalid mca param value",
                       true, "invalid value for btl_openib_ib_mtu",
                       "btl_openib_ib_mtu reset to 1024");
        mca_sbgp_ibnet_component.mtu = IBV_MTU_1024;
    } else {
        mca_sbgp_ibnet_component.mtu = (uint32_t) ival;
    }

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
