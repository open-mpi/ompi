/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include <dlfcn.h>
#include <libgen.h>

#include "coll_mccl.h"
#include "opal/mca/installdirs/installdirs.h"
#include "coll_mccl_dtypes.h"

/*
 * Public string showing the coll ompi_mccl component version number
 */
const char *mca_coll_mccl_component_version_string =
  "Open MPI MCCL collective MCA component version " OMPI_VERSION;


static int mccl_open(void);
static int mccl_close(void);
static int mccl_register(void);
int mca_coll_mccl_output = -1;
mca_coll_mccl_component_t mca_coll_mccl_component = {
    /* First, the mca_component_t struct containing meta information
       about the component  */
    {
        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "mccl",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = mccl_open,
            .mca_close_component = mccl_close,
            .mca_register_component_params = mccl_register,
        },
        .collm_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        .collm_init_query = mca_coll_mccl_init_query,
        .collm_comm_query = mca_coll_mccl_comm_query,
    },
    120, /* priority */
    0,  /* verbose level */
    0,   /* mccl_enable */
    NULL /*mccl version */
};

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
 * Utility routine for integer parameter registration
 */
static int reg_int(const char* param_name, const char* deprecated_param_name,
                   const char* param_desc, int default_value, int *storage, int flags)
{
    int index;
    *storage = default_value;
    index = mca_base_component_var_register(&mca_coll_mccl_component.super.collm_version,
                                            param_name, param_desc, MCA_BASE_VAR_TYPE_INT,
                                            NULL, 0, 0,OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index, "ompi", "coll", "mccl", deprecated_param_name,
                                             MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if (0 != (flags & REGINT_NEG_ONE_OK) && -1 == *storage) {
        return OMPI_SUCCESS;
    }

    if ((0 != (flags & REGINT_GE_ZERO) && *storage < 0) ||
        (0 != (flags & REGINT_GE_ONE) && *storage < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == *storage)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"", param_name);
        return OMPI_ERR_BAD_PARAM;
    }
    return OMPI_SUCCESS;
}


static int mccl_register(void)
{
    int ret, tmp;
    ret = OMPI_SUCCESS;

#define CHECK(expr) do {                        \
        tmp = (expr);                           \
        if (OMPI_SUCCESS != tmp) ret = tmp;     \
    } while (0)


    CHECK(reg_int("priority", NULL, "Priority of the mccl coll component",
                  120, &mca_coll_mccl_component.mccl_priority, 0));

    CHECK(reg_int("verbose", NULL, "Verbose level of the mccl coll component",
                  0, &mca_coll_mccl_component.mccl_verbose, 0));

    CHECK(reg_int("enable", NULL, "[1|0|] Enable/Disable MCCL",
                  1, &mca_coll_mccl_component.mccl_enable, 0));

    CHECK(reg_int("np", NULL, "Minimal number of processes in the communicator"
                  " for the corresponding mccl context to be created (default: 32)",
                  2, &mca_coll_mccl_component.mccl_np, 0));

    /* mca_coll_mccl_component.compiletime_version = MCCL_VERNO_STRING; */

    mca_base_component_var_register(&mca_coll_mccl_component.super.collm_version,
                                    MCA_COMPILETIME_VER,
                                    "Version of the libmccl library with which Open MPI was compiled",
                                    MCA_BASE_VAR_TYPE_VERSION_STRING, NULL, 0, 0,
                                    OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_mccl_component.compiletime_version);
    /* mca_coll_mccl_component.runtime_version = mccl_get_version(); */
    mca_base_component_var_register(&mca_coll_mccl_component.super.collm_version,
                                    MCA_RUNTIME_VER,
                                    "Version of the libmccl library with which Open MPI is running",
                                    MCA_BASE_VAR_TYPE_VERSION_STRING, NULL, 0, 0,
                                    OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_coll_mccl_component.runtime_version);
    return ret;
}

static int mccl_open(void)
{
    mca_coll_mccl_component_t *cm;
    cm  = &mca_coll_mccl_component;
    mca_coll_mccl_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_coll_mccl_output, cm->mccl_verbose);
    cm->libmccl_initialized = false;
    return OMPI_SUCCESS;
}

static int mccl_close(void)
{
    return OMPI_SUCCESS;
}
