/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */
#include <stdio.h>

#include <dlfcn.h>
#include <libgen.h>

#include "scoll_mpi.h"

/*
 * Public string showing the oshmem scoll_mpi component version number
 */
const char *mca_scoll_mpi_component_version_string =
  "OpenSHMEM MPI collective MCA component version " OSHMEM_VERSION;


static int mpi_open(void);
static int mpi_close(void);
static int mpi_register(void);
int mca_scoll_mpi_output = -1;
mca_scoll_mpi_component_t mca_scoll_mpi_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itfca */
    {
        {
            MCA_SCOLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            "mpi",
            OSHMEM_MAJOR_VERSION,
            OSHMEM_MINOR_VERSION,
            OSHMEM_RELEASE_VERSION,

            /* Component open and close functions */
            mpi_open,
            mpi_close,
            NULL,
            mpi_register
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */

        mca_scoll_mpi_init_query,
        mca_scoll_mpi_comm_query,
    },
    77, /* priority */
    0,  /* verbose level */
    0,   /* mpi_enable */
    2   /*mpi_np */
};

/*
 *  * Local flags
 *   */
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
static int reg_int(const char* param_name,
        const char* deprecated_param_name,
        const char* param_desc,
        int default_value, int *storage, int flags)
{
    int index;

    *storage = default_value;
    index = mca_base_component_var_register(
            &mca_scoll_mpi_component.super.scoll_version,
            param_name, param_desc, MCA_BASE_VAR_TYPE_INT,
            NULL, 0, 0,OPAL_INFO_LVL_9,
            MCA_BASE_VAR_SCOPE_READONLY, storage);
    if (NULL != deprecated_param_name) {
        (void) mca_base_var_register_synonym(index,
                "oshmem", "scoll", "mpi", deprecated_param_name,
                MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    if (0 != (flags & REGINT_NEG_ONE_OK) && -1 == *storage) {
        return OSHMEM_SUCCESS;
    }

    if ((0 != (flags & REGINT_GE_ZERO) && *storage < 0) ||
            (0 != (flags & REGINT_GE_ONE) && *storage < 1) ||
            (0 != (flags & REGINT_NONZERO) && 0 == *storage)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OSHMEM_ERR_BAD_PARAM;
    }

    return OSHMEM_SUCCESS;
}


static int mpi_register(void)
{

    int ret, tmp;

    ret = OSHMEM_SUCCESS;

#define CHECK(expr) do {                        \
        tmp = (expr);                           \
        if (OSHMEM_SUCCESS != tmp) ret = tmp;     \
    } while (0)


    CHECK(reg_int("priority",NULL,
                  "Priority of the mpi coll component",
                  mca_scoll_mpi_component.mpi_priority,
                  &mca_scoll_mpi_component.mpi_priority,
                  0));

    CHECK(reg_int("verbose", NULL,
                  "Verbose level of the mpi coll component",
                  mca_scoll_mpi_component.mpi_verbose,
                  &mca_scoll_mpi_component.mpi_verbose,
                  0));

    CHECK(reg_int("enable",NULL,
                  "[1|0|] Enable/Disable MPI scoll component",
                  mca_scoll_mpi_component.mpi_enable,
                  &mca_scoll_mpi_component.mpi_enable,
                  0));

    CHECK(reg_int("np",NULL,
                  "Minimal number of processes in the communicator"
                  " for the corresponding mpi context to be created",
                  mca_scoll_mpi_component.mpi_np,
                  &mca_scoll_mpi_component.mpi_np,
                  0));

    return ret;
}

static int mpi_open(void)
{
    mca_scoll_mpi_component_t *cm;
    cm  = &mca_scoll_mpi_component;

    mca_scoll_mpi_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_scoll_mpi_output, cm->mpi_verbose);
    return OSHMEM_SUCCESS;
}

static int mpi_close(void)
{
    return OSHMEM_SUCCESS;
}
