/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <stdio.h>

#include <dlfcn.h>
#include <libgen.h>

#include "scoll_ucc.h"
#include "opal/util/argv.h"

/*
 * Public string showing the oshmem scoll_ucc component version number
 */
const char * mca_scoll_ucc_component_version_string =
  "OpenSHMEM UCC collective MCA component version " OSHMEM_VERSION;


static int ucc_open(void);
static int ucc_close(void);
static int ucc_register(void);

int mca_scoll_ucc_output = -1;

mca_scoll_ucc_component_t mca_scoll_ucc_component = {

    /* First, the mca_component_t struct containing meta information
       about the component */
    {
        .scoll_version = {
            MCA_SCOLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "ucc",
            MCA_BASE_MAKE_VERSION(component, OSHMEM_MAJOR_VERSION, OSHMEM_MINOR_VERSION,
                                  OSHMEM_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = ucc_open,
            .mca_close_component = ucc_close,
            .mca_register_component_params = ucc_register,
            .mca_query_component = NULL,
        },
        .scoll_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        .scoll_init = mca_scoll_ucc_init_query,
        .scoll_query = mca_scoll_ucc_comm_query,
    },
    75,                 /* priority */
    0,                  /* verbose level */
    0,                  /* ucc_enable */
    2,                  /* ucc_np */
    "",                 /* cls */
    SCOLL_UCC_CTS_STR,  /* cts */
    0,                  /* nr_modules */
    false,              /* libucc_initialized */
    NULL                /* ucc_context */
};

static int ucc_register(void)
{
    mca_scoll_ucc_component_t * cm = &mca_scoll_ucc_component;
    mca_base_component_t * c       = &cm->super.scoll_version;

    mca_base_component_var_register(c, "priority", 
                                    "Priority of the UCC scoll component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, 
                                    OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                    &cm->ucc_priority);

    mca_base_component_var_register(c, "verbose", 
                                    "Verbose level of the UCC scoll component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, 
                                    OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                    &cm->ucc_verbose);

    mca_base_component_var_register(c, "enable", 
                                    "[1|0|] Enable/Disable UCC scoll component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, 
                                    OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                    &cm->ucc_enable);

    mca_base_component_var_register(c, "np", 
                                    "Minimal Active Set / Team size for UCC scoll component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, 
                                    OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                    &cm->ucc_np);

    cm->cts = "";
    mca_base_component_var_register(c, "cls", 
                                    "Comma separated list of UCC CLS to be used for team creation",
                                    MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, 
                                    OPAL_INFO_LVL_6, MCA_BASE_VAR_SCOPE_READONLY,
                                    &cm->cls);
    cm->cts = SCOLL_UCC_CTS_STR;
    mca_base_component_var_register(c, "cts", 
                                    "Comma separated list of UCC coll types to be enabled",
                                    MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, 
                                    OPAL_INFO_LVL_6, MCA_BASE_VAR_SCOPE_READONLY,
                                    &cm->cts);
    return OSHMEM_SUCCESS;
}

static ucc_coll_type_t mca_scoll_ucc_str_to_type(const char *str)
{
    if (0 == strcasecmp(str, "barrier")) {
        return UCC_COLL_TYPE_BARRIER;
    } else if (0 == strcasecmp(str, "broadcast")) {
        return UCC_COLL_TYPE_BCAST;
    } else if (0 == strcasecmp(str, "reduce")) {
        return UCC_COLL_TYPE_ALLREDUCE;
    } else if (0 == strcasecmp(str, "alltoall")) {
        return UCC_COLL_TYPE_ALLTOALL;
    } else if (0 == strcasecmp(str, "collect")) {
        return UCC_COLL_TYPE_ALLGATHER;
    }
    UCC_ERROR("incorrect value for cts: %s, allowed: %s",
              str, SCOLL_UCC_CTS_STR);
    return UCC_COLL_TYPE_LAST;
}

static void mca_scoll_ucc_init_default_cts(void)
{
    mca_scoll_ucc_component_t *cm = &mca_scoll_ucc_component;
    bool disable;
    char ** cts;
    int n_cts, i;
    char * str;
    ucc_coll_type_t *ct, c;

    disable              = (cm->cts[0] == '^') ? true : false;
    cts                  = opal_argv_split(disable ? (cm->cts + 1) : cm->cts, ',');
    n_cts                = opal_argv_count(cts);
    cm->cts_requested    = disable ? SCOLL_UCC_CTS : 0;
    for (i = 0; i < n_cts; i++) {
        str = cts[i];
        ct  = &cm->cts_requested;
        
        c = mca_scoll_ucc_str_to_type(str);
        if (UCC_COLL_TYPE_LAST == c) {
            *ct = SCOLL_UCC_CTS;
            break;
        }
        if (disable) {
            (*ct) &= ~c;
        } else {
            (*ct) |= c;
        }
    }
    opal_argv_free(cts);
}

static int ucc_open(void)
{
    mca_scoll_ucc_component_t *cm;
    cm = &mca_scoll_ucc_component;
    cm->libucc_initialized = false;

    mca_scoll_ucc_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_scoll_ucc_output, cm->ucc_verbose);
    mca_scoll_ucc_init_default_cts();
    return OSHMEM_SUCCESS;
}

static int ucc_close(void)
{
    return OSHMEM_SUCCESS;
}
