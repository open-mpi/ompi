/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022 NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2024 NVIDIA CORPORATION. All rights reserved.
 * Copyright (c) 2025      Fujitsu Limited. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include "coll_ucc.h"
#include "coll_ucc_dtypes.h"
#include "opal/util/argv.h"

static int mca_coll_ucc_open(void);
static int mca_coll_ucc_close(void);
static int mca_coll_ucc_register(void);

int mca_coll_ucc_output = -1;

mca_coll_ucc_component_t mca_coll_ucc_component = {
    /* First, the mca_component_t struct containing meta information
       about the component  */
    {
        .collm_version = {
            MCA_COLL_BASE_VERSION_3_0_0,

            /* Component name and version */
            .mca_component_name = "ucc",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component            = mca_coll_ucc_open,
            .mca_close_component           = mca_coll_ucc_close,
            .mca_register_component_params = mca_coll_ucc_register,
            .mca_query_component           = NULL,
        },
        .collm_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        .collm_init_query = mca_coll_ucc_init_query,
        .collm_comm_query = mca_coll_ucc_comm_query,
    },
    10,                /* ucc_priority                */
    0,                 /* ucc_verbose                 */
    0,                 /* ucc_enable                  */
    2,                 /* ucc_np                      */
    "",                /* cls                         */
    COLL_UCC_CTS_STR,  /* requested coll_types string */
    UCC_VERSION_STRING /* ucc version                 */
};
MCA_BASE_COMPONENT_INIT(ompi, coll, ucc)

static int mca_coll_ucc_register(void)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    mca_base_component_t     *c  = &cm->super.collm_version;
    mca_base_component_var_register(c, "priority", "Priority of the UCC coll component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->ucc_priority);

    mca_base_component_var_register(c, "verbose", "Verbose level of the UCC coll component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->ucc_verbose);

    mca_base_component_var_register(c, "enable", "[0|1] Enable/Disable the UCC coll component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->ucc_enable);

    mca_base_component_var_register(c, "np", "Minimal communicator size for the UCC coll component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->ucc_np);

    mca_base_component_var_register(c, MCA_COMPILETIME_VER,
                                    "Version of the libucc library with which Open MPI was compiled",
                                    MCA_BASE_VAR_TYPE_VERSION_STRING, NULL, 0, 0,
                                    OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY,
                                    &cm->compiletime_version);

    mca_base_component_var_register(c, MCA_RUNTIME_VER,
                                    "Version of the libucc library with which Open MPI is running",
                                    MCA_BASE_VAR_TYPE_VERSION_STRING, NULL, 0, 0,
                                    OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY,
                                    &cm->runtime_version);

    cm->cls = "";
    mca_base_component_var_register(c, "cls",
                                    "Comma separated list of UCC CLS to be used for team creation",
                                    MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_6, MCA_BASE_VAR_SCOPE_ALL, &cm->cls);

    cm->cts = COLL_UCC_CTS_STR;
    mca_base_component_var_register(c, "cts",
                                    "Comma separated list of UCC coll types to be enabled",
                                    MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_6, MCA_BASE_VAR_SCOPE_ALL, &cm->cts);
    return OMPI_SUCCESS;
}

static ucc_coll_type_t mca_coll_ucc_str_to_type(const char *str)
{
    if (0 == strcasecmp(str, "barrier")) {
        return UCC_COLL_TYPE_BARRIER;
    } else if (0 == strcasecmp(str, "bcast")) {
        return UCC_COLL_TYPE_BCAST;
    } else if (0 == strcasecmp(str, "allreduce")) {
        return UCC_COLL_TYPE_ALLREDUCE;
    } else if (0 == strcasecmp(str, "alltoall")) {
        return UCC_COLL_TYPE_ALLTOALL;
    } else if (0 == strcasecmp(str, "alltoallv")) {
        return UCC_COLL_TYPE_ALLTOALLV;
    } else if (0 == strcasecmp(str, "allgather")) {
        return UCC_COLL_TYPE_ALLGATHER;
    } else if (0 == strcasecmp(str, "allgatherv")) {
        return UCC_COLL_TYPE_ALLGATHERV;
    } else if (0 == strcasecmp(str, "reduce")) {
        return UCC_COLL_TYPE_REDUCE;
    } else if (0 == strcasecmp(str, "gather")) {
        return UCC_COLL_TYPE_GATHER;
    } else if (0 == strcasecmp(str, "gatherv")) {
        return UCC_COLL_TYPE_GATHERV;
    } else if (0 == strcasecmp(str, "reduce_scatter_block")) {
        return UCC_COLL_TYPE_REDUCE_SCATTER;
    } else if (0 == strcasecmp(str, "reduce_scatter")) {
        return UCC_COLL_TYPE_REDUCE_SCATTERV;
    } else if (0 == strcasecmp(str, "scatterv")) {
        return UCC_COLL_TYPE_SCATTERV;
    } else if (0 == strcasecmp(str, "scatter")) {
        return UCC_COLL_TYPE_SCATTER;
    }
    UCC_ERROR("incorrect value for cts: %s, allowed: %s",
              str, COLL_UCC_CTS_STR);
    return UCC_COLL_TYPE_LAST;
}

/* is a persistent collective */
static inline int mca_coll_ucc_init_cts_is_persistent(const char *cp, char *bp, size_t bz)
{
    size_t len = strlen(cp), len_suffix = sizeof("_init") - 1;

    assert((bz > 0) && (bp != 0));
    /* check if it is a persistent collective */
    if (len > len_suffix) {
        size_t blen = len - len_suffix;
        const char *cp_suffix = &cp[blen];

        if (0 == strcmp(cp_suffix, "_init")) {
            int wc = snprintf(bp, bz, "%*.*s", (int)blen, (int)blen, cp);
            if ((wc < 0) || ((size_t)wc >= bz)) {
                return -1 /* XXX internal error */;
            }
            return 1 /* true */;
        }
    }
    return 0 /* false */;
}

/* is an alias (special) name */
static inline int mca_coll_ucc_init_cts_is_alias(const char *cp, bool disable,
                                                 mca_coll_ucc_component_t *cm)
{
    if (0 == strcmp(cp, "colls_b")) { /* all blocking colls */
        if (disable) {
            cm->cts_requested &= ~COLL_UCC_CTS;
        } else {
            cm->cts_requested |= COLL_UCC_CTS;
        }
        return 1 /* true */;
    } else if ((0 == strcmp(cp, "colls_i")) || (0 == strcmp(cp, "colls_nb"))) {
        /* all non-blocking colls */
        if (disable) {
            cm->nb_cts_requested &= ~COLL_UCC_CTS;
        } else {
            cm->nb_cts_requested |= COLL_UCC_CTS;
        }
        return 1 /* true */;
    } else if (0 == strcmp(cp, "colls_p")) { /* all persistent colls */
        if (disable) {
            cm->ps_cts_requested &= ~COLL_UCC_CTS;
        } else {
            cm->ps_cts_requested |= COLL_UCC_CTS;
        }
        return 1 /* true */;
    }
    return 0 /* false */;
}

static void mca_coll_ucc_init_default_cts(void)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    bool                      disable;
    char**                    cts;
    int                       n_cts, i;
    char*                     str;
    ucc_coll_type_t           *ct, c;

    disable              = (cm->cts[0] == '^') ? true : false;
    cts                  = opal_argv_split(disable ? (cm->cts + 1) : cm->cts, ',');
    n_cts                = opal_argv_count(cts);
    cm->cts_requested    = disable ? COLL_UCC_CTS : 0;
    cm->nb_cts_requested = disable ? COLL_UCC_CTS : 0;
    cm->ps_cts_requested = disable ? COLL_UCC_CTS : 0;
    for (i = 0; i < n_cts; i++) {
        char l_str[64]; /* XXX sizeof("reduce_scatter_block") */
        size_t l_stz = sizeof(l_str);

        if (0 < mca_coll_ucc_init_cts_is_alias(cts[i], disable, cm)) {
            continue;
        }
        if (('i' == cts[i][0]) || ('I' == cts[i][0])) {
            /* non blocking collective setting */
            str = cts[i] + 1;
            ct  = &cm->nb_cts_requested;
        } else if (0 < mca_coll_ucc_init_cts_is_persistent(cts[i], l_str, l_stz)) {
            /* persistent collective setting */
            str = l_str;
            ct = &cm->ps_cts_requested;
        } else {
            str = cts[i];
            ct  = &cm->cts_requested;
        }
        c = mca_coll_ucc_str_to_type(str);
        if (UCC_COLL_TYPE_LAST == c) {
            *ct = COLL_UCC_CTS;
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

static int mca_coll_ucc_open(void)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    mca_coll_ucc_output          = opal_output_open(NULL);
    cm->libucc_initialized       = false;
    opal_output_set_verbosity(mca_coll_ucc_output, cm->ucc_verbose);
    mca_coll_ucc_init_default_cts();
    return OMPI_SUCCESS;
}

static int mca_coll_ucc_close(void)
{
    return OMPI_SUCCESS;
}
