/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022 NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2024-2026 NVIDIA CORPORATION. All rights reserved.
 * Copyright (c) 2025      Fujitsu Limited. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */
#include "ompi_config.h"
#include "coll_ucc.h"
#include "coll_ucc_dtypes.h"
#include "opal/util/argv.h"
#include <limits.h>

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

#if OPAL_ENABLE_DEBUG
    cm->fail_domain_index = -1;
    mca_base_component_var_register(c, "fail_domain_index",
                                    "Testing only: make the N-th UCC context creation in this process fail (-1 = never)",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->fail_domain_index);
#endif

    cm->domain_reuse = true;
    mca_base_component_var_register(c, "domain_reuse",
                                    "Reuse an existing UCC context whose bootstrap group covers a new communicator instead of creating one per communicator family",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->domain_reuse);

    cm->sessions_teardown = false;
    mca_base_component_var_register(c, "sessions_teardown",
                                    "Destroy UCC contexts when the last session finalizes without an MPI_Finalize fence; "
                                    "waits on peers that may be in unrelated sessions, so off by default (contexts are leaked)",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->sessions_teardown);
    cm->team_post_agreement = true;
    mca_base_component_var_register(c, "team_post_agreement",
                                    "Agree across the communicator that every rank posted its UCC team before waiting on it; needed where a TL's team creation blocks (tl/sharp), costs one allreduce per communicator when it is enabled",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->team_post_agreement);

    cm->team_create_timeout = 60;
    mca_base_component_var_register(c, "team_create_timeout",
                                    "Seconds to wait for a UCC team creation to complete before giving up on UCC for that communicator (<= 0: wait forever)",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->team_create_timeout);

    cm->team_destroy_timeout = 60;
    mca_base_component_var_register(c, "team_destroy_timeout",
                                    "Seconds to wait for a UCC team destroy at communicator release before leaving the team to finalize (<= 0: wait forever)",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->team_destroy_timeout);

    cm->max_domains = 0;
    mca_base_component_var_register(c, "max_domains",
                                    "Maximum number of live UCC contexts per process; a communicator that would need another one falls back to the previous coll modules (<= 0: unlimited)",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->max_domains);

    cm->derived_sharp = false;
    mca_base_component_var_register(c, "derived_sharp",
                                    "Allow SHARP on communicators other than MPI_COMM_WORLD that do not set the ompi_comm_coll_ucc_sharp info key; "
                                    "when false they use a UCC context with tl/sharp disabled, which costs one extra UCC context per process when tl/sharp is configured",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->derived_sharp);

#if OPAL_ENABLE_DEBUG
    cm->sharp_flavor_force = false;
    mca_base_component_var_register(c, "sharp_flavor_force",
                                    "Testing only: create SHARP-free UCC contexts even when tl/sharp is not in the UCC library",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->sharp_flavor_force);
#endif

#if OPAL_ENABLE_DEBUG
    cm->fail_domain_no_resource = false;
    mca_base_component_var_register(c, "fail_domain_no_resource",
                                    "Testing only: treat the fail_domain_index failure as a SHARP resource refusal",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->fail_domain_no_resource);
#endif

#if OPAL_ENABLE_DEBUG
    cm->fail_team_index = -1;
    mca_base_component_var_register(c, "fail_team_index",
                                    "Testing only: make the N-th UCC team create post in this process fail (-1 = never)",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->fail_team_index);
#endif

#if OPAL_ENABLE_DEBUG
    cm->fail_team_rank = -1;
    mca_base_component_var_register(c, "fail_team_rank",
                                    "Testing only: restrict fail_team_index to this MPI_COMM_WORLD rank (-1 = every rank)",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->fail_team_rank);
#endif
#if OPAL_ENABLE_DEBUG
    cm->team_id_force = -1;
    mca_base_component_var_register(c, "team_id_force",
                                    "Testing only: use this UCC team id for every communicator without a global index (-1 = hashed)",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_ALL, &cm->team_id_force);
#endif

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
    cm->domain_count             = 0;
    cm->progress_ctx             = NULL;     /* a reopened component (Sessions) must not trust BSS */
    cm->progress_ctx_cap         = 0;
    cm->progressing              = 0;
    cm->progress_registered      = false;
    cm->ucc_lib                  = NULL;
    cm->teams_posted             = 0;
    cm->refused_at_count         = 0;
    cm->requests_initialized     = false;
    cm->finalize_hook_registered = false;
    cm->finalizing               = false;
    cm->lib_failed               = false;
    cm->orphans                  = 0;
    cm->domains_created          = 0;
    cm->sharp_in_lib             = false;
    cm->sharp_domain_count       = 0;
    cm->sharp_flavor_force       = false;    /* registered only in debug builds, read unconditionally */
    OBJ_CONSTRUCT(&cm->domains, opal_list_t);
    OBJ_CONSTRUCT(&cm->lock, opal_mutex_t);
    OBJ_CONSTRUCT(&cm->modules, opal_pointer_array_t);
    OBJ_CONSTRUCT(&cm->abandoned, opal_list_t);
    opal_pointer_array_init(&cm->modules, 8, INT_MAX, 8);
    opal_output_set_verbosity(mca_coll_ucc_output, cm->ucc_verbose);
    mca_coll_ucc_init_default_cts();
    return OMPI_SUCCESS;
}

static int mca_coll_ucc_close(void)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;

    /* By now every UCC communicator has been freed, so all domains and the
       UCC library are already gone.  Drop the request free list (created
       lazily with the first library init) and the (now empty) domain list. */
    if (cm->requests_initialized) {
        OBJ_DESTRUCT(&cm->requests);
        cm->requests_initialized = false;
    }
    if (!opal_list_is_empty(&cm->domains)) {
        UCC_VERBOSE(1, "%d ucc oob domain(s) still alive at component close",
                    (int)opal_list_get_size(&cm->domains));
    }
    if (NULL != cm->ucc_lib) {
        UCC_VERBOSE(1, "ucc library still initialized at component close");
    }
    if (!opal_list_is_empty(&cm->abandoned)) {
        UCC_VERBOSE(1, "%d quarantined ucc team(s) leaked at component close",
                    (int)opal_list_get_size(&cm->abandoned));
        OPAL_LIST_DESTRUCT(&cm->abandoned);
    } else {
        OBJ_DESTRUCT(&cm->abandoned);
    }
    OBJ_DESTRUCT(&cm->modules);
    free(cm->progress_ctx);
    cm->progress_ctx     = NULL;
    cm->progress_ctx_cap = 0;
    OBJ_DESTRUCT(&cm->lock);
    OBJ_DESTRUCT(&cm->domains);
    return OMPI_SUCCESS;
}
