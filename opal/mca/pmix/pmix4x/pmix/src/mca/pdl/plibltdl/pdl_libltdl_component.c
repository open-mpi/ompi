/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015       Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015       Los Alamos National Security, Inc.  All rights
 *                          reserved.
 * Copyright (c) 2017      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include "pmix_common.h"
#include "pmix/mca/pdl/pdl.h"
#include "pmix/mca/base/pmix_mca_base_var.h"
#include "pmix/util/argv.h"

#include "pdl_libltdl.h"


/*
 * Public string showing the sysinfo ompi_linux component version number
 */
const char *pmix_pdl_plibltpdl_component_version_string =
    "PMIX pdl plibltdl MCA component version " PMIX_VERSION;


/*
 * Local functions
 */
static int plibltpdl_component_register(void);
static int plibltpdl_component_open(void);
static int plibltpdl_component_close(void);
static int plibltpdl_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

pmix_pdl_plibltpdl_component_t mca_pdl_plibltpdl_component = {

    /* Fill in the mca_pdl_base_component_t */
    .base = {

        /* First, the mca_component_t struct containing meta information
           about the component itself */
        .base_version = {
            PMIX_DL_BASE_VERSION_1_0_0,

            /* Component name and version */
            .mca_component_name = "plibltdl",
            MCA_BASE_MAKE_VERSION(component, PMIX_MAJOR_VERSION, PMIX_MINOR_VERSION,
                                  PMIX_RELEASE_VERSION),

            /* Component functions */
            .mca_register_component_params = plibltpdl_component_register,
            .mca_open_component = plibltpdl_component_open,
            .mca_close_component = plibltpdl_component_close,
            .mca_query_component = plibltpdl_component_query,
        },

        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* The dl framework members */
        .priority = 50
    }

    /* Now fill in the plibltdl component-specific members */
};


static int plibltpdl_component_register(void)
{
    /* Register an info param indicating whether we have lt_dladvise
       support or not */
    bool supported = PMIX_INT_TO_BOOL(PMIX_DL_LIBLTDL_HAVE_LT_DLADVISE);
    mca_base_component_var_register(&mca_pdl_plibltpdl_component.base.base_version,
                                    "have_lt_dladvise",
                                    "Whether the version of plibltdl that this component is built against supports lt_dladvise functionality or not",
                                    MCA_BASE_VAR_TYPE_BOOL,
                                    NULL,
                                    0,
                                    MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                    PMIX_INFO_LVL_7,
                                    MCA_BASE_VAR_SCOPE_CONSTANT,
                                    &supported);

    return PMIX_SUCCESS;
}

static int plibltpdl_component_open(void)
{
    if (lt_dlinit()) {
        return PMIX_ERROR;
    }

#if PMIX_DL_LIBLTDL_HAVE_LT_DLADVISE
    pmix_pdl_plibltpdl_component_t *c = &mca_pdl_plibltpdl_component;

    if (lt_dladvise_init(&c->advise_private_noext)) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    if (lt_dladvise_init(&c->advise_private_ext) ||
        lt_dladvise_ext(&c->advise_private_ext)) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    if (lt_dladvise_init(&c->advise_public_noext) ||
        lt_dladvise_global(&c->advise_public_noext)) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    if (lt_dladvise_init(&c->advise_public_ext) ||
        lt_dladvise_global(&c->advise_public_ext) ||
        lt_dladvise_ext(&c->advise_public_ext)) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
#endif

    return PMIX_SUCCESS;
}


static int plibltpdl_component_close(void)
{
#if PMIX_DL_LIBLTDL_HAVE_LT_DLADVISE
    pmix_pdl_plibltpdl_component_t *c = &mca_pdl_plibltpdl_component;

    lt_dladvise_destroy(&c->advise_private_noext);
    lt_dladvise_destroy(&c->advise_private_ext);
    lt_dladvise_destroy(&c->advise_public_noext);
    lt_dladvise_destroy(&c->advise_public_ext);
#endif

    lt_dlexit();

    return PMIX_SUCCESS;
}


static int plibltpdl_component_query(mca_base_module_t **module, int *priority)
{
    /* The priority value is somewhat meaningless here; by
       pmix/mca/dl/configure.m4, there's at most one component
       available. */
    *priority = mca_pdl_plibltpdl_component.base.priority;
    *module = &pmix_pdl_plibltpdl_module.super;

    return PMIX_SUCCESS;
}
