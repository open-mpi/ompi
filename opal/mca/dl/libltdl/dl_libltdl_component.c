/*
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/dl/dl.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/util/argv.h"

#include "dl_libltdl.h"


/*
 * Public string showing the sysinfo ompi_linux component version number
 */
const char *opal_dl_libltdl_component_version_string =
    "OPAL dl libltdl MCA component version " OPAL_VERSION;


/*
 * Local functions
 */
static int libltdl_component_register(void);
static int libltdl_component_open(void);
static int libltdl_component_close(void);
static int libltdl_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_dl_libltdl_component_t mca_dl_libltdl_component = {

    /* Fill in the mca_dl_base_component_t */
    .base = {

        /* First, the mca_component_t struct containing meta information
           about the component itself */
        .base_version = {
            OPAL_DL_BASE_VERSION_1_0_0,

            /* Component name and version */
            "libltdl",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* Component functions */
            .mca_register_component_params = libltdl_component_register,
            .mca_open_component = libltdl_component_open,
            .mca_close_component = libltdl_component_close,
            .mca_query_component = libltdl_component_query,
        },

        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* The dl framework members */
        .priority = 50
    }

    /* Now fill in the libltdl component-specific members */
};


static int libltdl_component_register(void)
{
    /* Register an info param indicating whether we have lt_dladvise
       support or not */
    bool supported = OPAL_INT_TO_BOOL(OPAL_DL_LIBLTDL_HAVE_LT_DLADVISE);
    mca_base_component_var_register(&mca_dl_libltdl_component.base.base_version,
                                    "have_lt_dladvise",
                                    "Whether the version of libltdl that this component is built against supports lt_dladvise functionality or not",
                                    MCA_BASE_VAR_TYPE_BOOL,
                                    NULL,
                                    0,
                                    MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                    OPAL_INFO_LVL_7,
                                    MCA_BASE_VAR_SCOPE_CONSTANT,
                                    &supported);

    return OPAL_SUCCESS;
}

static int libltdl_component_open(void)
{
    if (lt_dlinit()) {
        return OPAL_ERROR;
    }

#if OPAL_DL_LIBLTDL_HAVE_LT_DLADVISE
    opal_dl_libltdl_component_t *c = &mca_dl_libltdl_component;

    if (lt_dladvise_init(&c->advise_private_noext)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (lt_dladvise_init(&c->advise_private_ext) ||
        lt_dladvise_ext(&c->advise_private_ext)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (lt_dladvise_init(&c->advise_public_noext) ||
        lt_dladvise_global(&c->advise_public_noext)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (lt_dladvise_init(&c->advise_public_ext) ||
        lt_dladvise_global(&c->advise_public_ext) ||
        lt_dladvise_ext(&c->advise_public_ext)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
#endif

    return OPAL_SUCCESS;
}


static int libltdl_component_close(void)
{
#if OPAL_DL_LIBLTDL_HAVE_LT_DLADVISE
    opal_dl_libltdl_component_t *c = &mca_dl_libltdl_component;

    lt_dladvise_destroy(&c->advise_private_noext);
    lt_dladvise_destroy(&c->advise_private_ext);
    lt_dladvise_destroy(&c->advise_public_noext);
    lt_dladvise_destroy(&c->advise_public_ext);
#endif

    lt_dlexit();

    return OPAL_SUCCESS;
}


static int libltdl_component_query(mca_base_module_t **module, int *priority)
{
    /* The priority value is somewhat meaningless here; by
       opal/mca/dl/configure.m4, there's at most one component
       available. */
    *priority = mca_dl_libltdl_component.base.priority;
    *module = &opal_dl_libltdl_module.super;

    return OPAL_SUCCESS;
}
