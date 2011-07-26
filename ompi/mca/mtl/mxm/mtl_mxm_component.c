/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "orte/util/show_help.h"
#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/proc/proc.h"

#include "mtl_mxm.h"
#include "mtl_mxm_types.h"
#include "mtl_mxm_request.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

static int ompi_mtl_mxm_component_open(void);
static int ompi_mtl_mxm_component_close(void);
static int ompi_mtl_mxm_component_register(void);

int mca_mtl_mxm_output = -1;


static mca_mtl_base_module_t
        * ompi_mtl_mxm_component_init(bool enable_progress_threads,
                                      bool enable_mpi_threads);

mca_mtl_mxm_component_t mca_mtl_mxm_component = {
{
    /*
     * First, the mca_base_component_t struct containing meta
     * information about the component itself
     */
    {
        MCA_MTL_BASE_VERSION_2_0_0,
        "mxm", /* MCA component name */
        OMPI_MAJOR_VERSION, /* MCA component major version */
        OMPI_MINOR_VERSION, /* MCA component minor version */
        OMPI_RELEASE_VERSION, /* MCA component release version */
        ompi_mtl_mxm_component_open, /* component open */
        ompi_mtl_mxm_component_close, /* component close */
        NULL,
        ompi_mtl_mxm_component_register
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },
    ompi_mtl_mxm_component_init /* component init */
}
};

static int ompi_mtl_mxm_component_register(void)
{

    mca_base_param_reg_int(&mca_mtl_mxm_component.super.mtl_version, "verbose",
                           "Verbose level of the MXM component",
                           false, false,
                           0,
                           &ompi_mtl_mxm.verbose);

    return OMPI_SUCCESS;
}

static int ompi_mtl_mxm_component_open(void)
{
    struct stat st;

    /* Component available only if IB hardware is present */
    if (0 == stat("/dev/infiniband/uverbs0", &st)) {
        return OMPI_SUCCESS;
    } else {
        return OPAL_ERR_NOT_AVAILABLE;
    }
}

static int ompi_mtl_mxm_component_close(void)
{
    return OMPI_SUCCESS;
}

static mca_mtl_base_module_t*
ompi_mtl_mxm_component_init(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    mxm_context_opts_t mxm_opts;
    mxm_error_t err;
    int rc;

    mca_mtl_mxm_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_mtl_mxm_output, ompi_mtl_mxm.verbose);

    mxm_fill_context_opts(&mxm_opts);
    err = mxm_init(&mxm_opts, &ompi_mtl_mxm.mxm_context);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "mxm init", true,
                       mxm_error_string(err));
        return NULL;
    }

    rc = ompi_mtl_mxm_module_init();
    if (OMPI_SUCCESS != rc) {
    	return NULL;
    }

    /* Calculate MTL constraints according to MXM types */
    ompi_mtl_mxm.super.mtl_max_contextid = 1UL << (sizeof(mxm_ctxid_t) * 8);
    ompi_mtl_mxm.super.mtl_max_tag       = 1UL << (sizeof(mxm_tag_t) * 8 - 2);
    ompi_mtl_mxm.super.mtl_request_size  =
            sizeof(mca_mtl_mxm_request_t) - sizeof(struct mca_mtl_request_t);
    return &ompi_mtl_mxm.super;
}

