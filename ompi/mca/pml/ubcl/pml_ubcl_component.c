/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2024 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_component.c
 *
 * UBCL PML component implementation
 *
 * Functions parameters and return values defined in ompi/mca/pml/pml.h.
 */

#include "opal/include/opal_config.h"

#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_utils.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_request.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_endpoint.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "opal/prefetch.h"
#include "opal/util/proc.h"

#include <ubcl_api.h>

/**
 * PML UBCL Component
 */
mca_pml_ubcl_component_t mca_pml_ubcl_component = {
    {
        .pmlm_version = {
            MCA_PML_BASE_VERSION_2_1_0,

            .mca_component_name = "ubcl",
            .mca_component_major_version = OMPI_MAJOR_VERSION,
            .mca_component_minor_version = OMPI_MINOR_VERSION,
            .mca_component_release_version = OMPI_RELEASE_VERSION,
            .mca_open_component = mca_pml_ubcl_component_open,
            .mca_close_component = mca_pml_ubcl_component_close,
            .mca_register_component_params = mca_pml_ubcl_component_register
        },
        .pmlm_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        .pmlm_init = mca_pml_ubcl_component_init,
        .pmlm_finalize = mca_pml_ubcl_component_finalize,
    },

    .is_init = 0,
    .accelerator_is_cuda = false,
    .nprocs = 0,
};

/**
 * Open opal output, 0-initialize some parameters and forward to communication
 * modules
 */
int mca_pml_ubcl_component_open(void)
{
    /* Open output stream */
    if (0 < mca_pml_ubcl_component.verbose || mca_pml_ubcl_component.gdb_attach) {
        mca_pml_ubcl_component.output = opal_output_open(NULL);
        int verbose = mca_pml_ubcl_component.verbose > 0 ? mca_pml_ubcl_component.verbose : 1;
        opal_output_set_verbosity(mca_pml_ubcl_component.output, verbose);
    } else {
        mca_pml_ubcl_component.output = -1;
    }

    /* If MCA param set, wait until gdb_attach is set to 0 from outside */
    if (mca_pml_ubcl_component.gdb_attach) {
        opal_output_verbose(1, mca_pml_ubcl_component.output,
                            "set mca_pml_ubcl_component.gdb_attach = 0\n");
        while (mca_pml_ubcl_component.gdb_attach) {
            sleep(1);
        };
    }

    return OMPI_SUCCESS;
}

/**
 * Close communication modules and opal output
 */
int mca_pml_ubcl_component_close(void)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_COMPONENT_CLOSE\n"));
    return OMPI_SUCCESS;
}

int mca_pml_ubcl_component_register(void)
{
    mca_base_component_t *component = &mca_pml_ubcl_component.super.pmlm_version;

    mca_pml_ubcl_component.verbose = 0;
    (void) mca_base_component_var_register(component, "verbose", "Verbosity level of the pml/ubcl.",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_pml_ubcl_component.verbose);

    mca_pml_ubcl_component.priority = 90;
    (void) mca_base_component_var_register(component, "priority",
                                           "Priority of the pml/ubcl component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_pml_ubcl_component.priority);

    mca_pml_ubcl_component.force_intranode_bxi = false;
    (void) mca_base_component_var_register(component, "force_intranode_bxi",
                                           "Whether to force intranode communication to go through "
                                           "BXI network instead of shared memory.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_pml_ubcl_component.force_intranode_bxi);

    mca_pml_ubcl_component.force_cuda_custom_dt = false;
    (void) mca_base_component_var_register(component, "force_cuda_custom_dt",
                                           "Force the pml/ubcl to use custom datatype to pack/unpack cuda "
                                           "buffers. This prevents the use of ADGE by UBCL",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_pml_ubcl_component.force_cuda_custom_dt);

    mca_pml_ubcl_component.can_progress = false;
    (void) mca_base_component_var_register(
        component, "can_progress",
        "Allow PML to call opal_progress() once at the end of each primitive.",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_pml_ubcl_component.can_progress);

    mca_pml_ubcl_component.warn_on_truncate = true;
    (void) mca_base_component_var_register(
        component, "warn_on_truncate",
        "Allow PML to print warning messages whenever a truncation error is detected",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_pml_ubcl_component.warn_on_truncate);

    mca_pml_ubcl_component.abort_on_truncate = true;
    (void) mca_base_component_var_register(
        component, "abort_on_truncate",
        "Allow PML to print error and abort in case of MPI_ERR_TRUNCATE", MCA_BASE_VAR_TYPE_BOOL,
        NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_pml_ubcl_component.abort_on_truncate);

    mca_pml_ubcl_component.use_mpi_wildcards = true;
    (void) mca_base_component_var_register(
        component, "use_mpi_wildcards",
        "MPI_ANY_SOURCE or MPI_ANY_TAG are used. For better performance this should be disabled.",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_pml_ubcl_component.use_mpi_wildcards);

    mca_pml_ubcl_component.gdb_attach = false;
    (void) mca_base_component_var_register(
        component, "gdb_attach",
        "Allow to attach a debugger by looping indefinitly on this value until 0.",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_pml_ubcl_component.gdb_attach);


    mca_pml_ubcl_component.max_req = 32768;
    (void) mca_base_component_var_register(component, "max_req",
                                           "Maximum number of requests allocated. (0 means infinite)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_pml_ubcl_component.max_req);

    mca_pml_ubcl_component.min_req = 1024;
    (void) mca_base_component_var_register(component, "min_req",
                                           "Minimum (and initial) number of requests allocated.",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_pml_ubcl_component.min_req);

    mca_pml_ubcl_component.incr_req = 1024;
    (void) mca_base_component_var_register(component, "incr_req",
                                           "Increasing number of requests allocated.",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_pml_ubcl_component.incr_req);

    mca_common_ubcl_register_mca();

    return OMPI_SUCCESS;
}

static void mca_pml_ubcl_check_cuda_accelerator()
{
    const char* cuda_component_name = "cuda";
    const char* selected_component_name = opal_accelerator_base_selected_component.base_version.mca_component_name;

    /* Check if we are currently using accelerator cuda */
    /* Only one single accelerator can be selected/active. Knowing if it's the
     * cuda accelerator let us know if our device buffers are cuda or not */
    if (0 == strcmp(cuda_component_name, selected_component_name)) {
        mca_pml_ubcl_component.accelerator_is_cuda = true;
    }
}

/**
 * Initialize parameters and forward to communication modules
 */
mca_pml_base_module_t *mca_pml_ubcl_component_init(int *priority, bool enable_progress_threads,
                                                   bool enable_mpi_threads)
{
    int err;

    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_COMPONENT_INIT\n"));

    /* Register thread level */
    mca_pml_ubcl_component.thread_multiple_enabled = enable_progress_threads || enable_mpi_threads;

    if (OPAL_SUCCESS != mca_common_ubcl_init()) {
        mca_pml_ubcl_warn(OMPI_ERR_NOT_AVAILABLE, "common_ubcl could not load UBCL library\n");
        return NULL;
    }

    OBJ_CONSTRUCT(&mca_pml_ubcl_component.pml_req_free_list, opal_free_list_t);
    err = opal_free_list_init (&mca_pml_ubcl_component.pml_req_free_list,
			       sizeof(mca_pml_ubcl_request_t),
			       opal_cache_line_size,
			       OBJ_CLASS(mca_pml_ubcl_request_t),
			       0, opal_cache_line_size,
			       mca_pml_ubcl_component.min_req,
			       mca_pml_ubcl_component.max_req,
			       mca_pml_ubcl_component.incr_req,
			       NULL, 0, NULL, NULL, NULL);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
        mca_pml_ubcl_warn(OMPI_ERR_OUT_OF_RESOURCE, "Not enough memory (%d)", err);
        return NULL;
    }

    /* Initialize UBCL */
    if (UBCL_SUCCESS != ubcl_init(mca_pml_ubcl_component.thread_multiple_enabled)) {
        return NULL;
    }

    err = mca_pml_ubcl_create_local_endpoint();
    if (OMPI_SUCCESS != err) {
        return NULL;
    }
    mca_pml_ubcl_check_cuda_accelerator();

    /* Mark as initialized, set priority and return */
    mca_pml_ubcl_component.is_init = 1;
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "INITIATION DONE\n"));
    *priority = mca_pml_ubcl_component.priority;
    return &mca_pml_ubcl_module.super;
}

/**
 * Finalize parameters and forward to communication modules
 */
int mca_pml_ubcl_component_finalize(void)
{
    int ompi_ret;
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "ubcl_COMPONENT_FINALIZE"));

    if (0 == mca_pml_ubcl_component.is_init) {
        return OMPI_SUCCESS;
    }

    ompi_ret = mca_pml_ubcl_free_local_endpoints();
    if (OMPI_SUCCESS != ompi_ret) {
        return ompi_ret;
    }

    /* Finalize UBCL */
    if (UBCL_SUCCESS != ubcl_fini()) {
        return OMPI_ERROR;
    }

    OBJ_DESTRUCT(&mca_pml_ubcl_component.pml_req_free_list);

    if (OPAL_SUCCESS != mca_common_ubcl_fini()) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
