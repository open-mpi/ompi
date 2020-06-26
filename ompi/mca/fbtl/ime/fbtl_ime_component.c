/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "fbtl_ime.h"
#include "mpi.h"

int mca_fbtl_ime_priority = FBTL_IME_BASE_PRIORITY;
int mca_fbtl_ime_iov_max = FBTL_IME_IOV_MAX;
int mca_fbtl_ime_aio_reqs_max = FBTL_IME_AIO_REQS_MAX;

/*
 * Private functions
 */
static int register_component(void);

/*
 * Public string showing the fbtl ime component version number
 */
const char *mca_fbtl_ime_component_version_string =
  "OMPI/MPI IME FBTL MCA component version " OMPI_VERSION;


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_fbtl_base_component_2_0_0_t mca_fbtl_ime_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    .fbtlm_version = {
        MCA_FBTL_BASE_VERSION_2_0_0,

        /* Component name and version */
        .mca_component_name = "ime",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_register_component_params = register_component,
    },
    .fbtlm_data = {
        /* This component is checkpointable */
      MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .fbtlm_init_query = mca_fbtl_ime_component_init_query,      /* get thread level */
    .fbtlm_file_query = mca_fbtl_ime_component_file_query,      /* get priority and actions */
    .fbtlm_file_unquery = mca_fbtl_ime_component_file_unquery,  /* undo what was done by previous function */
};

static int register_component(void)
{
    mca_fbtl_ime_iov_max = FBTL_IME_IOV_MAX;
    (void) mca_base_component_var_register(&mca_fbtl_ime_component.fbtlm_version,
                                           "iov_max", "Maximum iov count that should be used when "
                                           "calling an IME native function",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fbtl_ime_iov_max);

    mca_fbtl_ime_aio_reqs_max = FBTL_IME_AIO_REQS_MAX;
    (void) mca_base_component_var_register(&mca_fbtl_ime_component.fbtlm_version,
                                           "aio_reqs_max", "Maximum number of aiocb requests that should "
                                           "be sent simultaneously when calling an IME native function",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_fbtl_ime_aio_reqs_max );

    return OMPI_SUCCESS;
}
