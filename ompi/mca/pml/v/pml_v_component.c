/*
 * Copyright (c) 2004-2015 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/vprotocol/vprotocol.h"
#include "ompi/mca/vprotocol/base/base.h"
#include "pml_v_output.h"
#include "pml_v.h"

static int mca_pml_v_component_register(void);
static int mca_pml_v_component_open(void);
static int mca_pml_v_component_close(void);
static int mca_pml_v_component_parasite_close(void);

static mca_pml_base_module_t *mca_pml_v_component_init(int* priority, bool enable_progress_threads, bool enable_mpi_thread_multiple);
static int mca_pml_v_component_finalize(void);
static int mca_pml_v_component_parasite_finalize(void);

static int mca_pml_v_enable(bool enable);

mca_pml_base_component_2_0_0_t mca_pml_v_component =
{
  /* First, the mca_base_component_t struct containing meta
   * information about the component itself */
  {
    MCA_PML_BASE_VERSION_2_0_0,
    "v", /* MCA component name */
    OMPI_MAJOR_VERSION,  /* MCA component major version */
    OMPI_MINOR_VERSION,  /* MCA component minor version */
    OMPI_RELEASE_VERSION,  /* MCA component release version */
    mca_pml_v_component_open,
    mca_pml_v_component_close,
    NULL,
    mca_pml_v_component_register
  },
  {
      MCA_BASE_METADATA_PARAM_NONE /* Component is not checkpointable */
  },

  mca_pml_v_component_init,  /* component init */
  mca_pml_v_component_finalize   /* component finalize */
};

static bool pml_v_enable_progress_treads = OPAL_ENABLE_PROGRESS_THREADS;
static bool pml_v_enable_mpi_thread_multiple = OMPI_ENABLE_THREAD_MULTIPLE;

static char *ompi_pml_vprotocol_include_list;
static char *ompi_pml_v_output;
static int ompi_pml_v_verbose;

/*******************************************************************************
 * MCA level functions - parasite setup
 */
static int mca_pml_v_component_register(void)
{
    int var_id;

    ompi_pml_v_output = "stderr";
    (void) mca_base_component_var_register(&mca_pml_v_component.pmlm_version,
                                           "output", NULL, MCA_BASE_VAR_TYPE_STRING,
                                           NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_pml_v_output);

    ompi_pml_v_verbose = 0;
    (void) mca_base_component_var_register(&mca_pml_v_component.pmlm_version,
                                           "verbose", "Verbosity of the pml v component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_pml_v_verbose);

    ompi_pml_vprotocol_include_list = "";
    /* This parameter needs to go away if pml/v is unloaded so register it with a pml/v name */
    var_id = mca_base_component_var_register(&mca_pml_v_component.pmlm_version,
                                             "vprotocol", "Specify a specific vprotocol to use",
                                             MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                             OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                             &ompi_pml_vprotocol_include_list);
    (void) mca_base_var_register_synonym(var_id, "ompi", "vprotocol", NULL, NULL, 0);

    return OMPI_SUCCESS;
}

static int mca_pml_v_component_open(void)
{
    pml_v_output_open(ompi_pml_v_output, ompi_pml_v_verbose);

    V_OUTPUT_VERBOSE(500, "loaded");

    mca_vprotocol_base_set_include_list(ompi_pml_vprotocol_include_list);

    return mca_base_framework_open(&ompi_vprotocol_base_framework, 0);
}

static int mca_pml_v_component_close(void)
{
    if( NULL == mca_vprotocol_base_include_list ) {
        /* Nothing to do, let's just close and move away */
        return OMPI_SUCCESS;
    }

    /* Save original PML before making any changes  */
    mca_pml_v.host_pml_component = mca_pml_base_selected_component;
    mca_pml_v.host_pml = mca_pml;
    mca_pml_v.host_request_fns = ompi_request_functions;

    /* Do not load anything if no FT protocol is selected */
    if (NULL != mca_vprotocol_base_include_list && !mca_vprotocol_base_include_list[0]) {
        return mca_pml_v_component_parasite_close();
    }

    /* Make sure to close out output even if vprotocol isn't in use */
    pml_v_output_close ();

    /* Mark that we have changed something */
    snprintf(mca_pml_base_selected_component.pmlm_version.mca_component_name,
             MCA_BASE_MAX_TYPE_NAME_LEN, "%s]v%s",
             mca_pml_v.host_pml_component.pmlm_version.mca_component_name,
             mca_vprotocol_component.pmlm_version.mca_component_name);

    /* Replace finalize */
    mca_pml_base_selected_component.pmlm_finalize =
        mca_pml_v_component_parasite_finalize;

    /* Make sure we get initialized if some Vprotocol is enabled */
    mca_pml.pml_enable = mca_pml_v_enable;

    return OMPI_SUCCESS;
}

/*******************************************************************************
 * Parasite cleanup
 */
static int mca_pml_v_component_parasite_finalize(void)
{
    mca_base_component_list_item_t *cli = NULL;

    V_OUTPUT_VERBOSE(500, "parasite_finalize");

    /* Make sure we'll get closed again with the true close function */
    mca_pml_v_component.pmlm_version.mca_close_component =
        mca_pml_v_component_parasite_close;
    cli = OBJ_NEW(mca_base_component_list_item_t);
    cli->cli_component = (mca_base_component_t *) &mca_pml_v_component;
    opal_list_prepend(&ompi_pml_base_framework.framework_components,
                      (opal_list_item_t *) cli);

    /* finalize vprotocol component */
    if(mca_vprotocol_base_selected())
        mca_vprotocol_component.pmlm_finalize();

    if(mca_pml_v.host_pml_component.pmlm_finalize != NULL)
        return mca_pml_v.host_pml_component.pmlm_finalize();
    else
        return OMPI_SUCCESS;
}

static int mca_pml_v_component_parasite_close(void)
{
    V_OUTPUT_VERBOSE(500, "parasite_close: Ok, I accept to die and let %s component finish",
                          mca_pml_v.host_pml_component.pmlm_version.mca_component_name);
    mca_pml_base_selected_component = mca_pml_v.host_pml_component;

    (void) mca_base_framework_close(&ompi_vprotocol_base_framework);
    pml_v_output_close();

    mca_pml.pml_enable = mca_pml_v.host_pml.pml_enable;
    /* don't need to call the host component's close: pml_base will do it */
    return OMPI_SUCCESS; /* ignore any errors as we are leaving anyway */
}


/*******************************************************************************
 * Init/finalize for MCA PML components
 */
static mca_pml_base_module_t *mca_pml_v_component_init(int *priority,
                                                      bool enable_progress_threads,
                                                      bool enable_mpi_thread_multiple)
{
    V_OUTPUT_VERBOSE(1, "init: I'm not supposed to be here until BTL loading stuff gets fixed!? That's strange...");

    pml_v_enable_progress_treads = enable_progress_threads;
    pml_v_enable_mpi_thread_multiple = enable_mpi_thread_multiple;

    /* I NEVER want to be the selected PML, so I report less than possible
     * priority and a NULL module
     */
    *priority = -1;
    return NULL;
}

static int mca_pml_v_component_finalize(void)
{
    V_OUTPUT_VERBOSE(1, "finalize: I'm not supposed to be here until BTL loading stuff gets fixed!? That's strange...");
    /* Nothing to do here. We are not sure we need to be unloaded or not at
     * this stage
     */
    return OMPI_SUCCESS;
}


/*******************************************************************************
 * Enable the PML V (and initialize the Vprotocol)
 */
static int mca_pml_v_enable(bool enable)
{
    int ret;

    /* Enable the real PML (no threading issues there as threads are started
     * later)
     */
    ret = mca_pml_v.host_pml.pml_enable(enable);
    if(OMPI_SUCCESS != ret) return ret;

    if(enable) {
        /* Check if a protocol have been selected during init */
        if(! mca_vprotocol_base_selected())
            mca_vprotocol_base_select(pml_v_enable_progress_treads,
                                      pml_v_enable_mpi_thread_multiple);

        /* Check if we succeeded selecting a protocol */
        if(mca_vprotocol_base_selected()) {
            V_OUTPUT_VERBOSE(1, "I don't want to die: I will parasite %s host component %s with %s %s",
                             mca_pml_base_selected_component.pmlm_version.mca_type_name,
                             mca_pml_base_selected_component.pmlm_version.mca_component_name,
                             mca_vprotocol_component.pmlm_version.mca_type_name,
                             mca_vprotocol_component.pmlm_version.mca_component_name);

            ret = mca_vprotocol_base_parasite();
            if(OMPI_SUCCESS != ret) return ret;
            if(mca_vprotocol.enable)
                return mca_vprotocol.enable(enable);
            else
                return OMPI_SUCCESS;
        }
        V_OUTPUT_VERBOSE(1, "No fault tolerant protocol selected. All are unloaded");
    }
    /* Disable */
    mca_pml = mca_pml_v.host_pml;
    mca_pml.pml_enable = mca_pml_v_enable;
    /* /!\ This is incorrect if another component also changed the requests */
    ompi_request_functions = mca_pml_v.host_request_fns;
    return OMPI_SUCCESS;
}
