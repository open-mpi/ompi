/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <pml_monitoring.h>
#include <ompi/constants.h>
#include <ompi/mca/pml/base/base.h>
#include <opal/mca/base/mca_base_component_repository.h>

static int mca_pml_monitoring_enabled = 0;
static int mca_pml_monitoring_active = 0;
mca_pml_base_component_t pml_selected_component;
mca_pml_base_module_t pml_selected_module;

extern void output_monitoring( void );
extern void finalize_monitoring( void );
extern int  ompi_mca_pml_monitoring_flush(char* filename);
int filter_monitoring( void );



/* Return 1 if the the seperation between internal tags and external tags is enabled*/
int filter_monitoring( void )
{
    if (mca_pml_monitoring_enabled == 2)
        return 1;
    else
        return 0;
}

static unsigned long hidden_fct = (unsigned long)((void*)ompi_mca_pml_monitoring_flush);
int mca_pml_monitoring_enable(bool enable)
{
    /* If we reach this point we were succesful at hijacking the interface of
     * the real PML, and we are now correctly interleaved between the upper
     * layer and the real PML.
     */
    mca_base_component_var_register(&mca_pml_monitoring_component.pmlm_version, "flush",
                                    "Hidden argument to provide the flush function pointer",
                                    MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL, 0, 0,
                                    OPAL_INFO_LVL_1,
                                    MCA_BASE_VAR_SCOPE_CONSTANT,
                                    &hidden_fct);
    return pml_selected_module.pml_enable(enable);
}

static int mca_pml_monitoring_component_open(void)
{
    if( mca_pml_monitoring_enabled ) {
        opal_pointer_array_add(&mca_pml_base_pml,
                               strdup(mca_pml_monitoring_component.pmlm_version.mca_component_name));
    }
    return OMPI_SUCCESS;
}

static int mca_pml_monitoring_component_close(void)
{
    if( mca_pml_monitoring_enabled ) {
        if( !mca_pml_monitoring_active ) {
            /* Save a copy of the selected PML */
            pml_selected_component = mca_pml_base_selected_component;
            pml_selected_module = mca_pml;
            /* And now install the interception layer */
            mca_pml_base_selected_component = mca_pml_monitoring_component;
            mca_pml = mca_pml_monitoring;
            mca_pml.pml_progress = pml_selected_module.pml_progress;
            /* Bump my ref count up to avoid getting released too early */
            mca_base_component_repository_retain_component(mca_pml_monitoring_component.pmlm_version.mca_type_name,
                                                           mca_pml_monitoring_component.pmlm_version.mca_component_name);
            mca_pml_monitoring_active = 1;
        }
    }
    return OMPI_SUCCESS;
}

static mca_pml_base_module_t*
mca_pml_monitoring_component_init(int* priority,
                                  bool enable_progress_threads,
                                  bool enable_mpi_threads)
{
    if( mca_pml_monitoring_enabled ) {
        *priority = 0;  /* I'm up but don't select me */
        return &mca_pml_monitoring;
    }
    return NULL;
}

static int mca_pml_monitoring_component_finish(void)
{
    if( mca_pml_monitoring_enabled && mca_pml_monitoring_active ) {
        /* It is over... Output what has been monitored*/
        output_monitoring();
        /* Free internal data structure */
        finalize_monitoring();
        /* Call the original PML and then close */
        mca_pml_monitoring_active = 0;
        mca_pml_monitoring_enabled = 0;
        /* Restore the original PML */
        mca_pml_base_selected_component = pml_selected_component;
        mca_pml = pml_selected_module;
        /* Redirect the close call to the original PML */
        pml_selected_component.pmlm_finalize();
        /**
         * We should never release the last ref on the current component or face forever punishement.
         */
        /* mca_base_component_repository_release(&mca_pml_monitoring_component.pmlm_version); */
    }
    return OMPI_SUCCESS;
}

static int mca_pml_monitoring_component_register(void)
{
    (void)mca_base_component_var_register(&mca_pml_monitoring_component.pmlm_version, "enable",
                                          "Enable the monitoring at the PML level. This value should be different than 0 in order for the monitoring to be enabled (default disable)", MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY, &mca_pml_monitoring_enabled);
    return OMPI_SUCCESS;
}

mca_pml_base_component_2_0_0_t mca_pml_monitoring_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        MCA_PML_BASE_VERSION_2_0_0,

        "monitoring", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        mca_pml_monitoring_component_open,  /* component open */
        mca_pml_monitoring_component_close, /* component close */
        NULL,
        mca_pml_monitoring_component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    mca_pml_monitoring_component_init,  /* component init */
    mca_pml_monitoring_component_finish   /* component finalize */

};

