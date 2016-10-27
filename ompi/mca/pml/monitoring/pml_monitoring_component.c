/*
 * Copyright (c) 2013-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2016 Inria.  All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include <ompi/mca/common/monitoring/common_monitoring.h>
#include <opal/mca/base/mca_base_component_repository.h>

mca_pml_base_component_t pml_selected_component = {{0}};
mca_pml_base_module_t pml_selected_module = {0};

mca_pml_monitoring_module_t mca_pml_monitoring = {
    mca_pml_monitoring_add_procs,
    mca_pml_monitoring_del_procs,
    mca_pml_monitoring_enable,
    NULL,
    mca_pml_monitoring_add_comm,
    mca_pml_monitoring_del_comm,
    mca_pml_monitoring_irecv_init,
    mca_pml_monitoring_irecv,
    mca_pml_monitoring_recv,
    mca_pml_monitoring_isend_init,
    mca_pml_monitoring_isend,
    mca_pml_monitoring_send,
    mca_pml_monitoring_iprobe,
    mca_pml_monitoring_probe,
    mca_pml_monitoring_start,
    mca_pml_monitoring_improbe,
    mca_pml_monitoring_mprobe,
    mca_pml_monitoring_imrecv,
    mca_pml_monitoring_mrecv,
    mca_pml_monitoring_dump,
    NULL,
    65535,
    INT_MAX
};

/**
 * This PML monitors only the processes in the MPI_COMM_WORLD. As OMPI is now lazily
 * adding peers on the first call to add_procs we need to check how many processes
 * are in the MPI_COMM_WORLD to create the storage with the right size.
 */
int mca_pml_monitoring_add_procs(struct ompi_proc_t **procs,
                                 size_t nprocs)
{
    int ret = common_monitoring_add_procs(procs, nprocs);
    if( OMPI_SUCCESS == ret )
        ret = pml_selected_module.pml_add_procs(procs, nprocs);
    return ret;
}

/**
 * Pass the information down the PML stack.
 */
int mca_pml_monitoring_del_procs(struct ompi_proc_t **procs,
                                 size_t nprocs)
{
    return pml_selected_module.pml_del_procs(procs, nprocs);
}

int mca_pml_monitoring_dump(struct ompi_communicator_t* comm,
                            int verbose)
{
    return pml_selected_module.pml_dump(comm, verbose);
}

int mca_pml_monitoring_enable(bool enable)
{
    common_monitoring_enable(enable, &mca_pml_monitoring_component);
    return pml_selected_module.pml_enable(enable);
}

static int mca_pml_monitoring_component_open(void)
{
    if( mca_common_monitoring_enabled ) {
        opal_pointer_array_add(&mca_pml_base_pml,
                               strdup(mca_pml_monitoring_component.pmlm_version.mca_component_name));
    }
    return OMPI_SUCCESS;
}

static int mca_pml_monitoring_component_close(void)
{
    if( !mca_common_monitoring_enabled )
        goto release_and_return;

    /**
     * If this component is already active, then we are currently monitoring
     * the execution and this call to close if the one from MPI_Finalize.
     * Clean up and release the extra reference on ourselves.
     */
    if( mca_common_monitoring_active ) {  /* Already active, turn off */
        pml_selected_component.pmlm_version.mca_close_component();
        memset(&pml_selected_component, 0, sizeof(mca_pml_base_component_t));
        memset(&pml_selected_module, 0, sizeof(mca_pml_base_module_t));
        mca_base_component_repository_release((mca_base_component_t*)&mca_pml_monitoring_component);
        mca_common_monitoring_active = 0;
        goto release_and_return;
    }

    /**
     * We are supposed to monitor the execution. Save the winner PML component and
     * module, and swap it with ourselves. Increase our refcount so that we are
     * not dlclose.
     */
    if( OPAL_SUCCESS != mca_base_component_repository_retain_component(mca_pml_monitoring_component.pmlm_version.mca_type_name,
                                                                       mca_pml_monitoring_component.pmlm_version.mca_component_name) ) {
        return OMPI_ERROR;
    }

    /* Save a copy of the selected PML */
    pml_selected_component = mca_pml_base_selected_component;
    pml_selected_module = mca_pml;
    /* Install our interception layer */
    mca_pml_base_selected_component = mca_pml_monitoring_component;
    mca_pml = mca_pml_monitoring;
    /* Restore some of the original valued: progress, flags, tags and context id */
    mca_pml.pml_progress = pml_selected_module.pml_progress;
    mca_pml.pml_max_contextid = pml_selected_module.pml_max_contextid;
    mca_pml.pml_max_tag = pml_selected_module.pml_max_tag;
    mca_pml.pml_flags = pml_selected_module.pml_flags;

    mca_common_monitoring_active = 1;

    return OMPI_SUCCESS;

 release_and_return:
    if( NULL != mca_common_monitoring_current_filename ) {
        if( NULL != *mca_common_monitoring_current_filename ) {
            free(*mca_common_monitoring_current_filename);
            *mca_common_monitoring_current_filename = NULL;
        }
        /* The variable will be freed when desallocating the pvar's */
        mca_common_monitoring_current_filename = NULL;
    }
    return OMPI_SUCCESS;
}

static mca_pml_base_module_t*
mca_pml_monitoring_component_init(int* priority,
                                  bool enable_progress_threads,
                                  bool enable_mpi_threads)
{
    if( mca_common_monitoring_enabled ) {
        *priority = 0;  /* I'm up but don't select me */
        return &mca_pml_monitoring;
    }
    return NULL;
}

static int mca_pml_monitoring_component_finish(void)
{
    if( mca_common_monitoring_enabled && mca_common_monitoring_active ) {
        /* If we are not drived by MPIT then dump the monitoring information */
        if( mca_common_monitoring_output_enabled )
  	    common_monitoring_flush(mca_common_monitoring_output_enabled, *mca_common_monitoring_current_filename);
        /* Free internal data structure */
        common_monitoring_finalize();
        /* Call the original PML and then close */
        mca_common_monitoring_active = 0;
        mca_common_monitoring_enabled = 0;
        /* Restore the original PML */
        mca_pml_base_selected_component = pml_selected_component;
        mca_pml = pml_selected_module;
        /* Redirect the close call to the original PML */
        pml_selected_component.pmlm_finalize();
        /**
         * We should never release the last ref on the current
         * component or face forever punishement.
         */
        /* mca_base_component_repository_release(&mca_common_monitoring_component.pmlm_version); */
    }
    return OMPI_SUCCESS;
}

static int mca_pml_monitoring_component_register(void)
{
    /* Because we are playing tricks with the component close, we should not
     * use mca_base_component_var_register but instead stay with the basic
     * version mca_base_var_register.
     */
    (void)mca_base_component_var_register(&mca_pml_monitoring_component.pmlm_version, "enable",
                                          "Enable the monitoring at the PML level. A value of 0 "
                                          "will disable the monitoring (default). "
                                          "A value of 1 will aggregate all monitoring "
                                          "information (point-to-point and collective). "
                                          "Any other value will enable filtered monitoring",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_4,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_common_monitoring_enabled);

    (void)mca_base_component_var_register(&mca_pml_monitoring_component.pmlm_version,
                                          "enable_output", "Enable the PML monitoring textual "
                                          "output at MPI_Finalize (it will be automatically "
                                          "turned off when MPIT is used to monitor "
                                          "communications). This value should be different than "
                                          "0 in order for the output to be enabled (default "
                                          "disable)", MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_common_monitoring_output_enabled);
    
    mca_common_monitoring_current_state = mca_common_monitoring_enabled;

    if( NULL == mca_common_monitoring_current_filename )
	mca_common_monitoring_current_filename = malloc(sizeof(char*));

    (void)mca_base_var_register("ompi", "pml", "monitoring", "filename",
                                /*&mca_common_monitoring_component.pmlm_version, "filename",*/
                                "The name of the file where the monitoring information should be "
                                "saved (the filename will be extended with the process rank and "
                                "the \".prof\" extension). If this field is NULL the monitoring "
                                "will not be saved.", MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                OPAL_INFO_LVL_4,
                                MCA_BASE_VAR_SCOPE_READONLY,
                                mca_common_monitoring_current_filename);
    /* Now that the MCA variables are automatically unregistered when their component
     * close, we need to keep a safe copy of the filename.
     */
    if( NULL != mca_common_monitoring_current_filename && NULL != *mca_common_monitoring_current_filename )
        *mca_common_monitoring_current_filename = strdup(*mca_common_monitoring_current_filename);

    (void)mca_base_pvar_register("ompi", "pml", "monitoring", "messages_count",
                                 "Number of messages sent to each peer in a communicator",
                                 OPAL_INFO_LVL_4, MPI_T_PVAR_CLASS_SIZE,
                                 MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, MPI_T_BIND_MPI_COMM,
                                 MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                 common_monitoring_get_messages_count, NULL,
                                 common_monitoring_comm_size_notify, NULL);

    (void)mca_base_pvar_register("ompi", "pml", "monitoring", "messages_size", "Size of messages "
                                 "sent to each peer in a communicator", OPAL_INFO_LVL_4,
                                 MPI_T_PVAR_CLASS_SIZE,
                                 MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, MPI_T_BIND_MPI_COMM,
                                 MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                 common_monitoring_get_messages_size, NULL,
                                 common_monitoring_comm_size_notify, NULL);

    return OMPI_SUCCESS;
}

mca_pml_base_component_2_0_0_t mca_pml_monitoring_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    .pmlm_version = {
        MCA_PML_BASE_VERSION_2_0_0,

        .mca_component_name = "monitoring", /* MCA component name */
        MCA_MONITORING_MAKE_VERSION,
        .mca_open_component = mca_pml_monitoring_component_open,  /* component open */
        .mca_close_component = mca_pml_monitoring_component_close, /* component close */
        .mca_register_component_params = mca_pml_monitoring_component_register
    },
    .pmlm_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    .pmlm_init = mca_pml_monitoring_component_init,  /* component init */
    .pmlm_finalize = mca_pml_monitoring_component_finish   /* component finalize */
};

