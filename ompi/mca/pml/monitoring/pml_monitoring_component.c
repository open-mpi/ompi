/*
 * Copyright (c) 2013-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2017 Inria.  All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "pml_monitoring.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/common/monitoring/common_monitoring.h"
#include "opal/mca/base/mca_base_component_repository.h"

static int mca_pml_monitoring_active = 0;

mca_pml_base_component_t pml_selected_component = {{0}};
mca_pml_base_module_t pml_selected_module = {0};

mca_pml_monitoring_module_t mca_pml_monitoring_module = {
    .pml_add_procs          = mca_pml_monitoring_add_procs,
    .pml_del_procs          = mca_pml_monitoring_del_procs,
    .pml_enable             = mca_pml_monitoring_enable,
    .pml_progress           = NULL,
    .pml_add_comm           = mca_pml_monitoring_add_comm,
    .pml_del_comm           = mca_pml_monitoring_del_comm,
#if OPAL_ENABLE_FT_MPI
    .pml_revoke_comm        = mca_pml_monitoring_revoke_comm,
#endif
    .pml_irecv_init         = mca_pml_monitoring_irecv_init,
    .pml_irecv              = mca_pml_monitoring_irecv,
    .pml_recv               = mca_pml_monitoring_recv,
    .pml_isend_init         = mca_pml_monitoring_isend_init,
    .pml_isend              = mca_pml_monitoring_isend,
    .pml_send               = mca_pml_monitoring_send,
    .pml_iprobe             = mca_pml_monitoring_iprobe,
    .pml_probe              = mca_pml_monitoring_probe,
    .pml_start              = mca_pml_monitoring_start,
    .pml_improbe            = mca_pml_monitoring_improbe,
    .pml_mprobe             = mca_pml_monitoring_mprobe,
    .pml_imrecv             = mca_pml_monitoring_imrecv,
    .pml_mrecv              = mca_pml_monitoring_mrecv,
    .pml_dump               = mca_pml_monitoring_dump,
    .pml_max_contextid      = 65535,
    .pml_max_tag            = INT_MAX,
    .pml_flags              = 0 /* flags */
};

/**
 * This PML monitors only the processes in the MPI_COMM_WORLD. As OMPI is now lazily
 * adding peers on the first call to add_procs we need to check how many processes
 * are in the MPI_COMM_WORLD to create the storage with the right size.
 */
int mca_pml_monitoring_add_procs(struct ompi_proc_t **procs,
                                 size_t nprocs)
{
    int ret = mca_common_monitoring_add_procs(procs, nprocs);
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
    return pml_selected_module.pml_enable(enable);
}

static int mca_pml_monitoring_component_open(void)
{
    /* CF: What if we are the only PML available ?? */
    if( mca_common_monitoring_enabled ) {
        opal_pointer_array_add(&mca_pml_base_pml,
                               strdup(mca_pml_monitoring_component.pmlm_version.mca_component_name));
    }
    return OMPI_SUCCESS;
}

static mca_pml_base_module_t*
mca_pml_monitoring_component_init(int* priority,
                                  bool enable_progress_threads,
                                  bool enable_mpi_threads)
{
    if( (OMPI_SUCCESS == mca_common_monitoring_init()) &&
        mca_common_monitoring_enabled ) {
        *priority = 0;  /* I'm up but don't select me */
        return &mca_pml_monitoring_module;
    }
    return NULL;
}

static int mca_pml_monitoring_component_finish(void)
{
    if( !mca_common_monitoring_enabled )
        return OMPI_SUCCESS;
    if( !mca_pml_monitoring_active ) {
        /* The monitoring component priority is always low to guarantee that the component
         * is never selected. Thus, the first time component_finish is called it is right
         * after the selection of the best PML was done, and the perfect moment to intercept
         * it. At this point we remove ourselves from ompi_pml_base_framework.framework_components
         * so that the component never gets closed and unloaded and it's VARs are safe for
         * the rest of the execution.
         */
        mca_pml_base_component_t *component = NULL;
        mca_base_component_list_item_t *cli = NULL;
        OPAL_LIST_FOREACH(cli, &ompi_pml_base_framework.framework_components, mca_base_component_list_item_t) {
            component = (mca_pml_base_component_t *) cli->cli_component;

            if( component == &mca_pml_monitoring_component ) {
                opal_list_remove_item(&ompi_pml_base_framework.framework_components, (opal_list_item_t*)cli);
                OBJ_RELEASE(cli);
                break;
            }
        }
        /**
         * We are supposed to monitor the execution. Save the winner PML component and
         * module, and swap it with ourselves. Increase our refcount so that we are
         * not dlclose.
         */
        /* Save a copy of the selected PML */
        pml_selected_component = mca_pml_base_selected_component;
        pml_selected_module = mca_pml;
        /* Install our interception layer */
        mca_pml_base_selected_component = mca_pml_monitoring_component;
        mca_pml = mca_pml_monitoring_module;

        /* Restore some of the original values: progress, flags, tags and context id */
        mca_pml.pml_progress = pml_selected_module.pml_progress;
        mca_pml.pml_max_contextid = pml_selected_module.pml_max_contextid;
        mca_pml.pml_max_tag = pml_selected_module.pml_max_tag;
        /* Add MCA_PML_BASE_FLAG_REQUIRE_WORLD flag to ensure the hashtable is properly initialized */
        mca_pml.pml_flags = pml_selected_module.pml_flags | MCA_PML_BASE_FLAG_REQUIRE_WORLD;

        mca_pml_monitoring_active = 1;
    } else {
        /**
         * This is the second call to component_finalize, and the component is actively
         * intercepting the calls to the best PML. Time to stop and cleanly finalize ourself.
         */

        /* Restore the original PML */
        mca_pml_base_selected_component = pml_selected_component;
        mca_pml = pml_selected_module;
        /* Redirect the close call to the original PML */
        pml_selected_component.pmlm_finalize();

        /* Free internal data structure */
        mca_common_monitoring_finalize();

        /**
         * We are in the compoenent code itself, we need to prevent the dlloader from
         * removing the code. This will result in minimal memory leaks, but it is the only
         * way to remove most of the references to the component (including the *vars).
         */
        mca_base_component_repository_retain_component(mca_pml_monitoring_component.pmlm_version.mca_type_name,
                                                       mca_pml_monitoring_component.pmlm_version.mca_component_name);
        /* Release all memory and be gone. */
        mca_base_component_close((mca_base_component_t*)&mca_pml_monitoring_component,
                                 ompi_pml_base_framework.framework_output);
    }
    return OMPI_SUCCESS;
}

mca_pml_base_component_2_1_0_t mca_pml_monitoring_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    .pmlm_version = {
        MCA_PML_BASE_VERSION_2_1_0,

        .mca_component_name = "monitoring", /* MCA component name */
        MCA_MONITORING_MAKE_VERSION,
        .mca_open_component = mca_pml_monitoring_component_open,  /* component open */
        .mca_close_component = NULL, /* component close */
        .mca_register_component_params = mca_common_monitoring_register
    },
    .pmlm_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    .pmlm_init = mca_pml_monitoring_component_init,  /* component init */
    .pmlm_finalize = mca_pml_monitoring_component_finish   /* component finalize */
};

