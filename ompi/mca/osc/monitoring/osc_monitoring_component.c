/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <osc_monitoring.h>
#include <ompi/constants.h>
#include <ompi/mca/osc/base/base.h>
#include <opal/mca/base/mca_base_component_repository.h>

int mca_osc_monitoring_enabled = 0;
int mca_osc_monitoring_active = 0;

ompi_osc_base_module_t mca_osc_monitoring_template = {
    .osc_win_attach = ompi_osc_monitoring_attach,
    .osc_win_detach  = ompi_osc_monitoring_detach,
    .osc_free = ompi_osc_monitoring_free,

    .osc_put = ompi_osc_monitoring_put,
    .osc_get = ompi_osc_monitoring_get,
    .osc_accumulate = ompi_osc_monitoring_accumulate,
    .osc_compare_and_swap = ompi_osc_monitoring_compare_and_swap,
    .osc_fetch_and_op = ompi_osc_monitoring_fetch_and_op,
    .osc_get_accumulate = ompi_osc_monitoring_get_accumulate,

    .osc_rput = ompi_osc_monitoring_rput,
    .osc_rget = ompi_osc_monitoring_rget,
    .osc_raccumulate = ompi_osc_monitoring_raccumulate,
    .osc_rget_accumulate = ompi_osc_monitoring_rget_accumulate,

    .osc_fence = ompi_osc_monitoring_fence,

    .osc_start = ompi_osc_monitoring_start,
    .osc_complete = ompi_osc_monitoring_complete,
    .osc_post = ompi_osc_monitoring_post,
    .osc_wait = ompi_osc_monitoring_wait,
    .osc_test = ompi_osc_monitoring_test,

    .osc_lock = ompi_osc_monitoring_lock,
    .osc_unlock = ompi_osc_monitoring_unlock,
    .osc_lock_all = ompi_osc_monitoring_lock_all,
    .osc_unlock_all = ompi_osc_monitoring_unlock_all,

    .osc_sync = ompi_osc_monitoring_sync,
    .osc_flush = ompi_osc_monitoring_flush,
    .osc_flush_all = ompi_osc_monitoring_flush_all,
    .osc_flush_local = ompi_osc_monitoring_flush_local,
    .osc_flush_local_all = ompi_osc_monitoring_flush_local_all,

    .osc_set_info = ompi_osc_monitoring_set_info,
    .osc_get_info = ompi_osc_monitoring_get_info
};

static int mca_osc_monitoring_component_open(void)
{
    return OMPI_SUCCESS;
}

static int mca_osc_monitoring_component_close(void)
{
    if( !mca_osc_monitoring_enabled )
        goto release_and_return;

    /**
     * If this component is already active, then we are currently monitoring
     * the execution and this call to close if the one from MPI_Finalize.
     * Clean up and release the extra reference on ourselves.
     */
    if( mca_osc_monitoring_active ) {  /* Already active, turn off */
        mca_base_component_repository_release(&mca_osc_monitoring_component.super.osc_version);
        mca_osc_monitoring_active = 0;
        goto release_and_return;
    }

    /**
     * We are supposed to monitor the execution. Save the winner OSC component and
     * module, and swap it with ourselves. Increase our refcount so that we are
     * not dlclose.
     */
    /* if( OPAL_SUCCESS != mca_base_component_repository_retain_component(mca_osc_monitoring_component.super.osc_version.mca_type_name, */
    /*                                                                    mca_osc_monitoring_component.super.osc_version.mca_component_name) ) { */
    /*     return OMPI_ERROR; */
    /* } */

    /* Save a copy of the selected OSC */
    /* osc_selected_component = ompi_osc_base_selected_component; */
    /* osc_selected_module = mca_osc; */
    /* Install our interception layer */
    /* mca_osc_base_selected_component = mca_osc_monitoring_component; */
    /* mca_osc = mca_osc_monitoring; */
    /* Restore some of the original valued: progress, flags, tags and context id */
    /* mca_osc.osc_progress = osc_selected_module.osc_progress; */
    /* mca_osc.osc_max_contextid = osc_selected_module.osc_max_contextid; */
    /* mca_osc.osc_max_tag = osc_selected_module.osc_max_tag; */
    /* mca_osc.osc_flags = osc_selected_module.osc_flags; */

    mca_osc_monitoring_active = 1;

    return OMPI_SUCCESS;

 release_and_return:
    /* if( NULL != mca_osc_monitoring_current_filename ) { */
    /*     free(mca_osc_monitoring_current_filename); */
    /*     mca_osc_monitoring_current_filename = NULL; */
    /* } */
    return OMPI_SUCCESS;
}

static int
mca_osc_monitoring_component_init(bool enable_progress_threads,
                                  bool enable_mpi_threads)
{
    opal_output(0, "component_init");
    return OMPI_SUCCESS;
}

static int mca_osc_monitoring_component_finish(void)
{
    opal_output(0, "component_finish");
    return OMPI_SUCCESS;
}

static int mca_osc_monitoring_component_register(void)
{
    return OMPI_SUCCESS;
}

static int mca_osc_monitoring_component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                              struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                              int flavor)
{
    opal_output(0, "component_query");
    return mca_osc_monitoring_component.priority;
}

static int mca_osc_monitoring_component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                               struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                               int flavor, int *model)
{
    opal_output(0, "component_select");
    opal_list_item_t *item;
    ompi_osc_base_component_t *best_component = NULL;
    int best_priority = -1, priority, ret = OMPI_SUCCESS;

    /* Redo the select loop to add our layer in the middle */
    for (item = opal_list_get_first(&ompi_osc_base_framework.framework_components) ;
         item != opal_list_get_end(&ompi_osc_base_framework.framework_components) ;
         item = opal_list_get_next(item)) {
        ompi_osc_base_component_t *component = (ompi_osc_base_component_t*)
            ((mca_base_component_list_item_t*) item)->cli_component;

        if( component == (ompi_osc_base_component_t*)(&mca_osc_monitoring_component) )
            continue; /* skip self */
        
        priority = component->osc_query(win, base, size, disp_unit, comm, info, flavor);
        if (priority < 0) {
            if (MPI_WIN_FLAVOR_SHARED == flavor && OMPI_ERR_RMA_SHARED == priority) {
                /* NTH: quick fix to return OMPI_ERR_RMA_SHARED */
                return OMPI_ERR_RMA_SHARED;
            }
            continue;
        }

        if (priority > best_priority) {
            best_component = component;
            best_priority = priority;
        }
    }

    if (NULL == best_component) return OMPI_ERR_NOT_SUPPORTED;
    opal_output(0, "Chosen one: %s", best_component->osc_version.mca_component_name);
    ret = best_component->osc_select(win, base, size, disp_unit, comm, info, flavor, model);
    if( OMPI_SUCCESS == ret ) {
        /* Initialize the proper union field, based on selected component */
        size_t size_of_module;
        if( 0 == strcmp("rdma", best_component->osc_version.mca_component_name) )
            size_of_module = sizeof(ompi_osc_rdma_module_t);
        else if( 0 == strcmp("sm", best_component->osc_version.mca_component_name) )
            size_of_module = sizeof(ompi_osc_sm_module_t);
        /* else if( 0 == strcmp("portals4", best_component->osc_version.mca_component_name) ) */
        /*     size_of_module = sizeof(ompi_osc_portals4_module_t); */
        else if( 0 == strcmp("pt2pt", best_component->osc_version.mca_component_name) )
            size_of_module = sizeof(ompi_osc_pt2pt_module_t);
        else {
            OSC_MONITORING_VERBOSE(MCA_BASE_VERBOSE_INFO, "Monitoring disabled: no module for this component (%s)", best_component->osc_version.mca_component_name);
            return ret;
        }
        /* Intercept module to add our own between */
        ompi_osc_monitoring_module_t*module = (ompi_osc_monitoring_module_t*) calloc(1, sizeof(ompi_osc_monitoring_module_t));
        if( NULL == module ) {
            OSC_MONITORING_VERBOSE(MCA_BASE_VERBOSE_INFO, "Monitoring disabled: out of ressources.", best_component->osc_version.mca_component_name);
            return ret;
        }
        memcpy(&module->osc_selected_module_real, win->w_osc_module, size_of_module);
        /* Save selected module function pointers */
        memcpy(&module->osc_selected_module, &module->osc_selected_module_real, sizeof(ompi_osc_base_module_t));
        /* Replace base structure function pointers with ours */
        memcpy(&module->osc_selected_module_real, &mca_osc_monitoring_template, sizeof(ompi_osc_base_module_t));
        /* Replace module with ours, modify to correspond to the original one */
        win->w_osc_module = (ompi_osc_base_module_t*)module;
    }
    return ret;
}

ompi_osc_monitoring_component_t mca_osc_monitoring_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */
        .osc_version = {
            OMPI_OSC_BASE_VERSION_3_0_0,

            .mca_component_name = "monitoring", /* MCA component name */
            MCA_MONITORING_MAKE_VERSION,
            .mca_open_component = mca_osc_monitoring_component_open,  /* component open */
            .mca_close_component = mca_osc_monitoring_component_close, /* component close */
            .mca_register_component_params = mca_osc_monitoring_component_register
        },
        .osc_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .osc_init = mca_osc_monitoring_component_init,  /* component init */
        .osc_finalize = mca_osc_monitoring_component_finish,   /* component finalize */
        .osc_query = mca_osc_monitoring_component_query,
        .osc_select = mca_osc_monitoring_component_select
    },
    .priority = INT_MAX
};
