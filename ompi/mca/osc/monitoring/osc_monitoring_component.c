/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include "osc_monitoring.h"
#include <ompi/constants.h>
#include <ompi/communicator/communicator.h>
#include <ompi/info/info.h>
#include <ompi/win/win.h>
#include <ompi/info/info.h>
#include <ompi/mca/osc/osc.h>
#include <ompi/mca/osc/base/base.h>
#include <opal/mca/base/mca_base_component_repository.h>

/***************************************/
/* Include template generating macros */
#include "osc_monitoring_template.h"

#include <ompi/mca/osc/rdma/osc_rdma.h>
OSC_MONITORING_MODULE_TEMPLATE_GENERATE(rdma, ompi_osc_rdma_module_t, comm)
#undef GET_MODULE

#include <ompi/mca/osc/sm/osc_sm.h>
OSC_MONITORING_MODULE_TEMPLATE_GENERATE(sm, ompi_osc_sm_module_t, comm)
#undef GET_MODULE

#include <ompi/mca/osc/pt2pt/osc_pt2pt.h>
OSC_MONITORING_MODULE_TEMPLATE_GENERATE(pt2pt, ompi_osc_pt2pt_module_t, comm)
#undef GET_MODULE

#ifdef OMPI_WITH_OSC_PORTALS4
#include <ompi/mca/osc/portals4/osc_portals4.h>
OSC_MONITORING_MODULE_TEMPLATE_GENERATE(portals4, ompi_osc_portals4_module_t, comm)
#undef GET_MODULE
#endif /* OMPI_WITH_OSC_PORTALS4 */

/***************************************/

static int mca_osc_monitoring_component_init(bool enable_progress_threads,
                                             bool enable_mpi_threads)
{
    OPAL_MONITORING_PRINT_INFO("osc_component_init");
    mca_common_monitoring_init();
    return OMPI_SUCCESS;
}

static int mca_osc_monitoring_component_finish(void)
{
    OPAL_MONITORING_PRINT_INFO("osc_component_finish");
    mca_common_monitoring_finalize();
    return OMPI_SUCCESS;
}

static int mca_osc_monitoring_component_register(void)
{
    return OMPI_SUCCESS;
}

static int mca_osc_monitoring_component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                              struct ompi_communicator_t *comm, struct opal_info_t *info,
                                              int flavor)
{
    OPAL_MONITORING_PRINT_INFO("osc_component_query");
    return mca_osc_monitoring_component.priority;
}

static int mca_osc_monitoring_component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                               struct ompi_communicator_t *comm, struct opal_info_t *info,
                                               int flavor, int *model)
{
    OPAL_MONITORING_PRINT_INFO("osc_component_select");
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
    OPAL_MONITORING_PRINT_INFO("osc: chosen one: %s", best_component->osc_version.mca_component_name);
    ret = best_component->osc_select(win, base, size, disp_unit, comm, info, flavor, model);
    if( OMPI_SUCCESS == ret ) {
        /* Intercept module functions with ours, based on selected component */
        if( 0 == strcmp("rdma", best_component->osc_version.mca_component_name) ) {
            OSC_MONITORING_SET_TEMPLATE(rdma, win->w_osc_module);
        } else if( 0 == strcmp("sm", best_component->osc_version.mca_component_name) ) {
            OSC_MONITORING_SET_TEMPLATE(sm, win->w_osc_module);
        } else if( 0 == strcmp("pt2pt", best_component->osc_version.mca_component_name) ) {
            OSC_MONITORING_SET_TEMPLATE(pt2pt, win->w_osc_module);
#ifdef OMPI_WITH_OSC_PORTALS4
        } else if( 0 == strcmp("portals4", best_component->osc_version.mca_component_name) ) {
            OSC_MONITORING_SET_TEMPLATE(portals4, win->w_osc_module);
#endif /* OMPI_WITH_OSC_PORTALS4 */
        } else {
            OPAL_MONITORING_PRINT_WARN("osc: monitoring disabled: no module for this component "
                                       "(%s)", best_component->osc_version.mca_component_name);
            return ret;
        }
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

