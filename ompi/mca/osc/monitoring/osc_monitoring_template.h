/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_TEMPLATE_H
#define MCA_OSC_MONITORING_TEMPLATE_H

#include <ompi/communicator/communicator.h>
#include <ompi/win/win.h>

#include <osc_monitoring_accumulate.h>
#include <osc_monitoring_active_target.h>
#include <osc_monitoring_comm.h>
#include <osc_monitoring_dynamic.h>
#include <osc_monitoring_module.h>
#include <osc_monitoring_passive_target.h>

/* module_type correspond to the ompi_osc_## template ##_module_t type 
 * comm correspond to the field name in ompi_osc_## template ##_module_t
 */
#define OSC_MONITORING_MODULE_TEMPLATE_GENERATE(template, module_type, comm) \
    /* Generate the proper symbol for ompi_osc_monitoring_module_## template ##_template */ \
    OMPI_OSC_MONITORING_MODULE_GENERATE(template);                      \
    /* Generate module specific module->comm accessor */                \
    static inline struct ompi_communicator_t*                           \
    ompi_osc_monitoring_## template ##_get_comm(ompi_win_t*win)         \
    {                                                                   \
        return ((module_type*)win->w_osc_module)->comm;                 \
    }                                                                   \
    /* Generate each module specific functions */                       \
    OSC_MONITORING_GENERATE_TEMPLATE_ACCUMULATE(template)               \
    OSC_MONITORING_GENERATE_TEMPLATE_ACTIVE_TARGET(template)            \
    OSC_MONITORING_GENERATE_TEMPLATE_COMM(template)                     \
    OSC_MONITORING_GENERATE_TEMPLATE_DYNAMIC(template)                  \
    OSC_MONITORING_GENERATE_TEMPLATE_MODULE(template)                   \
    OSC_MONITORING_GENERATE_TEMPLATE_PASSIVE_TARGET(template)           \
    /* Generates the mca_osc_monitoring_## template ##_template variable */ \
    MCA_OSC_MONITORING_MODULE_TEMPLATE_GENERATE(template)

#endif /* MCA_OSC_MONITORING_TEMPLATE_H */

