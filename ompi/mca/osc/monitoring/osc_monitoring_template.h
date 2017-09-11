/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_TEMPLATE_H
#define MCA_OSC_MONITORING_TEMPLATE_H

#include <ompi_config.h>
#include <ompi/communicator/communicator.h>
#include <ompi/win/win.h>
#include <opal/sys/atomic.h>
#include "osc_monitoring_accumulate.h"
#include "osc_monitoring_active_target.h"
#include "osc_monitoring_comm.h"
#include "osc_monitoring_dynamic.h"
#include "osc_monitoring_module.h"
#include "osc_monitoring_passive_target.h"

/* module_type correspond to the ompi_osc_## template ##_module_t type 
 * comm correspond to the comm field name in ompi_osc_## template ##_module_t
 * 
 * The magic used here is that for a given module type (given with the
 * {template} parameter), we generate a set of every functions defined
 * in ompi_osc_base_module_t, the ompi_osc_monitoring_module_##
 * template ##_template variable recording the original set of
 * functions, and the ompi_osc_monitoring_## template ##_template
 * variable that record the generated set of functions. When a
 * function is called from the original module, we route the call to
 * our generated function that does the monitoring, and then we call
 * the original function that had been saved in the
 * ompi_osc_monitoring_module_## template ##_template variable.
 */
#define OSC_MONITORING_MODULE_TEMPLATE_GENERATE(template, module_type, comm) \
    /* Generate the proper symbol for the                               \
       ompi_osc_monitoring_module_## template ##_template variable */   \
    OMPI_OSC_MONITORING_MODULE_GENERATE(template);                      \
    OMPI_OSC_MONITORING_MODULE_INIT_GENERATE(template);			\
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
    /* Set the mca_osc_monitoring_## template ##_template variable */   \
    MCA_OSC_MONITORING_MODULE_TEMPLATE_GENERATE(template);              \
    /* Generate template specific module initialization function */     \
    static inline void*                                                 \
    ompi_osc_monitoring_## template ##_set_template (ompi_osc_base_module_t*module) \
    {                                                                   \
        if( 1 == opal_atomic_add_32(&(OMPI_OSC_MONITORING_MODULE_INIT(template)), 1) ) { \
	    /* Saves the original module functions in			\
	     * ompi_osc_monitoring_module_## template ##_template	\
	     */								\
	    memcpy(&OMPI_OSC_MONITORING_MODULE_VARIABLE(template),	\
		   module, sizeof(ompi_osc_base_module_t));		\
	}								\
	/* Replace the original functions with our generated ones */	\
	memcpy(module, &OMPI_OSC_MONITORING_TEMPLATE_VARIABLE(template), \
	       sizeof(ompi_osc_base_module_t));				\
	return module;							\
    }

#define OSC_MONITORING_SET_TEMPLATE(template, module)           \
    ompi_osc_monitoring_## template ##_set_template(module)

#endif /* MCA_OSC_MONITORING_TEMPLATE_H */

