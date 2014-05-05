/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Open RTE Run-Time Control Framework (RTC)
 * 
 */

#ifndef ORTE_MCA_RTC_H
#define ORTE_MCA_RTC_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

/* Set run-time controls for a given job and/or process. This can include
 * controls for power, binding, memory, and any other resource on the node.
 * Each active plugin will be given a chance to operate on the request, setting
 * whatever controls that lie within its purview.
 *
 * Each module is responsible for reporting errors via the state machine. Thus,
 * no error code is returned. However, warnings and error messages for the user
 * can be output via the provided error_fd */
typedef void (*orte_rtc_base_module_set_fn_t)(orte_job_t *jdata,
                                              orte_proc_t *proc,
                                              char ***env,
                                              int error_fd);

/* provide a way for the module to init during selection */
typedef int (*orte_rtc_base_module_init_fn_t)(void);

/* provide a chance for the module to finalize */
typedef void (*orte_rtc_base_module_fini_fn_t)(void);

/*
 * rtc module version 1.0.0
 */
typedef struct {
    orte_rtc_base_module_init_fn_t  init;
    orte_rtc_base_module_fini_fn_t  finalize;
    orte_rtc_base_module_set_fn_t   set;
} orte_rtc_base_module_t;


/* provide a public API version */
typedef struct {
    orte_rtc_base_module_set_fn_t   set;
} orte_rtc_API_module_t;


/**
 * rtc component version 1.0.0
 */
typedef struct orte_rtc_base_component_1_0_0_t {
    /** Base MCA structure */
    mca_base_component_t base_version;
    /** Base MCA data */
    mca_base_component_data_t base_data;
} orte_rtc_base_component_t;

/* declare the struct containing the public API */
ORTE_DECLSPEC extern orte_rtc_API_module_t orte_rtc;

/*
 * Macro for use in components that are of type rtc
 */
#define ORTE_RTC_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "rtc", 1, 0, 0


END_C_DECLS

#endif
