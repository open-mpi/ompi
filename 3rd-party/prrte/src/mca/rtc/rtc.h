/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The PRTE Run-Time Control Framework (RTC)
 *
 */

#ifndef PRTE_MCA_RTC_H
#define PRTE_MCA_RTC_H

#include "prte_config.h"
#include "types.h"

#include "src/class/pmix_list.h"
#include "src/mca/mca.h"
#include "src/mca/odls/base/base.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

typedef struct {
    pmix_list_item_t super;
    char *component;
    char *category;
    prte_value_t control;
} prte_rtc_resource_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_rtc_resource_t);

/* Assign run-time controls for a given job. This provides each component with
 * an opportunity to insert attributes into the prte_job_t and/or its
 * associated proc structures that will be passed to backend daemons for
 * controlling the job. For example, if the user specified a frequency
 * setting for the job, then the freq component will have an opportunity
 * to add an attribute to the job so the freq component on the remote daemons
 * can "catch" it and perform the desired action
 */
typedef void (*prte_rtc_base_module_assign_fn_t)(prte_job_t *jdata);

/* Set run-time controls for a given job and/or process. This can include
 * controls for power, binding, memory, and any other resource on the node.
 * Each active plugin will be given a chance to operate on the request, setting
 * whatever controls that lie within its purview.
 *
 * Each module is responsible for reporting errors via the state machine. Thus,
 * no error code is returned. However, warnings and error messages for the user
 * can be output via the provided error_fd */
typedef void (*prte_rtc_base_module_set_fn_t)(prte_odls_spawn_caddy_t *cd,
                                              int error_fd);

/* Return a list of valid controls values for this component.
 * Each module is responsible for adding its control values
 * to a list of prte_value_t objects.
 */
typedef void (*prte_rtc_base_module_get_avail_vals_fn_t)(pmix_list_t *vals);

/* provide a way for the module to init during selection */
typedef int (*prte_rtc_base_module_init_fn_t)(void);

/* provide a chance for the module to finalize */
typedef void (*prte_rtc_base_module_fini_fn_t)(void);

/*
 * rtc module version 1.0.0
 */
typedef struct {
    prte_rtc_base_module_init_fn_t init;
    prte_rtc_base_module_fini_fn_t finalize;
    prte_rtc_base_module_assign_fn_t assign;
    prte_rtc_base_module_set_fn_t set;
    prte_rtc_base_module_get_avail_vals_fn_t get_available_values;
} prte_rtc_base_module_t;

/* provide a public API version */
typedef struct {
    prte_rtc_base_module_assign_fn_t assign;
    prte_rtc_base_module_set_fn_t set;
    prte_rtc_base_module_get_avail_vals_fn_t get_available_values;
} prte_rtc_API_module_t;

/**
 * rtc component version 1.0.0
 */
typedef pmix_mca_base_component_t prte_rtc_base_component_t;

/* declare the struct containing the public API */
PRTE_EXPORT extern prte_rtc_API_module_t prte_rtc;

/*
 * Macro for use in components that are of type rtc
 */
#define PRTE_RTC_BASE_VERSION_1_0_0 PRTE_MCA_BASE_VERSION_3_0_0("rtc", 1, 0, 0)

END_C_DECLS

#endif
