/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 * rtc framework base functionality.
 */

#ifndef PRTE_MCA_RTC_BASE_H
#define PRTE_MCA_RTC_BASE_H

/*
 * includes
 */
#include "prte_config.h"
#include "types.h"

#include "src/class/pmix_list.h"
#include "src/mca/mca.h"
#include "src/mca/odls/base/base.h"
#include "src/util/pmix_printf.h"

#include "src/mca/rtc/rtc.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_rtc_base_framework;
/* select a component */
PRTE_EXPORT int prte_rtc_base_select(void);

/*
 * Global functions for MCA overall collective open and close
 */

/**
 * Struct to hold data global to the rtc framework
 */
typedef struct {
    /* list of selected modules */
    pmix_list_t actives;
} prte_rtc_base_t;

/**
 * Global instance of rtc-wide framework data
 */
PRTE_EXPORT extern prte_rtc_base_t prte_rtc_base;

/**
 * Select an rtc component / module
 */
typedef struct {
    pmix_list_item_t super;
    int pri;
    prte_rtc_base_module_t *module;
    pmix_mca_base_component_t *component;
} prte_rtc_base_selected_module_t;
PMIX_CLASS_DECLARATION(prte_rtc_base_selected_module_t);

PRTE_EXPORT void prte_rtc_base_assign(prte_job_t *jdata);
PRTE_EXPORT void prte_rtc_base_set(prte_odls_spawn_caddy_t *cd, int error_fd);
PRTE_EXPORT void prte_rtc_base_get_avail_vals(pmix_list_t *vals);

/* Called from the child to send a warning show_help message up the
   pipe to the waiting parent. */
PRTE_EXPORT int prte_rtc_base_send_warn_show_help(int fd, const char *file, const char *topic, ...);

/* Called from the child to send an error message up the pipe to the
   waiting parent. */
PRTE_EXPORT void prte_rtc_base_send_error_show_help(int fd, int exit_status, const char *file,
                                                    const char *topic, ...);

END_C_DECLS

#endif
