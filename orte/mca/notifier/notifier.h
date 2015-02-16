/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All Rights Reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The OpenRTE Notifier Framework
 *
 * The OpenRTE Notifier framework provides a mechanism for notifying
 * system administrators or other fault monitoring systems that a
 * problem with the underlying cluster has been detected - e.g., a
 * failed connection in a network fabric
 */

#ifndef MCA_NOTIFIER_H
#define MCA_NOTIFIER_H

/*
 * includes
 */

#include "orte_config.h"

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif

#include "opal/mca/mca.h"

#include "orte/constants.h"
#include "orte/types.h"


BEGIN_C_DECLS

/* The maximum size of any on-stack buffers used in the notifier
 * so we can try to avoid calling malloc in OUT_OF_RESOURCES conditions.
 * The code has NOT been auditied for use of malloc, so this still
 * may fail to get the "OUT_OF_RESOURCE" message out.  Oh Well.
 */
#define ORTE_NOTIFIER_MAX_BUF	512

/* Severities */
typedef enum {
    ORTE_NOTIFIER_EMERG = LOG_EMERG,
    ORTE_NOTIFIER_ALERT = LOG_ALERT,
    ORTE_NOTIFIER_CRIT = LOG_CRIT,
    ORTE_NOTIFIER_ERROR = LOG_ERR,
    ORTE_NOTIFIER_WARN = LOG_WARNING,
    ORTE_NOTIFIER_NOTICE = LOG_NOTICE,
    ORTE_NOTIFIER_INFO = LOG_INFO,
    ORTE_NOTIFIER_DEBUG = LOG_DEBUG
} orte_notifier_severity_t;

/*
 * Component functions - all MUST be provided!
 */

/* initialize the selected module */
typedef int (*orte_notifier_base_module_init_fn_t)(void);
    
/* finalize the selected module */
typedef void (*orte_notifier_base_module_finalize_fn_t)(void);

/* Log a failure message */
typedef void (*orte_notifier_base_module_log_fn_t)(orte_notifier_severity_t severity, int errcode, const char *msg, va_list *ap)
    __opal_attribute_format_funcptr__(__printf__, 3, 0);


/*
 * Ver 1.0
 */
struct orte_notifier_base_module_1_0_0_t {
    orte_notifier_base_module_init_fn_t             init;
    orte_notifier_base_module_finalize_fn_t         finalize;
    orte_notifier_base_module_log_fn_t              log;
};

typedef struct orte_notifier_base_module_1_0_0_t orte_notifier_base_module_1_0_0_t;
typedef orte_notifier_base_module_1_0_0_t orte_notifier_base_module_t;

/*
 * API functions
 */
/* Log a failure message */
typedef void (*orte_notifier_base_API_log_fn_t)(orte_notifier_severity_t severity, int errcode, const char *msg, ...);

/*
 * Define a struct to hold the API functions that users will call
 */
struct orte_notifier_base_API_module_1_0_0_t {
    orte_notifier_base_API_log_fn_t              log;
};
typedef struct orte_notifier_base_API_module_1_0_0_t orte_notifier_base_API_module_1_0_0_t;
typedef orte_notifier_base_API_module_1_0_0_t orte_notifier_base_API_module_t;

ORTE_DECLSPEC extern orte_notifier_base_API_module_t orte_notifier;

/*
 * the standard component data structure
 */
struct orte_notifier_base_component_1_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct orte_notifier_base_component_1_0_0_t orte_notifier_base_component_1_0_0_t;
typedef orte_notifier_base_component_1_0_0_t orte_notifier_base_component_t;


/*
 * Macro for use in components that are of type notifier v1.0.0
 */
#define ORTE_NOTIFIER_BASE_VERSION_1_0_0 \
  /* notifier v1.0 is chained to MCA v2.0 */ \
  MCA_BASE_VERSION_2_0_0, \
  /* notifier v1.0 */ \
  "notifier", 1, 0, 0

END_C_DECLS

#endif /* MCA_NOTIFIER_H */
