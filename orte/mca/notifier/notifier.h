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

#include "opal/mca/mca.h"
#include "opal/util/opal_sos.h"

#include "orte/constants.h"
#include "orte/types.h"

#include "notifier_event_types.h"

BEGIN_C_DECLS

/* The maximum size of any on-stack buffers used in the notifier
 * so we can try to avoid calling malloc in OUT_OF_RESOURCES conditions.
 * The code has NOT been auditied for use of malloc, so this still
 * may fail to get the "OUT_OF_RESOURCE" message out.  Oh Well.
 */
#define ORTE_NOTIFIER_MAX_BUF	512

/* Severities, based on OPAL SOS */
typedef enum {
    ORTE_NOTIFIER_EMERG = OPAL_SOS_SEVERITY_EMERG,
    ORTE_NOTIFIER_ALERT = OPAL_SOS_SEVERITY_ALERT,
    ORTE_NOTIFIER_CRIT = OPAL_SOS_SEVERITY_CRIT,
    ORTE_NOTIFIER_ERROR = OPAL_SOS_SEVERITY_ERROR,
    ORTE_NOTIFIER_WARN = OPAL_SOS_SEVERITY_WARN,
    ORTE_NOTIFIER_NOTICE = OPAL_SOS_SEVERITY_NOTICE,
    ORTE_NOTIFIER_INFO = OPAL_SOS_SEVERITY_INFO,
    ORTE_NOTIFIER_DEBUG = OPAL_SOS_SEVERITY_DEBUG
} orte_notifier_base_severity_t;

/*
 * Component functions - all MUST be provided!
 */

/* initialize the selected module */
typedef int (*orte_notifier_base_module_init_fn_t)(void);
    
/* finalize the selected module */
typedef void (*orte_notifier_base_module_finalize_fn_t)(void);

/* Log a failure message */
typedef void (*orte_notifier_base_module_log_fn_t)(orte_notifier_base_severity_t severity, int errcode, const char *msg, va_list ap)
    __opal_attribute_format_funcptr__(__printf__, 3, 0);

/* Log a failure that is based upon a show_help message */
typedef void (*orte_notifier_base_module_log_show_help_fn_t)(orte_notifier_base_severity_t severity, int errcode, const char *file, const char *topic, va_list ap);

/* Log a failure related to a peer */
typedef void (*orte_notifier_base_module_log_peer_fn_t)(orte_notifier_base_severity_t severity, int errcode, orte_process_name_t *peer_proc, const char *msg, va_list ap)
    __opal_attribute_format_funcptr__(__printf__, 4, 0);

/* Log an unusual event message */
typedef void (*orte_notifier_base_module_log_event_fn_t)(const char *msg);

/*
 * Ver 1.0
 */
struct orte_notifier_base_module_1_0_0_t {
    orte_notifier_base_module_init_fn_t             init;
    orte_notifier_base_module_finalize_fn_t         finalize;
    orte_notifier_base_module_log_fn_t              log;
    orte_notifier_base_module_log_show_help_fn_t    help;
    orte_notifier_base_module_log_peer_fn_t         peer;
    orte_notifier_base_module_log_event_fn_t        log_event;
};

typedef struct orte_notifier_base_module_1_0_0_t orte_notifier_base_module_1_0_0_t;
typedef orte_notifier_base_module_1_0_0_t orte_notifier_base_module_t;

/*
 * API functions
 */
/* Log a failure message */
typedef void (*orte_notifier_base_API_log_fn_t)(orte_notifier_base_severity_t severity, int errcode, const char *msg, ...);

/* Log a failure that is based upon a show_help message */
typedef void (*orte_notifier_base_API_log_show_help_fn_t)(orte_notifier_base_severity_t severity, int errcode, const char *file, const char *topic, ...);

/* Log a failure related to a peer */
typedef void (*orte_notifier_base_API_log_peer_fn_t)(orte_notifier_base_severity_t severity, int errcode, orte_process_name_t *peer_proc, const char *msg, ...);
    
/*
 * Define a struct to hold the API functions that users will call
 */
struct orte_notifier_API_module_1_0_0_t {
    orte_notifier_base_API_log_fn_t              log;
    orte_notifier_base_API_log_show_help_fn_t    show_help;
    orte_notifier_base_API_log_peer_fn_t         log_peer;
};
typedef struct orte_notifier_API_module_1_0_0_t orte_notifier_API_module_1_0_0_t;
typedef orte_notifier_API_module_1_0_0_t orte_notifier_API_module_t;

ORTE_DECLSPEC extern orte_notifier_API_module_t orte_notifier;

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

/*
 * To manage unusual events notifications
 * Set to noop if not wanted
 */

#if ORTE_WANT_NOTIFIER_LOG_EVENT

#include "notifier_event_calls.h"

#else /* ORTE_WANT_NOTIFIER_LOG_EVENT */

#define ORTE_NOTIFIER_DEFINE_EVENT(i, m)
#define ORTE_NOTIFIER_LOG_EVENT(i, c, t) do {} while (0)

#endif /* ORTE_WANT_NOTIFIER_LOG_EVENT */

END_C_DECLS

#endif /* MCA_NOTIFIER_H */
