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
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif /* HAVE_SYSLOG_H */

#include "opal/mca/mca.h"

BEGIN_C_DECLS

/* define priorities - this will eventually be replaced by OPAL_SOS priorities */
#define ORTE_NOTIFIER_INFRA     LOG_CRIT
#define ORTE_NOTIFIER_WARNING   LOG_WARNING

/*
 * Component functions - all MUST be provided!
 */

/* initialize the selected module */
typedef int (*orte_notifier_base_module_init_fn_t)(void);
    
/* finalize the selected module */
typedef void (*orte_notifier_base_module_finalize_fn_t)(void);

/* Log a failure message */
typedef void (*orte_notifier_base_module_log_fn_t)(int priority, const char *msg, ...);

/* Log a failure that is based upon a show_help message */
typedef void (*orte_notifier_base_module_log_show_help_fn_t)(int priority, const char *file, const char *topic, ...);

/*
 * Ver 1.0
 */
struct orte_notifier_base_module_1_0_0_t {
    orte_notifier_base_module_init_fn_t             init;
    orte_notifier_base_module_finalize_fn_t         finalize;
    orte_notifier_base_module_log_fn_t              log;
    orte_notifier_base_module_log_show_help_fn_t    log_help;
};

typedef struct orte_notifier_base_module_1_0_0_t orte_notifier_base_module_1_0_0_t;
typedef orte_notifier_base_module_1_0_0_t orte_notifier_base_module_t;

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

/* Global structure for accessing notifier functions
 */
ORTE_DECLSPEC extern orte_notifier_base_module_t orte_notifier;  /* holds selected module's function pointers */

END_C_DECLS

#endif /* MCA_NOTIFIER_H */
