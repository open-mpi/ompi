/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_NOTIFIER_BASE_H
#define MCA_NOTIFIER_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/notifier/notifier.h"

BEGIN_C_DECLS

/*
 * Type for holding selected module / component pairs
 */
typedef struct {
    opal_list_item_t super;
    /* Component */
    orte_notifier_base_component_t *onbsp_component;
    /* Module */
    orte_notifier_base_module_t *onbsp_module;
    /* Priority */
    int onbsp_priority;
} orte_notifier_base_selected_pair_t;

OBJ_CLASS_DECLARATION(orte_notifier_base_selected_pair_t);

#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * function definitions
 */
ORTE_DECLSPEC int orte_notifier_base_open(void);
ORTE_DECLSPEC int orte_notifier_base_select(void);
ORTE_DECLSPEC void orte_notifier_log(orte_notifier_base_severity_t severity, 
                                     int errcode, 
                                     const char *msg, ...);
ORTE_DECLSPEC void orte_notifier_show_help(orte_notifier_base_severity_t severity, 
                                           int errcode, 
                                           const char *file, 
                                           const char *topic, ...);
ORTE_DECLSPEC void orte_notifier_log_peer(orte_notifier_base_severity_t severity, 
                                          int errcode, 
                                          orte_process_name_t *peer_proc, 
                                          const char *msg, ...);
ORTE_DECLSPEC const char* orte_notifier_base_sev2str(orte_notifier_base_severity_t severity);
ORTE_DECLSPEC char *orte_notifier_base_peer_log(int errcode, 
                                                orte_process_name_t *peer_proc, 
                                                const char *msg, va_list ap);
ORTE_DECLSPEC int orte_notifier_base_close(void);

#if ORTE_WANT_NOTIFIER_LOG_EVENT

ORTE_DECLSPEC int orte_notifier_base_events_init(void);
ORTE_DECLSPEC void orte_notifier_base_events_finalize(void);

#else /* ORTE_WANT_NOTIFIER_LOG_EVENT */

#define orte_notifier_base_events_init()     do {} while (0)
#define orte_notifier_base_events_finalize() do {} while (0)

#endif /* ORTE_WANT_NOTIFIER_LOG_EVENT */

/*
 * global variables in the base
 * Needs to be declspec'ed for ompi_info and others 
 */
/*
 * Indication of whether a component was successfully selected or not
 * (1 component per interface)
 */
ORTE_DECLSPEC extern bool orte_notifier_base_log_selected;
ORTE_DECLSPEC extern bool orte_notifier_base_help_selected;
ORTE_DECLSPEC extern bool orte_notifier_base_log_peer_selected;
ORTE_DECLSPEC extern bool orte_notifier_base_log_event_selected;
/*
 * Lists of selected modules (1 per interface)
 */
ORTE_DECLSPEC extern opal_list_t orte_notifier_log_selected_modules;
ORTE_DECLSPEC extern opal_list_t orte_notifier_help_selected_modules;
ORTE_DECLSPEC extern opal_list_t orte_notifier_log_peer_selected_modules;
ORTE_DECLSPEC extern opal_list_t orte_notifier_log_event_selected_modules;
/*
 * That one is a merge of the per interface lists
 * It is used during finalize phase to finalize only once each selected module
 */
ORTE_DECLSPEC extern opal_list_t orte_notifier_base_selected_modules;
ORTE_DECLSPEC extern int orte_notifier_base_output;
ORTE_DECLSPEC extern orte_notifier_base_severity_t orte_notifier_threshold_severity;
ORTE_DECLSPEC extern opal_list_t orte_notifier_base_components_available;

#endif /* !ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif
