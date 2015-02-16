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
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"

#include "opal/mca/event/event.h"
#include "opal/runtime/opal_progress_threads.h"

#include "orte/constants.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/notifier/base/base.h"


static void process_log(int fd, short args, void *cbdata)
{
    int i = 0, j = 0;
    orte_notifier_active_module_t *i_module;
    orte_notifier_request_t *req = (orte_notifier_request_t *)cbdata;
    char **modules = req->modules;

    /* call the log function of all modules in priority order */
    for (i = 0; i < orte_notifier_base.modules.size; i++) {
        if (NULL == (i_module = (orte_notifier_active_module_t*)opal_pointer_array_get_item (&orte_notifier_base.modules, i))) {
            continue;
        }
        for (j = 0; NULL != modules[j]; j++) {
            if (0 == strncasecmp(i_module->component->base_version.mca_component_name, modules[j], strlen(modules[j]))) {
                if (NULL != i_module->module->log) {
                    i_module->module->log(req->severity, req->errcode, req->msg, req->ap);
                    va_end(*req->ap);
                }
            }
        }
    }
}

void orte_notifier_base_log(orte_notifier_severity_t severity, 
                            int errcode, const char *msg, ...)
{
    va_list ap;
    char **modules;
    orte_notifier_request_t *req;

    /* if no modules are active, then there is nothing to do */
    if (0 == orte_notifier_base.modules.size) {
        return;
    }

    /* check if the severity is >= severity level set for reporting */
    if (orte_notifier_base.severity_level < severity ) {
        return;
    }

    if (ORTE_NOTIFIER_EMERG == severity &&  
        (NULL != orte_notifier_base.emerg_actions)) {
        modules = opal_argv_split(orte_notifier_base.emerg_actions, ',');    
    } else if (ORTE_NOTIFIER_ALERT == severity &&  
               (NULL != orte_notifier_base.alert_actions)) {
        modules = opal_argv_split(orte_notifier_base.alert_actions, ',');    
    } else if (ORTE_NOTIFIER_CRIT == severity &&  
               (NULL != orte_notifier_base.crit_actions)) {
        modules = opal_argv_split(orte_notifier_base.crit_actions, ',');    
    } else if (ORTE_NOTIFIER_WARN == severity &&  
               (NULL != orte_notifier_base.warn_actions)) {
        modules = opal_argv_split(orte_notifier_base.warn_actions, ',');    
    } else if (ORTE_NOTIFIER_NOTICE == severity &&  
               (NULL != orte_notifier_base.notice_actions)) {
        modules = opal_argv_split(orte_notifier_base.notice_actions, ',');    
    } else if (ORTE_NOTIFIER_INFO == severity &&  
               (NULL != orte_notifier_base.info_actions)) {
        modules = opal_argv_split(orte_notifier_base.info_actions, ',');    
    } else if (ORTE_NOTIFIER_DEBUG == severity &&  
               (NULL != orte_notifier_base.debug_actions)) {
        modules = opal_argv_split(orte_notifier_base.debug_actions, ',');    
    } else if (ORTE_NOTIFIER_ERROR == severity &&  
               (NULL != orte_notifier_base.error_actions)) {
        modules = opal_argv_split(orte_notifier_base.error_actions, ',');    
    } else if (NULL != orte_notifier_base.default_actions) {
        modules = opal_argv_split(orte_notifier_base.default_actions, ',');    
    } else {
        /* no modules selected */
        return;
    }

    /* set the event base and push this request into event base
     */
    req = OBJ_NEW(orte_notifier_request_t);

    req->modules = modules;
    req->severity = severity;
    req->errcode = errcode;
    req->msg = msg;
    va_start(ap, msg);
    req->ap = &ap;

    /*
     * set the event and activate
     */
    opal_event_set(orte_notifier_base.ev_base, &req->ev, -1, 
                    OPAL_EV_WRITE,
                    process_log, req);
    opal_event_set_priority(&req->ev, OPAL_EV_SYS_HI_PRI);
    opal_event_active (&req->ev, OPAL_EV_WRITE, 1);
}


const char* orte_notifier_base_sev2str(orte_notifier_severity_t severity)
{
    switch (severity) {
    case ORTE_NOTIFIER_EMERG:  return "EMERG";  break;
    case ORTE_NOTIFIER_ALERT:  return "ALERT";  break;
    case ORTE_NOTIFIER_CRIT:   return "CRIT";   break;
    case ORTE_NOTIFIER_ERROR:  return "ERROR";  break;
    case ORTE_NOTIFIER_WARN:   return "WARN";   break;
    case ORTE_NOTIFIER_NOTICE: return "NOTICE"; break;
    case ORTE_NOTIFIER_INFO:   return "INFO";   break;
    case ORTE_NOTIFIER_DEBUG:  return "DEBUG";  break;
    default: return "UNKNOWN"; break;
    }
}

