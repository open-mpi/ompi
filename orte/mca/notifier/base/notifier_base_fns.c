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
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/util/argv.h"

#include "orte/mca/notifier/base/base.h"


void orte_notifier_base_log(int sd, short args, void *cbdata)
{
    orte_notifier_request_t *req = (orte_notifier_request_t*)cbdata;
    char **modules = NULL;
    orte_notifier_active_module_t *imod;
    int i;
    
    /* if no modules are active, then there is nothing to do */
    if (0 == opal_list_get_size(&orte_notifier_base.modules)) {
        return;
    }

    /* check if the severity is >= severity level set for
     * reporting - note that the severity enum value goes up
     * as severity goes down */
    if (orte_notifier_base.severity_level < req->severity ) {
        return;
    }

    if (ORTE_NOTIFIER_EMERG == req->severity &&  
        (NULL != orte_notifier_base.emerg_actions)) {
        modules = opal_argv_split(orte_notifier_base.emerg_actions, ',');    
    } else if (ORTE_NOTIFIER_ALERT == req->severity &&  
               (NULL != orte_notifier_base.alert_actions)) {
        modules = opal_argv_split(orte_notifier_base.alert_actions, ',');    
    } else if (ORTE_NOTIFIER_CRIT == req->severity &&  
               (NULL != orte_notifier_base.crit_actions)) {
        modules = opal_argv_split(orte_notifier_base.crit_actions, ',');    
    } else if (ORTE_NOTIFIER_WARN == req->severity &&  
               (NULL != orte_notifier_base.warn_actions)) {
        modules = opal_argv_split(orte_notifier_base.warn_actions, ',');    
    } else if (ORTE_NOTIFIER_NOTICE == req->severity &&  
               (NULL != orte_notifier_base.notice_actions)) {
        modules = opal_argv_split(orte_notifier_base.notice_actions, ',');    
    } else if (ORTE_NOTIFIER_INFO == req->severity &&  
               (NULL != orte_notifier_base.info_actions)) {
        modules = opal_argv_split(orte_notifier_base.info_actions, ',');    
    } else if (ORTE_NOTIFIER_DEBUG == req->severity &&  
               (NULL != orte_notifier_base.debug_actions)) {
        modules = opal_argv_split(orte_notifier_base.debug_actions, ',');    
    } else if (ORTE_NOTIFIER_ERROR == req->severity &&  
               (NULL != orte_notifier_base.error_actions)) {
        modules = opal_argv_split(orte_notifier_base.error_actions, ',');    
    } else if (NULL != orte_notifier_base.default_actions) {
        modules = opal_argv_split(orte_notifier_base.default_actions, ',');    
    } else {
        /* no modules selected */
        return;
    }

    for (i=0; NULL != modules[i]; i++) {
        OPAL_LIST_FOREACH(imod, &orte_notifier_base.modules, orte_notifier_active_module_t) {
            if (NULL != imod->module->log &&
                0 == strcmp(imod->component->base_version.mca_component_name, modules[i]))
                imod->module->log(req);
        }
    }
    opal_argv_free(modules);
}

void orte_notifier_base_report(int sd, short args, void *cbdata)
{
    orte_notifier_request_t *req = (orte_notifier_request_t*)cbdata;
    char *notifies = NULL;
    
    /* if no modules are active, then there is nothing to do */
    if (0 == opal_list_get_size(&orte_notifier_base.modules)) {
        return;
    }

    /* see if the job requested any notifications */
    if (!orte_get_attribute(&req->jdata->attributes, ORTE_JOB_NOTIFICATIONS, (void**)notifies, OPAL_STRING)) {
        return;
    }

    /* need to process the notification string to get the names of the modules */
    return;
#if 0
    OPAL_LIST_FOREACH(imod, &orte_notifier_base.modules, orte_notifier_active_module_t) {
        if (NULL != imod->module->report &&
            0 == strcmp(imod->component->base_version.mca_component_name, modules[i]))
            imod->module->report(req);
    }
#endif
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

