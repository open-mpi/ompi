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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"

#include "orte/constants.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/base/base.h"

#if !ORTE_DISABLE_FULL_SUPPORT

void orte_notifier_log(orte_notifier_base_severity_t severity, 
                       int errcode, const char *msg, ...)
{
    va_list ap;
    opal_list_item_t *item;
    orte_notifier_base_selected_pair_t *pair;

    if (!orte_notifier_base_log_selected) {
        return;
    }

    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }

    for (item = opal_list_get_first(&orte_notifier_log_selected_modules);
         opal_list_get_end(&orte_notifier_log_selected_modules) != item;
         item = opal_list_get_next(item)) {
        pair = (orte_notifier_base_selected_pair_t*) item;
        if (NULL != pair->onbsp_module->log) {
            va_start(ap, msg);
            pair->onbsp_module->log(severity, errcode, msg, ap);
            va_end(ap);
        }
    }
}

void orte_notifier_show_help(orte_notifier_base_severity_t severity, 
                             int errcode, const char *file, 
                             const char *topic, ...)
{
    va_list ap;
    opal_list_item_t *item;
    orte_notifier_base_selected_pair_t *pair;

    if (!orte_notifier_base_help_selected) {
        return;
    }

    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }

    for (item = opal_list_get_first(&orte_notifier_help_selected_modules);
         opal_list_get_end(&orte_notifier_help_selected_modules) != item;
         item = opal_list_get_next(item)) {
        pair = (orte_notifier_base_selected_pair_t*) item;
        if (NULL != pair->onbsp_module->help) {
            va_start(ap, topic);
            pair->onbsp_module->help(severity, errcode, file, topic, ap);
            va_end(ap);
        }
    }
}

void orte_notifier_log_peer(orte_notifier_base_severity_t severity, 
                            int errcode, 
                            orte_process_name_t *peer_proc, 
                            const char *msg, ...)
{
    va_list ap;
    opal_list_item_t *item;
    orte_notifier_base_selected_pair_t *pair;

    if (!orte_notifier_base_log_peer_selected) {
        return;
    }

    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }

    for (item = opal_list_get_first(&orte_notifier_log_peer_selected_modules);
         opal_list_get_end(&orte_notifier_log_peer_selected_modules) != item;
         item = opal_list_get_next(item)) {
        pair = (orte_notifier_base_selected_pair_t*) item;
        if (NULL != pair->onbsp_module->peer) {
            va_start(ap, msg);
            pair->onbsp_module->peer(severity, errcode, peer_proc, msg, ap);
            va_end(ap);
        }
    }
}


const char* orte_notifier_base_sev2str(orte_notifier_base_severity_t severity)
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


char *orte_notifier_base_peer_log(int errcode, orte_process_name_t *peer_proc, 
                                  const char *msg, va_list ap)
{
    char *buf = (char *) malloc(ORTE_NOTIFIER_MAX_BUF + 1);
    char *peer_host = NULL, *peer_name = NULL;
    char *pos = buf;
    char *errstr;
    int ret, len, space = ORTE_NOTIFIER_MAX_BUF;

    if (NULL == buf) {
        return NULL;
    }

    if (peer_proc) {
        peer_host = orte_ess.proc_get_hostname(peer_proc);
        peer_name = ORTE_NAME_PRINT(peer_proc);
    }

    len = snprintf(pos, space,
                   "While communicating to proc %s on node %s,"
                   " proc %s on node %s encountered an error ",
                   peer_name ? peer_name : "UNKNOWN",
                   peer_host ? peer_host : "UNKNOWN",
                   ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                   orte_process_info.nodename);
    space -= len;
    pos += len;
    
    if (0 < space) {
        ret = orte_err2str(errcode, (const char **)&errstr);
        if (ORTE_SUCCESS == ret) {
            len = snprintf(pos, space, "'%s':", errstr);
            free(errstr);
        } else {
            len = snprintf(pos, space, "(%d):", errcode);
        }
        space -= len;
        pos += len;
    }

    if (0 < space) {
        vsnprintf(pos, space, msg, ap);
    }

    buf[ORTE_NOTIFIER_MAX_BUF] = '\0';
    return buf;
}

#endif
