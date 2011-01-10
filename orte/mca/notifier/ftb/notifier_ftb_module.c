/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/show_help.h"
#include "opal/util/os_path.h"

#include "orte/mca/ess/ess.h"
#include "orte/util/show_help.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "orte/mca/notifier/base/base.h"
#include "notifier_ftb.h"

/* Static API's */
static int init(void);
static void finalize(void);
static void ftb_log(orte_notifier_base_severity_t severity, int errcode, 
                  const char *msg, va_list ap);
static void ftb_help(orte_notifier_base_severity_t severity, int errcode, 
                      const char *filename, const char *topic, va_list ap);
static void ftb_peer(orte_notifier_base_severity_t severity, int errcode, 
                      orte_process_name_t *peer_proc, const char *msg, 
                      va_list ap);

/* Module def */
orte_notifier_base_module_t orte_notifier_ftb_module = {
    init,
    finalize,
    ftb_log,
    ftb_help,
    ftb_peer,
    NULL
};

/* FTB client information */
FTB_client_t ftb_client_info;

/* FTB client handle */
FTB_client_handle_t ftb_client_handle;

static int init(void) {
    int ret;
    char *schema_file;

    /* Locate the FTB events schema file */
    if (NULL == (schema_file = opal_os_path(false, opal_install_dirs.pkgdatadir,
                                            "help-ftb-event-schema.txt", NULL))) {
        schema_file = strdup("help-ftb-event-schema.txt");
    }

    /* Declare the Open MPI publishable events to the FTB */
    ret = FTB_Declare_publishable_events(ftb_client_handle, schema_file, NULL, 0);
    free(schema_file);

    if (FTB_SUCCESS != ret) {
        orte_show_help("help-orte-notifier-ftb.txt", "declare events failed", true, 
                       "FTB_Declare_publishable_events() failed", ret);

        FTB_Disconnect(ftb_client_handle);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static void finalize(void) {
    FTB_Disconnect(ftb_client_handle);
}

static const char* get_ftb_event_severity(orte_notifier_base_severity_t severity)
{
    switch (severity) {
    case ORTE_NOTIFIER_EMERG:
    case ORTE_NOTIFIER_ALERT:
        return "ALL";
    case ORTE_NOTIFIER_CRIT:
        return "FATAL";
    case ORTE_NOTIFIER_ERROR:
        return "ERROR";
    case ORTE_NOTIFIER_WARN:
    case ORTE_NOTIFIER_NOTICE:
        return "WARNING";
    case ORTE_NOTIFIER_INFO:
    case ORTE_NOTIFIER_DEBUG:
        return "INFO";
    default:
        return "UNKNOWN";
    }
}

static const char* get_ftb_event_name(int errnum)
{
    switch (errnum) {

    case ORTE_SNAPC_CKPT_STATE_ESTABLISHED:
        return FTB_EVENT(FTB_MPI_PROCS_CKPTED);

    case ORTE_SNAPC_CKPT_STATE_NO_CKPT:
    case ORTE_SNAPC_CKPT_STATE_ERROR:
        return FTB_EVENT(FTB_MPI_PROCS_CKPT_FAIL);

    case ORTE_ERR_CONNECTION_REFUSED:
    case ORTE_ERR_CONNECTION_FAILED:
    case ORTE_ERR_UNREACH:
        return FTB_EVENT(FTB_MPI_PROCS_UNREACHABLE);

    case ORTE_ERR_COMM_FAILURE:
        return FTB_EVENT(FTB_MPI_PROCS_COMM_ERROR);

    default:
        return NULL;
    }

    return NULL;
}

static void publish_ftb_event(orte_notifier_base_severity_t severity, int errcode, char *payload)
{
    int ret;
    const char *event_name;
    FTB_event_handle_t ehandle;
    FTB_event_properties_t eprop;

    /* Only normal FTB events are supported currently. */
    eprop.event_type = (int) FTB_EVENT_NORMAL;

    /* Copy the event payload, if we have one */ 
    if (NULL != payload) {
        strncpy(eprop.event_payload, payload, FTB_MAX_PAYLOAD_DATA);
    }

    /* Publish the event to the Fault Tolerant Backplane */
    event_name = get_ftb_event_name(errcode);
    if (NULL != event_name) {
        ret = FTB_Publish(ftb_client_handle, event_name, &eprop, &ehandle);
        if (FTB_SUCCESS != ret) {
            orte_show_help("help-orte-notifier-ftb.txt", "publish failed", true,
                           "FTB_Publish() failed", ret, get_ftb_event_severity(severity),
                           event_name, payload, errcode);
        }
    }
}

static void ftb_log(orte_notifier_base_severity_t severity, int errcode, const char *msg,
                    va_list ap)
{
    char *payload;

    vasprintf(&payload, msg, ap);
    if (NULL != payload) {
        publish_ftb_event(severity, errcode, payload);
        free(payload);
    }
}

static void ftb_help(orte_notifier_base_severity_t severity, int errcode,
                     const char *filename, const char *topic, va_list ap)
{
    char *payload;

    payload = opal_show_help_vstring(filename, topic, false, ap);
    if (NULL != payload) {
        publish_ftb_event(severity, errcode, payload);
        free(payload);
    }
}

static void ftb_peer(orte_notifier_base_severity_t severity, int errcode,
                     orte_process_name_t *peer_proc, const char *msg,
                     va_list ap)
{
    char payload[FTB_MAX_PAYLOAD_DATA + 1];
    char *peer_host = NULL;
    char *pos = payload;
    int len, space = FTB_MAX_PAYLOAD_DATA;

    if (peer_proc) {
        peer_host = orte_ess.proc_get_hostname(peer_proc);
    }
    len = snprintf(pos, space, "%s:", peer_host ? peer_host : "UNKNOWN");
    space -= len;
    pos += len;

    /* If there was a message, and space left, output it */
    if (0 < space) {
        vsnprintf(pos, space, msg, ap);
    }

    payload[FTB_MAX_PAYLOAD_DATA] = '\0';
    publish_ftb_event(severity, errcode, payload);
}
