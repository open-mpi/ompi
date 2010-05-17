/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
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

#include "opal/util/show_help.h"

#include "orte/util/error_strings.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/show_help.h"
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

static FTB_event_info_t ftb_event_info[] = {
/* 0 */    {"UNKNOWN_ERROR",	"error"},
/* 1 */    {"OUT_OF_RESOURCES",	"error"},
/* 2 */    {"UNREACHABLE",	"error"},
/* 3 */    {"COMM_FAILURE",	"error"},
/* 4 */    {"FATAL",		"fatal"},
};
static const int ftb_event_info_count = sizeof(ftb_event_info)/sizeof(FTB_event_info_t);

static int orte_err2ftb(int errnum)
{
    int retval;

    switch (OPAL_SOS_GET_ERROR_CODE(errnum)) {
    case ORTE_ERR_OUT_OF_RESOURCE:
    case ORTE_ERR_TEMP_OUT_OF_RESOURCE:
        retval =  1;
        break;
    case ORTE_ERR_CONNECTION_REFUSED:
    case ORTE_ERR_CONNECTION_FAILED:
    case ORTE_ERR_UNREACH:
        retval =  2;
        break;
    case ORTE_ERR_COMM_FAILURE:
        retval =  3;
        break;
    case ORTE_ERR_FATAL:
        retval =  4;
        break;
    default:
        retval = 0;
    }

    if ((ftb_event_info_count <= retval) || (0 > retval)) {
        retval = 0;
    }
    return retval;
}

static int init(void) {
    int ret;

    ret = FTB_Declare_publishable_events(ftb_client_handle, 0, ftb_event_info, ftb_event_info_count);
    if (FTB_SUCCESS != ret) {
        orte_show_help("help-orte-notifier-ftb.txt",
                       "declare events failed",
                       true, "FTB_Declare_publishable_events() failed", ret);

        FTB_Disconnect(ftb_client_handle);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static void finalize(void) {
    FTB_Disconnect(ftb_client_handle);
}

static void send_to_ftb(int errcode, char *payload)
{
    int ret, event_id;
    FTB_event_handle_t ehandle;
    FTB_event_properties_t eprop;
    eprop.event_type = 1;
    snprintf(eprop.event_payload, FTB_MAX_PAYLOAD_DATA, "%s", (payload != NULL) ? payload : "");

    event_id = orte_err2ftb(errcode);
    ret = FTB_Publish(ftb_client_handle, ftb_event_info[event_id].event_name, &eprop, &ehandle);
    if (FTB_SUCCESS != ret) {
        orte_show_help("help-orte-notifier-ftb.txt",
                       "publish failed",
                       true, "FTB_Publish() failed", ret,
                       ftb_event_info[event_id].severity,
                       ftb_event_info[event_id].event_name,
                       eprop.event_payload, errcode);
    }
}

static void ftb_log(orte_notifier_base_severity_t severity, int errcode, const char *msg,
                  va_list ap)
{
    char *payload;

    /* If there was a message, output it */
    vasprintf(&payload, msg, ap);
    if (NULL != payload) {
        send_to_ftb(errcode, payload);
        free(payload);
    }
}

static void ftb_help(orte_notifier_base_severity_t severity, int errcode,
                      const char *filename, const char *topic, va_list ap)
{
    char *output = opal_show_help_vstring(filename, topic, false, ap);
    
    if (NULL != output) {
        send_to_ftb(errcode, output);
        free(output);
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
    send_to_ftb(errcode, payload);
}
