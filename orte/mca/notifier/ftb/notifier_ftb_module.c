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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "opal/util/show_help.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/notifier/base/base.h"
#include "notifier_ftb.h"


/* Static API's */
static int init(void);
static void finalize(void);
static void mylog(int severity, int errcode, const char *msg, ...);
static void myhelplog(int severity, int errcode, const char *filename, const char *topic, ...);
static void mypeerlog(int severity, int errcode, orte_process_name_t *peer_proc, const char *msg, ...);

/* Module def */
orte_notifier_base_module_t orte_notifier_ftb_module = {
    init,
    finalize,
    mylog,
    myhelplog,
    mypeerlog
};

/* Module "global" variables */
static FTB_client_t cinfo = {
    .event_space = "ftb.mpi.openmpi",
    .client_name = "",
    .client_jobid = "",
    .client_subscription_style = "FTB_SUBSCRIPTION_NONE"
};
static FTB_client_handle_t chandle;

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

    switch (errnum) {
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

/*    snprintf(cinfo.client_name, FTB_MAX_CLIENT_NAME, "%s", argv[0]); 
 * How to obtain argv[0] at this point?  I don't know...
 * similarly, how do we obtain client_jobid?
 *    snprintf(cinfo.client_jobid, FTB_MAX_CLIENT_JOBID, "%s", orte_jobid???); 
 */
    
    if (FTB_SUCCESS != (ret = FTB_Connect(&cinfo, &chandle))) {
        opal_output(orte_notifier_base_output,
            "notifier:ftb:init FTB_Connect failed ret=%d\n", ret);
        return ORTE_ERROR;
    }

    ret = FTB_Declare_publishable_events(chandle, 0, ftb_event_info, ftb_event_info_count);
    if (FTB_SUCCESS != ret) {
        opal_output(orte_notifier_base_output,
            "notifier:ftb:init FTB_Declare_publishable_events failed ret=%d\n", ret);
        FTB_Disconnect(chandle);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static void finalize(void) {
    FTB_Disconnect(chandle);
}

static void convert2ftb(int errcode, char *payload)
{
    int ret, event_id;
    FTB_event_handle_t ehandle;
    FTB_event_properties_t eprop;
    eprop.event_type = 1;
    snprintf(eprop.event_payload, FTB_MAX_PAYLOAD_DATA, "%s", (payload != NULL) ? payload : "");

    event_id = orte_err2ftb(errcode);
    ret = FTB_Publish(chandle, ftb_event_info[event_id].event_name, &eprop, &ehandle);
    if (FTB_SUCCESS != ret) {
        opal_output(orte_notifier_base_output,
            "notifier:ftb:convert2ftb(%d,'%s') FTB_Publish failed ret=%d\n", errcode, eprop.event_payload, ret);
    }
}

static void mylog(int severity, int errcode, const char *msg, ...)
{
    va_list arglist;
    char payload[FTB_MAX_PAYLOAD_DATA + 1];

    /* If there was a message, output it */
    va_start(arglist, msg);
    vsnprintf(payload, FTB_MAX_PAYLOAD_DATA, msg, arglist);
    payload[FTB_MAX_PAYLOAD_DATA] = '\0'; /* not needed? */
    va_end(arglist);

    convert2ftb(errcode, payload);
}

static void myhelplog(int severity, int errcode, const char *filename, const char *topic, ...)
{
    va_list arglist;
    char *output;
    
    va_start(arglist, topic);
    output = opal_show_help_vstring(filename, topic, false, arglist);
    va_end(arglist);
    
    convert2ftb(errcode, output);

    if (NULL != output) {
        free(output);
    }
}

static void mypeerlog(int severity, int errcode, orte_process_name_t *peer_proc, const char *msg, ...)
{
    va_list arglist;
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
        va_start(arglist, msg);
        vsnprintf(pos, space, msg, arglist);
        va_end(arglist);
    }

    payload[FTB_MAX_PAYLOAD_DATA] = '\0'; /* not needed? */
    convert2ftb(errcode, payload);
}
