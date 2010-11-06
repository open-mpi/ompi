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
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "opal/util/show_help.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"

#include "orte/mca/notifier/base/base.h"
#include "notifier_syslog.h"


/* Static API's */
static int init(void);
static void finalize(void);
static void mylog(orte_notifier_base_severity_t severity, int errcode, 
                  const char *msg, va_list ap);
static void myhelplog(orte_notifier_base_severity_t severity, int errcode, 
                      const char *filename, const char *topic, va_list ap);
static void mypeerlog(orte_notifier_base_severity_t severity, int errcode, 
                      orte_process_name_t *peer_proc, const char *msg, 
                      va_list ap);
static void myeventlog(const char *msg);

/* Module def */
orte_notifier_base_module_t orte_notifier_syslog_module = {
    init,
    finalize,
    mylog,
    myhelplog,
    mypeerlog,
    myeventlog
};


static int init(void) 
{
    int opts;
    
    opts = LOG_CONS | LOG_PID;
    openlog("Open MPI Error Report:", opts, LOG_USER);
    
    return ORTE_SUCCESS;
}

static void finalize(void) 
{
    closelog();
}

static void mylog(orte_notifier_base_severity_t severity, int errcode, 
                  const char *msg, va_list ap)
{
    /* If there was a message, output it */
    vsyslog(severity, msg, ap);
}

static void myhelplog(orte_notifier_base_severity_t severity, int errcode, 
                      const char *filename, const char *topic, va_list ap)
{
    char *output = opal_show_help_vstring(filename, topic, false, ap);
    
    /* if nothing came  back, then nothing to do */
    if (NULL == output) {
        return;
    }
    
    /* go ahead and output it */
    syslog(severity, output);
    free(output);
}

static void mypeerlog(orte_notifier_base_severity_t severity, int errcode, 
                      orte_process_name_t *peer_proc, const char *msg, 
                      va_list ap)
{
    char *buf = orte_notifier_base_peer_log(errcode, peer_proc, msg, ap);

    if (NULL != buf) {
        syslog(severity, buf);
        free(buf);
    }
}

static void myeventlog(const char *msg)
{
    /* If there was a message, output it */
    syslog(LOG_LOCAL0 | LOG_NOTICE, msg);
}

