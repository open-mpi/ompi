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

#include "orte/mca/notifier/base/base.h"
#include "notifier_syslog.h"


/* Static API's */
static int init(void);
static void finalize(void);
static void mylog(int priority, const char *msg, ...);
static void myhelplog(int priority, const char *filename, const char *topic, ...);

/* Module def */
orte_notifier_base_module_t orte_notifier_syslog_module = {
    init,
    finalize,
    mylog,
    myhelplog
};


static int init(void) {
    int opts;
    
    opts = LOG_CONS | LOG_PID | LOG_SYSLOG;
    openlog("Open MPI Error Report:", opts, LOG_USER);
    
    return ORTE_SUCCESS;
}

static void finalize(void) {
    closelog();
}

static void mylog(int priority, const char *msg, ...)
{
    va_list arglist;
    
    /* If there was a message, output it */
    va_start(arglist, msg);
    vsyslog(priority, msg, arglist);
    va_end(arglist);
}

static void myhelplog(int priority, const char *filename, const char *topic, ...)
{
    va_list arglist;
    char *output;
    
    va_start(arglist, topic);
    output = opal_show_help_vstring(filename, topic, false, arglist);
    va_end(arglist);
    
    /* if nothing came  back, then nothing to do */
    if (NULL == output) {
        return;
    }
    
    /* go ahead and output it */
    syslog(priority, output);
    free(output);
}
