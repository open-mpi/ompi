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
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
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
static void mylog(orte_notifier_severity_t severity, int errcode, 
                  const char *msg, va_list *ap);

/* Module def */
orte_notifier_base_module_t orte_notifier_syslog_module = {
    init,
    finalize,
    mylog,
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

static void mylog(orte_notifier_severity_t severity, int errcode, 
                  const char *msg, va_list *ap)
{
    opal_output_verbose(5, orte_notifier_base_framework.framework_output,
                           "notifier:syslog:mylog function called with severity %d errcode %d and messg %s",
                           (int) severity, errcode, msg);
    /* If there was a message, output it */
#if defined(HAVE_VSYSLOG)
    vsyslog(severity, msg, *ap);
#else
    char *output;
    vasprintf(&output, msg, *ap);
    syslog(severity, output, NULL);
    free(output);
#endif
}
