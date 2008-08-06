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
#include "orte/types.h"

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

#include "opal/threads/condition.h"
#include "opal/util/bit_ops.h"
#include "opal/class/opal_hash_table.h"
#include "opal/dss/dss.h"


#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/notifier/base/base.h"
#include "notifier_syslog.h"


/* Static API's */
static int init(void);
static void finalize(void);
static void mylog(int priority, const char *msg, ...);

/* Module def */
orte_notifier_base_module_t orte_notifier_syslog_module = {
    init,
    finalize,
    mylog
};


static int init(void) {
    int opts;
    
    opts = LOG_CONS | LOG_PID | LOG_SYSLOG;
    openlog("OpenMPI Error Report:", opts, LOG_USER);
    
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
