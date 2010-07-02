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
 * Copyright (c) 2009      Bull SAS.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif /* HAVE_STDIO_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "opal/util/os_path.h"
#include "opal/util/show_help.h"

#include "orte/mca/notifier/base/base.h"
#include "orte/util/proc_info.h"
#include "notifier_file.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int open_file(void);
static void file_log(orte_notifier_base_severity_t severity, int errcode, 
                     const char *msg, va_list ap);
static void file_helplog(orte_notifier_base_severity_t severity, int errcode, 
                         const char *filename, const char *topic, va_list ap);
static void file_peerlog(orte_notifier_base_severity_t severity, int errcode, 
                         orte_process_name_t *peer_proc, const char *msg, 
                         va_list ap);
static void file_eventlog(const char *msg);

/* Module def */
orte_notifier_base_module_t orte_notifier_file_module = {
    init,
    finalize,
    file_log,
    file_helplog,
    file_peerlog,
    file_eventlog
};

static int mylogfd = -1;

static int init(void) 
{
    orte_notifier_file_component_t *comp = &mca_notifier_file_component;

    if (!strcmp(comp->fname, "stdout")) {
        mylogfd = fileno(stdout);
    } else if (!strcmp(comp->fname, "stderr")) {
        mylogfd = fileno(stderr);
    }
    /* Don't open in the case of a plain file: wait for the 1st write */

    return ORTE_SUCCESS;
}

static void finalize(void) 
{
    if (-1 == mylogfd || fileno(stderr) == mylogfd
                      || fileno(stdout) == mylogfd) {
        return;
    }
    close(mylogfd);
}

static int open_file(void)
{
    orte_notifier_file_component_t *comp = &mca_notifier_file_component;
    char *full_name = NULL;
    char *fname = NULL;
    int rc = ORTE_SUCCESS;

    if (-1 != mylogfd) {
        return ORTE_SUCCESS;
    }

    asprintf(&fname, "output-%s", comp->fname);
    if (NULL == fname) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    full_name = opal_os_path(false, orte_process_info.job_session_dir,
                             fname, NULL);
    if (NULL == full_name) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto out_err;
    }

    mylogfd = open(full_name, O_CREAT | O_RDWR | O_APPEND, S_IRWXU);
    if (-1 == mylogfd) {
        rc = ORTE_ERR_FILE_OPEN_FAILURE;
    }

    free(full_name);
out_err:
    free(fname);
    return rc;
}

static void file_log(orte_notifier_base_severity_t severity, int errcode, 
                     const char *msg, va_list ap)
{
    char *output;
    char *tmp = NULL;

    /* Add a newline at the end of the format string */
    asprintf(&tmp, "%s\n", msg);
    if (NULL == tmp) {
        return;
    }

    vasprintf(&output, tmp, ap);
    free(tmp);

    if (NULL == output) {
        return;
    }

    /* If not done yet, open the log file */
    if (-1 == mylogfd) {
        if (ORTE_SUCCESS != open_file()) {
            free(output);
            return;
        }
    }

    write(mylogfd, output, strlen(output));
    fflush(NULL);
    free(output);
}

static void file_helplog(orte_notifier_base_severity_t severity, int errcode, 
                         const char *filename, const char *topic, va_list ap)
{
    char *output = opal_show_help_vstring(filename, topic, false, ap);
    
    if (NULL == output) {
        return;
    }

    if (-1 == mylogfd) {
        if (ORTE_SUCCESS != open_file()) {
            free(output);
            return;
        }
    }

    write(mylogfd, output, strlen(output));
    fflush(NULL);
    free(output);
}

static void file_peerlog(orte_notifier_base_severity_t severity, int errcode, 
                         orte_process_name_t *peer_proc, const char *msg, 
                         va_list ap)
{
    char *buf;
    char *tmp = NULL;

    /* Add a newline at the end of the format string */
    asprintf(&tmp, "%s\n", msg);
    if (NULL == tmp) {
        return;
    }

    buf = orte_notifier_base_peer_log(errcode, peer_proc, tmp, ap);
    free(tmp);

    if (NULL == buf) {
        return;
    }

    /* If not done yet, open the log file */
    if (-1 == mylogfd) {
        if (ORTE_SUCCESS != open_file()) {
            free(buf);
            return;
        }
    }

    write(mylogfd, buf, strlen(buf));
    fflush(NULL);
    free(buf);
}

static void file_eventlog(const char *msg)
{
    char *tmp = NULL;

    /* Add a newline at the end of the string */
    asprintf(&tmp, "%s\n", msg);
    if (NULL == tmp) {
        return;
    }

    /* If not done yet, open the log file */
    if (-1 == mylogfd) {
        if (ORTE_SUCCESS != open_file()) {
            free(tmp);
            return;
        }
    }

    write(mylogfd, tmp, strlen(tmp));
    fflush(NULL);
    free(tmp);
}
