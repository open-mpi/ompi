/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "pmix_config.h"
#include "pmix_common.h"
#include "src/include/types.h"

#include <string.h>
#include <signal.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "src/mca/mca.h"
#include "src/mca/base/base.h"
#include "src/threads/threads.h"
#include "src/include/pmix_globals.h"
#include "src/common/pmix_iof.h"
#include "src/client/pmix_client_ops.h"
#include "src/util/error.h"

#include "src/mca/pfexec/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/pfexec/base/static-components.h"

/*
 * Instantiate globals
 */
pmix_pfexec_base_module_t pmix_pfexec = {0};

/*
 * Framework global variables
 */
pmix_pfexec_globals_t pmix_pfexec_globals = {0};

static int pmix_pfexec_base_close(void)
{
    PMIX_LIST_DESTRUCT(&pmix_pfexec_globals.children);
    if (pmix_pfexec_globals.active) {
        pmix_event_del(&pmix_pfexec_globals.handler);
    }
    pmix_pfexec_globals.active = false;

    return pmix_mca_base_framework_components_close(&pmix_pfexec_base_framework, NULL);
}

/* callback from the event library whenever a SIGCHLD is received */
static void wait_signal_callback(int fd, short event, void *arg)
{
    (void)fd;
    (void)event;
    pmix_event_t *signal = (pmix_event_t*) arg;
    int status;
    pid_t pid;
    pmix_pfexec_child_t *child;

    PMIX_ACQUIRE_OBJECT(signal);

    if (SIGCHLD != PMIX_EVENT_SIGNAL(signal)) {
        return;
    }

    /* if we haven't spawned anyone, then ignore this */
    if (0 == pmix_list_get_size(&pmix_pfexec_globals.children)) {
        return;
    }

    /* reap all queued waitpids until we
     * don't get anything valid back */
    while (1) {
        pid = waitpid(-1, &status, WNOHANG);
        if (-1 == pid && EINTR == errno) {
            /* try it again */
            continue;
        }
        /* if we got garbage, then nothing we can do */
        if (pid <= 0) {
            return;
        }

        /* we are already in an event, so it is safe to access globals */
        PMIX_LIST_FOREACH(child, &pmix_pfexec_globals.children, pmix_pfexec_child_t) {
            if (pid == child->pid) {
                /* record the exit status */
                if (WIFEXITED(status)) {
                    child->exitcode = WEXITSTATUS(status);
                } else {
                    if (WIFSIGNALED(status)) {
                        child->exitcode = WTERMSIG(status) + 128;
                    }
                }
                /* mark the child as complete */
                child->completed = true;
                PMIX_PFEXEC_CHK_COMPLETE(child);
                break;
            }
        }
    }
}

void pmix_pfexec_check_complete(int sd, short args, void *cbdata)
{
    (void)sd;
    (void)args;
    pmix_pfexec_cmpl_caddy_t *cd = (pmix_pfexec_cmpl_caddy_t*)cbdata;
    pmix_info_t info[2];
    pmix_status_t rc;

    /* if the waitpid fired and the sink is empty, then that means
     * it terminated and all output has been written, so remove
     * it from the list of children */
    if (cd->child->completed &&
        (NULL == cd->child->stdoutev || !cd->child->stdoutev->active) &&
        (NULL == cd->child->stderrev || !cd->child->stderrev->active)) {
        pmix_list_remove_item(&pmix_pfexec_globals.children, &cd->child->super);
        PMIX_RELEASE(cd->child);
        if (0 == pmix_list_get_size(&pmix_pfexec_globals.children)) {
            /* generate a local event indicating job terminated */
            PMIX_INFO_LOAD(&info[0], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
            PMIX_INFO_LOAD(&info[1], PMIX_EVENT_AFFECTED_PROC, &pmix_globals.myid, PMIX_PROC);
            rc = PMIx_Notify_event(PMIX_ERR_JOB_TERMINATED,
                                   &pmix_globals.myid, PMIX_RANGE_PROC_LOCAL,
                                   info, 2, NULL, NULL);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
        }
    }
    PMIX_RELEASE(cd);
}

static int pmix_pfexec_register(pmix_mca_base_register_flag_t flags)
{
    (void)flags;
    pmix_pfexec_globals.timeout_before_sigkill = 1;
    pmix_mca_base_var_register("pmix", "pfexec", "base", "sigkill_timeout",
                                 "Time to wait for a process to die after issuing a kill signal to it",
                               PMIX_MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                               PMIX_INFO_LVL_2,
                               PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                               &pmix_pfexec_globals.timeout_before_sigkill);
    return PMIX_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int pmix_pfexec_base_open(pmix_mca_base_open_flag_t flags)
{
    sigset_t unblock;

    memset(&pmix_pfexec_globals, 0, sizeof(pmix_pfexec_globals_t));

    /* setup the list of children */
    PMIX_CONSTRUCT(&pmix_pfexec_globals.children, pmix_list_t);
    pmix_pfexec_globals.next = 1;

    /* ensure that SIGCHLD is unblocked as we need to capture it */
    if (0 != sigemptyset(&unblock)) {
        return PMIX_ERROR;
    }
    if (0 != sigaddset(&unblock, SIGCHLD)) {
        return PMIX_ERROR;
    }
    if (0 != sigprocmask(SIG_UNBLOCK, &unblock, NULL)) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* set to catch SIGCHLD events */
    pmix_event_set(pmix_globals.evbase,
                   &pmix_pfexec_globals.handler,
                   SIGCHLD, PMIX_EV_SIGNAL|PMIX_EV_PERSIST,
                   wait_signal_callback,
                   &pmix_pfexec_globals.handler);
    pmix_pfexec_globals.active = true;
    pmix_event_add(&pmix_pfexec_globals.handler, NULL);

     /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_pfexec_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pfexec, "PMIx fork/exec Subsystem",
                                pmix_pfexec_register, pmix_pfexec_base_open, pmix_pfexec_base_close,
                                mca_pfexec_base_static_components, 0);


/**** FRAMEWORK CLASS INSTANTIATIONS ****/

static void chcon(pmix_pfexec_child_t *p)
{
    pmix_proc_t proc;

    p->rank = pmix_pfexec_globals.next;
    PMIX_LOAD_PROCID(&proc, pmix_globals.myid.nspace, p->rank);
    pmix_pfexec_globals.next++;
    p->pid = 0;
}
static void chdes(pmix_pfexec_child_t *p)
{
    if (NULL != p->stdoutev) {
        PMIX_RELEASE(p->stdoutev);
    }
    if (NULL != p->stderrev) {
        PMIX_RELEASE(p->stderrev);
    }
}
PMIX_CLASS_INSTANCE(pmix_pfexec_child_t,
                    pmix_list_item_t,
                    chcon, chdes);

static void fccon(pmix_pfexec_fork_caddy_t *p)
{
    p->jobinfo = NULL;
    p->njinfo = 0;
    p->apps = NULL;
    p->napps = 0;
}
PMIX_CLASS_INSTANCE(pmix_pfexec_fork_caddy_t,
                    pmix_object_t,
                    fccon, NULL);

PMIX_CLASS_INSTANCE(pmix_pfexec_signal_caddy_t,
                    pmix_object_t,
                    NULL, NULL);

PMIX_CLASS_INSTANCE(pmix_pfexec_cmpl_caddy_t,
                    pmix_object_t,
                    NULL, NULL);
