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
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"
#include "pmix_common.h"
#include "src/include/pmix_types.h"

#include <signal.h>
#include <string.h>
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif

#include "src/client/pmix_client_ops.h"
#include "src/common/pmix_iof.h"
#include "src/include/pmix_globals.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_error.h"

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
pmix_pfexec_base_module_t pmix_pfexec = {
    .spawn_job = NULL,
    .kill_proc = NULL,
    .signal_proc = NULL
};

/*
 * Framework global variables
 */
pmix_pfexec_globals_t pmix_pfexec_globals = {
    .handler = NULL,
    .active = false,
    .children = PMIX_LIST_STATIC_INIT,
    .timeout_before_sigkill = 0,
    .nextid = 0,
    .selected = false
};

static int pmix_pfexec_base_close(void)
{
    if (pmix_pfexec_globals.active) {
        pmix_event_del(pmix_pfexec_globals.handler);
        pmix_pfexec_globals.active = false;
    }
    PMIX_LIST_DESTRUCT(&pmix_pfexec_globals.children);
    free(pmix_pfexec_globals.handler);
    pmix_pfexec_globals.selected = false;

    return pmix_mca_base_framework_components_close(&pmix_pfexec_base_framework, NULL);
}

void pmix_pfexec_check_complete(int sd, short args, void *cbdata)
{
    (void) sd;
    (void) args;
    pmix_pfexec_cmpl_caddy_t *cd = (pmix_pfexec_cmpl_caddy_t *) cbdata;
    pmix_info_t info[2];
    pmix_status_t rc;
    pmix_pfexec_child_t *child;
    bool stillalive = false;
    pmix_proc_t wildcard;

    pmix_list_remove_item(&pmix_pfexec_globals.children, &cd->child->super);
    /* see if any more children from this nspace are alive */
    PMIX_LIST_FOREACH (child, &pmix_pfexec_globals.children, pmix_pfexec_child_t) {
        if (PMIX_CHECK_NSPACE(child->proc.nspace, cd->child->proc.nspace)) {
            stillalive = true;
        }
    }
    if (!stillalive) {
        /* generate a local event indicating job terminated */
        PMIX_INFO_LOAD(&info[0], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
        PMIX_LOAD_NSPACE(wildcard.nspace, cd->child->proc.nspace);
        PMIX_INFO_LOAD(&info[1], PMIX_EVENT_AFFECTED_PROC, &wildcard, PMIX_PROC);
        rc = PMIx_Notify_event(PMIX_ERR_JOB_TERMINATED, &pmix_globals.myid, PMIX_RANGE_PROC_LOCAL,
                               info, 2, NULL, NULL);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }
    PMIX_RELEASE(cd->child);
    PMIX_RELEASE(cd);
}

static int pmix_pfexec_register(pmix_mca_base_register_flag_t flags)
{
    (void) flags;
    pmix_pfexec_globals.timeout_before_sigkill = 1;
    pmix_mca_base_var_register("pmix", "pfexec", "base", "sigkill_timeout",
                               "Time to wait for a process to die after issuing a kill signal to it",
                               PMIX_MCA_BASE_VAR_TYPE_INT,
                               &pmix_pfexec_globals.timeout_before_sigkill);
    return PMIX_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int pmix_pfexec_base_open(pmix_mca_base_open_flag_t flags)
{
    memset(&pmix_pfexec_globals, 0, sizeof(pmix_pfexec_globals_t));

    /* setup the list of children */
    PMIX_CONSTRUCT(&pmix_pfexec_globals.children, pmix_list_t);
    pmix_pfexec_globals.nextid = 1;

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_pfexec_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pfexec, "PMIx fork/exec Subsystem", pmix_pfexec_register,
                                pmix_pfexec_base_open, pmix_pfexec_base_close,
                                pmix_mca_pfexec_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);

/**** FRAMEWORK CLASS INSTANTIATIONS ****/

static void chcon(pmix_pfexec_child_t *p)
{
    memset(&p->ev, 0, sizeof(pmix_event_t));
    PMIX_LOAD_PROCID(&p->proc, NULL, PMIX_RANK_UNDEF);
    p->pid = 0;
    p->completed = false;
    p->keepalive[0] = -1;
    p->keepalive[1] = -1;
    memset(&p->opts, 0, sizeof(pmix_pfexec_base_io_conf_t));
    p->opts.p_stdin[0] = -1;
    p->opts.p_stdin[1] = -1;
    p->opts.p_stdout[0] = -1;
    p->opts.p_stdout[1] = -1;
    p->opts.p_stderr[0] = -1;
    p->opts.p_stderr[1] = -1;
    PMIX_CONSTRUCT(&p->stdinsink, pmix_iof_sink_t);
    p->stdoutev = NULL;
    p->stderrev = NULL;
}
static void chdes(pmix_pfexec_child_t *p)
{
    PMIX_DESTRUCT(&p->stdinsink);
    if (NULL != p->stdoutev) {
        PMIX_RELEASE(p->stdoutev);
    }
    if (NULL != p->stderrev) {
        PMIX_RELEASE(p->stderrev);
    }
    if (0 <= p->keepalive[0]) {
        close(p->keepalive[0]);
    }
    if (0 <= p->keepalive[1]) {
        close(p->keepalive[1]);
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
    p->frkfn = NULL;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
PMIX_CLASS_INSTANCE(pmix_pfexec_fork_caddy_t,
                    pmix_object_t, fccon, NULL);

PMIX_CLASS_INSTANCE(pmix_pfexec_signal_caddy_t,
                    pmix_object_t, NULL, NULL);

PMIX_CLASS_INSTANCE(pmix_pfexec_cmpl_caddy_t,
                    pmix_object_t, NULL, NULL);
