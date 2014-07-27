/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <time.h>
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/runtime/opal_params.h"
#include "opal/dss/dss.h"
#include "opal/util/show_help.h"

char *opal_signal_string = NULL;
char *opal_net_private_ipv4 = NULL;
char *opal_set_max_sys_limits = NULL;
int opal_pmi_version = 0;

bool opal_built_with_cuda_support = OPAL_INT_TO_BOOL(OPAL_CUDA_SUPPORT);
bool opal_cuda_support;
#if OPAL_ENABLE_FT_CR == 1
bool opal_base_distill_checkpoint_ready = false;
#endif

/**
 * Globals imported from the OMPI layer.
 */
int opal_leave_pinned = -1;
bool opal_leave_pinned_pipeline = false;

static bool opal_register_done = false;

int opal_register_params(void)
{
    int ret;

    if (opal_register_done) {
        return OPAL_SUCCESS;
    }

    opal_register_done = true;

    /*
     * This string is going to be used in opal/util/stacktrace.c
     */
    {
        char *string = NULL;
        int j;
        int signals[] = {
#ifdef SIGABRT
            SIGABRT,
#endif
#ifdef SIGBUS
            SIGBUS,
#endif
#ifdef SIGFPE
            SIGFPE,
#endif
#ifdef SIGSEGV
            SIGSEGV,
#endif
            -1
        };
        for (j = 0 ; signals[j] != -1 ; ++j) {
            if (j == 0) {
                asprintf(&string, "%d", signals[j]);
            } else {
                char *tmp;
                asprintf(&tmp, "%s,%d", string, signals[j]);
                free(string);
                string = tmp;
            }
        }

        opal_signal_string = string;
        ret = mca_base_var_register ("opal", "opal", NULL, "signal",
				     "Comma-delimited list of integer signal numbers to Open MPI to attempt to intercept.  Upon receipt of the intercepted signal, Open MPI will display a stack trace and abort.  Open MPI will *not* replace signals if handlers are already installed by the time MPI_INIT is invoked.  Optionally append \":complain\" to any signal number in the comma-delimited list to make Open MPI complain if it detects another signal handler (and therefore does not insert its own).",
				     MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
				     OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_LOCAL,
				     &opal_signal_string);
        free (string);
        if (0 > ret) {
            return ret;
        }
    }

#if defined(HAVE_SCHED_YIELD)
    opal_progress_yield_when_idle = false;
    ret = mca_base_var_register ("opal", "opal", "progress", "yield_when_idle",
                                 "Yield the processor when waiting on progress",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                 OPAL_INFO_LVL_8, MCA_BASE_VAR_SCOPE_LOCAL,
                                 &opal_progress_yield_when_idle);
#endif

#if OPAL_ENABLE_DEBUG
    opal_progress_debug = false;
    ret = mca_base_var_register ("opal", "opal", "progress", "debug",
				 "Set to non-zero to debug progress engine features",
				 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
				 OPAL_INFO_LVL_8, MCA_BASE_VAR_SCOPE_LOCAL,
				 &opal_progress_debug);
    if (0 > ret) {
        return ret;
    }

    opal_debug_threads = false;
    ret = mca_base_var_register ("opal", "opal", "debug", "threads",
				 "Debug thread usage within OPAL. Reports out "
				 "when threads are acquired and released.",
				 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
				 OPAL_INFO_LVL_8, MCA_BASE_VAR_SCOPE_LOCAL,
				 &opal_debug_threads);
    if (0 > ret) {
        return ret;
    }
#endif

#if OPAL_ENABLE_FT_CR == 1
    opal_base_distill_checkpoint_ready = false;
    ret = mca_base_var_register("opal", "opal", "base", "distill_checkpoint_ready",
                                "Distill only those components that are Checkpoint Ready",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                OPAL_INFO_LVL_8, MCA_BASE_VAR_SCOPE_LOCAL,
                                &opal_base_distill_checkpoint_ready);

    if (0 > ret) {
        return ret;
    }
#endif

    /* RFC1918 defines
       - 10.0.0./8
       - 172.16.0.0/12
       - 192.168.0.0/16
       
       RFC3330 also mentiones
       - 169.254.0.0/16 for DHCP onlink iff there's no DHCP server
    */
    opal_net_private_ipv4 = "10.0.0.0/8;172.16.0.0/12;192.168.0.0/16;169.254.0.0/16";
    ret = mca_base_var_register ("opal", "opal", "net", "private_ipv4",
				 "Semicolon-delimited list of CIDR notation entries specifying what networks are considered \"private\" (default value based on RFC1918 and RFC3330)",
				 MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
				 OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_ALL_EQ,
				 &opal_net_private_ipv4);
    if (0 > ret) {
        return ret;
    }

    opal_set_max_sys_limits = NULL;
    ret = mca_base_var_register ("opal", "opal", NULL, "set_max_sys_limits",
				 "Set the specified system-imposed limits to the specified value, including \"unlimited\"."
                                 "Supported params: core, filesize, maxmem, openfiles, stacksize, maxchildren",
				 MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
				 OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_ALL_EQ,
				 &opal_set_max_sys_limits);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register ("opal", "opal", NULL, "cuda_support",
                                 "Whether CUDA GPU buffer support is enabled or not",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                 OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_ALL_EQ,
                                 &opal_cuda_support);

    /* Leave pinned parameter */
    opal_leave_pinned = -1;
    ret = mca_base_var_register("ompi", "mpi", NULL, "leave_pinned",
                                "Whether to use the \"leave pinned\" protocol or not.  Enabling this setting can help bandwidth performance when repeatedly sending and receiving large messages with the same buffers over RDMA-based networks (0 = do not use \"leave pinned\" protocol, 1 = use \"leave pinned\" protocol, -1 = allow network to choose at runtime).",
                                MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                OPAL_INFO_LVL_9,
                                MCA_BASE_VAR_SCOPE_READONLY,
                                &opal_leave_pinned);
    mca_base_var_register_synonym(ret, "opal", "opal", NULL, "leave_pinned",
                                  MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    opal_leave_pinned_pipeline = false;
    ret = mca_base_var_register("ompi", "mpi", NULL, "leave_pinned_pipeline",
                                "Whether to use the \"leave pinned pipeline\" protocol or not.",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_leave_pinned_pipeline);
    mca_base_var_register_synonym(ret, "opal", "opal", NULL, "leave_pinned_pipeline",
                                  MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    if (opal_leave_pinned > 0 && opal_leave_pinned_pipeline) {
        opal_leave_pinned_pipeline = 0;
        opal_show_help("help-mpi-runtime.txt",
                       "mpi-params:leave-pinned-and-pipeline-selected",
                       true);
    }

    opal_pmi_version = 0;
#ifdef WANT_PMI2_SUPPORT
    (void) mca_base_var_register ("opal", "opal", NULL, "pmi_version",
                                  "Set preferred PMI version: 0 => auto detect, 1 = PMIv1, 2 = PMIv2",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &opal_pmi_version);
#else
    opal_pmi_version = 1;
#endif

    opal_warn_on_fork = true;
    (void) mca_base_var_register("ompi", "mpi", NULL, "warn_on_fork",
                                 "If nonzero, issue a warning if program forks under conditions that could cause system errors",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_warn_on_fork);

    /* The ddt engine has a few parameters */
    ret = opal_datatype_register_params();
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* dss has parameters */
    ret = opal_dss_register_vars ();
    if (OPAL_SUCCESS != ret) { 
        return ret; 
    }

    return OPAL_SUCCESS;
}

int opal_deregister_params(void)
{
    opal_signal_string = NULL;
    opal_net_private_ipv4 = NULL;
    opal_set_max_sys_limits = NULL;
    opal_pmi_version = 0;
    opal_register_done = false;

    return OPAL_SUCCESS;
}
