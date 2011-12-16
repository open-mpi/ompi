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
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
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
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/threads/mutex.h"
#include "opal/mca/paffinity/base/base.h"

int opal_register_params(void)
{
    int value;

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

        mca_base_param_reg_string_name("opal", "signal", 
                                       "Comma-delimited list of integer signal numbers to Open MPI to attempt to intercept.  Upon receipt of the intercepted signal, Open MPI will display a stack trace and abort.  Open MPI will *not* replace signals if handlers are already installed by the time MPI_INIT is invoked.  Optionally append \":complain\" to any signal number in the comma-delimited list to make Open MPI complain if it detects another signal handler (and therefore does not insert its own).",
                                       false, false, string, NULL);
        free(string);
    }

#if OMPI_ENABLE_DEBUG


    mca_base_param_reg_int_name("opal", "progress_debug", 
                                "Set to non-zero to debug progress engine features",
                                false, false, 0, NULL);

    {
        mca_base_param_reg_int_name("opal", "debug_locks",
                                    "Debug mutex usage within Open MPI.  On a "
                                    "non-threaded build, this enables integer counters and "
                                    "warning messages when double-locks are detected.",
                                    false, false, 0, &value);
        if (value) opal_mutex_check_locks = true;
    }
#endif

    /*
     * Do we want the "warning: your mmap file is on NFS!" message?  Per a
     * thread on the OMPI devel list
     * (http://www.open-mpi.org/community/lists/devel/2011/12/10054.php),
     * on some systems, it doesn't seem to matter.  But per older threads,
     * it definitely does matter on some systems.  Perhaps newer kernels
     * are smarter about this kind of stuff...?  Regardless, we should
     * provide the ability to turn off this message for systems where the
     * effect doesn't matter.
     *
     * v1.4.x-specific note: the MCA param name is "shmem_mmap_...",
     * where "shmem" is not a framework that exists in the v1.4
     * series.  This parameter was added right before 1.4.5, and at a
     * similar time as 1.5.5 (where the "shmem" framework *does*
     * exist).  The idea was to have a consistent MCA param name
     * starting with v1.4.5.  Hence, we put a slightly non-sensiscal
     * name here in v1.4.x so that we'd have a correct/good name
     * moving forward.
     */
    mca_base_param_reg_int_name("shmem",
                                "mmap_enable_nfs_warning", 
                                "Enable the warning emitted when Open MPI detects that its shared memory backing file is located on a network filesystem (1 = enabled, 0 = disabled).",
                                false, false,
                                (int)true, &value);
    opal_mmap_on_nfs_warning = OPAL_INT_TO_BOOL(value);

    /* Paffinity base also has some parameters */
    return opal_paffinity_base_register_params();
}
