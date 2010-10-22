/* -*- C -*-
 *
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/*
 * Debugger support for orterun
 *
 * We interpret the MPICH debugger interface as follows:
 *
 * a) The launcher
 *      - spawns the other processes,
 *      - fills in the table MPIR_proctable, and sets MPIR_proctable_size
 *      - sets MPIR_debug_state to MPIR_DEBUG_SPAWNED ( = 1)
 *      - calls MPIR_Breakpoint() which the debugger will have a
 *	  breakpoint on.
 *
 *  b) Applications start and then spin until MPIR_debug_gate is set
 *     non-zero by the debugger.
 *
 * This file implements (a).
 *
 **************************************************************************
 *
 * Note that we have presently tested both TotalView and DDT parallel
 * debuggers.  They both nominally subscribe to the Etnus attaching
 * interface, but there are differences between the two.
 *
 * TotalView: user launches "totalview mpirun -a ...<mpirun args>...".
 * TV launches mpirun.  mpirun launches the application and then calls
 * MPIR_Breakpoint().  This is the signal to TV that it's a parallel
 * MPI job.  TV then reads the proctable in mpirun and attaches itself
 * to all the processes (it takes care of launching itself on the
 * remote nodes).  Upon attaching to all the MPI processes, the
 * variable MPIR_being_debugged is set to 1.  When it has finished
 * attaching itself to all the MPI processes that it wants to,
 * MPIR_Breakpoint() returns.
 *
 * DDT: user launches "ddt bin -np X <mpi app name>".  DDT fork/exec's
 * mpirun to launch ddt-debugger on the back-end nodes via "mpirun -np
 * X ddt-debugger" (not the lack of other arguments -- we can't pass
 * anything to mpirun).  This app will eventually fork/exec the MPI
 * app.  DDT does not current set MPIR_being_debugged in the MPI app.
 *
 **************************************************************************
 *
 * We support two ways of waiting for attaching debuggers.  The
 * implementation spans this file and ompi/debuggers/ompi_debuggers.c.
 *
 * 1. If using orterun: MPI processes will have the
 * orte_in_parallel_debugger MCA param set to true (because not all
 * debuggers consistently set MPIR_being_debugged in both the launcher
 * and in the MPI procs).  The HNP will call MPIR_Breakpoint() and
 * then RML send a message to VPID 0 (MCW rank 0) when it returns
 * (MPIR_Breakpoint() doesn't return until the debugger has attached
 * to all relevant processes).  Meanwhile, VPID 0 blocks waiting for
 * the RML message.  All other VPIDs immediately call the grpcomm
 * barrier (and therefore block until the debugger attaches).  Once
 * VPID 0 receives the RML message, we know that the debugger has
 * attached to all processes that it cares about, and VPID 0 then
 * joins the grpcomm barrier, allowing the job to continue.  This
 * scheme has the side effect of nicely supporting partial attaches by
 * parallel debuggers (i.e., attaching to only some of the MPI
 * processes; not necessarily all of them).
 *
 * 2. If not using orterun: in this case, ORTE_DISABLE_FULL_SUPPORT
 * will be true, and we know that there will not be an RML message
 * sent to VPID 0.  So we have to look for a magic environment
 * variable from the launcher to know if the jobs will be attached by
 * a debugger (e.g., set by yod, srun, ...etc.), and if so, spin on
 * MPIR_debug_gate.  These environment variable names must be
 * hard-coded in the OMPI layer (see ompi/debuggers/ompi_debuggers.c).
 */

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif  /* HAVE_STRINGS_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/os_path.h"
#include "opal/util/opal_sos.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_getcwd.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"

#include "orte/mca/debugger/base/base.h"
#include "mpir.h"


#include "mpir.h"

/* Static API's */
static int init(void);
static void finalize(void);
static void init_before_spawn(orte_job_t *jdata);

/* Module definition */
orte_debugger_base_module_t orte_debugger_mpir_module = {
    init,
    finalize,
    init_before_spawn,
    orte_debugger_base_init_after_spawn
};

/* local globals */

static int init(void)
{
    return ORTE_SUCCESS;
}

/**
 * Release resources associated with data structures for running under
 * a debugger using the MPICH/TotalView parallel debugger interface.
 */
void finalize(void)
{
    if (MPIR_proctable) {
        free(MPIR_proctable);
        MPIR_proctable = NULL;
    }
}

/**
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface.  Before the
 * spawn we need to check if we are being run under a TotalView-like
 * debugger; if so then inform applications via an MCA parameter.
 */
void init_before_spawn(orte_job_t *jdata)
{
    char *env_name;
    orte_app_context_t *app;
    int i;

    if (!MPIR_being_debugged && !orte_in_parallel_debugger) {
        return;
    }
    
    opal_output_verbose(1, orte_debugger_base.output, "Info: Spawned by a debugger");

    /* tell the procs they are being debugged */
    env_name = mca_base_param_environ_variable("orte", 
                                               "in_parallel_debugger", NULL);
    
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        opal_setenv(env_name, "1", true, &app->env);
    }
    free(env_name);
}
