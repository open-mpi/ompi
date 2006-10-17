/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * MPI portion of debugger support: TotalView
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/mca/base/base.h"
#include "debuggers.h"

OMPI_DECLSPEC int MPIR_being_debugged = 0;
OMPI_DECLSPEC volatile int MPIR_debug_gate = 0;
OMPI_DECLSPEC volatile int MPIR_debug_state = 0;
#if defined(OMPI_TV_DLL)
OMPI_DECLSPEC char MPIR_dll_name[] = OMPI_TV_DLL;
#endif  /* defined(OMPI_TV_DLL) */

/**
 * There is an issue with the debugger running on different architectures
 * compared with the debugged program. We need to know the sizes of the types
 * on the debugged program. The problem is that the size depend on the
 * compilation options (32 or 64 bits) and on the compiler. Therefore,
 * the simplest and more accurate way is to export these sizes directly from
 * the debugged program.
 */
OMPI_DECLSPEC int MPIR_debug_typedefs_sizeof[] = {
    sizeof(bool),
    sizeof(short),
    sizeof(int),
    sizeof(long),
    sizeof(long long),
    sizeof(size_t)
};
    
struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};
OMPI_DECLSPEC struct MPIR_PROCDESC *MPIR_proctable = NULL;
OMPI_DECLSPEC int MPIR_proctable_size = 0;

/**
 * Wait for a TotalView-like debugger if asked.
 */
void ompi_wait_for_totalview(void)
{
    int wait_for_totalview;

    /* Do we need to wait for a TotalView-like debugger? */
    mca_base_param_reg_int_name("orte",
                                "mpi_wait_for_totalview",
                                "Whether the MPI application "
                                "should wait for a debugger or not",
                                false, false, (int) false,
                                &wait_for_totalview);
    if (wait_for_totalview) {
        while (MPIR_debug_gate == 0) {
#if defined(__WINDOWS__)
            Sleep(100);     /* milliseconds */
#else
            usleep(100000); /* microseconds */
#endif
        }
    }
}
