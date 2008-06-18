/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights resereved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * MPI portion of debugger support: initially based on the
 * TotalView/Etnus API for debuggers to attach to MPI jobs.
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/mca/installdirs/installdirs.h"
#include "debuggers.h"
/**
 * A lot of include files that are required by al optimized builds in order
 * to get access to the type information. Beware, this file have to always
 * be compiled with the -g flag, otherwise the type information will be
 * missing and the parallel debuggers will be unable to initialize the
 * Open MPI debug library.
 */
#include "opal/class/opal_list.h"

#include "ompi/class/ompi_free_list.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"
#include "opal/class/opal_pointer_array.h"
#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/datatype/datatype.h"
#include "ompi/include/mpi.h"

#if defined(OMPI_MSGQ_DLL)
/* This variable is old/deprecated -- the mpimsgq_dll_locations[]
   method is preferred because it's more flexible */
OMPI_DECLSPEC char MPIR_dll_name[] = OMPI_MSGQ_DLL;
#endif  /* defined(OMPI_MSGQ_DLL) */
OMPI_DECLSPEC char **mpidbg_dll_locations = NULL;
OMPI_DECLSPEC char **mpimsgq_dll_locations = NULL;

OMPI_DECLSPEC int MPIR_debug_typedefs_sizeof[] = {
    sizeof(short),
    sizeof(int),
    sizeof(long),
    sizeof(long long),
    sizeof(void*),
    sizeof(bool),
    sizeof(size_t)
};
    
/**
 * There is an issue with the debugger running on different architectures
 * compared with the debugged program. We need to know the sizes of the types
 * on the debugged program. The problem is that the size depend on the
 * compilation options (32 or 64 bits) and on the compiler. Therefore,
 * the simplest and more accurate way is to export these sizes directly from
 * the debugged program.
 */
OMPI_DECLSPEC opal_list_item_t* opal_list_item_t_type_inclusion = NULL;
OMPI_DECLSPEC opal_list_t* opal_list_t_type_inclusion = NULL;
OMPI_DECLSPEC ompi_free_list_item_t* ompi_free_list_item_t_type_inclusion = NULL;
OMPI_DECLSPEC ompi_free_list_t* ompi_free_list_t_type_inclusion = NULL;
OMPI_DECLSPEC ompi_request_t* ompi_request_t_type_inclusion = NULL;
OMPI_DECLSPEC mca_pml_base_request_t* mca_pml_base_request_t_type_inclusion = NULL;
OMPI_DECLSPEC mca_pml_base_send_request_t* mca_pml_base_send_request_t_type_inclusion = NULL;
OMPI_DECLSPEC mca_pml_base_recv_request_t* mca_pml_base_recv_request_t_type_inclusion = NULL;
OMPI_DECLSPEC opal_pointer_array_t* opal_pointer_array_t_type_inclusion = NULL;
OMPI_DECLSPEC ompi_communicator_t* ompi_communicator_t_type_inclusion = NULL;
OMPI_DECLSPEC ompi_group_t* ompi_group_t_type_inclusion = NULL;
OMPI_DECLSPEC ompi_status_public_t* ompi_status_public_t_type_inclusion = NULL;
OMPI_DECLSPEC ompi_datatype_t* ompi_datatype_t_type_inclusion = NULL;

OMPI_DECLSPEC volatile int MPIR_debug_gate=0;

/* Check for a file in few dirrect ways for portability */
static void check(char *dir, char *file, char **locations) 
{
    char *str;

    asprintf(&str, "%s/%s.so", dir, file);
    
#if defined(HAVE_SYS_STAT_H)
    {
        struct stat buf;
        
        /* Use stat() */
        if (0 == stat(str, &buf)) {
            opal_argv_append_nosize(&locations, file);
        }
    }
#else
    {
        FILE *fp;
        
        /* Just try to open the file */
        if (NULL != (fp = fopen(str, "r"))) {
            fclose(fp);
            opal_argv_append_nosize(&locations, file);
        }
    }
#endif /* defined(HAVE_SYS_STAT_H) */

    free(str);
}


/**
 * Wait for a debugger if asked.
 */
void ompi_wait_for_debugger(void)
{
    int i, wait_for_debugger, wait_for_tv;
    char *a, *b, **dirs;

    /* Do we need to wait for a TotalView-like debugger? */
    mca_base_param_reg_int_name("ompi",
                                "mpi_wait_for_debugger",
                                "Whether the MPI application "
                                "should wait for a debugger or not",
                                false, false, (int) false,
                                &wait_for_debugger);
    mca_base_param_reg_int_name("ompi",
                                "mpi_wait_for_totalview",
                                "Deprecated synonym for mpi_wait_for_debugger",
                                false, false, (int) false,
                                &wait_for_tv);
    wait_for_debugger |= wait_for_tv;

    a = strdup(opal_install_dirs.pkglibdir);
    mca_base_param_reg_string_name("ompi",
                                   "debugger_dll_path",
                                   "List of directories where MPI_INIT should search for debugger plugins",
                                   false, false, a, &b);
    free(a);

    /* Search the directory for MPI debugger DLLs */
    if (NULL != b) {
        dirs = opal_argv_split(b, ':');
        for (i = 0; dirs[i] != NULL; ++i) {
            check(dirs[i], OMPI_MPIHANDLES_DLL_PREFIX, mpidbg_dll_locations);
            check(dirs[i], OMPI_MSGQ_DLL_PREFIX, mpimsgq_dll_locations);
        }
    }

    /* If we're waiting for the debugger, then, well, wait for it.  :-) */
    if (wait_for_debugger) {
        /* RHC: the following is a temporary hack until we figure
         * out how to resolve the problem of where to
         * instance the MPIR* variables so that multiple
         * launchers can access them
         */
        while (MPIR_debug_gate == 0) {
#if defined(__WINDOWS__)
            Sleep(100);     /* milliseconds */
#elif defined(HAVE_USLEEP)
            usleep(100000); /* microseconds */
#else
            sleep(1);       /* seconds */
#endif
        }
    }
}

