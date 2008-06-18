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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

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

/* we don't believe we need MPIR_being_debugged here */

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
    int i, debugger, rc;
    char *a, *b, **dirs;
    opal_buffer_t buf;

    /* are we being debugged by a TotalView-like debugger? */
    mca_base_param_reg_int_name("ompi",
                                "mpi_being_debugged",
                                "Whether the MPI application "
                                "is being debugged (default: false)",
                                false, false, (int) false,
                                &debugger);
    
    if (!debugger) {
        /* if not, just return */
        return;
    }
    
    /* if we are being debugged, then we need to find
     * the correct plug-in
     */
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
    
    /* only the rank=0 proc waits for the debugger - everyone else will just
     * spin in the barrier in mpi_init until rank=0 joins them
     */
    if (0 != ORTE_PROC_MY_NAME->vpid) {
        return;
    }
    
    /* we have to support at least two ways of completing the
     * debug attachment - either we will get a message from
     * the HNP telling us it is okay to release, or the debugger
     * itself will reach into us and set a gate.
     *
     * First, attempt to get a message-based release
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &buf, ORTE_RML_TAG_DEBUGGER_RELEASE, 0);
    OBJ_DESTRUCT(&buf);  /* don't care about contents of message */
    
    if (rc > 0) {
        /* message received - we can go! */
        return;
    } else if (ORTE_ERR_NOT_SUPPORTED == rc) {
        /* if the recv isn't supported, then we fall back
         * to the alternative method for waiting
         */
        goto spin_wait;
    } else {
        /* if it failed for some other reason, then we are
         * in trouble - for now, just report the problem
         * and give up waiting
         */
        opal_output(0, "Debugger_attach[rank=%ld]: could not wait for debugger - error %s!",
                    (long)ORTE_PROC_MY_NAME->vpid, ORTE_ERROR_NAME(rc));
        return;
    }
    
spin_wait:
    /* spin until debugger attaches and releases us */
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

