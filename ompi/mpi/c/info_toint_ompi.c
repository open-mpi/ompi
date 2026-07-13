/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/info/info.h"
#include "ompi/errhandler/errhandler.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Info_toint = PMPI_Info_toint
#endif
#define MPI_Info_toint PMPI_Info_toint
#endif

static const char FUNC_NAME[] = "MPI_Info_toint";

int MPI_Info_toint(MPI_Info info)
{
    int o_index;

    if ( MPI_PARAM_CHECK ) {
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_INFO, FUNC_NAME);
        }
    }

    o_index = info->i_f_to_c_index;

    return o_index;

}

#if OMPI_BUILD_MPI_PROFILING && !OPAL_HAVE_WEAK_ALIASES
/*
 * Mach-O cannot express a weak *alias* -- there is no way to mark a ".set"
 * alias as a weak definition -- so where weak aliases are unavailable the
 * public MPI_* symbol is defined here as a weak function that forwards to the
 * strong PMPI_* one.  That is what lets these bindings be compiled exactly
 * once: this translation unit provides both the strong PMPI_* symbol
 * (above) and the weak MPI_* symbol (here).
 */
#undef MPI_Info_toint
__opal_attribute_weak__ int MPI_Info_toint(MPI_Info info)
{
    return PMPI_Info_toint(info);
}
#endif
