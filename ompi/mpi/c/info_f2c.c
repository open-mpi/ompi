/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"
#include "ompi/info/info.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Info_f2c = PMPI_Info_f2c
#endif
#define MPI_Info_f2c PMPI_Info_f2c
#endif

static const char FUNC_NAME[] __opal_attribute_unused__ = "MPI_Info_f2c";


/**
 * Converts the MPI_Fint info into a valid C MPI_Info handle
 *
 * @param info Integer handle to an MPI_INFO object
 * @retval C handle corresponding to MPI_INFO object
 */
MPI_Info MPI_Info_f2c(MPI_Fint info)
{
    int info_index = OMPI_FINT_2_INT(info);

    /* Per MPI-2:4.12.4, do not invoke an error handler if we get an
       invalid fortran handle.  If we get an invalid fortran handle,
       return an invalid C handle. */
    /*
     * Deal with special pre-defined cases for MPI 4.0
     */

    if (info_index == 0) {
        return MPI_INFO_NULL;
    }

    if (info_index == 1) {
        return MPI_INFO_ENV;
    }

    /* 
     * if the application has not created an info object yet
     * then the size of the ompi_info_f_to_c_table is zero
     * so this check can be done even if an info object has not
     * previously been created.
     */

    if (info_index < 0 ||
        info_index >=
        opal_pointer_array_get_size(&ompi_info_f_to_c_table)) {
        return NULL;
    }

    /*
     * if we get here, then the info support infrastructure has been initialized
     * either via a prior call to MPI_Info_create or one of the MPI initialization
     * methods.
     */
    return (MPI_Info)opal_pointer_array_get_item(&ompi_info_f_to_c_table, info_index);
}
