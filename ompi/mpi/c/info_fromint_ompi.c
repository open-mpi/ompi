/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2024-2025 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/runtime/params.h"
#include "ompi/info/info.h"
#include "ompi/errhandler/errhandler.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Info_fromint = PMPI_Info_fromint
#endif
#define MPI_Info_fromint PMPI_Info_fromint
#endif

static const char __opal_attribute_unused__ FUNC_NAME[] = "MPI_Info_fromint";

MPI_Info MPI_Info_fromint(int info)
{
    int o_index;

    if (info == 0) {
        return MPI_INFO_NULL;
    }

    if (info == 1) {
        return MPI_INFO_ENV;
    }

    o_index = info;

    return (MPI_Info)opal_pointer_array_get_item(&ompi_info_f_to_c_table, o_index);
}
