/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *   Copyright (C) 2013 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifndef HAVE_MPI_TYPE_SIZE_X
int MPI_Type_size_x(MPI_Datatype datatype, MPI_Count *size)
{
    int size_int, ret;
    ret = MPI_Type_size(datatype, &size_int);
    *size = size_int;
    return ret;
}
#endif

#ifndef HAVE_MPI_STATUS_SET_ELEMENTS_X
int MPI_Status_set_elements_x(MPI_Status *status, MPI_Datatype datatype,
	MPI_Count count)
{
    int count_int = (int) count;
    return MPI_Status_set_elements(status, datatype, count_int);
}
#endif
