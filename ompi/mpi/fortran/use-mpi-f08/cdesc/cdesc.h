/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2014      Argonne National Laboratory.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <stdlib.h>
#include <ISO_Fortran_binding.h>
#include <mpi.h>

extern int ompi_cdesc_create_datatype(CFI_cdesc_t *cdesc, int oldcount, MPI_Datatype oldtype, MPI_Datatype *newtype);
