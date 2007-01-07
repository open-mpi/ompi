/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/datatype/dt_arch.h"

/* From the MPI standard. external32 use the following types:
 *   Type Length
 * MPI_PACKED               1
 * MPI_BYTE                 1
 * MPI_CHAR                 1
 * MPI_UNSIGNED_CHAR        1
 * MPI_SIGNED_CHAR          1
 * MPI_WCHAR                2
 * MPI_SHORT                2
 * MPI_UNSIGNED_SHORT       2
 * MPI_INT                  4
 * MPI_UNSIGNED             4
 * MPI_LONG                 4
 * MPI_UNSIGNED_LONG        4
 * MPI_FLOAT                4
 * MPI_DOUBLE               8
 * MPI_LONG_DOUBLE         16
 * Fortran types
 * MPI_CHARACTER            1
 * MPI_LOGICAL              4
 * MPI_INTEGER              4
 * MPI_REAL                 4
 * MPI_DOUBLE_PRECISION     8
 * MPI_COMPLEX              2*4
 * MPI_DOUBLE_COMPLEX       2*8
 * Optional types
 * MPI_INTEGER1             1
 * MPI_INTEGER2             2
 * MPI_INTEGER4             4
 * MPI_INTEGER8             8
 * MPI_LONG_LONG_INT        8
 * MPI_UNSIGNED_LONG_LONG   8
 * MPI_REAL4                4
 * MPI_REAL8                8
 * MPI_REAL16              16
 *
 * All floating point values are in big-endian IEEE format. Double extended use 16 bytes, with
 * 15 exponent bits (bias = 10383), 112 mantissa bits and the same encoding as double.All
 * integers are in two's complement big-endian format.
 * 
 * All data are byte aligned, regardless of type. That's exactly what we expect as we can
 * consider the data stored in external32 as being packed.
 */

uint32_t ompi_ddt_external32_arch_id = OMPI_ARCH_LDEXPSIZEIS15 | OMPI_ARCH_LDMANTDIGIS113 |
                                       OMPI_ARCH_LONGDOUBLEIS128 | OMPI_ARCH_ISBIGENDIAN |
                                       OMPI_ARCH_HEADERMASK | OMPI_ARCH_HEADERMASK2 |
                                       OMPI_ARCH_BOOLIS8 | OMPI_ARCH_LOGICALIS8;

ompi_convertor_t* ompi_mpi_external32_convertor = NULL;
ompi_convertor_t* ompi_mpi_local_convertor = NULL;
uint32_t          ompi_mpi_local_arch = 0xFFFFFFFF;

int32_t ompi_ddt_default_convertors_init( void )
{ 
    ompi_arch_compute_local_id( &ompi_mpi_local_arch );

   /* create the extern32 convertor */
    ompi_mpi_external32_convertor = ompi_convertor_create( ompi_ddt_external32_arch_id, 0 );

    /* create the local convertor */
    ompi_mpi_local_convertor = ompi_convertor_create( ompi_mpi_local_arch, 0 );

    return OMPI_SUCCESS;
}

int32_t ompi_ddt_default_convertors_fini( void )
{
    OBJ_RELEASE( ompi_mpi_external32_convertor );
    OBJ_RELEASE( ompi_mpi_local_convertor );

    return OMPI_SUCCESS;
}
