/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/util/arch.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"

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
 * 15 exponent bits (bias = 10383), 112 mantissa bits and the same encoding as double. All
 * integers are in two's complement big-endian format.
 *
 * All data are byte aligned, regardless of type. That's exactly what we expect as we can
 * consider the data stored in external32 as being packed.
 */

uint32_t ompi_datatype_external32_arch_id = OPAL_ARCH_LDEXPSIZEIS15 | OPAL_ARCH_LDMANTDIGIS113 |
                                            OPAL_ARCH_LONGDOUBLEIS128 | OPAL_ARCH_ISBIGENDIAN |
                                            OPAL_ARCH_HEADERMASK | OPAL_ARCH_HEADERMASK2 |
                                            OPAL_ARCH_BOOLIS8 | OPAL_ARCH_LOGICALIS8;

opal_convertor_t* ompi_mpi_external32_convertor = NULL;
opal_convertor_t* ompi_mpi_local_convertor = NULL;

static void set_external32_sizes(opal_convertor_t *convertor);

int32_t ompi_datatype_default_convertors_init( void )
{
   /* create the extern32 convertor */
    ompi_mpi_external32_convertor = opal_convertor_create( ompi_datatype_external32_arch_id, 0 );
    set_external32_sizes(ompi_mpi_external32_convertor);

    /* create the local convertor */
    ompi_mpi_local_convertor = opal_convertor_create( opal_local_arch, 0 );

    return OMPI_SUCCESS;
}


int32_t ompi_datatype_default_convertors_fini( void )
{
    OBJ_RELEASE( ompi_mpi_external32_convertor );
    OBJ_RELEASE( ompi_mpi_local_convertor );

    return OMPI_SUCCESS;
}

static void
set_external32_sizes(opal_convertor_t *convertor)
{
    int i;

    /* We have to give every predefined datatype a size, initialize as */
    /* the default sizes, then change whatever is specified in the standard */
    for (i=OMPI_DATATYPE_MPI_EMPTY+1; i<OMPI_DATATYPE_MPI_MAX_PREDEFINED; ++i) {
        if (i != OMPI_DATATYPE_MPI_LB && i != OMPI_DATATYPE_MPI_UB) {
            opal_convertor_set_ompi_remote_size(convertor,
                i, ompi_datatype_basicDatatypes[i]->super.size);
        }
    }

    opal_convertor_set_ompi_remote_size(convertor, MPI_PACKED->super.ompi_id, 1);
    opal_convertor_set_ompi_remote_size(convertor, MPI_BYTE->super.ompi_id, 1);
    opal_convertor_set_ompi_remote_size(convertor, MPI_CHAR->super.ompi_id, 1);
    opal_convertor_set_ompi_remote_size(convertor, MPI_UNSIGNED_CHAR->super.ompi_id, 1);
    opal_convertor_set_ompi_remote_size(convertor, MPI_SIGNED_CHAR->super.ompi_id, 1);
    opal_convertor_set_ompi_remote_size(convertor, MPI_WCHAR->super.ompi_id, 2);
    opal_convertor_set_ompi_remote_size(convertor, MPI_SHORT->super.ompi_id, 2);
    opal_convertor_set_ompi_remote_size(convertor, MPI_UNSIGNED_SHORT->super.ompi_id, 2);
    opal_convertor_set_ompi_remote_size(convertor, MPI_INT->super.ompi_id, 4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_UNSIGNED->super.ompi_id, 4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_LONG->super.ompi_id, 4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_UNSIGNED_LONG->super.ompi_id, 4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_LONG_LONG_INT->super.ompi_id, 8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_UNSIGNED_LONG_LONG->super.ompi_id, 8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_FLOAT->super.ompi_id, 4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_DOUBLE->super.ompi_id, 8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_LONG_DOUBLE->super.ompi_id, 16);

    opal_convertor_set_ompi_remote_size(convertor, MPI_C_BOOL->super.ompi_id, 1);
    opal_convertor_set_ompi_remote_size(convertor, MPI_AINT->super.ompi_id, 8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_COUNT->super.ompi_id, 8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_OFFSET->super.ompi_id, 8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_C_COMPLEX->super.ompi_id, 2*4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_C_FLOAT_COMPLEX->super.ompi_id, 2*4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_C_DOUBLE_COMPLEX->super.ompi_id, 2*8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_C_LONG_DOUBLE_COMPLEX->super.ompi_id, 2*16);

    opal_convertor_set_ompi_remote_size(convertor, MPI_CHARACTER->super.ompi_id, 1);
    opal_convertor_set_ompi_remote_size(convertor, MPI_LOGICAL->super.ompi_id, 4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_INTEGER->super.ompi_id, 4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_REAL->super.ompi_id, 4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_DOUBLE_PRECISION->super.ompi_id, 8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_COMPLEX->super.ompi_id, 2*4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_DOUBLE_COMPLEX->super.ompi_id, 2*8);

    opal_convertor_set_ompi_remote_size(convertor, MPI_CXX_BOOL->super.ompi_id, 1);
    opal_convertor_set_ompi_remote_size(convertor, MPI_CXX_FLOAT_COMPLEX->super.ompi_id, 2*4);
    opal_convertor_set_ompi_remote_size(convertor, MPI_CXX_DOUBLE_COMPLEX->super.ompi_id, 2*8);
    opal_convertor_set_ompi_remote_size(convertor, MPI_CXX_LONG_DOUBLE_COMPLEX->super.ompi_id, 2*16);

    opal_convertor_ompi_remote_size_is_ready(convertor);
}
