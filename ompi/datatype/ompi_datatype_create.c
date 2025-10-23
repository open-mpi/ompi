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
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stddef.h>
#include <string.h>

#include "opal/class/opal_pointer_array.h"
#include "opal/util/printf.h"
#include "opal/util/string_copy.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/attribute/attribute.h"

static void __ompi_datatype_allocate( ompi_datatype_t* datatype )
{
    datatype->args               = NULL;
    /* Do not add the newly created datatypes to the f2c translation table. We will add them only
     * if necessary, basically upon the first call the MPI_Datatype_f2c.
     */
    datatype->d_f_to_c_index     = -1;
    datatype->id                 = -1;
    datatype->d_keyhash          = NULL;
    datatype->name[0]            = '\0';
    datatype->packed_description = 0;
    datatype->pml_data           = 0;
}

static void __ompi_datatype_release(ompi_datatype_t * datatype)
{
    if( NULL != datatype->args ) {
        ompi_datatype_release_args( datatype );
        datatype->args = NULL;
    }

    free ((void *) datatype->packed_description );
    datatype->packed_description = 0;

    if( datatype->d_f_to_c_index >= 0 ) {
        opal_pointer_array_set_item( &ompi_datatype_f_to_c_table, datatype->d_f_to_c_index, NULL );
        datatype->d_f_to_c_index = -1;
    }
    /* any pending attributes ? */
    if (NULL != datatype->d_keyhash) {
        ompi_attr_delete_all( TYPE_ATTR, datatype, datatype->d_keyhash );
        OBJ_RELEASE( datatype->d_keyhash );
    }
    /* make sure the name is set to empty */
    datatype->name[0] = '\0';
}

OBJ_CLASS_INSTANCE(ompi_datatype_t, opal_datatype_t, __ompi_datatype_allocate, __ompi_datatype_release);

ompi_datatype_t * ompi_datatype_create( size_t expectedSize )
{
    int ret;
    ompi_datatype_t * datatype = (ompi_datatype_t*)OBJ_NEW(ompi_datatype_t);

    ret = opal_datatype_create_desc( &(datatype->super), expectedSize);
    if (OPAL_SUCCESS != ret)
        return NULL;

    return datatype;
}

int32_t ompi_datatype_destroy( ompi_datatype_t** type)
{
    ompi_datatype_t* pData = *type;

    if( ompi_datatype_is_predefined(pData) && (pData->super.super.obj_reference_count <= 1) )
        return OMPI_ERROR;

    OBJ_RELEASE(pData);
    *type = NULL;
    return OMPI_SUCCESS;
}

int32_t
ompi_datatype_duplicate( const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t * new_ompi_datatype = ompi_datatype_create( oldType->super.desc.used + 2 );

    *newType = new_ompi_datatype;
    if( NULL == new_ompi_datatype ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    opal_datatype_clone( &oldType->super, &new_ompi_datatype->super);
    /* Strip the predefined flag at the OMPI level. */
    new_ompi_datatype->super.flags &= ~OMPI_DATATYPE_FLAG_PREDEFINED;
    /* By default maintain the relationships related to the old data (such as ops) */
    new_ompi_datatype->id = oldType->id;

    /* Set the keyhash to NULL -- copying attributes is *only* done at
       the top level (specifically, MPI_TYPE_DUP). */
    new_ompi_datatype->d_keyhash = NULL;
    new_ompi_datatype->args = NULL;

    char *new_name;
    opal_asprintf(&new_name, "Dup %s", oldType->name);
    opal_string_copy(new_ompi_datatype->name, new_name, MPI_MAX_OBJECT_NAME);
    new_ompi_datatype->name[MPI_MAX_OBJECT_NAME - 1] = '\0';
    free(new_name);

    return OMPI_SUCCESS;
}

/*
 * Note this is not a complete implementation for the MPI_Type_get_value_index function described in
 * MPI 4.1 and newer as it doesn't support possible unnamed datatypes returned for other value_type/index_type
 * pairs.
 */
int
ompi_datatype_get_value_index(const ompi_datatype_t *value_type, const ompi_datatype_t *index_type, ompi_datatype_t **pair_type)
{
    *pair_type = (ompi_datatype_t *)&ompi_mpi_datatype_null;
    bool is_fortran = ((index_type->super.flags & OMPI_DATATYPE_FLAG_DATA_FORTRAN) == OMPI_DATATYPE_FLAG_DATA_FORTRAN) ? true : false;

    if (false == is_fortran) {
        if (index_type->id == OMPI_DATATYPE_MPI_INT) {
            if (value_type->id == OMPI_DATATYPE_MPI_FLOAT) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_float_int;
            } else if (value_type->id == OMPI_DATATYPE_MPI_DOUBLE) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_double_int;
            } else if (value_type->id == OMPI_DATATYPE_MPI_LONG) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_long_int;
            } else if (value_type->id == OMPI_DATATYPE_MPI_SHORT) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_short_int;
            } else if (value_type->id == OMPI_DATATYPE_MPI_INT) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_2int;
            } else if (value_type->id == OMPI_DATATYPE_MPI_LONG_DOUBLE) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_longdbl_int;
            }
        }
    /* Fortran predefined data types */
    } else {
        if ((index_type->id == OMPI_DATATYPE_MPI_INTEGER) &&
            (value_type->id == OMPI_DATATYPE_MPI_INTEGER)) {
               *pair_type = (ompi_datatype_t *)&ompi_mpi_2integer;
        } else if ((index_type->id == OMPI_DATATYPE_MPI_FLOAT) &&
            (value_type->id == OMPI_DATATYPE_MPI_FLOAT)) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_2real;
        } else if ((index_type->id == OMPI_DATATYPE_MPI_DOUBLE) &&
            (value_type->id == OMPI_DATATYPE_MPI_DOUBLE)) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_2dblprec;
#if OMPI_HAVE_FORTRAN_COMPLEX
        } else if ((index_type->id == OMPI_DATATYPE_MPI_COMPLEX) &&
            (value_type->id == OMPI_DATATYPE_MPI_COMPLEX)) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_2cplex;
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX
        } else if ((index_type->id == OMPI_DATATYPE_MPI_DOUBLE_COMPLEX) &&
            (value_type->id == OMPI_DATATYPE_MPI_DOUBLE_COMPLEX)) {
                *pair_type = (ompi_datatype_t *)&ompi_mpi_2dblcplex;
#endif
        }
    }

    return OMPI_SUCCESS;
}
