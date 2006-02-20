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

#include "mpi.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/proc/proc.h"

static inline int
__ompi_ddt_pack_description( ompi_datatype_t* datatype,
                             void** packed_buffer, int* next_index );
static ompi_datatype_t*
__ompi_ddt_create_from_args( int32_t* i, MPI_Aint* a,
                             MPI_Datatype* d, int32_t type );

typedef struct __dt_args {
    int           create_type;
    size_t        total_pack_size;
    int           ci;
    int           ca;
    int           cd;
    int*          i;
    MPI_Aint*     a;
    MPI_Datatype* d;
} ompi_ddt_args_t;

#define ALLOC_ARGS(PDATA, IC, AC, DC)					\
    do {                                                                \
        int length = sizeof(ompi_ddt_args_t) + (IC) * sizeof(int) +     \
            (AC) * sizeof(MPI_Aint) + (DC) * sizeof(MPI_Datatype);      \
        char* buf = (char*)malloc( length );				\
        ompi_ddt_args_t* pArgs = (ompi_ddt_args_t*)buf;			\
        pArgs->ci = (IC);                                               \
        pArgs->ca = (AC);                                               \
        pArgs->cd = (DC);                                               \
        buf += sizeof(ompi_ddt_args_t);					\
        if( pArgs->ci == 0 ) pArgs->i = NULL;				\
        else {								\
            pArgs->i = (int*)buf;                                       \
            buf += pArgs->ci * sizeof(int);                             \
        }                                                               \
        if( pArgs->ca == 0 ) pArgs->a = NULL;				\
        else {								\
            pArgs->a = (MPI_Aint*)buf;					\
            buf += pArgs->ca * sizeof(MPI_Aint);                        \
        }                                                               \
        if( pArgs->cd == 0 ) pArgs->d = NULL;				\
        else pArgs->d = (MPI_Datatype*)buf;                             \
        (PDATA)->args = (void*)pArgs;					\
        pArgs->total_pack_size = (4 + (IC)) * sizeof(int) +             \
            (AC) * sizeof(MPI_Aint) + (DC) * sizeof(int);               \
        (PDATA)->packed_description = NULL;                             \
    } while(0)

int32_t ompi_ddt_set_args( ompi_datatype_t* pData,
                           int32_t ci, int32_t** i,
                           int32_t ca, MPI_Aint* a,
                           int32_t cd, MPI_Datatype* d, int32_t type)
{
    int pos;
    ompi_ddt_args_t* pArgs;

    assert( NULL == pData->args );
    ALLOC_ARGS( pData, ci, ca, cd );

    pArgs = (ompi_ddt_args_t*)pData->args;
    pArgs->create_type = type;

    switch(type){
        /******************************************************************/
    case MPI_COMBINER_DUP:
        break;
        /******************************************************************/
    case MPI_COMBINER_CONTIGUOUS:
        pArgs->i[0] = i[0][0];
        break;
        /******************************************************************/
    case MPI_COMBINER_VECTOR:
        pArgs->i[0] = i[0][0];
        pArgs->i[1] = i[1][0];
        pArgs->i[2] = i[2][0];
        break;
        /******************************************************************/
    case MPI_COMBINER_HVECTOR_INTEGER:
    case MPI_COMBINER_HVECTOR:
        pArgs->i[0] = i[0][0];
        pArgs->i[1] = i[1][0];
        break;
        /******************************************************************/
    case MPI_COMBINER_INDEXED:
        pos = 1;
        pArgs->i[0] = i[0][0];
        memcpy( pArgs->i + pos, i[1], i[0][0] * sizeof(int) );
        pos += i[0][0];
        memcpy( pArgs->i + pos, i[2], i[0][0] * sizeof(int) );
        break;
        /******************************************************************/
    case MPI_COMBINER_HINDEXED_INTEGER:
    case MPI_COMBINER_HINDEXED:
        pArgs->i[0] = i[0][0];
        memcpy( pArgs->i + 1, i[1], i[0][0] * sizeof(int) );
        break;
        /******************************************************************/
    case MPI_COMBINER_INDEXED_BLOCK:
        pArgs->i[0] = i[0][0];
        pArgs->i[1] = i[1][0];
        memcpy( pArgs->i + 2, i[2], i[0][0] * sizeof(int) );
        break;
        /******************************************************************/
    case MPI_COMBINER_STRUCT_INTEGER:
    case MPI_COMBINER_STRUCT:
        pArgs->i[0] = i[0][0];
        memcpy( pArgs->i + 1, i[1], i[0][0] * sizeof(int) );
        break;
        /******************************************************************/
    case MPI_COMBINER_SUBARRAY:
        pos = 1;
        pArgs->i[0] = i[0][0];
        memcpy( pArgs->i + pos, i[1], pArgs->i[0] * sizeof(int) );
        pos += pArgs->i[0];
        memcpy( pArgs->i + pos, i[2], pArgs->i[0] * sizeof(int) );
        pos += pArgs->i[0];
        memcpy( pArgs->i + pos, i[3], pArgs->i[0] * sizeof(int) );
        pos += pArgs->i[0];
        pArgs->i[pos] = i[4][0];
        break;
        /******************************************************************/
    case MPI_COMBINER_DARRAY:
        pos = 3;
        pArgs->i[0] = i[0][0];
        pArgs->i[1] = i[1][0];
        pArgs->i[2] = i[2][0];

        memcpy( pArgs->i + pos, i[3], i[2][0] * sizeof(int) );
        pos += i[2][0];
        memcpy( pArgs->i + pos, i[4], i[2][0] * sizeof(int) );
        pos += i[2][0];
        memcpy( pArgs->i + pos, i[5], i[2][0] * sizeof(int) );
        pos += i[2][0];
        memcpy( pArgs->i + pos, i[6], i[2][0] * sizeof(int) );
        pos += i[2][0];
        pArgs->i[pos] = i[7][0];
        break;
        /******************************************************************/
    case MPI_COMBINER_F90_REAL:
    case MPI_COMBINER_F90_COMPLEX:
        pArgs->i[0] = i[0][0];
        pArgs->i[1] = i[1][0];
        break;
        /******************************************************************/
    case MPI_COMBINER_F90_INTEGER:
        pArgs->i[0] = i[0][0];
        break;
        /******************************************************************/
    case MPI_COMBINER_RESIZED:
        break;
        /******************************************************************/
    default:
        break;
    }

    /* copy the array of MPI_Aint */
    if( pArgs->a != NULL )
        memcpy( pArgs->a, a, ca * sizeof(MPI_Aint) );

    for( pos = 0; pos < cd; pos++ ) {
        pArgs->d[pos] = d[pos];
        if( !(d[pos]->flags & DT_FLAG_PREDEFINED) ) {
            /* We handle a user defined datatype. We should make sure that the
             * user will not have the oportunity to destroy it before all derived
             * datatypes are destroyed. As we keep pointers to every datatype
             * (for MPI_Type_get_content and MPI_Type_get_envelope) we have to make
             * sure that those datatype will be available if the user ask for them.
             * However, there is no easy way to free them in this case ...
             */
            OBJ_RETAIN( d[pos] );
            pArgs->total_pack_size += ((ompi_ddt_args_t*)d[pos]->args)->total_pack_size;
        }
    }
    return MPI_SUCCESS;
}

int32_t ompi_ddt_print_args( const ompi_datatype_t* pData )
{
    int32_t i;
    ompi_ddt_args_t* pArgs = pData->args;

    if( pData->flags & DT_FLAG_PREDEFINED ) {
        /* nothing to do for predefined data-types */
        return(MPI_SUCCESS);
    }

    if( pArgs == NULL ) return MPI_ERR_INTERN;

    printf( "type %d count ints %d count disp %d count datatype %d\n",
            pArgs->create_type, pArgs->ci, pArgs->ca, pArgs->cd );
    if( pArgs->i != NULL ) {
        for( i = 0; i < pArgs->ci; i++ ) {
            printf( "%d ", pArgs->i[i] );
        }
        printf( "\n" );
    }
    if( pArgs->a != NULL ) {
        for( i = 0; i < pArgs->ca; i++ ) {
            printf( "%ld ", (long)pArgs->a[i] );
        }
        printf( "\n" );
    }
    if( pArgs->d != NULL ) {
        for( i = 0; i < pArgs->cd; i++ ) {
            ompi_datatype_t* temp = pArgs->d[i];
            if( temp->flags & DT_FLAG_PREDEFINED ) {
                printf( "%s ", temp->name );
            } else {
                printf( "%p ", (void*)temp );
            }
        }
        printf( "\n" );
    }
    return MPI_SUCCESS;
}

int32_t ompi_ddt_get_args( const ompi_datatype_t* pData, int32_t which,
                           int32_t* ci, int32_t* i,
                           int32_t* ca, MPI_Aint* a,
                           int32_t* cd, MPI_Datatype* d, int32_t* type)
{
    ompi_ddt_args_t* pArgs = pData->args;

    if( pData->flags & DT_FLAG_PREDEFINED ) {
        switch(which){
        case 0:
            *ci = 0;
            *ca = 0;
            *cd = 0;
            *type = MPI_COMBINER_NAMED;
            break;
        default:
            return MPI_ERR_INTERN;
        }
        return(MPI_SUCCESS);
    }

    if( pArgs == NULL ) return MPI_ERR_INTERN;

    switch(which){
    case 0:     /* GET THE LENGTHS */
        *ci = pArgs->ci;
        *ca = pArgs->ca;
        *cd = pArgs->cd;
        *type = pArgs->create_type;
        break;
    case 1:     /* GET THE ARGUMENTS */
        if(*ci < pArgs->ci || *ca < pArgs->ca || *cd < pArgs->cd)
            return MPI_ERR_ARG;
        if( pArgs->i != NULL )
            memcpy( i, pArgs->i, pArgs->ci * sizeof(int) );
        if( pArgs->a != NULL )
            memcpy( a, pArgs->a, pArgs->ca * sizeof(MPI_Aint) );
        if( pArgs->d != NULL )
            memcpy( d, pArgs->d, pArgs->cd * sizeof(MPI_Datatype) );
        break;
    default:
        return MPI_ERR_INTERN;
    }
    return MPI_SUCCESS;
}

/* In the dt_add function we increase the reference count for all datatypes
 * (except for the predefined ones) that get added to another datatype. This
 * insure that they cannot get released until all the references to them
 * get removed.
 */
int32_t ompi_ddt_release_args( ompi_datatype_t* pData )
{
    int i;
    ompi_ddt_args_t* pArgs = pData->args;

    for( i = 0; i < pArgs->cd; i++ ) {
        if( !(pArgs->d[i]->flags & DT_FLAG_PREDEFINED) ) {
            OBJ_RELEASE( pArgs->d[i] );
        }
    }
    free( pData->args );
    pData->args = NULL;

    return OMPI_SUCCESS;
}

size_t ompi_ddt_pack_description_length( ompi_datatype_t* datatype )
{
    if( datatype->flags & DT_FLAG_PREDEFINED ) {
        return sizeof(int) * 2;
    }
    return ((ompi_ddt_args_t*)datatype->args)->total_pack_size;
}

static inline int __ompi_ddt_pack_description( ompi_datatype_t* datatype,
                                               void** packed_buffer, int* next_index )
{
    int* position = (int*)*packed_buffer;
    int local_index = 0, i;
    ompi_ddt_args_t* args = (ompi_ddt_args_t*)datatype->args;
    char* next_packed = (char*)*packed_buffer;

    if( datatype->flags & DT_FLAG_PREDEFINED ) {
        position[0] = MPI_COMBINER_DUP;
        position[1] = datatype->id;
        return OMPI_SUCCESS;
    }
    position[local_index++] = args->create_type;
    position[local_index++] = args->ci;
    position[local_index++] = args->ca;
    position[local_index++] = args->cd;
    memcpy( &(position[local_index]), args->i, sizeof(int) * args->ci );
    next_packed += ( 4 + args->ci) * sizeof(int);
    local_index += args->ci;
    if( 0 < args->ca ) {
        memcpy( &(position[local_index]), args->a, sizeof(MPI_Aint) * args->ca );
        next_packed += sizeof(MPI_Aint) * args->ca;
    }
    position = (int*)next_packed;
    next_packed += sizeof(int) * args->cd;
    for( i = 0; i < args->cd; i++ ) {
        ompi_datatype_t* temp_data = args->d[i];
        if( temp_data->flags & DT_FLAG_PREDEFINED ) {
            position[i] = temp_data->id;
        } else {
            position[i] = *next_index;
            (*next_index)++;
            __ompi_ddt_pack_description( temp_data,
                                         (void**)&next_packed,
                                         next_index );
        }
    }
    *packed_buffer = next_packed;
    return OMPI_SUCCESS;
}
                               
int ompi_ddt_get_pack_description( ompi_datatype_t* datatype,
                                   const void** packed_buffer )
{
    ompi_ddt_args_t* args = datatype->args;
    int next_index = DT_MAX_PREDEFINED;
    void* recursive_buffer;

    if( NULL == datatype->packed_description ) {
        if( datatype->flags & DT_FLAG_PREDEFINED ) {
            datatype->packed_description = malloc( 2 * sizeof(int) );
        } else {
            datatype->packed_description = malloc( args->total_pack_size );
        }
        recursive_buffer = datatype->packed_description;
        __ompi_ddt_pack_description( datatype, &recursive_buffer, &next_index );
    }
    *packed_buffer = (const void*)datatype->packed_description;
    return OMPI_SUCCESS;
}

ompi_datatype_t*
ompi_ddt_create_from_packed_description( void** packed_buffer,
                                         struct ompi_proc_t* remote_processor )
{
    int* position = (int*)*packed_buffer;
    ompi_datatype_t* datatype = NULL;
    ompi_datatype_t** array_of_datatype;
    MPI_Aint* array_of_disp;
    int* array_of_length;
    int number_of_length, number_of_disp, number_of_datatype;
    int create_type, i;
    char* next_buffer = (char*)*packed_buffer;

    create_type = position[0];
    if( MPI_COMBINER_DUP == create_type ) {
        /* there we have a simple predefined datatype */
        assert( position[1] < DT_MAX_PREDEFINED );
        *packed_buffer = position + 2;
        return (ompi_datatype_t*)ompi_ddt_basicDatatypes[position[1]];
    }
    number_of_length   = position[1];
    number_of_disp     = position[2];
    number_of_datatype = position[3];
    array_of_datatype = (ompi_datatype_t**)malloc( sizeof(ompi_datatype_t*) *
                                                   number_of_datatype );
    array_of_length    = &(position[4]);
    next_buffer += (4 + number_of_length) * sizeof(int);
    array_of_disp      = (MPI_Aint*)next_buffer;
    next_buffer += number_of_disp * sizeof(MPI_Aint);
    position = (int*)next_buffer;
    next_buffer += number_of_datatype * sizeof(int);
    for( i = 0; i < number_of_datatype; i++ ) {
        if( position[i] < DT_MAX_PREDEFINED ) {
            assert( position[1] < DT_MAX_PREDEFINED );
            array_of_datatype[i] = (ompi_datatype_t*)ompi_ddt_basicDatatypes[position[i]];
        } else {
            array_of_datatype[i] =
                ompi_ddt_create_from_packed_description( (void**)&next_buffer,
                                                         remote_processor );
            if( NULL == array_of_datatype[i] )
                goto cleanup_and_exit;
        }
    }
    datatype = __ompi_ddt_create_from_args( array_of_length, array_of_disp,
                                            array_of_datatype, create_type );
    *packed_buffer = next_buffer;
 cleanup_and_exit:
    for( i = 0; i < number_of_datatype; i++ ) {
        if( !(array_of_datatype[i]->flags & DT_FLAG_PREDEFINED) ) {
            OBJ_RELEASE(array_of_datatype[i]);
        }
    }
    free( array_of_datatype );
    return datatype;
}

static ompi_datatype_t*
__ompi_ddt_create_from_args( int32_t* i, MPI_Aint* a,
                             MPI_Datatype* d, int32_t type )
{
    ompi_datatype_t* datatype = NULL;

    switch(type){
        /******************************************************************/
    case MPI_COMBINER_DUP:
        break;
        /******************************************************************/
    case MPI_COMBINER_CONTIGUOUS:
        ompi_ddt_create_contiguous( i[0], d[0], &datatype );
        break;
        /******************************************************************/
    case MPI_COMBINER_VECTOR:
        ompi_ddt_create_vector( i[0], i[1], i[2], d[0], &datatype );
        break;
        /******************************************************************/
    case MPI_COMBINER_HVECTOR_INTEGER:
    case MPI_COMBINER_HVECTOR:
        ompi_ddt_create_hvector( i[0], i[1], a[0], d[0], &datatype );
        break;
        /******************************************************************/
    case MPI_COMBINER_INDEXED:  /* TO CHECK */
        ompi_ddt_create_indexed( i[0], &(i[1]), &(i[1+i[0]]), d[0], &datatype );
        break;
        /******************************************************************/
    case MPI_COMBINER_HINDEXED_INTEGER:
    case MPI_COMBINER_HINDEXED:
        ompi_ddt_create_hindexed( i[0], &(i[1]), a, d[0], &datatype );
        break;
        /******************************************************************/
    case MPI_COMBINER_INDEXED_BLOCK:
        ompi_ddt_create_indexed_block( i[0], i[1], &(i[1+i[0]]), d[0], &datatype );
        break;
        /******************************************************************/
    case MPI_COMBINER_STRUCT_INTEGER:
    case MPI_COMBINER_STRUCT:
        ompi_ddt_create_struct( i[0], &(i[1]), a, d, &datatype );
        break;
        /******************************************************************/
    case MPI_COMBINER_SUBARRAY:
        /*pos = 1;
          pArgs->i[0] = i[0][0];
          memcpy( pArgs->i + pos, i[1], pArgs->i[0] * sizeof(int) );
          pos += pArgs->i[0];
          memcpy( pArgs->i + pos, i[2], pArgs->i[0] * sizeof(int) );
          pos += pArgs->i[0];
          memcpy( pArgs->i + pos, i[3], pArgs->i[0] * sizeof(int) );
          pos += pArgs->i[0];
          pArgs->i[pos] = i[4][0];
        */
        break;
        /******************************************************************/
    case MPI_COMBINER_DARRAY:
        /*pos = 3;
          pArgs->i[0] = i[0][0];
          pArgs->i[1] = i[1][0];
          pArgs->i[2] = i[2][0];
          
          memcpy( pArgs->i + pos, i[3], i[2][0] * sizeof(int) );
          pos += i[2][0];
          memcpy( pArgs->i + pos, i[4], i[2][0] * sizeof(int) );
          pos += i[2][0];
          memcpy( pArgs->i + pos, i[5], i[2][0] * sizeof(int) );
          pos += i[2][0];
          memcpy( pArgs->i + pos, i[6], i[2][0] * sizeof(int) );
          pos += i[2][0];
          pArgs->i[pos] = i[7][0];
        */
        break;
        /******************************************************************/
    case MPI_COMBINER_F90_REAL:
    case MPI_COMBINER_F90_COMPLEX:
        /*pArgs->i[0] = i[0][0];
          pArgs->i[1] = i[1][0];
        */
        break;
        /******************************************************************/
    case MPI_COMBINER_F90_INTEGER:
        /*pArgs->i[0] = i[0][0];
         */
        break;
        /******************************************************************/
    case MPI_COMBINER_RESIZED:
        break;
        /******************************************************************/
    default:
        break;
    }

    return datatype;
}
