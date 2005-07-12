/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "datatype/datatype.h"
#include "datatype/datatype_internal.h"

typedef struct __dt_args {
   int create_type;
   int ci;
   int ca;
   int cd;
   int* i;
   MPI_Aint* a;
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
    } while(0)

#define FREE_ARGS(PDATA) \
if( (PDATA)->args != NULL ) free( (PDATA)->args );

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
    /* copy the array of types */
    if( pArgs->d != NULL )
        memcpy( pArgs->d, d, cd * sizeof(MPI_Datatype) );

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
    FREE_ARGS( pData );

    return OMPI_SUCCESS;
}
