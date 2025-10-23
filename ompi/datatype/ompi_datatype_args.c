/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2025      Stony Brook University. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stddef.h>

#include "opal/align.h"
#include "opal/types.h"
#include "opal/util/arch.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/proc/proc.h"

static inline int
__ompi_datatype_pack_description( ompi_datatype_t* datatype,
                                  void** packed_buffer, int* next_index );
static ompi_datatype_t*
__ompi_datatype_create_from_args( const int* i, const size_t *l, const ptrdiff_t * a,
                                  ompi_datatype_t** d, int32_t type );

typedef struct __dt_args {
    opal_atomic_int32_t ref_count;
    int32_t            create_type;
    size_t             total_pack_size;
    /* TODO: should they be size_t? */
    size_t             ci;
    size_t             ca;
    size_t             cd;
    size_t             cl;
    ptrdiff_t*          a;
    ompi_datatype_t**   d;
    size_t*             l; // array of size_t counts
    int*                i; // array of integer counts
} ompi_datatype_args_t;

/**
 * Some architectures really don't like having unaligned
 * accesses.  We'll be int aligned, because any sane system will
 * require that.  But we might not be long aligned, and some
 * architectures will complain if a long is accessed on int
 * alignment (but not long alignment).  On those architectures,
 * copy the buffer into an aligned buffer first.
 */
#if OPAL_ALIGN_WORD_SIZE_INTEGERS
#define OMPI_DATATYPE_ALIGN_PTR(PTR, TYPE) \
    (PTR) = OPAL_ALIGN_PTR((PTR), sizeof(ptrdiff_t), TYPE)
#else
#define OMPI_DATATYPE_ALIGN_PTR(PTR, TYPE)
#endif  /* OPAL_ALIGN_WORD_SIZE_INTEGERS */

/**
 * Copies count elements from the given count array into either
 * the integer or size_t destination depending on whether the
 * count array is 32 or 64 bit. Advances the destination pointer.
 */
static inline void copy_count_array(size_t count, int**__restrict__ desti, size_t**__restrict__ destc, ompi_count_array_t array) {
    size_t elem_size = opal_count_array_is_64bit(array) ? sizeof(size_t) : sizeof(int);
    void *dest = opal_count_array_is_64bit(array) ? (void*)*destc : (void*)*desti;
    memcpy(dest, opal_count_array_ptr(array), count * elem_size);
    if (opal_count_array_is_64bit(array)) {
        *destc += count;
    } else {
        *desti += count;
    }
}

int32_t ompi_datatype_set_args( ompi_datatype_t* pData,
                                size_t ci, size_t cl, const ompi_count_array_t *counts,
                                size_t ca, const opal_disp_array_t a,
                                size_t cd, ompi_datatype_t* const * d, int32_t type)
{
    size_t pos;

    assert( NULL == pData->args );

    size_t length = sizeof(ompi_datatype_args_t) + ci * sizeof(int) +
                    cl * sizeof(size_t) + ca * sizeof(ptrdiff_t) +
                    cd * sizeof(MPI_Datatype);
    char* buf = (char*)malloc( length );
    ompi_datatype_args_t* pArgs = (ompi_datatype_args_t*)buf;
    size_t *pl = NULL;
    int *pi = NULL;
    pArgs->ci = ci; pArgs->i = NULL;
    pArgs->cl = cl; pArgs->l = NULL;
    pArgs->ca = ca; pArgs->a = NULL;
    pArgs->cd = cd; pArgs->d = NULL;
    pArgs->create_type = type;

    buf += sizeof(ompi_datatype_args_t);
    if( 0 != pArgs->ca ) {
        pArgs->a = (ptrdiff_t*)buf;
        buf += pArgs->ca * sizeof(ptrdiff_t);
    }
    if( 0 != pArgs->cd ) {
        pArgs->d = (ompi_datatype_t**)buf;
        buf += pArgs->cd * sizeof(MPI_Datatype);
    }
    if (0 != pArgs->cl ) {
        pArgs->l = pl = (size_t*)buf;
        buf += pArgs->cl * sizeof(size_t);
    }
    if( 0 != pArgs->ci ) {
        pArgs->i = pi = (int*)buf;
        buf += pArgs->ci * sizeof(int);
    }

    pArgs->ref_count = 1;
    pArgs->total_pack_size = 5 * sizeof(size_t) + ci * sizeof(int) +
                             cl * sizeof(size_t) +
                             cd * sizeof(MPI_Datatype) +
                             ca * sizeof(ptrdiff_t);
    switch(type) {

    case MPI_COMBINER_DUP:
        pArgs->total_pack_size = 0;  /* store no extra data */
        break;

    case MPI_COMBINER_CONTIGUOUS:
        copy_count_array(1, &pi, &pl, counts[0]);
        break;

    case MPI_COMBINER_VECTOR:
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(1, &pi, &pl, counts[1]);
        copy_count_array(1, &pi, &pl, counts[2]);
        break;

    case MPI_COMBINER_HVECTOR_INTEGER:
    case MPI_COMBINER_HVECTOR:
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(1, &pi, &pl, counts[1]);
        if (cl > 0) {
            // copy the stride
            memcpy(pl, opal_count_array_ptr(counts[2]), sizeof(MPI_Count));
            pl++;
        }
        break;

    case MPI_COMBINER_INDEXED: {
        size_t count = opal_count_array_get(counts[0], 0);
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(count, &pi, &pl, counts[1]);
        copy_count_array(count, &pi, &pl, counts[2]);
        break;
    }

    case MPI_COMBINER_HINDEXED_INTEGER:
    case MPI_COMBINER_HINDEXED: {
        size_t count = opal_count_array_get(counts[0], 0);
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(count, &pi, &pl, counts[1]);
        if (cl > 0) {
            // copy the displacements
            memcpy(pl, opal_count_array_ptr(counts[2]), count * sizeof(MPI_Count));
            pl += count;
        }
        break;
    }

    case MPI_COMBINER_INDEXED_BLOCK: {
        size_t count = opal_count_array_get(counts[0], 0);
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(1, &pi, &pl, counts[1]);
        copy_count_array(count, &pi, &pl, counts[2]);
        break;
    }

    case MPI_COMBINER_STRUCT_INTEGER:
    case MPI_COMBINER_STRUCT: {
        size_t count = opal_count_array_get(counts[0], 0);
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(count, &pi, &pl, counts[1]);
        if (cl > 0) {
            // copy the displacements
            memcpy(pl, opal_count_array_ptr(counts[2]), count * sizeof(MPI_Count));
            pl += count;
        }
        break;
    }

    case MPI_COMBINER_SUBARRAY: {
        size_t count = opal_count_array_get(counts[0], 0);
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(count, &pi, &pl, counts[1]);
        copy_count_array(count, &pi, &pl, counts[2]);
        copy_count_array(count, &pi, &pl, counts[3]);
        copy_count_array(1, &pi, &pl, counts[4]);
        break;
    }

    case MPI_COMBINER_DARRAY: {
        size_t ndim = opal_count_array_get(counts[2], 0);
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(1, &pi, &pl, counts[1]);
        copy_count_array(1, &pi, &pl, counts[2]);
        copy_count_array(ndim, &pi, &pl, counts[3]);
        copy_count_array(ndim, &pi, &pl, counts[4]);
        copy_count_array(ndim, &pi, &pl, counts[5]);
        copy_count_array(ndim, &pi, &pl, counts[6]);
        copy_count_array(1, &pi, &pl, counts[7]);
        break;
    }

    case MPI_COMBINER_F90_REAL:
    case MPI_COMBINER_F90_COMPLEX:
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(1, &pi, &pl, counts[1]);
        break;

    case MPI_COMBINER_F90_INTEGER:
        copy_count_array(1, &pi, &pl, counts[0]);
        break;

    case MPI_COMBINER_RESIZED:
        break;

    case MPI_COMBINER_HINDEXED_BLOCK:
        copy_count_array(1, &pi, &pl, counts[0]);
        copy_count_array(1, &pi, &pl, counts[1]);
        if (cl > 0) {
            // copy the displacements
            size_t count = opal_count_array_get(counts[0], 0);
            memcpy(pl, opal_count_array_ptr(counts[2]), count * sizeof(MPI_Count));
            pl += count;
        }
        break;

    default:
        break;
    }

    /* copy the array of MPI_Aint, aka ptrdiff_t */
    if( pArgs->a != NULL )
        memcpy( pArgs->a, ompi_disp_array_ptr(a), ca * sizeof(ptrdiff_t) );

    for( pos = 0; pos < cd; pos++ ) {
        pArgs->d[pos] = d[pos];
        if( !(ompi_datatype_is_predefined(d[pos])) ) {
            /* We handle a user defined datatype. We should make sure that the
             * user will not have the opportunity to destroy it before all derived
             * datatypes are destroyed. As we keep pointers to every datatype
             * (for MPI_Type_get_content and MPI_Type_get_envelope) we have to make
             * sure that those datatype will be available if the user ask for them.
             * However, there is no easy way to free them in this case ...
             */
            OBJ_RETAIN( d[pos] );
            pArgs->total_pack_size += ((ompi_datatype_args_t*)d[pos]->args)->total_pack_size;
        } else {
            pArgs->total_pack_size += sizeof(int); /* _NAMED */
        }
        pArgs->total_pack_size += sizeof(int);  /* each data has an ID */
    }

    pData->args = (void*)pArgs;
    pData->packed_description = 0;

    return OMPI_SUCCESS;
}

int32_t ompi_datatype_print_args( const ompi_datatype_t* pData )
{
    size_t i;
    ompi_datatype_args_t* pArgs = (ompi_datatype_args_t*)pData->args;

    if( ompi_datatype_is_predefined(pData) ) {
        /* nothing to do for predefined data-types */
        return OMPI_SUCCESS;
    }

    if( pArgs == NULL ) return MPI_ERR_INTERN;

    printf( "type %d count ints %zu count counts %zu count disp %zu count datatype %zu\n",
            pArgs->create_type, pArgs->ci, pArgs->cl, pArgs->ca, pArgs->cd );
    if( pArgs->i != NULL ) {
        printf( "ints:     ");
        for( i = 0; i < pArgs->ci; i++ ) {
            printf( "%d ", pArgs->i[i] );
        }
        printf( "\n" );
    }
    if( pArgs->l != NULL ) {
        printf( "counts:     ");
        for( i = 0; i < pArgs->cl; i++ ) {
            printf( "%zu ", pArgs->l[i] );
        }
        printf( "\n" );
    }
    if( pArgs->a != NULL ) {
        printf( "MPI_Aint: " );
        for( i = 0; i < pArgs->ca; i++ ) {
            printf( "%ld ", (long)pArgs->a[i] );
        }
        printf( "\n" );
    }
    if( pArgs->d != NULL ) {
        int count = 1;
        ompi_datatype_t *temp, *old;

        printf( "types:    " );
        old = pArgs->d[0];
        for( i = 1; i < pArgs->cd; i++ ) {
            temp = pArgs->d[i];
            if( old == temp ) {
                count++;
                continue;
            }
            if( count <= 1 ) {
                if( ompi_datatype_is_predefined(old) )
                    printf( "%s ", old->name );
                else
                    printf( "%p ", (void*)old );
            } else {
                if( ompi_datatype_is_predefined(old) )
                    printf( "(%d * %s) ", count, old->name );
                else
                    printf( "(%d * %p) ", count, (void*)old );
            }
            count = 1;
            old = temp;
        }
        if( count <= 1 ) {
            if( ompi_datatype_is_predefined(old) )
                printf( "%s ", old->name );
            else
                printf( "%p ", (void*)old );
        } else {
            if( ompi_datatype_is_predefined(old) )
                printf( "(%d * %s) ", count, old->name );
            else
                printf( "(%d * %p) ", count, (void*)old );
        }
        printf( "\n" );
    }
    return OMPI_SUCCESS;
}


int32_t ompi_datatype_get_args( const ompi_datatype_t* pData, int32_t which,
                                size_t* ci, int* i,
                                size_t* cl, MPI_Count* l,
                                size_t* ca, ptrdiff_t* a,
                                size_t* cd, ompi_datatype_t** d, int32_t* type)
{
    ompi_datatype_args_t* pArgs = (ompi_datatype_args_t*)pData->args;

    if( NULL == pArgs ) {  /* only for predefined datatypes */
        if( ompi_datatype_is_predefined(pData) ) {
            switch(which){
            case 0:
                *ci = 0;
                *cl = 0;
                *ca = 0;
                *cd = 0;
                *type = MPI_COMBINER_NAMED;
                break;
            default:
                return MPI_ERR_INTERN;
            }
            return OMPI_SUCCESS;
        }
        return MPI_ERR_INTERN;
    }

    switch(which){
    case 0:     /* GET THE LENGTHS */
        *ci = pArgs->ci;
        *cl = pArgs->cl;
        *ca = pArgs->ca;
        *cd = pArgs->cd;
        *type = pArgs->create_type;
        break;
    case 1:     /* GET THE ARGUMENTS */
        if( (NULL != i) && (NULL != pArgs->i) ) {
            memcpy( i, pArgs->i, pArgs->ci * sizeof(int) );
        }
        if( (NULL != l) && (NULL != pArgs->l) ) {
            memcpy( l, pArgs->l, pArgs->cl * sizeof(size_t) );
        }
        if( (NULL != a) && (NULL != pArgs->a) ) {
            memcpy( a, pArgs->a, pArgs->ca * sizeof(ptrdiff_t) );
        }
        if( (NULL != d) && (NULL != pArgs->d) ) {
            memcpy( d, pArgs->d, pArgs->cd * sizeof(MPI_Datatype) );
        }
        break;
    default:
        return MPI_ERR_INTERN;
    }
    return OMPI_SUCCESS;
}


int32_t ompi_datatype_copy_args( const ompi_datatype_t* source_data,
                                 ompi_datatype_t* dest_data )
{
    ompi_datatype_args_t* pArgs = (ompi_datatype_args_t*)source_data->args;

    /* Increase the reference count of the datatype envelope. This
     * prevent us from making extra copies for the envelope (which is mostly
     * a read only memory).
     */
    if( NULL != pArgs ) {
        OPAL_THREAD_ADD_FETCH32(&pArgs->ref_count, 1);
        dest_data->args = pArgs;
    }
    return OMPI_SUCCESS;
}


/* In the dt_add function we increase the reference count for all datatypes
 * (except for the predefined ones) that get added to another datatype. This
 * insure that they cannot get released until all the references to them
 * get removed.
 */
int32_t ompi_datatype_release_args( ompi_datatype_t* pData )
{
    size_t i;
    ompi_datatype_args_t* pArgs = (ompi_datatype_args_t*)pData->args;

    assert( 0 < pArgs->ref_count );
    OPAL_THREAD_ADD_FETCH32(&pArgs->ref_count, -1);
    if( 0 == pArgs->ref_count ) {
        /* There are some duplicated datatypes around that have a pointer to this
         * args. We will release them only when the last datatype will disappear.
         */
        for( i = 0; i < pArgs->cd; i++ ) {
            if( !(ompi_datatype_is_predefined(pArgs->d[i])) ) {
                OBJ_RELEASE( pArgs->d[i] );
            }
        }
        free( pData->args );
    }
    pData->args = NULL;

    return OMPI_SUCCESS;
}


static inline int __ompi_datatype_pack_description( ompi_datatype_t* datatype,
                                                    void** packed_buffer, int* next_index )
{
    size_t i;
    int *iposition = NULL;
    ompi_datatype_args_t* args = (ompi_datatype_args_t*)datatype->args;
    char* next_packed = (char*)*packed_buffer;

    if( ompi_datatype_is_predefined(datatype) ) {
        iposition[0] = MPI_COMBINER_NAMED;
        iposition[1] = datatype->id;   /* On the OMPI - layer, copy the ompi_datatype.id */
        next_packed += (2 * sizeof(int));
        *packed_buffer = next_packed;
        return OMPI_SUCCESS;
    }
    /* For duplicated datatype we don't have to store all the information */
    if( MPI_COMBINER_DUP == args->create_type ) {
        ompi_datatype_t* temp_data = args->d[0];
        return __ompi_datatype_pack_description(temp_data,
                                                packed_buffer,
                                                next_index );
    }
    iposition[0] = args->create_type;
    next_packed += sizeof(int);
    /* align pointer to 64 bits */
    OMPI_DATATYPE_ALIGN_PTR(next_packed, char*);
    size_t *cposition = ((size_t*)next_packed);
    cposition[0] = args->ci;
    cposition[1] = args->cl;
    cposition[2] = args->ca;
    cposition[3] = args->cd;
    next_packed += (4 * sizeof(size_t));
    if( 0 < args->ca ) {
        memcpy( next_packed, args->a, sizeof(ptrdiff_t) * args->ca );
        next_packed += sizeof(ptrdiff_t) * args->ca;
    }
    if ( 0 < args->cl ) {
        memcpy( next_packed, args->l, sizeof(size_t) * args->cl );
        next_packed += sizeof(size_t) * args->cl;
    }
    iposition = (int*)next_packed;

    /* skip the datatypes */
    next_packed += sizeof(int) * args->cd;

    /* copy the array of 32bit counts at the end */
    memcpy( next_packed, args->i, sizeof(int) * args->ci );
    next_packed += args->ci * sizeof(int);

    /* copy the rest of the data */
    for( i = 0; i < args->cd; i++ ) {
        ompi_datatype_t* temp_data = args->d[i];
        if( ompi_datatype_is_predefined(temp_data) ) {
            iposition[i] = temp_data->id;  /* On the OMPI - layer, copy the ompi_datatype.id */
        } else {
            iposition[i] = *next_index;
            (*next_index)++;
            __ompi_datatype_pack_description( temp_data,
                                              (void**)&next_packed,
                                              next_index );
        }
    }
    *packed_buffer = next_packed;
    return OMPI_SUCCESS;
}


int ompi_datatype_get_pack_description( ompi_datatype_t* datatype,
                                        const void** packed_buffer )
{
    ompi_datatype_args_t* args = (ompi_datatype_args_t*)datatype->args;
    int next_index = OMPI_DATATYPE_MAX_PREDEFINED;
    void *packed_description = (void *) datatype->packed_description;
    void* recursive_buffer;

    if (NULL == packed_description) {
        void *_tmp_ptr = NULL;
        if (opal_atomic_compare_exchange_strong_ptr (&datatype->packed_description, (intptr_t *) &_tmp_ptr, 1)) {
            if( ompi_datatype_is_predefined(datatype) ) {
                packed_description = malloc(2 * sizeof(int));
            } else if( NULL == args ) {
                return OMPI_ERROR;
            } else {
                packed_description = malloc(args->total_pack_size);
            }
            recursive_buffer = packed_description;
            __ompi_datatype_pack_description( datatype, &recursive_buffer, &next_index );

            if (!ompi_datatype_is_predefined(datatype)) {
                /* If the precomputed size is not large enough we're already in troubles, we
                 * have overwritten outside of the allocated buffer. Raise the alarm !
                 * If not reassess the size of the packed buffer necessary for holding the
                 * datatype description.
                 */
                assert(args->total_pack_size >= (uintptr_t)((char*)recursive_buffer - (char *) packed_description));
                args->total_pack_size = (uintptr_t)((char*)recursive_buffer - (char *) packed_description);
            }

            opal_atomic_wmb ();
            datatype->packed_description = (intptr_t) packed_description;
        } else {
            /* another thread beat us to it */
            packed_description = (void *) datatype->packed_description;
        }
    }

    if ((void *) 1 == packed_description) {
        struct timespec interval = {.tv_sec = 0, .tv_nsec = 1000};

        /* wait until the packed description is updated */
        while (1 == datatype->packed_description) {
            nanosleep (&interval, NULL);
        }

        packed_description = (void *) datatype->packed_description;
    }

    *packed_buffer = (const void *) packed_description;
    return OMPI_SUCCESS;
}

size_t ompi_datatype_pack_description_length( ompi_datatype_t* datatype )
{
    void *packed_description = (void *) datatype->packed_description;

    if( ompi_datatype_is_predefined(datatype) ) {
        return 2 * sizeof(int);
    }
    if( NULL == packed_description || (void *) 1 == packed_description) {
        const void* buf;
        int rc;

        rc = ompi_datatype_get_pack_description(datatype, &buf);
        if( OMPI_SUCCESS != rc ) {
            return 0;
        }
    }
    assert( NULL != (ompi_datatype_args_t*)datatype->args );
    assert( NULL != (ompi_datatype_args_t*)datatype->packed_description );
    return ((ompi_datatype_args_t*)datatype->args)->total_pack_size;
}

static ompi_datatype_t* __ompi_datatype_create_from_packed_description( void** packed_buffer,
                                                                        const struct ompi_proc_t* remote_processor )
{
    int* iposition;
    size_t *cposition;
    ompi_datatype_t* datatype = NULL;
    ompi_datatype_t** array_of_datatype;
    ptrdiff_t* array_of_disp;
    int* array_of_ints;
    size_t *array_of_counts = NULL;
    size_t number_of_ints, number_of_counts, number_of_disp, number_of_datatype, data_id;
    int create_type;
    size_t i;
    char* next_buffer;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    bool need_swap = false;

    if( (remote_processor->super.proc_arch ^ ompi_proc_local()->super.proc_arch) &
        OPAL_ARCH_ISBIGENDIAN ) {
        need_swap = true;
    }
#endif

    next_buffer = (char*)*packed_buffer;
    cposition = (size_t*)next_buffer;
    iposition = (int*)next_buffer;

    create_type = (int)iposition[0];
    next_buffer += sizeof(int);
    /* align pointer to 64 bits */
    OMPI_DATATYPE_ALIGN_PTR(next_buffer, char*);
    cposition = (size_t*)next_buffer;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (need_swap) {
        create_type = opal_swap_bytes4(create_type);
    }
#endif
    if( MPI_COMBINER_NAMED == create_type ) {
        /* there we have a simple predefined datatype */
        data_id = iposition[1];
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if (need_swap) {
            data_id = opal_swap_bytes4(data_id);
        }
#endif
        assert( data_id < OMPI_DATATYPE_MAX_PREDEFINED );
        *packed_buffer = iposition + 2;
        return (ompi_datatype_t*)ompi_datatype_basicDatatypes[data_id];
    }

    number_of_ints     = cposition[0];
    number_of_counts   = cposition[1];
    number_of_disp     = cposition[2];
    number_of_datatype = cposition[3];
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (need_swap) {
        number_of_ints     = opal_swap_bytes8(number_of_ints);
        number_of_counts   = opal_swap_bytes8(number_of_counts);
        number_of_disp     = opal_swap_bytes8(number_of_disp);
        number_of_datatype = opal_swap_bytes8(number_of_datatype);
    }
#endif
    array_of_datatype = (ompi_datatype_t**)malloc( sizeof(ompi_datatype_t*) *
                                                   number_of_datatype );
    next_buffer += (4 * sizeof(size_t));  /* move after the header */
    /* the array of displacements */
    array_of_disp   = (ptrdiff_t*)next_buffer;
    next_buffer    += number_of_disp * sizeof(ptrdiff_t);
    if (number_of_counts > 0) {
        array_of_counts = (size_t*)next_buffer;
        next_buffer    += number_of_counts * sizeof(size_t);
    }
    /* the other datatypes */
    iposition        = (int*)next_buffer;
    next_buffer    += number_of_datatype * sizeof(int);
    /* the array of lengths (32 bits aligned) */
    if (number_of_ints > 0) {
        array_of_ints = (int*)next_buffer;
        next_buffer += number_of_ints * sizeof(int);
    }

    for( i = 0; i < number_of_datatype; i++ ) {
        data_id = iposition[i];
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if (need_swap) {
            data_id = opal_swap_bytes4(data_id);
        }
#endif
        if( data_id < OMPI_DATATYPE_MAX_PREDEFINED ) {
            array_of_datatype[i] = (ompi_datatype_t*)ompi_datatype_basicDatatypes[data_id];
            continue;
        }
        array_of_datatype[i] =
            __ompi_datatype_create_from_packed_description( (void**)&next_buffer,
                                                            remote_processor );
        if( NULL == array_of_datatype[i] ) {
            /* don't cleanup more than required. We can now modify these
             * values as we already know we have failed to rebuild the
             * datatype.
             */
            array_of_datatype[i] = (ompi_datatype_t*)ompi_datatype_basicDatatypes[OPAL_DATATYPE_INT1]; /*XXX TODO */
            number_of_datatype = i;
            goto cleanup_and_exit;
        }
    }

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (need_swap) {
        for (i = 0 ; i < number_of_ints ; ++i) {
            number_of_ints[i] = opal_swap_bytes8(number_of_ints[i]);
        }
        for (i = 0 ; i < number_of_counts ; ++i) {
            array_of_counts[i] = opal_swap_bytes4(array_of_counts[i]);
        }
        for (i = 0 ; i < number_of_disp ; ++i) {
#if SIZEOF_PTRDIFF_T == 4
            array_of_disp[i] = opal_swap_bytes4(array_of_disp[i]);
#elif SIZEOF_PTRDIFF_T == 8
            array_of_disp[i] = (MPI_Aint)opal_swap_bytes8(array_of_disp[i]);
#else
#error "Unknown size of ptrdiff_t"
#endif
        }
    }
#endif
    datatype = __ompi_datatype_create_from_args( array_of_ints, array_of_counts, array_of_disp,
                                                 array_of_datatype, create_type );
    *packed_buffer = next_buffer;
 cleanup_and_exit:
    for( i = 0; i < number_of_datatype; i++ ) {
        if( !(ompi_datatype_is_predefined(array_of_datatype[i])) ) {
            OBJ_RELEASE(array_of_datatype[i]);
        }
    }
    free( array_of_datatype );
    return datatype;
}

static ompi_datatype_t* __ompi_datatype_create_from_args( const int* i, const size_t *l, const ptrdiff_t* a,
                                                          ompi_datatype_t** d, int32_t type )
{
    size_t count, ci = 0, cl = 0;
    ompi_datatype_t* datatype = NULL;

    ompi_disp_array_t disp_array = OMPI_DISP_ARRAY_CREATE(a);

    switch(type){
        /******************************************************************/
    case MPI_COMBINER_DUP:
        /* should we duplicate d[0]? */
        /* ompi_datatype_set_args( datatype, 0, NULL, 0, NULL, 1, d[0], MPI_COMBINER_DUP ); */
        assert(0);  /* shouldn't happen */
        break;
        /******************************************************************/
    case MPI_COMBINER_CONTIGUOUS: {
        ompi_count_array_t a_i[1];
        if (l == NULL) {
            count = i[0];
            a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);
            ci = 1;
        } else { // large count variant
            count = l[0];
            a_i[0] = OMPI_COUNT_ARRAY_CREATE(l);
            cl = 1;
        }
        ompi_datatype_create_contiguous( count, d[0], &datatype );
        ompi_datatype_set_args( datatype, ci, cl, a_i, 0, OMPI_DISP_ARRAY_NULL, 1, d, MPI_COMBINER_CONTIGUOUS );
        break;
    }
        /******************************************************************/
    case MPI_COMBINER_VECTOR: {
        size_t blocklength, stride;
        opal_count_array_t a_i[3];
        if (l == NULL) {
            count      = i[0];
            blocklength= i[1];
            stride     = i[2];
            a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);
            a_i[1] = OMPI_COUNT_ARRAY_CREATE(i + 1);
            a_i[2] = OMPI_COUNT_ARRAY_CREATE(i + 2);
            ci = 3;
        } else { // large count variant
            count      = l[0];
            blocklength= l[1];
            stride     = l[2];
            a_i[0] = OMPI_COUNT_ARRAY_CREATE(l);
            a_i[1] = OMPI_COUNT_ARRAY_CREATE(l + 1);
            a_i[2] = OMPI_COUNT_ARRAY_CREATE(l + 2);
            cl = 3;
        }
        ompi_datatype_create_vector( count, blocklength, stride, d[0], &datatype );
        ompi_datatype_set_args( datatype, ci, cl, a_i, 0, OMPI_DISP_ARRAY_NULL, 1, d, MPI_COMBINER_VECTOR );
        break;
    }
        /******************************************************************/
    case MPI_COMBINER_HVECTOR_INTEGER:
    case MPI_COMBINER_HVECTOR:
        {
            size_t blocklength;
            opal_count_array_t a_i[2];
            if (l == NULL) {
                count          = i[0];
                blocklength    = i[1];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(i + 1);
                ci = 2;
            } else { // large count variant
                count          = l[0];
                blocklength    = l[1];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(l);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(l + 1);
                cl = 2;
            }
            ompi_datatype_create_hvector( count, blocklength, a[0], d[0], &datatype );
            ompi_datatype_set_args( datatype, ci, cl, a_i, 1, disp_array, 1, d, MPI_COMBINER_HVECTOR );
        }
        break;
        /******************************************************************/
    case MPI_COMBINER_INDEXED:  /* TO CHECK */
        {
            opal_count_array_t a_i[3];
            if (l == NULL) {
                count = i[0];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(i + 1);
                a_i[2] = OMPI_COUNT_ARRAY_CREATE(i + 1 + count);
                ci = 2 * count + 1;
            } else {
                count = l[0];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(l);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(l + 1);
                a_i[2] = OMPI_COUNT_ARRAY_CREATE(l + 1 + count);
                cl = 2 * count + 1;
            }
            ompi_datatype_create_indexed( count, a_i[1], a_i[2], d[0], &datatype );
            ompi_datatype_set_args( datatype, ci, cl, a_i, 0, OMPI_DISP_ARRAY_NULL, 1, d, MPI_COMBINER_INDEXED );
        }
        break;
        /******************************************************************/
    case MPI_COMBINER_HINDEXED_INTEGER:
    case MPI_COMBINER_HINDEXED:
        {
            opal_count_array_t a_i[2];
            if (l == NULL) {
                count = i[0];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(i + 1);
                ci = count+1;
            } else {
                count = l[0];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(l);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(l + 1);
                cl = count+1;
            }
            ompi_datatype_create_hindexed( count, a_i[1], disp_array, d[0], &datatype );
            ompi_datatype_set_args( datatype, ci, cl, a_i, count, disp_array, 1, d, MPI_COMBINER_HINDEXED );
        }
        break;
        /******************************************************************/
    case MPI_COMBINER_INDEXED_BLOCK:
        {
            opal_count_array_t a_i[3];
            size_t blocklength;
            if (l == NULL) {
                count        = i[0];
                blocklength  = i[1];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(i + 1);
                a_i[2] = OMPI_COUNT_ARRAY_CREATE(i + 2);
                ci = 2 + count;
            } else {
                count        = l[0];
                blocklength  = l[1];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(l);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(l + 1);
                a_i[2] = OMPI_COUNT_ARRAY_CREATE(l + 2);
                cl = 2 + count;
            }
            ompi_datatype_create_indexed_block( count, blocklength, a_i[2], d[0], &datatype );
            ompi_datatype_set_args( datatype, ci, cl, a_i, 0, OMPI_DISP_ARRAY_NULL, 1, d, MPI_COMBINER_INDEXED_BLOCK );
        }
        break;
        /******************************************************************/
    case MPI_COMBINER_STRUCT_INTEGER:
    case MPI_COMBINER_STRUCT:
        {
            opal_count_array_t a_i[2];
            if (l == NULL) {
                count = i[0];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(i + 1);
                ci = 2 * count + 1;
            } else {
                count = l[0];
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(l);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(l + 1);
                cl = count + 1;
            }
            ompi_datatype_create_struct( count, a_i[1], disp_array, d, &datatype );
            ompi_datatype_set_args( datatype, ci, cl, a_i, count, disp_array, count, d, MPI_COMBINER_STRUCT );
        }
        break;
        /******************************************************************/
    case MPI_COMBINER_SUBARRAY:
        {
            count = i[0]; // first element in int array
            int order;
            opal_count_array_t a_i[5];
            if (l == NULL) {
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(i + 1);
                a_i[2] = OMPI_COUNT_ARRAY_CREATE(i + 1 + count);
                a_i[3] = OMPI_COUNT_ARRAY_CREATE(i + 1 + 2*count);
                a_i[4] = OMPI_COUNT_ARRAY_CREATE(i + 1 + 3*count);
                order = i[3*count+1]; // last element in int array
                ci = 3 * count + 2;
            } else {
                a_i[0] = OMPI_COUNT_ARRAY_CREATE(i);            // ndim
                a_i[1] = OMPI_COUNT_ARRAY_CREATE(l);            // sizes
                a_i[2] = OMPI_COUNT_ARRAY_CREATE(l + count);    // subsizes
                a_i[3] = OMPI_COUNT_ARRAY_CREATE(l + 2*count);  // starts
                a_i[4] = OMPI_COUNT_ARRAY_CREATE(i+1);          // order
                order = i[1]; // second (and last) element in int array
                cl = 3 * count;
                ci = 2;
            }
            ompi_datatype_create_subarray( count, a_i[1], a_i[2], a_i[3], order, d[0], &datatype );
            ompi_datatype_set_args( datatype, ci, cl, a_i, 0, OMPI_DISP_ARRAY_NULL, 1, d, MPI_COMBINER_SUBARRAY );
        }
        break;
        /******************************************************************/
    case MPI_COMBINER_DARRAY:
        {
            int size  = i[0];
            int rank  = i[1];
            int ndims = i[2];
            ompi_count_array_t gsize_array;
            const int *distrib_array;
            const int *darg_array;
            const int *psize_array;
            int order;
            if (l == NULL) {
                gsize_array   = OMPI_COUNT_ARRAY_CREATE(i + 3);
                distrib_array = &i[3 + 1*ndims];
                darg_array    = &i[3 + 2*ndims];
                psize_array   = &i[3 + 3*ndims];
                order         =  i[3 + 4*ndims];
                ci = 4 + 4 * ndims;
            } else {
                gsize_array   = OMPI_COUNT_ARRAY_CREATE(l);
                distrib_array = &i[3 + 0*ndims];
                darg_array    = &i[3 + 1*ndims];
                psize_array   = &i[3 + 2*ndims];
                order         =  i[3 + 3*ndims];
                ci = 4 + 3 * ndims;
                cl = ndims;
            }
            opal_count_array_t a_i[8] = {OMPI_COUNT_ARRAY_CREATE(&size),
                                         OMPI_COUNT_ARRAY_CREATE(&rank),
                                         OMPI_COUNT_ARRAY_CREATE(&ndims),
                                         gsize_array,
                                         OMPI_COUNT_ARRAY_CREATE(distrib_array),
                                         OMPI_COUNT_ARRAY_CREATE(darg_array),
                                         OMPI_COUNT_ARRAY_CREATE(psize_array),
                                         OMPI_COUNT_ARRAY_CREATE(&order)};
            ompi_datatype_create_darray( size, rank, ndims, gsize_array, distrib_array, darg_array, psize_array, order, d[0], &datatype );
            ompi_datatype_set_args( datatype, ci, cl, a_i, 0, OMPI_DISP_ARRAY_NULL, 1, d, MPI_COMBINER_DARRAY);
        }
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
        /*pArgs->i[0] = i[0][0];*/
        break;
        /******************************************************************/
    case MPI_COMBINER_RESIZED:
        ompi_datatype_create_resized(d[0], a[0], a[1], &datatype);
        ompi_datatype_set_args( datatype, 0, 0, NULL, 2, disp_array, 1, d, MPI_COMBINER_RESIZED );
        break;
        /******************************************************************/
    case MPI_COMBINER_HINDEXED_BLOCK:
        {
            size_t bLength = 0;
            if (l == NULL) {
                count = i[0];
                bLength = i[1];
                ci = 2;
            } else {
                count = l[0];
                bLength = l[1];
                cl = 2;
            }
            ompi_datatype_create_hindexed_block( count, bLength, disp_array, d[0], &datatype );
            opal_count_array_t a_i[2] = {OMPI_COUNT_ARRAY_CREATE(&count), OMPI_COUNT_ARRAY_CREATE(&bLength)};
            ompi_datatype_set_args( datatype, ci, cl, a_i, count, disp_array, 1, d, MPI_COMBINER_HINDEXED_BLOCK );
        }
        break;
        /******************************************************************/
     default:
        break;
    }

    return datatype;
}

ompi_datatype_t* ompi_datatype_create_from_packed_description( void** packed_buffer,
                                                               struct ompi_proc_t* remote_processor )
{
    ompi_datatype_t* datatype;

    datatype = __ompi_datatype_create_from_packed_description( packed_buffer,
                                                               remote_processor );
    if( NULL == datatype ) {
        return NULL;
    }
    ompi_datatype_commit( &datatype );
    return datatype;
}

/**
 * Parse the datatype description from the args and find if the
 * datatype is created from a single predefined type. If yes,
 * return the type, otherwise return NULL.
 */
ompi_datatype_t* ompi_datatype_get_single_predefined_type_from_args( ompi_datatype_t* type )
{
    ompi_datatype_t *predef = NULL, *current_type, *current_predef;
    ompi_datatype_args_t* args = (ompi_datatype_args_t*)type->args;
    size_t i;

    if( ompi_datatype_is_predefined(type) )
        return type;

    for( i = 0; i < args->cd; i++ ) {
        current_type = args->d[i];
        if( ompi_datatype_is_predefined(current_type) ) {
            current_predef = current_type;
        } else {
            current_predef = ompi_datatype_get_single_predefined_type_from_args(current_type);
            if( NULL == current_predef ) { /* No single predefined datatype */
                return NULL;
            }
        }
        if (current_predef != MPI_LB && current_predef != MPI_UB) {
            if( NULL == predef ) {  /* This is the first iteration */
                predef = current_predef;
            } else {
                /**
                 *  What exactly should we consider as identical types?
                 *  If they are the same MPI level type, or if they map
                 *  to the same OPAL datatype? In other words, MPI_FLOAT
                 *  and MPI_REAL4 are they identical?
                 */
                if( predef != current_predef ) {
                    return NULL;
                }
            }
        }
    }
    return predef;
}
