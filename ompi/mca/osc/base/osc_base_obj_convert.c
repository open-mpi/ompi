/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * utility functions for dealing with remote datatype and op structures
 */

#include "ompi_config.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_prototypes.h"

#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"

#include "osc_base_obj_convert.h"
#include "ompi/memchecker.h"

int
ompi_osc_base_get_primitive_type_info(ompi_datatype_t *datatype,
                                      ompi_datatype_t **prim_datatype, 
                                      uint32_t *prim_count)
{
    ompi_datatype_t *primitive_datatype = NULL;
    size_t datatype_size, primitive_size, primitive_count;

    primitive_datatype = ompi_datatype_get_single_predefined_type_from_args(datatype);
    if( NULL == primitive_datatype ) {
        *prim_count = 0;
        return OMPI_SUCCESS;
    }
    ompi_datatype_type_size( datatype, &datatype_size );
    ompi_datatype_type_size( primitive_datatype, &primitive_size );
    primitive_count = datatype_size / primitive_size;
#if OPAL_ENABLE_DEBUG
    assert( 0 == (datatype_size % primitive_size) );
#endif  /* OPAL_ENABLE_DEBUG */

    /* We now have the count as a size_t, convert it to an uint32_t */
    *prim_datatype = primitive_datatype;
    *prim_count = (uint32_t)primitive_count;

    return OMPI_SUCCESS;
}


struct ompi_osc_base_convertor_t {
    opal_convertor_t convertor;
    ompi_op_t *op;
    ompi_datatype_t *datatype;
};
typedef struct ompi_osc_base_convertor_t ompi_osc_base_convertor_t;
static OBJ_CLASS_INSTANCE(ompi_osc_base_convertor_t, opal_convertor_t, NULL, NULL);

#define COPY_TYPE( TYPENAME, TYPE, COUNT )                              \
static int copy_##TYPENAME( opal_convertor_t *pConvertor, uint32_t count, \
                            char* from, size_t from_len, ptrdiff_t from_extent, \
                            char* to, size_t to_len, ptrdiff_t to_extent, \
                            ptrdiff_t *advance)                         \
{                                                                       \
    size_t remote_TYPE_size = sizeof(TYPE) * (COUNT); /* TODO */        \
    size_t local_TYPE_size = (COUNT) * sizeof(TYPE);                    \
    ompi_osc_base_convertor_t *osc_convertor =                          \
        (ompi_osc_base_convertor_t*) pConvertor;                        \
                                                                        \
    if( (from_extent == (ptrdiff_t)local_TYPE_size) &&                  \
        (to_extent == (ptrdiff_t)remote_TYPE_size) ) {                  \
        ompi_op_reduce(osc_convertor->op, from, to, count, osc_convertor->datatype); \
    } else {                                                            \
        uint32_t i;                                                     \
        for( i = 0; i < count; i++ ) {                                  \
            ompi_op_reduce(osc_convertor->op, from, to, 1, osc_convertor->datatype); \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}

/* set up copy functions for the basic C MPI data types */
COPY_TYPE( int1, int8_t, 1)
COPY_TYPE( int2, int16_t, 1)
COPY_TYPE( int4, int32_t, 1)
COPY_TYPE( int8, int64_t, 1)
COPY_TYPE( bool, bool, 1)
COPY_TYPE( float, float, 1)
COPY_TYPE( double, double, 1)
#if HAVE_LONG_DOUBLE
COPY_TYPE( long_double, long double, 1)
#endif
COPY_TYPE( float_complex, float _Complex, 1)
COPY_TYPE( double_complex, double _Complex, 1)
#if HAVE_LONG_DOUBLE__COMPLEX
COPY_TYPE( long_double_complex, long double _Complex, 1)
#endif

/* table of predefined copy functions - one for each opal basic type */
static conversion_fct_t ompi_osc_base_copy_functions[OPAL_DATATYPE_MAX_PREDEFINED] = {
    [OPAL_DATATYPE_INT1]        = (conversion_fct_t) copy_int1,
    [OPAL_DATATYPE_UINT1]       = (conversion_fct_t) copy_int1,
    [OPAL_DATATYPE_INT2]        = (conversion_fct_t) copy_int2,
    [OPAL_DATATYPE_UINT2]       = (conversion_fct_t) copy_int2,
    [OPAL_DATATYPE_INT4]        = (conversion_fct_t) copy_int4,
    [OPAL_DATATYPE_UINT4]       = (conversion_fct_t) copy_int4,
    [OPAL_DATATYPE_INT8]        = (conversion_fct_t) copy_int8,
    [OPAL_DATATYPE_UINT8]       = (conversion_fct_t) copy_int8,
#if SIZEOF_FLOAT == 2
    [OPAL_DATATYPE_FLOAT2]      = (conversion_fct_t) copy_float,
#elif SIZEOF_DOUBLE == 2
    [OPAL_DATATYPE_FLOAT2]      = (conversion_fct_t) copy_double,
#elif SIZEOF_LONG_DOUBLE == 2
    [OPAL_DATATYPE_FLOAT2]      = (conversion_fct_t) copy_long_double,
#endif
#if SIZEOF_FLOAT == 4
    [OPAL_DATATYPE_FLOAT4]      = (conversion_fct_t) copy_float,
#elif SIZEOF_DOUBLE == 4
    [OPAL_DATATYPE_FLOAT4]      = (conversion_fct_t) copy_double,
#elif SIZEOF_LONG_DOUBLE == 4
    [OPAL_DATATYPE_FLOAT4]      = (conversion_fct_t) copy_long_double,
#endif
#if SIZEOF_FLOAT == 8
    [OPAL_DATATYPE_FLOAT8]      = (conversion_fct_t) copy_float,
#elif SIZEOF_DOUBLE == 8
    [OPAL_DATATYPE_FLOAT8]      = (conversion_fct_t) copy_double,
#elif SIZEOF_LONG_DOUBLE == 8
    [OPAL_DATATYPE_FLOAT8]      = (conversion_fct_t) copy_long_double,
#endif
#if SIZEOF_FLOAT == 16
    [OPAL_DATATYPE_FLOAT16]     = (conversion_fct_t) copy_float,
#elif SIZEOF_DOUBLE == 16
    [OPAL_DATATYPE_FLOAT16]     = (conversion_fct_t) copy_double,
#elif SIZEOF_LONG_DOUBLE == 16
    [OPAL_DATATYPE_FLOAT16]     = (conversion_fct_t) copy_long_double,
#endif
    [OPAL_DATATYPE_FLOAT_COMPLEX]   = (conversion_fct_t) copy_float_complex,
    [OPAL_DATATYPE_DOUBLE_COMPLEX]  = (conversion_fct_t) copy_double_complex,
#if HAVE_LONG_DOUBLE__COMPLEX
    [OPAL_DATATYPE_LONG_DOUBLE_COMPLEX] = (conversion_fct_t) copy_long_double_complex,
#endif
    [OPAL_DATATYPE_BOOL]            = (conversion_fct_t) copy_bool,
};

int
ompi_osc_base_process_op(void *outbuf,
                         void *inbuf,
                         size_t inbuflen,
                         struct ompi_datatype_t *datatype,
                         int count,
                         ompi_op_t *op)
{
    if (op == &ompi_mpi_op_replace.op) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    if (ompi_datatype_is_predefined(datatype)) {
        ompi_op_reduce(op, inbuf, outbuf, count, datatype);
    } else {
        struct ompi_datatype_t *primitive_datatype = NULL;
        ompi_osc_base_convertor_t convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;
        struct opal_convertor_master_t master = {NULL, 0, 0, 0, {0, }, NULL};

        primitive_datatype = ompi_datatype_get_single_predefined_type_from_args(datatype);
        if (opal_datatype_is_contiguous_memory_layout (datatype, count) &&
            1 == datatype->super.desc.used) {
            /* NTH: the datatype is made up of a contiguous block of the primitive
             * datatype. do not use the convertor in this case since opal_unpack_general
             can not handle it */
            count *= datatype->super.desc.desc[0].elem.count;
            ompi_op_reduce(op, inbuf, outbuf, count, primitive_datatype);
            return OMPI_SUCCESS;
        }

        /* create convertor */
        OBJ_CONSTRUCT(&convertor, ompi_osc_base_convertor_t);
        convertor.op = op;
        convertor.datatype = primitive_datatype;

        /* initialize convertor */
        opal_convertor_copy_and_prepare_for_recv(ompi_proc_local()->proc_convertor,
                                                 &(datatype->super),
                                                 count,
                                                 outbuf,
                                                 0,
                                                 &convertor.convertor);

        memcpy(&master, convertor.convertor.master, sizeof(struct opal_convertor_master_t));
        master.next = convertor.convertor.master;
        master.pFunctions = (conversion_fct_t*) &ompi_osc_base_copy_functions;
        convertor.convertor.master = &master;
        convertor.convertor.fAdvance = opal_unpack_general;
        /* there are issues with using the optimized description here */
        convertor.convertor.use_desc = &datatype->super.desc;

        iov.iov_len  = inbuflen;
        iov.iov_base = (IOVBASE_TYPE*) inbuf;
        max_data     = iov.iov_len;
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &convertor.convertor);
        );
        opal_convertor_unpack(&convertor.convertor, 
                              &iov,
                              &iov_count,
                              &max_data);
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                      &convertor.convertor);
        );
        OBJ_DESTRUCT(&convertor);
    }

    return OMPI_SUCCESS;
}



int
ompi_osc_base_sndrcv_op(void *origin,
                        int32_t origin_count,
                        struct ompi_datatype_t *origin_dt,
                        void *target,
                        int32_t target_count,
                        struct ompi_datatype_t *target_dt,
                        ompi_op_t *op)
{
    if (ompi_datatype_is_predefined(origin_dt) && origin_dt == target_dt) {
        ompi_op_reduce(op, origin, target, origin_count, origin_dt);
    } else {
        ompi_osc_base_convertor_t recv_convertor;
        opal_convertor_t send_convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;
        int completed, length;
        struct opal_convertor_master_t master = {NULL, 0, 0, 0, {0, }, NULL};

        /* initialize send convertor */
        OBJ_CONSTRUCT(&send_convertor, opal_convertor_t);
        opal_convertor_copy_and_prepare_for_send(ompi_proc_local()->proc_convertor,
                                                  &(origin_dt->super), origin_count, origin, 0,
                                                  &send_convertor);

        /* initialize recv convertor */
        OBJ_CONSTRUCT(&recv_convertor, ompi_osc_base_convertor_t);
        recv_convertor.op = op;
        recv_convertor.datatype = ompi_datatype_get_single_predefined_type_from_args(target_dt);
        opal_convertor_copy_and_prepare_for_recv(ompi_proc_local()->proc_convertor,
                                                 &(target_dt->super), target_count,
                                                 target, 0, &recv_convertor.convertor);

        memcpy(&master, recv_convertor.convertor.master, sizeof(struct opal_convertor_master_t));
        master.next = recv_convertor.convertor.master;
        master.pFunctions = (conversion_fct_t*) &ompi_osc_base_copy_functions;
        recv_convertor.convertor.master = &master;
        recv_convertor.convertor.fAdvance = opal_unpack_general;

        /* copy */
        iov.iov_len = length = 64 * 1024;
        iov.iov_base = (IOVBASE_TYPE*)malloc( length * sizeof(char) );

        completed = 0;
        while(0 == completed) {
            iov.iov_len = length;
            iov_count = 1;
            max_data = length;
            completed |= opal_convertor_pack( &send_convertor, &iov, &iov_count, &max_data );
            completed |= opal_convertor_unpack( &recv_convertor.convertor, &iov, &iov_count, &max_data );
        }
        free( iov.iov_base );
        OBJ_DESTRUCT( &send_convertor );
        OBJ_DESTRUCT( &recv_convertor );
    }

    return OMPI_SUCCESS;
}
