/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
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

#include "ompi/op/op.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/convertor_internal.h"
#include "ompi/datatype/datatype_prototypes.h"

#include "osc_base_obj_convert.h"
#include "ompi/memchecker.h"

int
ompi_osc_base_get_primitive_type_info(ompi_datatype_t *datatype,
                                      ompi_datatype_t **prim_datatype, 
                                      uint32_t *prim_count)
{
    struct ompi_datatype_t *primitive_datatype = NULL;
    uint32_t primitive_count;

    /* get underlying type... */
    if (ompi_ddt_is_predefined(datatype)) {
        primitive_datatype = datatype;
        primitive_count = 1;
    } else {
        int i, found_index = -1;
        uint64_t mask = 1;
        for (i = 0 ; i < DT_MAX_PREDEFINED ; ++i) {
            if (datatype->bdt_used & mask) {
                found_index = i;
                break;
            }
            mask *= 2;
        }
        primitive_datatype = (ompi_datatype_t*)
            ompi_ddt_basicDatatypes[found_index];
        primitive_count = datatype->nbElems;
    }

    *prim_datatype = primitive_datatype;
    *prim_count = primitive_count;

    return OMPI_SUCCESS;
}


struct ompi_osc_base_convertor_t {
    ompi_convertor_t convertor;
    ompi_op_t *op;
    ompi_datatype_t *datatype;
};
typedef struct ompi_osc_base_convertor_t ompi_osc_base_convertor_t;
static OBJ_CLASS_INSTANCE(ompi_osc_base_convertor_t, ompi_convertor_t, NULL, NULL);

#define COPY_TYPE( TYPENAME, TYPE, COUNT )                              \
static int copy_##TYPENAME( ompi_convertor_t *pConvertor, uint32_t count, \
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
COPY_TYPE( char, char, 1 )
COPY_TYPE( short, short, 1 )
COPY_TYPE( int, int, 1 )
COPY_TYPE( long, long, 1 )
COPY_TYPE( long_long, long long, 1 )
COPY_TYPE( float, float, 1 )
COPY_TYPE( double, double, 1 )
COPY_TYPE( long_double, long double, 1 )
COPY_TYPE( complex_float, ompi_complex_float_t, 1 )
COPY_TYPE( complex_double, ompi_complex_double_t, 1 )
COPY_TYPE( complex_long_double, ompi_complex_long_double_t, 1 )

/* table of predefined copy functions - one for each MPI type */
static conversion_fct_t ompi_osc_base_copy_functions[DT_MAX_PREDEFINED] = {
   (conversion_fct_t)NULL,                      /* DT_LOOP                */
   (conversion_fct_t)NULL,                      /* DT_END_LOOP            */
   (conversion_fct_t)NULL,                      /* DT_LB                  */
   (conversion_fct_t)NULL,                      /* DT_UB                  */
   (conversion_fct_t)copy_char,                 /* DT_CHAR                */
   (conversion_fct_t)copy_char,                 /* DT_CHARACTER           */
   (conversion_fct_t)copy_char,                 /* DT_UNSIGNED_CHAR       */
   (conversion_fct_t)copy_char,                 /* DT_SIGNED_CHAR       */
   (conversion_fct_t)copy_char,                 /* DT_BYTE                */
   (conversion_fct_t)copy_short,                /* DT_SHORT               */
   (conversion_fct_t)copy_short,                /* DT_UNSIGNED_SHORT      */
   (conversion_fct_t)copy_int,                  /* DT_INT                 */
   (conversion_fct_t)copy_int,                  /* DT_UNSIGNED_INT        */
   (conversion_fct_t)copy_long,                 /* DT_LONG                */
   (conversion_fct_t)copy_long,                 /* DT_UNSIGNED_LONG       */
   (conversion_fct_t)copy_long_long,            /* DT_LONG_LONG_INT       */
   (conversion_fct_t)copy_long_long,            /* DT_UNSIGNED_LONG_LONG  */
   (conversion_fct_t)copy_float,                /* DT_FLOAT               */
   (conversion_fct_t)copy_double,               /* DT_DOUBLE              */
   (conversion_fct_t)copy_long_double,          /* DT_LONG_DOUBLE         */
   (conversion_fct_t)NULL,                      /* DT_PACKED              */
   (conversion_fct_t)NULL,                      /* DT_WCHAR               */
#if SIZEOF_BOOL == SIZEOF_CHAR
   (conversion_fct_t)copy_char,                 /* DT_CXX_BOOL            */
#elif SIZEOF_BOOL == SIZEOF_SHORT
   (conversion_fct_t)copy_short,                /* DT_CXX_BOOL            */
#elif SIZEOF_BOOL == SIZEOF_INT
   (conversion_fct_t)copy_int,                  /* DT_CXX_BOOL            */
#elif SIZEOF_BOOL == SIZEOF_LONG
   (conversion_fct_t)copy_long,                 /* DT_CXX_BOOL            */
#else
   (conversion_fct_t)NULL,                      /* DT_CXX_BOOL            */
#endif
#if OMPI_SIZEOF_FORTRAN_LOGICAL == SIZEOF_CHAR
   (conversion_fct_t)copy_char,                 /* DT_LOGIC               */
#elif OMPI_SIZEOF_FORTRAN_LOGICAL == SIZEOF_SHORT
   (conversion_fct_t)copy_short,                /* DT_LOGIC               */
#elif OMPI_SIZEOF_FORTRAN_LOGICAL == SIZEOF_INT
   (conversion_fct_t)copy_int,                  /* DT_LOGIC               */
#elif OMPI_SIZEOF_FORTRAN_LOGICAL == SIZEOF_LONG
   (conversion_fct_t)copy_long,                 /* DT_LOGIC               */
#else
   (conversion_fct_t)NULL,                      /* DT_LOGIC               */
#endif
   (conversion_fct_t)copy_int,                  /* DT_INTEGER             */
   (conversion_fct_t)copy_float,                /* DT_REAL                */
   (conversion_fct_t)copy_double,               /* DT_DBLPREC             */
   (conversion_fct_t)copy_complex_float,        /* DT_COMPLEX_FLOAT       */
   (conversion_fct_t)copy_complex_double,       /* DT_COMPLEX_DOUBLE      */
   (conversion_fct_t)copy_complex_long_double,  /* DT_COMPLEX_LONG_DOUBLE */
   (conversion_fct_t)NULL,                      /* DT_2INT                */
   (conversion_fct_t)NULL,                      /* DT_2INTEGER            */
   (conversion_fct_t)NULL,                      /* DT_2REAL               */
   (conversion_fct_t)NULL,                      /* DT_2DBLPREC            */
   (conversion_fct_t)NULL,                      /* DT_2COMPLEX            */
   (conversion_fct_t)NULL,                      /* DT_2DOUBLE_COMPLEX     */
   (conversion_fct_t)NULL,                      /* DT_FLOAT_INT           */
   (conversion_fct_t)NULL,                      /* DT_DOUBLE_INT          */
   (conversion_fct_t)NULL,                      /* DT_LONG_DOUBLE_INT     */
   (conversion_fct_t)NULL,                      /* DT_LONG_INT            */
   (conversion_fct_t)NULL,                      /* DT_SHORT_INT           */
   (conversion_fct_t)NULL,                      /* DT_UNAVAILABLE         */
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

    if (ompi_ddt_is_predefined(datatype)) {
        ompi_op_reduce(op, inbuf, outbuf, count, datatype);
    } else {
        struct ompi_datatype_t *primitive_datatype = NULL;
        uint32_t primitive_count;
        ompi_osc_base_convertor_t convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;
        struct ompi_convertor_master_t master = {NULL, 0, 0, 0, {0, }, NULL};
        int i, found_index = -1;
        uint64_t mask = 1;

        for (i = 0 ; i < DT_MAX_PREDEFINED ; ++i) {
            if (datatype->bdt_used & mask) {
                found_index = i;
                break;
            }
            mask *= 2;
        }
        primitive_datatype = (ompi_datatype_t*)
            ompi_ddt_basicDatatypes[found_index];
        primitive_count = datatype->nbElems;

        /* create convertor */
        OBJ_CONSTRUCT(&convertor, ompi_osc_base_convertor_t);
        convertor.op = op;
        convertor.datatype = primitive_datatype;

        /* initialize convertor */
        ompi_convertor_copy_and_prepare_for_recv(ompi_proc_local()->proc_convertor,
                                                 datatype,
                                                 count,
                                                 outbuf,
                                                 0,
                                                 &convertor.convertor);

        memcpy(&master, convertor.convertor.master, sizeof(struct ompi_convertor_master_t));
        master.next = convertor.convertor.master;
        master.pFunctions = (conversion_fct_t*) &ompi_osc_base_copy_functions;
        convertor.convertor.master = &master;
        convertor.convertor.fAdvance = ompi_unpack_general;

        iov.iov_len  = inbuflen;
        iov.iov_base = (IOVBASE_TYPE*) inbuf;
        max_data     = iov.iov_len;
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &convertor.convertor);
        );
        ompi_convertor_unpack(&convertor.convertor, 
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
