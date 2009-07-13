/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_CONVERTOR_H
#define OMPI_CONVERTOR_H

#include "ompi_config.h"

#include <stddef.h>

#include "opal/datatype/opal_convertor.h"
#include "ompi/datatype/ompi_datatype.h"

/*
 * XXX TODO To be deleted again.
 * Very small interface to have code, which depends on ompi_convertor_prepare... interface
 * to work, still...
 *
 * However, still any header #include "opal/datatype/opal_convertor.h" will need
 * to be renamed to #include "ompi/datatype/ompi_convertor.h"
 */
#warning "This header file should only be included as a convenience. Please use the opal_convert.h header, functions and macros"

#define ompi_convertor_t    opal_convertor_t

static inline int32_t ompi_convertor_prepare_for_send( opal_convertor_t* convertor,
                                                       const ompi_datatype_t* datatype,
                                                       int32_t count,
                                                       const void* pUserBuf)
{
    return opal_convertor_prepare_for_send( convertor,
                                            &(datatype->super),
                                            count,
                                            pUserBuf);
}

static inline int32_t ompi_convertor_copy_and_prepare_for_send( const opal_convertor_t* pSrcConv,
                                                                const ompi_datatype_t* datatype,
                                                                int32_t count,
                                                                const void* pUserBuf,
                                                                int32_t flags,
                                                                opal_convertor_t* convertor )
{
    return opal_convertor_copy_and_prepare_for_send( pSrcConv,
                                                     &(datatype->super),
                                                     count,
                                                     pUserBuf,
                                                     flags,
                                                     convertor );
}


static inline int32_t ompi_convertor_prepare_for_recv( opal_convertor_t* convertor,
                                                       const ompi_datatype_t* datatype,
                                                       int32_t count,
                                                       const void* pUserBuf )
{
    return opal_convertor_prepare_for_recv( convertor,
                                            &(datatype->super),
                                            count,
                                            pUserBuf );
}

static inline int32_t ompi_convertor_copy_and_prepare_for_recv( const opal_convertor_t* pSrcConv,
                                                                const ompi_datatype_t* datatype,
                                                                int32_t count,
                                                                const void* pUserBuf,
                                                                int32_t flags,
                                                                opal_convertor_t* convertor )
{
    return opal_convertor_copy_and_prepare_for_recv( pSrcConv,
                                                     &(datatype->super),
                                                     count,
                                                     pUserBuf,
                                                     flags,
                                                     convertor );
}

#endif /* OMPI_CONVERTOR_H */