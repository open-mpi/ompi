/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef DATATYPE_PROTOTYPES_H_HAS_BEEN_INCLUDED
#define DATATYPE_PROTOTYPES_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"

OMPI_DECLSPEC int32_t
ompi_pack_homogeneous_contig( ompi_convertor_t* pConv,
                          struct iovec* iov, uint32_t* out_size,
                          size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_pack_homogeneous_contig_checksum( ompi_convertor_t* pConv,
                                   struct iovec* iov, uint32_t* out_size,
                                   size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_pack_homogeneous_contig_with_gaps( ompi_convertor_t* pConv,
                                    struct iovec* iov, uint32_t* out_size,
                                    size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_pack_homogeneous_contig_with_gaps_checksum( ompi_convertor_t* pConv,
                                             struct iovec* iov, uint32_t* out_size,
                                             size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_generic_simple_pack( ompi_convertor_t* pConvertor,
                          struct iovec* iov, uint32_t* out_size,
                          size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_generic_simple_pack_checksum( ompi_convertor_t* pConvertor,
                                   struct iovec* iov, uint32_t* out_size,
                                   size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_unpack_general( ompi_convertor_t* pConvertor,
                     struct iovec* iov, uint32_t* out_size,
                     size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_unpack_general_checksum( ompi_convertor_t* pConvertor,
                              struct iovec* iov, uint32_t* out_size,
                              size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_unpack_homogeneous_contig( ompi_convertor_t* pConv,
                                struct iovec* iov, uint32_t* out_size,
                                size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_unpack_homogeneous_contig_checksum( ompi_convertor_t* pConv,
                                         struct iovec* iov, uint32_t* out_size,
                                         size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_generic_simple_unpack( ompi_convertor_t* pConvertor,
                            struct iovec* iov, uint32_t* out_size,
                            size_t* max_data );
OMPI_DECLSPEC int32_t
ompi_generic_simple_unpack_checksum( ompi_convertor_t* pConvertor,
                                     struct iovec* iov, uint32_t* out_size,
                                     size_t* max_data );

#endif  /* DATATYPE_PROTOTYPES_H_HAS_BEEN_INCLUDED */
