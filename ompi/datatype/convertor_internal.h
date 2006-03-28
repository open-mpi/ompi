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
#ifndef CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED
#define CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"

typedef struct ompi_convertor_master_t {
    struct ompi_convertor_master_t* next;
    uint32_t                        remote_arch;
    const int32_t                   remote_sizes[DT_MAX_PREDEFINED];
} ompi_convertor_master_t;

ompi_convertor_master_t* ompi_convertor_find_or_create_master( uint32_t remote_arch );

#endif  /* CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED */
