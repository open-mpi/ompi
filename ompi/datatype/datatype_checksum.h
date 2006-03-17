/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef DATATYPE_CHECKSUM_H_HAS_BEEN_INCLUDED
#define DATATYPE_CHECKSUM_H_HAS_BEEN_INCLUDED


#include "ompi/datatype/datatype_memcpy.h"
#include "opal/util/crc.h"

#if defined(CHECKSUM)

#define MEMCPY_CSUM( DST, SRC, BLENGTH, CONVERTOR ) \
do { \
    (CONVERTOR)->checksum += OPAL_CSUM_BCOPY_PARTIAL( (SRC), (DST), (BLENGTH), (BLENGTH), &(CONVERTOR)->csum_ui1, &(CONVERTOR)->csum_ui2 ); \
} while (0)

#define COMPUTE_CSUM( SRC, BLENGTH, CONVERTOR ) \
do { \
    (CONVERTOR)->checksum += OPAL_CSUM_PARTIAL( (SRC), (BLENGTH), &(CONVERTOR)->csum_ui1, &(CONVERTOR)->csum_ui2 ); \
} while (0)

#else

#define MEMCPY_CSUM( DST, SRC, BLENGTH, CONVERTOR ) \
    MEMCPY( (DST), (SRC), (BLENGTH) )

#define COMPUTE_CSUM( SRC, BLENGTH, CONVERTOR )

#endif
#endif  /* DATATYPE_CHECKSUM_H_HAS_BEEN_INCLUDED */
