/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSHMEM_CONSTANTS_H
#define OSHMEM_CONSTANTS_H

#include "opal/constants.h"

#include "oshmem/include/shmem.h"


#define OSHMEM_ERR_BASE   OPAL_ERR_MAX

/* error codes */
enum {
    /* Error codes inherited from OPAL.  Still enum values so
       that we might get nice debugger help */
    OSHMEM_SUCCESS                  = OPAL_SUCCESS,

    OSHMEM_ERROR                    = OPAL_ERROR,
    OSHMEM_ERR_OUT_OF_RESOURCE      = OPAL_ERR_OUT_OF_RESOURCE,
    OSHMEM_ERR_TEMP_OUT_OF_RESOURCE = OPAL_ERR_TEMP_OUT_OF_RESOURCE,
    OSHMEM_ERR_RESOURCE_BUSY        = OPAL_ERR_RESOURCE_BUSY,
    OSHMEM_ERR_BAD_PARAM            = OPAL_ERR_BAD_PARAM,
    OSHMEM_ERR_FATAL                = OPAL_ERR_FATAL,
    OSHMEM_ERR_NOT_IMPLEMENTED      = OPAL_ERR_NOT_IMPLEMENTED,
    OSHMEM_ERR_NOT_SUPPORTED        = OPAL_ERR_NOT_SUPPORTED,
    OSHMEM_ERR_INTERUPTED           = OPAL_ERR_INTERRUPTED,
    OSHMEM_ERR_WOULD_BLOCK          = OPAL_ERR_WOULD_BLOCK,
    OSHMEM_ERR_IN_ERRNO             = OPAL_ERR_IN_ERRNO,
    OSHMEM_ERR_UNREACH              = OPAL_ERR_UNREACH,
    OSHMEM_ERR_NOT_FOUND            = OPAL_ERR_NOT_FOUND,
    OSHMEM_EXISTS                   = OPAL_EXISTS, /* indicates that the specified object already exists */
    OSHMEM_ERR_TIMEOUT              = OPAL_ERR_TIMEOUT,
    OSHMEM_ERR_NOT_AVAILABLE        = OPAL_ERR_NOT_AVAILABLE,
    OSHMEM_ERR_PERM                 = OPAL_ERR_PERM,
    OSHMEM_ERR_VALUE_OUT_OF_BOUNDS  = OPAL_ERR_VALUE_OUT_OF_BOUNDS,
    OSHMEM_ERR_FILE_READ_FAILURE    = OPAL_ERR_FILE_READ_FAILURE,
    OSHMEM_ERR_FILE_WRITE_FAILURE   = OPAL_ERR_FILE_WRITE_FAILURE,
    OSHMEM_ERR_FILE_OPEN_FAILURE    = OPAL_ERR_FILE_OPEN_FAILURE,

    OSHMEM_ERR_RECV_LESS_THAN_POSTED      = (OSHMEM_ERR_BASE - 1),
    OSHMEM_ERR_RECV_MORE_THAN_POSTED      = (OSHMEM_ERR_BASE - 2),
    OSHMEM_ERR_NO_MATCH_YET               = (OSHMEM_ERR_BASE - 3),
    OSHMEM_ERR_BUFFER                     = OPAL_ERR_BUFFER,
    OSHMEM_ERR_REQUEST                    = (OSHMEM_ERR_BASE - 4),
    OSHMEM_ERR_NO_CONNECTION_ALLOWED      = (OSHMEM_ERR_BASE - 5),
    OSHMEM_ERR_CONNECTION_REFUSED         = (OSHMEM_ERR_BASE - 6),
    OSHMEM_ERR_CONNECTION_FAILED          = OPAL_ERR_CONNECTION_FAILED,
    OSHMEM_PACK_MISMATCH                  = OPAL_ERR_PACK_MISMATCH,
    OSHMEM_ERR_PACK_FAILURE               = OPAL_ERR_PACK_FAILURE,
    OSHMEM_ERR_UNPACK_FAILURE             = OPAL_ERR_UNPACK_FAILURE,
    OSHMEM_ERR_COMM_FAILURE               = OPAL_ERR_COMM_FAILURE,
    OSHMEM_UNPACK_INADEQUATE_SPACE        = OPAL_ERR_UNPACK_INADEQUATE_SPACE,
    OSHMEM_UNPACK_READ_PAST_END_OF_BUFFER = OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER,
    OSHMEM_ERR_TYPE_MISMATCH              = OPAL_ERR_TYPE_MISMATCH,
    OSHMEM_ERR_COMPARE_FAILURE            = (OSHMEM_ERR_BASE - 9),
    OSHMEM_ERR_COPY_FAILURE               = (OSHMEM_ERR_BASE - 10),
    OSHMEM_ERR_UNKNOWN_DATA_TYPE          = OPAL_ERR_UNKNOWN_DATA_TYPE,
    OSHMEM_ERR_DATA_TYPE_REDEF            = OPAL_ERR_DATA_TYPE_REDEF,
    OSHMEM_ERR_DATA_OVERWRITE_ATTEMPT     = OPAL_ERR_DATA_OVERWRITE_ATTEMPT
};


/* C datatypes */
/*
 * SHMEM_Init_thread constants
 * Do not change the order of these without also modifying mpif.h.in.
 */
enum {
  SHMEM_NULL	= 0,
  SHMEM_CHAR,
  SHMEM_UCHAR,
  SHMEM_SCHAR,
  SHMEM_SHORT,
  SHMEM_USHORT,
  SHMEM_INT,
  SHMEM_UINT,
  SHMEM_LONG,
  SHMEM_ULONG,
  SHMEM_LLONG,
  SHMEM_BYTE,
  SHMEM_INT8_T,
  SHMEM_INT16_T,
  SHMEM_INT32_T,
  SHMEM_INT64_T,
  SHMEM_UINT8_T,
  SHMEM_UINT16_T,
  SHMEM_UINT32_T,
  SHMEM_UINT64_T,
  SHMEM_SIZE_T,
  SHMEM_PTRDIFF_T,
  SHMEM_ULLONG,
  SHMEM_FLOAT,
  SHMEM_DOUBLE,
  SHMEM_LDOUBLE,
  SHMEM_COMPLEXD,
  SHMEM_COMPLEXF,

  SHMEM_FINT,
  SHMEM_FINT4,
  SHMEM_FINT8
};


/*
 * Miscellaneous constants
 */
#define SHMEM_ANY_SOURCE         -1                      /* match any source rank */
#define SHMEM_PROC_NULL          -2                      /* rank of null process */
#define SHMEM_UNDEFINED          -32766                  /* undefined stuff */


#ifndef UNREFERENCED_PARAMETER
#define UNREFERENCED_PARAMETER(P) ((void)P)
#endif

#define OSHMEM_PREDEFINED_GLOBAL(type, global) ((type) ((void *) &(global)))

#if OPAL_WANT_MEMCHECKER
#define MEMCHECKER(x) do {       \
                x;                     \
       } while(0)
#else
#define MEMCHECKER(x)
#endif /* OPAL_WANT_MEMCHECKER */

#endif /* OSHMEM_CONSTANTS_H */

