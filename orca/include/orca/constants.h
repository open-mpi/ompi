/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORCA_CONSTANTS_H
#define ORCA_CONSTANTS_H

#include "opal/constants.h"
#include "orca_config.h"

BEGIN_C_DECLS

#define ORCA_ERR_BASE            OPAL_ERR_MAX


enum {
    /* Error codes inherited from OPAL.  Still enum values so that we
       get the nice debugger help. */

    ORCA_SUCCESS                            = OPAL_SUCCESS,

    ORCA_ERROR                              = OPAL_ERROR,
    ORCA_ERR_OUT_OF_RESOURCE                = OPAL_ERR_OUT_OF_RESOURCE,
    ORCA_ERR_TEMP_OUT_OF_RESOURCE           = OPAL_ERR_TEMP_OUT_OF_RESOURCE,
    ORCA_ERR_RESOURCE_BUSY                  = OPAL_ERR_RESOURCE_BUSY,
    ORCA_ERR_BAD_PARAM                      = OPAL_ERR_BAD_PARAM,
    ORCA_ERR_FATAL                          = OPAL_ERR_FATAL,
    ORCA_ERR_NOT_IMPLEMENTED                = OPAL_ERR_NOT_IMPLEMENTED,
    ORCA_ERR_NOT_SUPPORTED                  = OPAL_ERR_NOT_SUPPORTED,
    ORCA_ERR_INTERUPTED                     = OPAL_ERR_INTERUPTED,
    ORCA_ERR_WOULD_BLOCK                    = OPAL_ERR_WOULD_BLOCK,
    ORCA_ERR_IN_ERRNO                       = OPAL_ERR_IN_ERRNO,
    ORCA_ERR_UNREACH                        = OPAL_ERR_UNREACH,
    ORCA_ERR_NOT_FOUND                      = OPAL_ERR_NOT_FOUND,
    ORCA_EXISTS                             = OPAL_EXISTS,
    ORCA_ERR_TIMEOUT                        = OPAL_ERR_TIMEOUT,
    ORCA_ERR_NOT_AVAILABLE                  = OPAL_ERR_NOT_AVAILABLE,
    ORCA_ERR_PERM                           = OPAL_ERR_PERM,
    ORCA_ERR_VALUE_OUT_OF_BOUNDS            = OPAL_ERR_VALUE_OUT_OF_BOUNDS,
    ORCA_ERR_FILE_READ_FAILURE              = OPAL_ERR_FILE_READ_FAILURE,
    ORCA_ERR_FILE_WRITE_FAILURE             = OPAL_ERR_FILE_WRITE_FAILURE,
    ORCA_ERR_FILE_OPEN_FAILURE              = OPAL_ERR_FILE_OPEN_FAILURE,
    ORCA_ERR_PACK_MISMATCH                  = OPAL_ERR_PACK_MISMATCH,
    ORCA_ERR_PACK_FAILURE                   = OPAL_ERR_PACK_FAILURE,
    ORCA_ERR_UNPACK_FAILURE                 = OPAL_ERR_UNPACK_FAILURE,
    ORCA_ERR_UNPACK_INADEQUATE_SPACE        = OPAL_ERR_UNPACK_INADEQUATE_SPACE,
    ORCA_ERR_UNPACK_READ_PAST_END_OF_BUFFER = OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER,
    ORCA_ERR_TYPE_MISMATCH                  = OPAL_ERR_TYPE_MISMATCH,
    ORCA_ERR_OPERATION_UNSUPPORTED          = OPAL_ERR_OPERATION_UNSUPPORTED, 
    ORCA_ERR_UNKNOWN_DATA_TYPE              = OPAL_ERR_UNKNOWN_DATA_TYPE,
    ORCA_ERR_BUFFER                         = OPAL_ERR_BUFFER,
    ORCA_ERR_DATA_TYPE_REDEF                = OPAL_ERR_DATA_TYPE_REDEF,
    ORCA_ERR_DATA_OVERWRITE_ATTEMPT         = OPAL_ERR_DATA_OVERWRITE_ATTEMPT,
    ORCA_ERR_MODULE_NOT_FOUND               = OPAL_ERR_MODULE_NOT_FOUND,
    ORCA_ERR_TOPO_SLOT_LIST_NOT_SUPPORTED   = OPAL_ERR_TOPO_SLOT_LIST_NOT_SUPPORTED,
    ORCA_ERR_TOPO_SOCKET_NOT_SUPPORTED      = OPAL_ERR_TOPO_SOCKET_NOT_SUPPORTED,
    ORCA_ERR_TOPO_CORE_NOT_SUPPORTED        = OPAL_ERR_TOPO_CORE_NOT_SUPPORTED,
    ORCA_ERR_NOT_ENOUGH_SOCKETS             = OPAL_ERR_NOT_ENOUGH_SOCKETS,
    ORCA_ERR_NOT_ENOUGH_CORES               = OPAL_ERR_NOT_ENOUGH_CORES,
    ORCA_ERR_INVALID_PHYS_CPU               = OPAL_ERR_INVALID_PHYS_CPU,
    ORCA_ERR_MULTIPLE_AFFINITIES            = OPAL_ERR_MULTIPLE_AFFINITIES,
    ORCA_ERR_SLOT_LIST_RANGE                = OPAL_ERR_SLOT_LIST_RANGE,
    ORCA_ERR_SILENT                         = OPAL_ERR_SILENT,

/* error codes specific to ORCA - don't forget to update
    orca/util/error_strings.c when adding new error codes!!
    Otherwise, the error reporting system will potentially crash,
    or at the least not be able to report the new error correctly.
 */
    ORCA_ERR_RECV_LESS_THAN_POSTED          = (ORCA_ERR_BASE -  1),
    ORCA_ERR_RECV_MORE_THAN_POSTED          = (ORCA_ERR_BASE -  2),
    ORCA_ERR_NO_MATCH_YET                   = (ORCA_ERR_BASE -  3),
    ORCA_ERR_REQUEST                        = (ORCA_ERR_BASE -  4),
    ORCA_ERR_NO_CONNECTION_ALLOWED          = (ORCA_ERR_BASE -  5),
    ORCA_ERR_CONNECTION_REFUSED             = (ORCA_ERR_BASE -  6),
    ORCA_ERR_CONNECTION_FAILED              = (ORCA_ERR_BASE -  7),
    ORCA_ERR_COMM_FAILURE                   = (ORCA_ERR_BASE -  8),
    ORCA_ERR_COMPARE_FAILURE                = (ORCA_ERR_BASE -  9),
    ORCA_ERR_COPY_FAILURE                   = (ORCA_ERR_BASE - 10),
    ORCA_ERR_PROC_STATE_MISSING             = (ORCA_ERR_BASE - 11),
    ORCA_ERR_PROC_EXIT_STATUS_MISSING       = (ORCA_ERR_BASE - 12),
    ORCA_ERR_INDETERMINATE_STATE_INFO       = (ORCA_ERR_BASE - 13),
    ORCA_ERR_NODE_FULLY_USED                = (ORCA_ERR_BASE - 14),
    ORCA_ERR_INVALID_NUM_PROCS              = (ORCA_ERR_BASE - 15),
    ORCA_ERR_ADDRESSEE_UNKNOWN              = (ORCA_ERR_BASE - 16),
    ORCA_ERR_SYS_LIMITS_PIPES               = (ORCA_ERR_BASE - 17),
    ORCA_ERR_PIPE_SETUP_FAILURE             = (ORCA_ERR_BASE - 18),
    ORCA_ERR_SYS_LIMITS_CHILDREN            = (ORCA_ERR_BASE - 19),
    ORCA_ERR_FAILED_GET_TERM_ATTRS          = (ORCA_ERR_BASE - 20),
    ORCA_ERR_WDIR_NOT_FOUND                 = (ORCA_ERR_BASE - 21),
    ORCA_ERR_EXE_NOT_FOUND                  = (ORCA_ERR_BASE - 22),
    ORCA_ERR_PIPE_READ_FAILURE              = (ORCA_ERR_BASE - 23),
    ORCA_ERR_EXE_NOT_ACCESSIBLE             = (ORCA_ERR_BASE - 24),
    ORCA_ERR_FAILED_TO_START                = (ORCA_ERR_BASE - 25),
    ORCA_ERR_FILE_NOT_EXECUTABLE            = (ORCA_ERR_BASE - 26),
    ORCA_ERR_HNP_COULD_NOT_START            = (ORCA_ERR_BASE - 27),
    ORCA_ERR_SYS_LIMITS_SOCKETS             = (ORCA_ERR_BASE - 28),
    ORCA_ERR_SOCKET_NOT_AVAILABLE           = (ORCA_ERR_BASE - 29),
    ORCA_ERR_SYSTEM_WILL_BOOTSTRAP          = (ORCA_ERR_BASE - 30),
    ORCA_ERR_RESTART_LIMIT_EXCEEDED         = (ORCA_ERR_BASE - 31),
    ORCA_ERR_INVALID_NODE_RANK              = (ORCA_ERR_BASE - 32),
    ORCA_ERR_INVALID_LOCAL_RANK             = (ORCA_ERR_BASE - 33),
    ORCA_ERR_UNRECOVERABLE                  = (ORCA_ERR_BASE - 34),
    ORCA_ERR_MEM_LIMIT_EXCEEDED             = (ORCA_ERR_BASE - 35),
    ORCA_ERR_HEARTBEAT_LOST                 = (ORCA_ERR_BASE - 36),
    ORCA_ERR_PROC_STALLED                   = (ORCA_ERR_BASE - 37),
    ORCA_ERR_NO_APP_SPECIFIED               = (ORCA_ERR_BASE - 38),
    ORCA_ERR_NO_EXE_SPECIFIED               = (ORCA_ERR_BASE - 39),
    ORCA_ERR_COMM_DISABLED                  = (ORCA_ERR_BASE - 40),
    ORCA_ERR_FAILED_TO_MAP                  = (ORCA_ERR_BASE - 41),
    ORCA_ERR_TAKE_NEXT_OPTION               = (ORCA_ERR_BASE - 42),
    ORCA_ERR_SENSOR_LIMIT_EXCEEDED          = (ORCA_ERR_BASE - 43)
};

#define ORCA_ERR_MAX                      (ORCA_ERR_BASE - 100)

END_C_DECLS

#endif /* ORCA_CONSTANTS_H */

