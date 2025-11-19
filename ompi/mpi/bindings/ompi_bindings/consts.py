# Copyright (c) 2024-2025 Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
"""Constants used for generating bindings."""
import re

FORTRAN_ERROR_NAME = 'ierror'
C_ERROR_NAME = 'ierr'
C_ERROR_TMP_NAME = 'c_ierr'
GENERATED_MESSAGE = 'THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT BY HAND.'

#
# C and ABI constants
#
# C type: const int
ERROR_CLASSES = [
    'MPI_SUCCESS',
    'MPI_ERR_BUFFER',
    'MPI_ERR_COUNT',
    'MPI_ERR_NOTIFY_IDX'
    'MPI_ERR_TYPE',
    'MPI_ERR_TAG',
    'MPI_ERR_COMM',
    'MPI_ERR_RANK',
    'MPI_ERR_REQUEST',
    'MPI_ERR_ROOT',
    'MPI_ERR_GROUP',
    'MPI_ERR_OP',
    'MPI_ERR_TOPOLOGY',
    'MPI_ERR_DIMS',
    'MPI_ERR_ARG',
    'MPI_ERR_UNKNOWN',
    'MPI_ERR_TRUNCATE',
    'MPI_ERR_OTHER',
    'MPI_ERR_INTERN',
    'MPI_ERR_PENDING',
    'MPI_ERR_IN_STATUS',
    'MPI_ERR_ACCESS',
    'MPI_ERR_AMODE',
    'MPI_ERR_ASSERT',
    'MPI_ERR_BAD_FILE',
    'MPI_ERR_BASE',
    'MPI_ERR_CONVERSION',
    'MPI_ERR_DISP',
    'MPI_ERR_DUP_DATAREP',
    'MPI_ERR_FILE_EXISTS',
    'MPI_ERR_FILE_IN_USE',
    'MPI_ERR_FILE',
    'MPI_ERR_INFO_KEY',
    'MPI_ERR_INFO_NOKEY',
    'MPI_ERR_INFO_VALUE',
    'MPI_ERR_INFO',
    'MPI_ERR_IO',
    'MPI_ERR_KEYVAL',
    'MPI_ERR_LOCKTYPE',
    'MPI_ERR_NAME',
    'MPI_ERR_NO_MEM',
    'MPI_ERR_NOT_SAME',
    'MPI_ERR_NO_SPACE',
    'MPI_ERR_NO_SUCH_FILE',
    'MPI_ERR_PORT',
    'MPI_ERR_PROC_ABORTED',
    'MPI_ERR_QUOTA',
    'MPI_ERR_READ_ONLY',
    'MPI_ERR_RMA_ATTACH',
    'MPI_ERR_RMA_CONFLICT',
    'MPI_ERR_RMA_RANGE',
    'MPI_ERR_RMA_SHARED',
    'MPI_ERR_RMA_SYNC',
    'MPI_ERR_RMA_FLAVOR',
    'MPI_ERR_SERVICE',
    'MPI_ERR_SESSION',
    'MPI_ERR_SIZE',
    'MPI_ERR_SPAWN',
    'MPI_ERR_UNSUPPORTED_DATAREP',
    'MPI_ERR_UNSUPPORTED_OPERATION',
    'MPI_ERR_WIN',
    'MPI_T_ERR_CANNOT_INIT',
    'MPI_T_ERR_NOT_INITIALIZED',
    'MPI_T_ERR_MEMORY',
    'MPI_T_ERR_INVALID',
    'MPI_T_ERR_INVALID_INDEX',
    'MPI_T_ERR_INVALID_ITEM',
    'MPI_T_ERR_INVALID_SESSION',
    'MPI_T_ERR_INVALID_HANDLE',
    'MPI_T_ERR_INVALID_NAME',
    'MPI_T_ERR_OUT_OF_HANDLES',
    'MPI_T_ERR_OUT_OF_SESSIONS',
    'MPI_T_ERR_CVAR_SET_NOT_NOW',
    'MPI_T_ERR_CVAR_SET_NEVER',
    'MPI_T_ERR_PVAR_NO_WRITE',
    'MPI_T_ERR_PVAR_NO_STARTSTOP',
    'MPI_T_ERR_PVAR_NO_ATOMIC',
    'MPI_ERR_LASTCODE',
]

PREDEFINED_DATATYPES = [
    'MPI_CHAR',
    'MPI_SHORT',
    'MPI_INT',
    'MPI_LONG',
    'MPI_LONG_LONG_INT',
    'MPI_LONG_LONG',
    'MPI_SIGNED_CHAR',
    'MPI_UNSIGNED_CHAR',
    'MPI_UNSIGNED_SHORT',
    'MPI_UNSIGNED',
    'MPI_UNSIGNED_LONG',
    'MPI_UNSIGNED_LONG_LONG',
    'MPI_FLOAT',
    'MPI_DOUBLE',
    'MPI_LONG_DOUBLE',
    'MPI_WCHAR',
    'MPI_C_BOOL',
    'MPI_INT8_T',
    'MPI_INT16_T',
    'MPI_INT32_T',
    'MPI_INT64_T',
    'MPI_UINT8_T',
    'MPI_UINT16_T',
    'MPI_UINT32_T',
    'MPI_UINT64_T',
    'MPI_AINT',
    'MPI_COUNT',
    'MPI_OFFSET',
    'MPI_C_COMPLEX',
    'MPI_C_FLOAT_COMPLEX',
    'MPI_C_DOUBLE_COMPLEX',
    'MPI_C_LONG_DOUBLE_COMPLEX',
    'MPI_BYTE',
    'MPI_PACKED',
    'MPI_CXX_BOOL',
    'MPI_CXX_FLOAT_COMPLEX',
    'MPI_CXX_DOUBLE_COMPLEX',
    'MPI_CXX_LONG_DOUBLE_COMPLEX',
    'MPI_FLOAT_INT',
    'MPI_DOUBLE_INT',
    'MPI_LONG_INT',
    'MPI_2INT',
    'MPI_SHORT_INT',
    'MPI_LONG_DOUBLE_INT',
]

# C type: MPI_Comm
RESERVED_COMMUNICATORS = [
    'MPI_COMM_NULL',
    'MPI_COMM_WORLD',
    'MPI_COMM_SELF',
]

COMMUNICATOR_SPLIT_TYPES = [
    'MPI_COMM_TYPE_SHARED',
    'MPI_COMM_TYPE_HW_UNGUIDED',
    'MPI_COMM_TYPE_HW_GUIDED',
    'MPI_COMM_TYPE_RESOURCE_GUIDED',
]

RESERVED_WINDOWS = [
    'MPI_WIN_NULL',
]

RESERVED_REQUESTS = [
    'MPI_REQUEST_NULL',
]

RESERVED_INFOS = [
    'MPI_INFO_ENV',
    'MPI_INFO_NULL',
]

RESERVED_FILES = [
    'MPI_FILE_NULL',
]

IGNORED_STATUS_HANDLES = [
    'MPI_STATUSES_IGNORE',
    'MPI_STATUS_IGNORE',
]

COLLECTIVE_OPERATIONS = [
    'MPI_MAX',
    'MPI_MIN',
    'MPI_SUM',
    'MPI_PROD',
    'MPI_MAXLOC',
    'MPI_MINLOC',
    'MPI_BAND',
    'MPI_BOR',
    'MPI_BXOR',
    'MPI_LAND',
    'MPI_LOR',
    'MPI_LXOR',
    'MPI_REPLACE',
    'MPI_NO_OP',
]

VARIOUS_CONSTANTS = {
    # Just setting this to the same as ompi ABI for right now, but will need to
    # match the standard ABI value when defined
    'MPI_MAX_LIBRARY_VERSION_STRING': 256,
    'MPI_MAX_PROCESSOR_NAME': 256,
}

# Types

C_OPAQUE_TYPES = {
    'MPI_Aint': 'intptr_t',
    'MPI_Offset': 'int64_t',
    'MPI_Count': 'size_t',
    # The below type needs to be set externally depending on Fortran compiler
    'MPI_Fint': 'int64_t',
}

C_HANDLES = [
    'MPI_Comm',
    'MPI_Datatype',
    'MPI_Errhandler',
    'MPI_File',
    'MPI_Group',
    'MPI_Info',
    'MPI_Message',
    'MPI_Op',
    'MPI_Request',
    'MPI_Session',
    'MPI_Win',
]

class ConvertFuncs:
    """Names of conversion functions (between standard ABI and OMPI ABI)."""

    ERROR_CLASS = 'ompi_convert_intern_error_abi_error'
    COMM = 'ompi_convert_abi_comm_intern_comm'
    DATATYPE = 'ompi_convert_abi_datatype_intern_datatype'
    REQUEST = 'ompi_convert_abi_request_intern_request'
    STATUS = 'ompi_convert_intern_status_abi_status'
    OP = 'ompi_convert_abi_op_intern_op'
    WIN = 'ompi_convert_abi_win_intern_win'
    INFO = 'ompi_convert_abi_info_intern_info'
    FILE = 'ompi_convert_abi_file_intern_file'


class ConvertOMPIToStandard:
    """Generated function for converting from OMPI to standard ABI."""

    COMM = 'ompi_convert_comm_ompi_to_standard'


# Inline function attributes
INLINE_ATTRS = '__opal_attribute_always_inline__ static inline'
