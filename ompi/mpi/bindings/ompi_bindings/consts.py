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
    'MPI_ERR_ABI',
    'MPI_ERR_ACCESS',
    'MPI_ERR_AMODE',
    'MPI_ERR_ARG',
    'MPI_ERR_ASSERT',
    'MPI_ERR_BAD_FILE',
    'MPI_ERR_BASE',
    'MPI_ERR_BUFFER',
    'MPI_ERR_COMM',
    'MPI_ERR_CONVERSION',
    'MPI_ERR_COUNT',
    'MPI_ERR_DIMS',
    'MPI_ERR_DISP',
    'MPI_ERR_DUP_DATAREP',
    'MPI_ERR_ERRHANDLER',
    'MPI_ERR_FILE',
    'MPI_ERR_FILE_EXISTS',
    'MPI_ERR_FILE_IN_USE',
    'MPI_ERR_GROUP',
    'MPI_ERR_INFO',
    'MPI_ERR_INFO_KEY',
    'MPI_ERR_INFO_NOKEY',
    'MPI_ERR_INFO_VALUE',
    'MPI_ERR_INTERN',
    'MPI_ERR_IN_STATUS',
    'MPI_ERR_IO',
    'MPI_ERR_KEYVAL',
    'MPI_ERR_LOCKTYPE',
    'MPI_ERR_NAME',
    'MPI_ERR_NOT_SAME',
    'MPI_ERR_NO_MEM',
    'MPI_ERR_NO_SPACE',
    'MPI_ERR_NO_SUCH_FILE',
    'MPI_ERR_OP',
    'MPI_ERR_OTHER',
    'MPI_ERR_PENDING',
    'MPI_ERR_PORT',
    'MPI_ERR_PROC_ABORTED',
    'MPI_ERR_QUOTA',
    'MPI_ERR_RANK',
    'MPI_ERR_READ_ONLY',
    'MPI_ERR_REQUEST',
    'MPI_ERR_RMA_ATTACH',
    'MPI_ERR_RMA_CONFLICT',
    'MPI_ERR_RMA_FLAVOR',
    'MPI_ERR_RMA_RANGE',
    'MPI_ERR_RMA_SHARED',
    'MPI_ERR_RMA_SYNC',
    'MPI_ERR_ROOT',
    'MPI_ERR_SERVICE',
    'MPI_ERR_SESSION',
    'MPI_ERR_SIZE',
    'MPI_ERR_SPAWN',
    'MPI_ERR_TAG',
    'MPI_ERR_TOPOLOGY',
    'MPI_ERR_TRUNCATE',
    'MPI_ERR_TYPE',
    'MPI_ERR_UNKNOWN',
    'MPI_ERR_UNSUPPORTED_DATAREP',
    'MPI_ERR_UNSUPPORTED_OPERATION',
    'MPI_ERR_VALUE_TOO_LARGE',
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
    'MPI_DATATYPE_NULL',
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
    'MPI_LOGICAL',
    'MPI_INTEGER',
    'MPI_REAL',
    'MPI_COMPLEX',
    'MPI_DOUBLE_PRECISION',
    'MPI_CHARACTER',
    'MPI_DOUBLE_COMPLEX',
]

PREDEFINED_OPTIONAL_FORTRAN_DATATYPES = [
    'MPI_LOGICAL1',
    'MPI_LOGICAL2',
    'MPI_LOGICAL4',
    'MPI_LOGICAL8',
    'MPI_LOGICAL16',
    'MPI_INTEGER1',
    'MPI_INTEGER2',
    'MPI_INTEGER4',
    'MPI_INTEGER8',
    'MPI_INTEGER16',
    'MPI_REAL2',
    'MPI_REAL4',
    'MPI_REAL8',
    'MPI_REAL16',
    'MPI_COMPLEX4',
    'MPI_COMPLEX8',
    'MPI_COMPLEX16',
    'MPI_COMPLEX32',
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

SUBARRAY_ORDER_TYPES = [
    'MPI_ORDER_C',
    'MPI_ORDER_FORTRAN',
]

SUBARRAY_DISTRIB_TYPES = [
    'MPI_DISTRIBUTE_NONE',
    'MPI_DISTRIBUTE_BLOCK',
    'MPI_DISTRIBUTE_CYCLIC',
]

SUBARRAY_DARGS_TYPES = [
    'MPI_DISTRIBUTE_DFLT_DARG',
]

RESERVED_GROUPS = [
    'MPI_GROUP_NULL',
    'MPI_GROUP_EMPTY'
]

RESERVED_WINDOWS = [
    'MPI_WIN_NULL',
]

RESERVED_REQUESTS = [
    'MPI_REQUEST_NULL',
]

RESERVED_INFOS = [
    'MPI_INFO_NULL',
    'MPI_INFO_ENV',
]

RESERVED_FILES = [
    'MPI_FILE_NULL',
]

RESERVED_OPS = [
    'MPI_OP_NULL',
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

RESERVED_MESSAGES = [
     'MPI_MESSAGE_NULL',
     'MPI_MESSAGE_NO_PROC',
]

RESERVED_SESSIONS = [
    'MPI_SESSION_NULL',
]

RESERVED_ERRHANDLERS = [
    'MPI_ERRHANDLER_NULL',
    'MPI_ERRORS_ARE_FATAL',
    'MPI_ERRORS_ABORT',
    'MPI_ERRORS_RETURN',
]

RESERVED_COMM_COPY_ATTR_FNS = [
    'MPI_COMM_NULL_COPY_FN',
    'MPI_COMM_DUP_FN',
]

RESERVED_COMM_DEL_ATTR_FNS = [
    'MPI_COMM_NULL_DELETE_FN'
]

RESERVED_TYPE_COPY_ATTR_FNS = [
    'MPI_TYPE_NULL_COPY_FN',
    'MPI_TYPE_DUP_FN',
]

RESERVED_TYPE_DEL_ATTR_FNS = [
    'MPI_TYPE_NULL_DELETE_FN'
]

RESERVED_WIN_COPY_ATTR_FNS = [
    'MPI_WIN_NULL_COPY_FN',
    'MPI_WIN_DUP_FN',
]

RESERVED_WIN_DEL_ATTR_FNS = [
    'MPI_WIN_NULL_DELETE_FN'
]

IGNORED_STATUS_HANDLES = [
    'MPI_STATUSES_IGNORE',
    'MPI_STATUS_IGNORE',
]

COMM_GROUP_COMPARE_VALS = [
    'MPI_IDENT',
    'MPI_CONGRUENT',
    'MPI_SIMILAR',
    'MPI_UNEQUAL',
]

RESERVED_ATTR_KEYS = [
    'MPI_KEYVAL_INVALID',
    'MPI_TAG_UB',
    'MPI_IO',
    'MPI_HOST',
    'MPI_WTIME_IS_GLOBAL',
    'MPI_APPNUM',
    'MPI_LASTUSEDCODE',
    'MPI_UNIVERSE_SIZE',
    'MPI_WIN_BASE',
    'MPI_WIN_DISP_UNIT',
    'MPI_WIN_SIZE',
    'MPI_WIN_CREATE_FLAVOR',
    'MPI_WIN_MODEL',
]

TS_LEVEL_VALUES = [
    'MPI_THREAD_SINGLE',
    'MPI_THREAD_FUNNELED',
    'MPI_THREAD_SERIALIZED',
    'MPI_THREAD_MULTIPLE',
]


T_SCOPE_VALUES = [
    'MPI_T_SCOPE_CONSTANT',
    'MPI_T_SCOPE_READONLY',
    'MPI_T_SCOPE_LOCAL',
    'MPI_T_SCOPE_GROUP',
    'MPI_T_SCOPE_GROUP_EQ',
    'MPI_T_SCOPE_ALL',
    'MPI_T_SCOPE_ALL_EQ',
]

RESERVED_TAGS = [
    'MPI_ANY_TAG',
]

RESERVED_SOURCE = [
    'MPI_ANY_SOURCE',
    'MPI_PROC_NULL',
]

RESERVED_ROOT = [
    'MPI_ROOT',
    'MPI_PROC_NULL',
]

RESERVED_WEIGHTS = [
    'MPI_UNWEIGHTED',
    'MPI_WEIGHTS_EMPTY'
]

RESERVED_PVAR_SESSIONS = [
    'MPI_T_PVAR_SESSION_NULL',
]

RESERVED_CVAR_HANDLES = [
    'MPI_T_CVAR_HANDLE_NULL',
]
    
RESERVED_PVAR_HANDLES = [
    'MPI_T_PVAR_HANDLE_NULL',
]

RESERVED_T_ENUMS = [
    'MPI_T_ENUM_NULL',
]

T_BIND_VALUES = [
    'MPI_T_BIND_NO_OBJECT',
    'MPI_T_BIND_MPI_COMM',
    'MPI_T_BIND_MPI_DATATYPE',
    'MPI_T_BIND_MPI_ERRHANDLER',
    'MPI_T_BIND_MPI_FILE',
    'MPI_T_BIND_MPI_GROUP',
    'MPI_T_BIND_MPI_OP',
    'MPI_T_BIND_MPI_REQUEST',
    'MPI_T_BIND_MPI_WIN',
    'MPI_T_BIND_MPI_MESSAGE',
    'MPI_T_BIND_MPI_INFO',
    'MPI_T_BIND_MPI_SESSION',
]

T_VERBOSITY_VALUES = [
    'MPI_T_VERBOSITY_USER_BASIC',
    'MPI_T_VERBOSITY_USER_DETAIL',
    'MPI_T_VERBOSITY_USER_ALL',
    'MPI_T_VERBOSITY_TUNER_BASIC',
    'MPI_T_VERBOSITY_TUNER_DETAIL',
    'MPI_T_VERBOSITY_TUNER_ALL',
    'MPI_T_VERBOSITY_MPIDEV_BASIC',
    'MPI_T_VERBOSITY_MPIDEV_DETAIL',
    'MPI_T_VERBOSITY_MPIDEV_ALL',
]

T_CB_SAFETY_VALUES = [
    'MPI_T_CB_REQUIRE_NONE',
    'MPI_T_CB_REQUIRE_MPI_RESTRICTED',
    'MPI_T_CB_REQUIRE_THREAD_SAFE',
    'MPI_T_CB_REQUIRE_ASYNC_SIGNAL_SAFE',
]

T_SOURCE_ORDER_VALUES = [
    'MPI_T_SOURCE_ORDERED',
    'MPI_T_SOURCE_UNORDERED',
]

T_PVAR_CLASS_VALUES = [
    'MPI_T_PVAR_CLASS_STATE',
    'MPI_T_PVAR_CLASS_LEVEL',
    'MPI_T_PVAR_CLASS_SIZE',
    'MPI_T_PVAR_CLASS_PERCENTAGE',
    'MPI_T_PVAR_CLASS_HIGHWATERMARK',
    'MPI_T_PVAR_CLASS_LOWWATERMARK',
    'MPI_T_PVAR_CLASS_COUNTER',
    'MPI_T_PVAR_CLASS_AGGREGATE',
    'MPI_T_PVAR_CLASS_TIMER',
    'MPI_T_PVAR_CLASS_GENERIC',
]

VARIOUS_CONSTANTS = {
    # Just setting this to the same as ompi ABI for right now, but will need to
    # match the standard ABI value when defined
    'MPI_MAX_LIBRARY_VERSION_STRING': 256,
    'MPI_MAX_PROCESSOR_NAME': 256,
}

MAX_STRING_LEN_CONSTANTS = [
    'MPI_MAX_DATAREP_STRING',
    'MPI_MAX_ERROR_STRING',
    'MPI_MAX_INFO_KEY',
    'MPI_MAX_INFO_VAL',
    'MPI_MAX_LIBRARY_VERSION_STRING',
    'MPI_MAX_OBJECT_NAME',
    'MPI_MAX_PORT_NAME',
    'MPI_MAX_PROCESSOR_NAME',
    'MPI_MAX_STRINGTAG_LEN',
    'MPI_MAX_PSET_NAME_LEN',
]

MODE_BITS = [
    'MPI_MODE_APPEND',
    'MPI_MODE_CREATE',
    'MPI_MODE_DELETE_ON_CLOSE',
    'MPI_MODE_EXCL',
    'MPI_MODE_RDONLY',
    'MPI_MODE_RDWR',
    'MPI_MODE_SEQUENTIAL',
    'MPI_MODE_UNIQUE_OPEN',
    'MPI_MODE_WRONLY',
]

RMA_MODE_BITS = [
    'MPI_MODE_NOCHECK',
    'MPI_MODE_NOPRECEDE',
    'MPI_MODE_NOPUT',
    'MPI_MODE_NOSTORE',
    'MPI_MODE_NOSUCCEED',
]

WHENCE_VALUES = [
    'MPI_SEEK_CUR',
    'MPI_SEEK_END',
    'MPI_SEEK_SET',
]

COMBINER_VALUES = [
    'MPI_COMBINER_NAMED',
    'MPI_COMBINER_DUP',
    'MPI_COMBINER_CONTIGUOUS',
    'MPI_COMBINER_VECTOR',
    'MPI_COMBINER_HVECTOR',
    'MPI_COMBINER_INDEXED',
    'MPI_COMBINER_HINDEXED',
    'MPI_COMBINER_INDEXED_BLOCK',
    'MPI_COMBINER_HINDEXED_BLOCK',
    'MPI_COMBINER_STRUCT',
    'MPI_COMBINER_SUBARRAY',
    'MPI_COMBINER_DARRAY',
    'MPI_COMBINER_F90_REAL',
    'MPI_COMBINER_F90_COMPLEX',
    'MPI_COMBINER_F90_INTEGER',
    'MPI_COMBINER_RESIZED',
    'MPI_COMBINER_VALUE_INDEX',
]

TYPECLASS_VALUES = [
    'MPI_TYPECLASS_INTEGER',
    'MPI_TYPECLASS_REAL',
    'MPI_TYPECLASS_COMPLEX',
]

WIN_LOCK_VALUES = [
    'MPI_LOCK_EXCLUSIVE',
    'MPI_LOCK_SHARED',
]
 
TOPO_VALUES = [
    'MPI_CART',
    'MPI_GRAPH',
    'MPI_DIST_GRAPH',
]

BUFFER_VALUES = [
    'MPI_BOTTOM',
    'MPI_BUFFER_AUTOMATIC',
    'MPI_IN_PLACE',
]

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

#
# C objects that can have attributes cached on them
#
C_ATTRIBUTE_OBJS = [
    'MPI_Comm',
    'MPI_Type',
    'MPI_Win',
]

class ConvertFuncs:
    """Names of conversion functions (between standard ABI and OMPI ABI)."""

    ERROR_CLASS = 'ompi_convert_abi_error_intern_error'
    ERRHANDLER = 'ompi_convert_abi_errorhandler_intern_errorhandler'
    COMM = 'ompi_convert_abi_comm_intern_comm'
    DATATYPE = 'ompi_convert_abi_datatype_intern_datatype'
    GROUP = 'ompi_convert_abi_group_intern_group'
    REQUEST = 'ompi_convert_abi_request_intern_request'
    STATUS = 'ompi_convert_abi_status_intern_status'
    MESSAGE = 'ompi_convert_abi_message_intern_message'
    OP = 'ompi_convert_abi_op_intern_op'
    SESSION = 'ompi_convert_abi_session_intern_session'
    WIN = 'ompi_convert_abi_win_intern_win'
    INFO = 'ompi_convert_abi_info_intern_info'
    FILE = 'ompi_convert_abi_file_intern_file'
    TS_LEVEL = 'ompi_convert_abi_ts_level_intern_ts_level'
    TAG = 'ompi_convert_abi_tag_intern_tag'
    ROOT = 'ompi_convert_abi_root_intern_root'
    SOURCE = 'ompi_convert_abi_source_intern_source'
    PVAR_SESSION = 'ompi_convert_abi_pvar_session_intern_pvar_session'
    PVAR_CLASS = 'ompi_convert_abi_pvar_class_intern_pvar_class'
    PVAR_HANDLE = 'ompi_convert_abi_pvar_handle_intern_pvar_handle'
    T_VERBOSITY = 'ompi_convert_abi_t_verbosity_intern_t_verbosity'
    CVAR_HANDLE = 'ompi_convert_abi_cvar_handle_intern_cvar_handle'
    T_ENUM = 'ompi_convert_abi_t_enum_intern_t_enum'
    T_BIND = 'ompi_convert_abi_t_bind_intern_t_bind'
    T_CB_SAFETY = 'ompi_convert_abi_cb_safety_intern_cb_safety'
    T_SOURCE_ORDER = 'ompi_convert_abi_source_order_intern_source_order'
    ATTR_KEY = 'ompi_convert_abi_attr_key_intern_attr_key'
    COMM_COPY_ATTR_FUNCTION = 'ompi_convert_comm_copy_attr_fn_intern_comm_copy_attr_fn'
    COMM_DELETE_ATTR_FUNCTION = 'ompi_convert_comm_delete_attr_fn_intern_comm_delete_attr_fn'
    TYPE_COPY_ATTR_FUNCTION = 'ompi_convert_type_copy_attr_fn_intern_type_copy_attr_fn'
    TYPE_DELETE_ATTR_FUNCTION = 'ompi_convert_type_delete_attr_fn_intern_type_delete_attr_fn'
    WIN_COPY_ATTR_FUNCTION = 'ompi_convert_win_copy_attr_fn_intern_win_copy_attr_fn'
    WIN_DELETE_ATTR_FUNCTION = 'ompi_convert_win_delete_attr_fn_intern_win_delete_attr_fn'
    SPLIT_TYPE = 'ompi_convert_split_type_intern_type'
    WEIGHTS = 'ompi_convert_weight_intern_weight'
    SUBARRAY_ORDER = 'ompi_convert_subarray_order_intern_subarray_order'
    SUBARRAY_DISTRIB_TYPES = 'ompi_convert_subarray_distrib_type_intern_distrib_type'
    SUBARRAY_DARGS_TYPES = 'ompi_convert_subarray_dargs_type_intern_dargs_type'
    MODE_BITS = 'ompi_convert_mode_bits_intern_mode_bits'
    RMA_MODE_BITS = 'ompi_convert_rma_mode_bits_intern_mode_bits'
    WHENCE = 'ompi_convert_whence_intern_whence'
    TYPECLASS = 'ompi_convert_typeclass_intern_typeclass'
    WIN_LOCK = 'ompi_convert_lock_assert_intern_lock_assert'
    BUFFER = 'ompi_convert_abi_buffer_intern_buffer'

class ConvertOMPIToStandard:
    """Generated function for converting from OMPI to standard ABI."""

    COMM = 'ompi_convert_comm_ompi_to_standard'
    ERROR_CLASS = 'ompi_convert_intern_error_abi_error'
    ERRHANDLER = 'ompi_convert_intern_errorhandler_abi_errorhandler'
    GROUP = 'ompi_convert_group_ompi_to_standard'
    DATATYPE = 'ompi_convert_datatype_ompi_to_standard'
    FILE = 'ompi_convert_file_ompi_to_standard'
    MESSAGE = 'ompi_convert_message_ompi_to_standard'
    OP = 'ompi_convert_op_ompi_to_standard'
    SESSION = 'ompi_convert_session_ompi_to_standard'
    STATUS = 'ompi_convert_intern_status_abi_status'
    WIN = 'ompi_convert_win_ompi_to_standard'
    REQUEST = 'ompi_convert_ompi_request_abi_request'
    INFO = 'ompi_convert_info_ompi_to_standard'
    TS_LEVEL = 'ompi_convert_ts_level_ompi_to_standard'
    TAG = 'ompi_convert_tag_ompi_to_standard'
    PVAR_SESSION = 'ompi_convert_pvar_session_ompi_to_standard'
    PVAR_CLASS = 'ompi_convert_pvar_class_ompi_to_standard'
    PVAR_HANDLE = 'ompi_convert_pvar_handle_ompi_to_standard'
    T_VERBOSITY = 'ompi_convert_t_verbosity_ompi_to_standard'
    CVAR_HANDLE = 'ompi_convert_cvar_handle_ompi_to_standard'
    T_ENUM = 'ompi_convert_t_enum_ompi_to_standard'
    T_BIND = 'ompi_convert_t_bind_ompi_to_standard'
    T_SOURCE_ORDER = 'ompi_convert_source_order_ompi_to_standard'
    ATTR_KEY = 'ompi_convert_attr_key_ompi_to_standard'
    COMM_CMP = 'ompi_convert_comm_cmp_ompi_to_standard'
    SOURCE = 'ompi_convert_source_ompi_to_standard'
    SUBARRAY_ORDER = 'ompi_convert_subarray_order_ompi_to_standard'
    MODE_BITS = 'ompi_convert_mode_bits_to_standard'
    RMA_MODE_BITS = 'ompi_convert_rma_mode_bits_to_standard'
    COMBINER = 'ompi_convert_combiner_to_standard'
    TOPO = 'ompi_convert_topo_to_standard'
    BUFFER = 'ompi_convert_buffer_to_standard'


# Inline function attributes
INLINE_ATTRS = '__opal_attribute_always_inline__ static inline'
