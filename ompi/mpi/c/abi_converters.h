/*
 * Copyright (c) 2025 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _ABI_CONVERTORS_
#define _ABI_CONVERTORS_

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

__opal_attribute_always_inline__ static inline int ompi_convert_abi_error_intern_error(int error_class)
{
    switch (error_class) {
        case MPI_SUCCESS_ABI_INTERNAL:
        return MPI_SUCCESS;
        case MPI_ERR_BUFFER_ABI_INTERNAL:
        return MPI_ERR_BUFFER;
        case MPI_ERR_COUNT_ABI_INTERNAL:
        return MPI_ERR_COUNT;
        case MPI_ERR_TYPE_ABI_INTERNAL:
        return MPI_ERR_TYPE;
        case MPI_ERR_TAG_ABI_INTERNAL:
        return MPI_ERR_TAG;
        case MPI_ERR_COMM_ABI_INTERNAL:
        return MPI_ERR_COMM;
        case MPI_ERR_RANK_ABI_INTERNAL:
        return MPI_ERR_RANK;
        case MPI_ERR_REQUEST_ABI_INTERNAL:
        return MPI_ERR_REQUEST;
        case MPI_ERR_ROOT_ABI_INTERNAL:
        return MPI_ERR_ROOT;
        case MPI_ERR_GROUP_ABI_INTERNAL:
        return MPI_ERR_GROUP;
        case MPI_ERR_OP_ABI_INTERNAL:
        return MPI_ERR_OP;
        case MPI_ERR_TOPOLOGY_ABI_INTERNAL:
        return MPI_ERR_TOPOLOGY;
        case MPI_ERR_DIMS_ABI_INTERNAL:
        return MPI_ERR_DIMS;
        case MPI_ERR_ARG_ABI_INTERNAL:
        return MPI_ERR_ARG;
        case MPI_ERR_UNKNOWN_ABI_INTERNAL:
        return MPI_ERR_UNKNOWN;
        case MPI_ERR_TRUNCATE_ABI_INTERNAL:
        return MPI_ERR_TRUNCATE;
        case MPI_ERR_OTHER_ABI_INTERNAL:
        return MPI_ERR_OTHER;
        case MPI_ERR_INTERN_ABI_INTERNAL:
        return MPI_ERR_INTERN;
        case MPI_ERR_PENDING_ABI_INTERNAL:
        return MPI_ERR_PENDING;
        case MPI_ERR_IN_STATUS_ABI_INTERNAL:
        return MPI_ERR_IN_STATUS;
        case MPI_ERR_ACCESS_ABI_INTERNAL:
        return MPI_ERR_ACCESS;
        case MPI_ERR_AMODE_ABI_INTERNAL:
        return MPI_ERR_AMODE;
        case MPI_ERR_ASSERT_ABI_INTERNAL:
        return MPI_ERR_ASSERT;
        case MPI_ERR_BAD_FILE_ABI_INTERNAL:
        return MPI_ERR_BAD_FILE;
        case MPI_ERR_BASE_ABI_INTERNAL:
        return MPI_ERR_BASE;
        case MPI_ERR_CONVERSION_ABI_INTERNAL:
        return MPI_ERR_CONVERSION;
        case MPI_ERR_DISP_ABI_INTERNAL:
        return MPI_ERR_DISP;
        case MPI_ERR_DUP_DATAREP_ABI_INTERNAL:
        return MPI_ERR_DUP_DATAREP;
        case MPI_ERR_FILE_EXISTS_ABI_INTERNAL:
        return MPI_ERR_FILE_EXISTS;
        case MPI_ERR_FILE_IN_USE_ABI_INTERNAL:
        return MPI_ERR_FILE_IN_USE;
        case MPI_ERR_FILE_ABI_INTERNAL:
        return MPI_ERR_FILE;
        case MPI_ERR_INFO_KEY_ABI_INTERNAL:
        return MPI_ERR_INFO_KEY;
        case MPI_ERR_INFO_NOKEY_ABI_INTERNAL:
        return MPI_ERR_INFO_NOKEY;
        case MPI_ERR_INFO_VALUE_ABI_INTERNAL:
        return MPI_ERR_INFO_VALUE;
        case MPI_ERR_INFO_ABI_INTERNAL:
        return MPI_ERR_INFO;
        case MPI_ERR_IO_ABI_INTERNAL:
        return MPI_ERR_IO;
        case MPI_ERR_KEYVAL_ABI_INTERNAL:
        return MPI_ERR_KEYVAL;
        case MPI_ERR_LOCKTYPE_ABI_INTERNAL:
        return MPI_ERR_LOCKTYPE;
        case MPI_ERR_NAME_ABI_INTERNAL:
        return MPI_ERR_NAME;
        case MPI_ERR_NO_MEM_ABI_INTERNAL:
        return MPI_ERR_NO_MEM;
        case MPI_ERR_NOT_SAME_ABI_INTERNAL:
        return MPI_ERR_NOT_SAME;
        case MPI_ERR_NO_SPACE_ABI_INTERNAL:
        return MPI_ERR_NO_SPACE;
        case MPI_ERR_NO_SUCH_FILE_ABI_INTERNAL:
        return MPI_ERR_NO_SUCH_FILE;
        case MPI_ERR_PORT_ABI_INTERNAL:
        return MPI_ERR_PORT;
        case MPI_ERR_PROC_ABORTED_ABI_INTERNAL:
        return MPI_ERR_PROC_ABORTED;
        case MPI_ERR_QUOTA_ABI_INTERNAL:
        return MPI_ERR_QUOTA;
        case MPI_ERR_READ_ONLY_ABI_INTERNAL:
        return MPI_ERR_READ_ONLY;
        case MPI_ERR_RMA_ATTACH_ABI_INTERNAL:
        return MPI_ERR_RMA_ATTACH;
        case MPI_ERR_RMA_CONFLICT_ABI_INTERNAL:
        return MPI_ERR_RMA_CONFLICT;
        case MPI_ERR_RMA_RANGE_ABI_INTERNAL:
        return MPI_ERR_RMA_RANGE;
        case MPI_ERR_RMA_SHARED_ABI_INTERNAL:
        return MPI_ERR_RMA_SHARED;
        case MPI_ERR_RMA_SYNC_ABI_INTERNAL:
        return MPI_ERR_RMA_SYNC;
        case MPI_ERR_RMA_FLAVOR_ABI_INTERNAL:
        return MPI_ERR_RMA_FLAVOR;
        case MPI_ERR_SERVICE_ABI_INTERNAL:
        return MPI_ERR_SERVICE;
        case MPI_ERR_SESSION_ABI_INTERNAL:
        return MPI_ERR_SESSION;
        case MPI_ERR_SIZE_ABI_INTERNAL:
        return MPI_ERR_SIZE;
        case MPI_ERR_SPAWN_ABI_INTERNAL:
        return MPI_ERR_SPAWN;
        case MPI_ERR_UNSUPPORTED_DATAREP_ABI_INTERNAL:
        return MPI_ERR_UNSUPPORTED_DATAREP;
        case MPI_ERR_UNSUPPORTED_OPERATION_ABI_INTERNAL:
        return MPI_ERR_UNSUPPORTED_OPERATION;
        case MPI_ERR_WIN_ABI_INTERNAL:
        return MPI_ERR_WIN;
        case MPI_T_ERR_CANNOT_INIT_ABI_INTERNAL:
        return MPI_T_ERR_CANNOT_INIT;
        case MPI_T_ERR_NOT_INITIALIZED_ABI_INTERNAL:
        return MPI_T_ERR_NOT_INITIALIZED;
        case MPI_T_ERR_MEMORY_ABI_INTERNAL:
        return MPI_T_ERR_MEMORY;
        case MPI_T_ERR_INVALID_ABI_INTERNAL:
        return MPI_T_ERR_INVALID;
        case MPI_T_ERR_INVALID_INDEX_ABI_INTERNAL:
        return MPI_T_ERR_INVALID_INDEX;
        case MPI_T_ERR_INVALID_ITEM_ABI_INTERNAL:
        return MPI_T_ERR_INVALID_ITEM;
        case MPI_T_ERR_INVALID_SESSION_ABI_INTERNAL:
        return MPI_T_ERR_INVALID_SESSION;
        case MPI_T_ERR_INVALID_HANDLE_ABI_INTERNAL:
        return MPI_T_ERR_INVALID_HANDLE;
        case MPI_T_ERR_INVALID_NAME_ABI_INTERNAL:
        return MPI_T_ERR_INVALID_NAME;
        case MPI_T_ERR_OUT_OF_HANDLES_ABI_INTERNAL:
        return MPI_T_ERR_OUT_OF_HANDLES;
        case MPI_T_ERR_OUT_OF_SESSIONS_ABI_INTERNAL:
        return MPI_T_ERR_OUT_OF_SESSIONS;
        case MPI_T_ERR_CVAR_SET_NOT_NOW_ABI_INTERNAL:
        return MPI_T_ERR_CVAR_SET_NOT_NOW;
        case MPI_T_ERR_CVAR_SET_NEVER_ABI_INTERNAL:
        return MPI_T_ERR_CVAR_SET_NEVER;
        case MPI_T_ERR_PVAR_NO_WRITE_ABI_INTERNAL:
        return MPI_T_ERR_PVAR_NO_WRITE;
        case MPI_T_ERR_PVAR_NO_STARTSTOP_ABI_INTERNAL:
        return MPI_T_ERR_PVAR_NO_STARTSTOP;
        case MPI_T_ERR_PVAR_NO_ATOMIC_ABI_INTERNAL:
        return MPI_T_ERR_PVAR_NO_ATOMIC;
        case MPI_ERR_LASTCODE_ABI_INTERNAL:
        return MPI_ERR_LASTCODE;
        default:
        return error_class;
    }
}
__opal_attribute_always_inline__ static inline MPI_Comm ompi_convert_abi_comm_intern_comm(MPI_Comm_ABI_INTERNAL comm)
{
    if (MPI_COMM_NULL_ABI_INTERNAL == comm) {
        return MPI_COMM_NULL;
    } else if (MPI_COMM_WORLD_ABI_INTERNAL == comm) {
        return MPI_COMM_WORLD;
    } else if (MPI_COMM_SELF_ABI_INTERNAL == comm) {
        return MPI_COMM_SELF;
    }
    return (MPI_Comm) comm;
}
__opal_attribute_always_inline__ static inline MPI_Comm_ABI_INTERNAL ompi_convert_comm_ompi_to_standard(MPI_Comm comm)
{
    if (MPI_COMM_NULL == comm) {
        return MPI_COMM_NULL_ABI_INTERNAL;
    } else if (MPI_COMM_WORLD == comm) {
        return MPI_COMM_WORLD_ABI_INTERNAL;
    } else if (MPI_COMM_SELF == comm) {
        return MPI_COMM_SELF_ABI_INTERNAL;
    }
    return (MPI_Comm_ABI_INTERNAL) comm;
}
__opal_attribute_always_inline__ static inline MPI_Info ompi_convert_abi_info_intern_info(MPI_Info_ABI_INTERNAL info)
{
    if (MPI_INFO_ENV_ABI_INTERNAL == info) {
        return MPI_INFO_ENV;
    } else if (MPI_INFO_NULL_ABI_INTERNAL == info) {
        return MPI_INFO_NULL;
    }
    return (MPI_Info) info;
}
__opal_attribute_always_inline__ static inline MPI_Info_ABI_INTERNAL ompi_convert_info_ompi_to_standard(MPI_Info info)
{
    if (MPI_INFO_ENV == info) {
        return MPI_INFO_ENV_ABI_INTERNAL;
    } else if (MPI_INFO_NULL == info) {
        return MPI_INFO_NULL_ABI_INTERNAL;
    }
    return (MPI_Info_ABI_INTERNAL) info;
}
__opal_attribute_always_inline__ static inline MPI_File ompi_convert_abi_file_intern_file(MPI_File_ABI_INTERNAL file)
{
    if (MPI_FILE_NULL_ABI_INTERNAL == file) {
        return MPI_FILE_NULL;
    }
    return (MPI_File) file;
}
__opal_attribute_always_inline__ static inline MPI_File_ABI_INTERNAL ompi_convert_file_ompi_to_standard(MPI_File file)
{
    if (MPI_FILE_NULL == file) {
        return MPI_FILE_NULL_ABI_INTERNAL;
    }
    return (MPI_File_ABI_INTERNAL) file;
}
__opal_attribute_always_inline__ static inline MPI_Group ompi_convert_abi_group_intern_group(MPI_Group_ABI_INTERNAL group)
{
    if (MPI_GROUP_NULL_ABI_INTERNAL == group) {
        return MPI_GROUP_NULL;
    }
    return (MPI_Group) group;
}
__opal_attribute_always_inline__ static inline MPI_Group_ABI_INTERNAL ompi_convert_group_ompi_to_standard(MPI_Group group)
{
    if (MPI_GROUP_NULL == group) {
        return MPI_GROUP_NULL_ABI_INTERNAL;
    }
    return (MPI_Group_ABI_INTERNAL) group;
}
__opal_attribute_always_inline__ static inline MPI_Datatype ompi_convert_abi_datatype_intern_datatype(MPI_Datatype_ABI_INTERNAL datatype)
{
    if (MPI_CHAR_ABI_INTERNAL == datatype) {
        return MPI_CHAR;
    } else if (MPI_SHORT_ABI_INTERNAL == datatype) {
        return MPI_SHORT;
    } else if (MPI_INT_ABI_INTERNAL == datatype) {
        return MPI_INT;
    } else if (MPI_LONG_ABI_INTERNAL == datatype) {
        return MPI_LONG;
    } else if (MPI_LONG_LONG_INT_ABI_INTERNAL == datatype) {
        return MPI_LONG_LONG_INT;
    } else if (MPI_LONG_LONG_ABI_INTERNAL == datatype) {
        return MPI_LONG_LONG;
    } else if (MPI_SIGNED_CHAR_ABI_INTERNAL == datatype) {
        return MPI_SIGNED_CHAR;
    } else if (MPI_UNSIGNED_CHAR_ABI_INTERNAL == datatype) {
        return MPI_UNSIGNED_CHAR;
    } else if (MPI_UNSIGNED_SHORT_ABI_INTERNAL == datatype) {
        return MPI_UNSIGNED_SHORT;
    } else if (MPI_UNSIGNED_ABI_INTERNAL == datatype) {
        return MPI_UNSIGNED;
    } else if (MPI_UNSIGNED_LONG_ABI_INTERNAL == datatype) {
        return MPI_UNSIGNED_LONG;
    } else if (MPI_UNSIGNED_LONG_LONG_ABI_INTERNAL == datatype) {
        return MPI_UNSIGNED_LONG_LONG;
    } else if (MPI_FLOAT_ABI_INTERNAL == datatype) {
        return MPI_FLOAT;
    } else if (MPI_DOUBLE_ABI_INTERNAL == datatype) {
        return MPI_DOUBLE;
    } else if (MPI_LONG_DOUBLE_ABI_INTERNAL == datatype) {
        return MPI_LONG_DOUBLE;
    } else if (MPI_WCHAR_ABI_INTERNAL == datatype) {
        return MPI_WCHAR;
    } else if (MPI_C_BOOL_ABI_INTERNAL == datatype) {
        return MPI_C_BOOL;
    } else if (MPI_INT8_T_ABI_INTERNAL == datatype) {
        return MPI_INT8_T;
    } else if (MPI_INT16_T_ABI_INTERNAL == datatype) {
        return MPI_INT16_T;
    } else if (MPI_INT32_T_ABI_INTERNAL == datatype) {
        return MPI_INT32_T;
    } else if (MPI_INT64_T_ABI_INTERNAL == datatype) {
        return MPI_INT64_T;
    } else if (MPI_UINT8_T_ABI_INTERNAL == datatype) {
        return MPI_UINT8_T;
    } else if (MPI_UINT16_T_ABI_INTERNAL == datatype) {
        return MPI_UINT16_T;
    } else if (MPI_UINT32_T_ABI_INTERNAL == datatype) {
        return MPI_UINT32_T;
    } else if (MPI_UINT64_T_ABI_INTERNAL == datatype) {
        return MPI_UINT64_T;
    } else if (MPI_AINT_ABI_INTERNAL == datatype) {
        return MPI_AINT;
    } else if (MPI_COUNT_ABI_INTERNAL == datatype) {
        return MPI_COUNT;
    } else if (MPI_OFFSET_ABI_INTERNAL == datatype) {
        return MPI_OFFSET;
    } else if (MPI_C_COMPLEX_ABI_INTERNAL == datatype) {
        return MPI_C_COMPLEX;
    } else if (MPI_C_FLOAT_COMPLEX_ABI_INTERNAL == datatype) {
        return MPI_C_FLOAT_COMPLEX;
    } else if (MPI_C_DOUBLE_COMPLEX_ABI_INTERNAL == datatype) {
        return MPI_C_DOUBLE_COMPLEX;
    } else if (MPI_C_LONG_DOUBLE_COMPLEX_ABI_INTERNAL == datatype) {
        return MPI_C_LONG_DOUBLE_COMPLEX;
    } else if (MPI_BYTE_ABI_INTERNAL == datatype) {
        return MPI_BYTE;
    } else if (MPI_PACKED_ABI_INTERNAL == datatype) {
        return MPI_PACKED;
    } else if (MPI_CXX_BOOL_ABI_INTERNAL == datatype) {
        return MPI_CXX_BOOL;
    } else if (MPI_CXX_FLOAT_COMPLEX_ABI_INTERNAL == datatype) {
        return MPI_CXX_FLOAT_COMPLEX;
    } else if (MPI_CXX_DOUBLE_COMPLEX_ABI_INTERNAL == datatype) {
        return MPI_CXX_DOUBLE_COMPLEX;
    } else if (MPI_CXX_LONG_DOUBLE_COMPLEX_ABI_INTERNAL == datatype) {
        return MPI_CXX_LONG_DOUBLE_COMPLEX;
    } else if (MPI_FLOAT_INT_ABI_INTERNAL == datatype) {
        return MPI_FLOAT_INT;
    } else if (MPI_DOUBLE_INT_ABI_INTERNAL == datatype) {
        return MPI_DOUBLE_INT;
    } else if (MPI_LONG_INT_ABI_INTERNAL == datatype) {
        return MPI_LONG_INT;
    } else if (MPI_2INT_ABI_INTERNAL == datatype) {
        return MPI_2INT;
    } else if (MPI_SHORT_INT_ABI_INTERNAL == datatype) {
        return MPI_SHORT_INT;
    } else if (MPI_LONG_DOUBLE_INT_ABI_INTERNAL == datatype) {
        return MPI_LONG_DOUBLE_INT;
    }
    return (MPI_Datatype) datatype;
}
__opal_attribute_always_inline__ static inline MPI_Datatype_ABI_INTERNAL ompi_convert_datatype_ompi_to_standard(MPI_Datatype datatype)
{
    if (MPI_CHAR == datatype) {
        return MPI_CHAR_ABI_INTERNAL;
    } else if (MPI_SHORT == datatype) {
        return MPI_SHORT_ABI_INTERNAL;
    } else if (MPI_INT == datatype) {
        return MPI_INT_ABI_INTERNAL;
    } else if (MPI_LONG == datatype) {
        return MPI_LONG_ABI_INTERNAL;
    } else if (MPI_LONG_LONG_INT == datatype) {
        return MPI_LONG_LONG_INT_ABI_INTERNAL;
    } else if (MPI_LONG_LONG == datatype) {
        return MPI_LONG_LONG_ABI_INTERNAL;
    } else if (MPI_SIGNED_CHAR == datatype) {
        return MPI_SIGNED_CHAR_ABI_INTERNAL;
    } else if (MPI_UNSIGNED_CHAR == datatype) {
        return MPI_UNSIGNED_CHAR_ABI_INTERNAL;
    } else if (MPI_UNSIGNED_SHORT == datatype) {
        return MPI_UNSIGNED_SHORT_ABI_INTERNAL;
    } else if (MPI_UNSIGNED == datatype) {
        return MPI_UNSIGNED_ABI_INTERNAL;
    } else if (MPI_UNSIGNED_LONG == datatype) {
        return MPI_UNSIGNED_LONG_ABI_INTERNAL;
    } else if (MPI_UNSIGNED_LONG_LONG == datatype) {
        return MPI_UNSIGNED_LONG_LONG_ABI_INTERNAL;
    } else if (MPI_FLOAT == datatype) {
        return MPI_FLOAT_ABI_INTERNAL;
    } else if (MPI_DOUBLE == datatype) {
        return MPI_DOUBLE_ABI_INTERNAL;
    } else if (MPI_LONG_DOUBLE == datatype) {
        return MPI_LONG_DOUBLE_ABI_INTERNAL;
    } else if (MPI_WCHAR == datatype) {
        return MPI_WCHAR_ABI_INTERNAL;
    } else if (MPI_C_BOOL == datatype) {
        return MPI_C_BOOL_ABI_INTERNAL;
    } else if (MPI_INT8_T == datatype) {
        return MPI_INT8_T_ABI_INTERNAL;
    } else if (MPI_INT16_T == datatype) {
        return MPI_INT16_T_ABI_INTERNAL;
    } else if (MPI_INT32_T == datatype) {
        return MPI_INT32_T_ABI_INTERNAL;
    } else if (MPI_INT64_T == datatype) {
        return MPI_INT64_T_ABI_INTERNAL;
    } else if (MPI_UINT8_T == datatype) {
        return MPI_UINT8_T_ABI_INTERNAL;
    } else if (MPI_UINT16_T == datatype) {
        return MPI_UINT16_T_ABI_INTERNAL;
    } else if (MPI_UINT32_T == datatype) {
        return MPI_UINT32_T_ABI_INTERNAL;
    } else if (MPI_UINT64_T == datatype) {
        return MPI_UINT64_T_ABI_INTERNAL;
    } else if (MPI_AINT == datatype) {
        return MPI_AINT_ABI_INTERNAL;
    } else if (MPI_COUNT == datatype) {
        return MPI_COUNT_ABI_INTERNAL;
    } else if (MPI_OFFSET == datatype) {
        return MPI_OFFSET_ABI_INTERNAL;
    } else if (MPI_C_COMPLEX == datatype) {
        return MPI_C_COMPLEX_ABI_INTERNAL;
    } else if (MPI_C_FLOAT_COMPLEX == datatype) {
        return MPI_C_FLOAT_COMPLEX_ABI_INTERNAL;
    } else if (MPI_C_DOUBLE_COMPLEX == datatype) {
        return MPI_C_DOUBLE_COMPLEX_ABI_INTERNAL;
    } else if (MPI_C_LONG_DOUBLE_COMPLEX == datatype) {
        return MPI_C_LONG_DOUBLE_COMPLEX_ABI_INTERNAL;
    } else if (MPI_BYTE == datatype) {
        return MPI_BYTE_ABI_INTERNAL;
    } else if (MPI_PACKED == datatype) {
        return MPI_PACKED_ABI_INTERNAL;
    } else if (MPI_CXX_BOOL == datatype) {
        return MPI_CXX_BOOL_ABI_INTERNAL;
    } else if (MPI_CXX_FLOAT_COMPLEX == datatype) {
        return MPI_CXX_FLOAT_COMPLEX_ABI_INTERNAL;
    } else if (MPI_CXX_DOUBLE_COMPLEX == datatype) {
        return MPI_CXX_DOUBLE_COMPLEX_ABI_INTERNAL;
    } else if (MPI_CXX_LONG_DOUBLE_COMPLEX == datatype) {
        return MPI_CXX_LONG_DOUBLE_COMPLEX_ABI_INTERNAL;
    } else if (MPI_FLOAT_INT == datatype) {
        return MPI_FLOAT_INT_ABI_INTERNAL;
    } else if (MPI_DOUBLE_INT == datatype) {
        return MPI_DOUBLE_INT_ABI_INTERNAL;
    } else if (MPI_LONG_INT == datatype) {
        return MPI_LONG_INT_ABI_INTERNAL;
    } else if (MPI_2INT == datatype) {
        return MPI_2INT_ABI_INTERNAL;
    } else if (MPI_SHORT_INT == datatype) {
        return MPI_SHORT_INT_ABI_INTERNAL;
    } else if (MPI_LONG_DOUBLE_INT == datatype) {
        return MPI_LONG_DOUBLE_INT_ABI_INTERNAL;
    }
    return (MPI_Datatype_ABI_INTERNAL) datatype;
}
__opal_attribute_always_inline__ static inline MPI_Errhandler ompi_convert_abi_errorhandler_intern_errorhandler(MPI_Errhandler_ABI_INTERNAL errorhandler)
{
    if (MPI_ERRHANDLER_NULL_ABI_INTERNAL == errorhandler) {
        return MPI_ERRHANDLER_NULL;
    }
    return (MPI_Errhandler) errorhandler;
}
__opal_attribute_always_inline__ static inline MPI_Errhandler_ABI_INTERNAL ompi_convert_intern_errorhandler_abi_errorhandler(MPI_Errhandler errorhandler)
{
    if (MPI_ERRHANDLER_NULL == errorhandler) {
        return MPI_ERRHANDLER_NULL_ABI_INTERNAL;
    }
    return (MPI_Errhandler_ABI_INTERNAL) errorhandler;
}
__opal_attribute_always_inline__ static inline MPI_Message ompi_convert_abi_message_intern_message(MPI_Message_ABI_INTERNAL message)
{
    if (MPI_MESSAGE_NULL_ABI_INTERNAL == message) {
        return MPI_MESSAGE_NULL;
    } else if (MPI_MESSAGE_NO_PROC_ABI_INTERNAL == message) {
        return MPI_MESSAGE_NO_PROC;
    }
    return (MPI_Message) message;
}
__opal_attribute_always_inline__ static inline MPI_Message_ABI_INTERNAL ompi_convert_message_ompi_to_standard(MPI_Message message)
{
    if (MPI_MESSAGE_NULL == message) {
        return MPI_MESSAGE_NULL_ABI_INTERNAL;
    } else if (MPI_MESSAGE_NO_PROC == message) {
        return MPI_MESSAGE_NO_PROC_ABI_INTERNAL;
    }
    return (MPI_Message_ABI_INTERNAL) message;
}
__opal_attribute_always_inline__ static inline MPI_Op ompi_convert_abi_op_intern_op(MPI_Op_ABI_INTERNAL op)
{
    if (MPI_MAX_ABI_INTERNAL == op) {
        return MPI_MAX;
    } else if (MPI_MIN_ABI_INTERNAL == op) {
        return MPI_MIN;
    } else if (MPI_SUM_ABI_INTERNAL == op) {
        return MPI_SUM;
    } else if (MPI_PROD_ABI_INTERNAL == op) {
        return MPI_PROD;
    } else if (MPI_MAXLOC_ABI_INTERNAL == op) {
        return MPI_MAXLOC;
    } else if (MPI_MINLOC_ABI_INTERNAL == op) {
        return MPI_MINLOC;
    } else if (MPI_BAND_ABI_INTERNAL == op) {
        return MPI_BAND;
    } else if (MPI_BOR_ABI_INTERNAL == op) {
        return MPI_BOR;
    } else if (MPI_BXOR_ABI_INTERNAL == op) {
        return MPI_BXOR;
    } else if (MPI_LAND_ABI_INTERNAL == op) {
        return MPI_LAND;
    } else if (MPI_LOR_ABI_INTERNAL == op) {
        return MPI_LOR;
    } else if (MPI_LXOR_ABI_INTERNAL == op) {
        return MPI_LXOR;
    } else if (MPI_REPLACE_ABI_INTERNAL == op) {
        return MPI_REPLACE;
    } else if (MPI_NO_OP_ABI_INTERNAL == op) {
        return MPI_NO_OP;
    }
    return (MPI_Op) op;
}
__opal_attribute_always_inline__ static inline MPI_Op_ABI_INTERNAL ompi_convert_op_ompi_to_standard(MPI_Op op)
{
    if (MPI_OP_NULL == op) {
        return MPI_OP_NULL_ABI_INTERNAL;
    }
    return (MPI_Op_ABI_INTERNAL) op;
}
__opal_attribute_always_inline__ static inline MPI_Session ompi_convert_abi_session_intern_session(MPI_Session_ABI_INTERNAL session)
{
    if (MPI_SESSION_NULL_ABI_INTERNAL == session) {
        return MPI_SESSION_NULL;
    }
    return (MPI_Session) session;
}
__opal_attribute_always_inline__ static inline MPI_Session_ABI_INTERNAL ompi_convert_session_ompi_to_standard(MPI_Session session)
{
    if (MPI_SESSION_NULL == session) {
        return MPI_SESSION_NULL_ABI_INTERNAL;
    }
    return (MPI_Session_ABI_INTERNAL) session;
}
__opal_attribute_always_inline__ static inline MPI_Win ompi_convert_abi_win_intern_win(MPI_Win_ABI_INTERNAL win)
{
    if (MPI_WIN_NULL_ABI_INTERNAL == win) {
        return MPI_WIN_NULL;
    }
    return (MPI_Win) win;
}
__opal_attribute_always_inline__ static inline MPI_Win_ABI_INTERNAL ompi_convert_win_ompi_to_standard(MPI_Win win)
{
    if (MPI_WIN_NULL == win) {
        return MPI_WIN_NULL_ABI_INTERNAL;
    }
    return (MPI_Win_ABI_INTERNAL) win;
}
__opal_attribute_always_inline__ static inline void ompi_convert_abi_request_intern_request(MPI_Request_ABI_INTERNAL *ptr)
{
    if (MPI_REQUEST_NULL == (MPI_Request) *ptr) {
        *ptr = MPI_REQUEST_NULL_ABI_INTERNAL;
    }
}
__opal_attribute_always_inline__ static inline MPI_Request_ABI_INTERNAL ompi_convert_ompi_request_abi_request(MPI_Request request)
{
    if (MPI_REQUEST_NULL == request) {
        return MPI_REQUEST_NULL_ABI_INTERNAL;
    }
    return (MPI_Request_ABI_INTERNAL) request;
}
__opal_attribute_always_inline__ static inline void ompi_convert_abi_status_intern_status(MPI_Status *out, MPI_Status_ABI_INTERNAL *inp)
{
    void *ptr = &out->_ucount;
    out->MPI_SOURCE = inp->MPI_SOURCE;
    out->MPI_TAG = inp->MPI_TAG;
    out->_cancelled = inp->MPI_Internal[0];
    memcpy(ptr, &inp->MPI_Internal[1],sizeof(out->_ucount));
    out->MPI_ERROR = ompi_convert_abi_error_intern_error(inp->MPI_ERROR);
}
__opal_attribute_always_inline__ static inline void ompi_convert_intern_status_abi_status(MPI_Status_ABI_INTERNAL *out, MPI_Status *inp)
{
    void *ptr = &out->MPI_Internal[1];
    out->MPI_SOURCE = inp->MPI_SOURCE;
    out->MPI_TAG = inp->MPI_TAG;
    out->MPI_Internal[0] =inp->_cancelled;
    memcpy(ptr, &inp->_ucount,sizeof(inp->_ucount));
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /*  _ABI_CONVERTORS_ */
