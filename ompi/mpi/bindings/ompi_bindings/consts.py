# Copyright (c) 2024      Triad National Security, LLC. All rights
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
import json
from enum import Enum

# TODO: remove?
from pathlib import Path


FORTRAN_ERROR_NAME = 'ierror'
C_ERROR_NAME = 'ierr'
C_ERROR_TMP_NAME = 'c_ierr'
GENERATED_MESSAGE = 'THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT BY HAND.'

DIR = Path("/mnt/")

with open(DIR / "abi.json") as f:
    abi = json.load(f)
    consts = abi["constants"]
    categories = abi["categories"]

class Lang(Enum):
    C = 1
    CPP = 2
    FORTRAN = 3
#
# C and ABI constants
#
# C type: const int

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


# Inline function attributes
INLINE_ATTRS = '__opal_attribute_always_inline__ static inline'
