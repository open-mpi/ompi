#!/usr/bin/env python3
# Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
# Copyright (c) 2023      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADERS$
#
#
"""MPI Standard ABI Generation.

TEMPLATE SOURCE FILE ASSUMPTIONS:
* Only one function per file
* Nothing (other than blank lines) after closing '}'
* Function prototype is preceded by PROTOTYPE
* All types in the function prototype are converted to one-word capital types
  as defined here (to be later converted to ompi or standard ABI types)
* Functions requiring a bigcount implementation should have type COUNT in
  place of MPI_Count or int for each count parameter. Bigcount functions will
  be generated automatically for any function that includes a COUNT type.
"""
from abc import ABC, abstractmethod
import argparse
import re
import sys
import os

# C type: const int
ERROR_CLASSES = [
    'MPI_SUCCESS',
    'MPI_ERR_BUFFER',
    'MPI_ERR_COUNT',
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


def mpi_fn_name_from_base_fn_name(name):
    """Convert from a base name to the standard 'MPI_*' name."""
    return f'MPI_{name.capitalize()}'


def abi_internal_name(extname):
    """Convert from the ABI external name to an internal name.

    Used to avoid conflicts with existing MPI names.
    """
    return f'{extname}_ABI_INTERNAL'


class ABIHeaderBuilder:
    """ABI header builder code."""

    def __init__(self, prototypes, external=False, file=sys.stdout):
        self.file = file
        self.external = external

        if external:
            mangle_name = lambda name: name
        else:
            mangle_name = abi_internal_name

        # Build up the list of standard ABI signatures
        signatures = []
        for prototype in prototypes:
            base_name = mpi_fn_name_from_base_fn_name(prototype.name)
            signatures.append(prototype.signature('standard', base_name,
                                                  mangle_name=mangle_name))
            # Profiling prototype
            signatures.append(prototype.signature('standard', f'P{base_name}',
                                                  mangle_name=mangle_name))
            if prototype.need_bigcount:
                signatures.append(prototype.signature('standard', f'{base_name}_c',
                                                      count_type='MPI_Count',
                                                      mangle_name=mangle_name))
                # Profiling prototype
                signatures.append(prototype.signature('standard', f'P{base_name}_c',
                                                      count_type='MPI_Count',
                                                      mangle_name=mangle_name))
        self.signatures = signatures

    def mangle_name(self, extname):
        """Mangle names, depending on whether building external or internal header."""
        if self.external:
            return extname
        return abi_internal_name(extname)

    def dump(self, *pargs, **kwargs):
        print(*pargs, **kwargs, file=self.file)

    def dump_lines(self, lines):
        lines = indent_lines(lines, 4 * ' ', start=1)
        for line in lines:
            self.dump(line)

    def generate_error_convert_fn(self):
        self.dump(f'static inline int {ConvertFuncs.ERROR_CLASS}(int error_class)')
        self.dump('{')
        lines = []
        lines.append('switch (error_class) {')
        for error in ERROR_CLASSES:
            lines.append(f'case {self.mangle_name(error)}:')
            lines.append(f'return {error};')
        lines.append('default:')
        lines.append('return error_class;')
        lines.append('}')
        self.dump_lines(lines)
        self.dump('}')

    def generic_convert(self, fn_name, param_name, type_, value_names):
        intern_type = self.mangle_name(type_)
        self.dump(f'static inline {type_} {fn_name}({intern_type} {param_name})')
        self.dump('{')
        lines = []
        for i, value_name in enumerate(value_names):
            intern_name = self.mangle_name(value_name)
            if i == 0:
                lines.append('if (%s == %s) {' % (intern_name, param_name))
            else:
                lines.append('} else if (%s == %s) {' % (intern_name, param_name))
            lines.append(f'return {value_name};')
        lines.append('}')
        lines.append(f'return ({type_}) {param_name};')
        self.dump_lines(lines)
        self.dump('}')

    def generic_convert_reverse(self, fn_name, param_name, type_, value_names):
        intern_type = self.mangle_name(type_)
        self.dump(f'static inline {intern_type} {fn_name}({type_} {param_name})')
        self.dump('{')
        lines = []
        for i, value_name in enumerate(value_names):
            intern_name = self.mangle_name(value_name)
            if i == 0:
                lines.append('if (%s == %s) {' % (value_name, param_name))
            else:
                lines.append('} else if (%s == %s) {' % (value_name, param_name))
            lines.append(f'return {intern_name};')
        lines.append('}')
        lines.append(f'return ({intern_type}) {param_name};')
        self.dump_lines(lines)
        self.dump('}')

    def generate_comm_convert_fn(self):
        self.generic_convert(ConvertFuncs.COMM, 'comm', 'MPI_Comm', RESERVED_COMMUNICATORS)

    def generate_comm_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.COMM, 'comm', 'MPI_Comm', RESERVED_COMMUNICATORS)

    def generate_info_convert_fn(self):
        self.generic_convert(ConvertFuncs.INFO, 'info', 'MPI_Info', RESERVED_INFOS)

    def generate_file_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertFuncs.FILE, 'file', 'MPI_File', RESERVED_FILES)

    def generate_datatype_convert_fn(self):
        self.generic_convert(ConvertFuncs.DATATYPE, 'datatype', 'MPI_Datatype', PREDEFINED_DATATYPES)

    def generate_op_convert_fn(self):
        self.generic_convert(ConvertFuncs.OP, 'op', 'MPI_Op', COLLECTIVE_OPERATIONS)

    def generate_win_convert_fn(self):
        self.generic_convert(ConvertFuncs.WIN, 'win', 'MPI_Win', RESERVED_WINDOWS)

    def generate_pointer_convert_fn(self, type_, fn_name, constants):
        abi_type = self.mangle_name(type_)
        self.dump(f'static inline void {fn_name}({abi_type} *ptr)')
        self.dump('{')
        lines = []
        for i, ompi_name in enumerate(constants):
            abi_name = self.mangle_name(ompi_name)
            if i == 0:
                lines.append('if (%s == (%s) *ptr) {' % (ompi_name, type_))
            else:
                lines.append('} else if (%s == (%s) *ptr) {' % (ompi_name, type_))
            lines.append(f'*ptr = {abi_name};')
        lines.append('}')
        self.dump_lines(lines)
        self.dump('}')

    def generate_request_convert_fn(self):
        self.generate_pointer_convert_fn('MPI_Request', ConvertFuncs.REQUEST, RESERVED_REQUESTS)

    def generate_file_convert_fn(self):
        self.generate_pointer_convert_fn('MPI_File', ConvertFuncs.FILE, RESERVED_FILES)

    def generate_status_convert_fn(self):
        type_ = 'MPI_Status'
        abi_type = self.mangle_name(type_)
        self.dump(f'static inline void {ConvertFuncs.STATUS}({abi_type} *out, {type_} *inp)')
        self.dump('{')
        self.dump('    out->MPI_SOURCE = inp->MPI_SOURCE;')
        self.dump('    out->MPI_TAG = inp->MPI_TAG;')
        self.dump(f'    out->MPI_ERROR = {ConvertFuncs.ERROR_CLASS}(inp->MPI_ERROR);')
        # TODO: What to do with the private fields?
        self.dump('}')

    def define(self, type_, name, value):
        self.dump(f'#define {name} OMPI_CAST_CONSTANT({type_}, {value})')

    def define_all(self, type_, constants):
        for i, const in enumerate(constants):
            self.define(self.mangle_name(type_), self.mangle_name(const), i + 1)
        self.dump()

    def dump_header(self):
        header_guard = '_ABI_INTERNAL_'
        self.dump(f'#ifndef {header_guard}')
        self.dump(f'#define {header_guard}')

        self.dump('#include "stddef.h"')
        self.dump('#include "stdint.h"')

        self.dump("""
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
""")

        self.dump("""
#if defined(c_plusplus) || defined(__cplusplus)
#define OMPI_CAST_CONSTANT(type, value) (static_cast<type> (static_cast<void *> (value)))
#else
#define OMPI_CAST_CONSTANT(type, value) ((type) ((void *) value))
#endif
""")

        for i, err in enumerate(ERROR_CLASSES):
            self.dump(f'#define {self.mangle_name(err)} {i + 1}')
        self.dump()

        self.define_all('MPI_Datatype', PREDEFINED_DATATYPES)
        self.define_all('MPI_Op', COLLECTIVE_OPERATIONS)
        self.define_all('MPI_Comm', RESERVED_COMMUNICATORS)
        self.define_all('MPI_Request', RESERVED_REQUESTS)
        self.define_all('MPI_Win', RESERVED_WINDOWS)
        self.define_all('MPI_Info', RESERVED_INFOS)
        self.define_all('MPI_File', RESERVED_FILES)

        for name, value in VARIOUS_CONSTANTS.items():
            self.dump(f'#define {self.mangle_name(name)} {value}')
        self.dump()

        status_type = self.mangle_name('MPI_Status')
        for i, name in enumerate(IGNORED_STATUS_HANDLES):
            self.define(f'{status_type} *', self.mangle_name(name), i + 1)
        self.dump()

        for i, name in enumerate(COMMUNICATOR_SPLIT_TYPES):
            self.dump(f'#define {self.mangle_name(name)} {i}')
        self.dump()

        for mpi_type, c_type in C_OPAQUE_TYPES.items():
            self.dump(f'typedef {c_type} {self.mangle_name(mpi_type)};')
        self.dump()

        for handle in C_HANDLES:
            prefix, suffix = handle.split('_')
            name = f'{prefix}_ABI_{suffix}'
            self.dump(f'typedef struct {self.mangle_name(name)} *{self.mangle_name(handle)};')
        self.dump()
        self.dump("""
struct MPI_Status_ABI {
    int MPI_SOURCE;
    int MPI_TAG;
    int MPI_ERROR;
    int mpi_abi_private[5];
};""")
        self.dump(f'typedef struct MPI_Status_ABI {self.mangle_name("MPI_Status")};')
        self.dump()
        # Function signatures
        for sig in self.signatures:
            self.dump(f'{sig};')
        self.dump('int MPI_Abi_details(int *buflen, char *details, MPI_Info *info);')
        self.dump('int MPI_Abi_supported(int *flag);')
        self.dump('int MPI_Abi_version(int *abi_major, int *abi_minor);')
        if not self.external:
            # Now generate the conversion code
            self.generate_error_convert_fn()
            self.generate_comm_convert_fn()
            self.generate_comm_convert_fn_intern_to_abi()
            self.generate_info_convert_fn()
            self.generate_file_convert_fn()
            self.generate_datatype_convert_fn()
            self.generate_op_convert_fn()
            self.generate_win_convert_fn()
            self.generate_request_convert_fn()
            self.generate_status_convert_fn()

        self.dump("""
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
""")
        self.dump(f'#endif /* {header_guard} */')


class Parameter:

    def __init__(self, text):
        """Parse a parameter."""
        # parameter in the form "TYPE NAME" or "TYPE NAME:COUNT_VAR"
        type_, namecount = text.split()
        if ':' in namecount:
            name, count_param = namecount.split(':')
        else:
            name, count_param = namecount, None
        self.type_ = type_
        self.name = name
        self.count_param = count_param

    def construct(self, abi_type, **kwargs):
        """Construct the type parameter for the given ABI."""
        return Type.construct(abi_type, type_=self.type_, name=self.name,
                              count_param=self.count_param, **kwargs)


class ReturnType:
    """Return type wrapper."""

    def __init__(self, type_):
        self.type_ = type_

    def construct(self, abi_type, **kwargs):
        """Construct the return type for the given ABI."""
        return Type.construct(abi_type, type_=self.type_, **kwargs)


class Type(ABC):
    """Type representation."""

    PARAMS_OMPI_ABI = {}

    PARAMS_STANDARD_ABI = {}

    def __init__(self, type_, name=None,
                 mangle_name=lambda name: abi_internal_name(name),
                 count_param=None, **kwargs):
        self.type = type_
        self.name = name
        self.count_param = count_param
        self.mangle_name = mangle_name

    @staticmethod
    def construct(abi_type, type_, **kwargs):
        """Construct the parameter for the given ABI and type."""
        if abi_type == 'ompi':
            return Type.PARAMS_OMPI_ABI[type_](type_, **kwargs)
        elif abi_type == 'standard':
            return Type.PARAMS_STANDARD_ABI[type_](type_, **kwargs)
        else:
            raise RuntimeError(f'invalid ABI type {abi_type}')

    @staticmethod
    def add_type(type_name, abi_type=('ompi', 'standard')):
        """Add a new class corresponding to a type."""
        def wrapper(class_):
            if 'ompi' in abi_type:
                Type.PARAMS_OMPI_ABI[type_name] = class_
            if 'standard' in abi_type:
                Type.PARAMS_STANDARD_ABI[type_name] = class_
            # Parameter.TYPES[type_] = class_
            return class_
        return wrapper

    @property
    def is_count(self):
        """Return True if this parameter is a count (requiring bigcount API)."""
        return False

    @property
    def init_code(self):
        """Return the initialization code needed for an ABI wrapper."""
        return []

    @property
    def final_code(self):
        """Return the finalization code needed for an ABI wrapper."""
        return []

    def return_code(self, name):
        """Process a value and then build up a return statement."""
        return [f'return {name};']

    @property
    def argument(self):
        """Return the argument text required for passing an argument to a function."""
        return self.name

    @abstractmethod
    def type_text(self, count_type=None):
        """Return the source text corresponding to a type definition."""

    def tmp_type_text(self, count_type=None):
        """Return source text corresponding to a temporary type definition before conversion."""
        return self.type_text(count_type=count_type)

    def parameter(self, count_type=None, **kwargs):
        return f'{self.type_text(count_type)} {self.name}'


@Type.add_type('ERROR_CLASS')
class TypeErrorClass(Type):

    def type_text(self, count_type=None):
        return 'int'

    def return_code(self, name):
        return [f'return {ConvertFuncs.ERROR_CLASS}({name});']


@Type.add_type('BUFFER')
class TypeBuffer(Type):

    def type_text(self, count_type=None):
        return 'const void *'


@Type.add_type('BUFFER_OUT')
class TypeBufferOut(Type):

    def type_text(self, count_type=None):
        return f'void *'


@Type.add_type('COUNT')
class TypeCount(Type):

    @property
    def is_count(self):
        return True

    def type_text(self, count_type=None):
        return 'int' if count_type is None else count_type


@Type.add_type('INT')
class TypeBufferOut(Type):

    def type_text(self, count_type=None):
        return 'int'


@Type.add_type('AINT')
class TypeBufferOut(Type):

    def type_text(self, count_type=None):
        return 'MPI_Aint'


@Type.add_type('INT_OUT')
class TypeBufferOut(Type):

    def type_text(self, count_type=None):
        return 'int *'

    def parameter(self, count_type=None, **kwargs):
        if self.count_param is None:
            return f'int *{self.name}'
        else:
            return f'int {self.name}[]'


@Type.add_type('DOUBLE')
class TypeDouble(Type):

    def type_text(self, count_type=None):
        return 'double'


@Type.add_type('ARGV')
class TypeArgv(Type):

    def type_text(self, count_type=None):
        return 'char ***'


@Type.add_type('DATATYPE', abi_type=['ompi'])
class TypeDatatype(Type):

    def type_text(self, count_type=None):
        return 'MPI_Datatype'


class StandardABIType(Type):

    @property
    def tmpname(self):
        return f'{self.name}_tmp'

    @property
    def argument(self):
        return self.tmpname


@Type.add_type('DATATYPE', abi_type=['standard'])
class TypeDatatype(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Datatype {self.tmpname} = {ConvertFuncs.DATATYPE}({self.name});']

    def type_text(self, count_type=None):
        return self.mangle_name('MPI_Datatype')


@Type.add_type('OP', abi_type=['ompi'])
class TypeDatatype(Type):

    def type_text(self, count_type=None):
        return 'MPI_Op'


@Type.add_type('OP', abi_type=['standard'])
class TypeDatatype(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Op {self.tmpname} = {ConvertFuncs.OP}({self.name});']

    def type_text(self, count_type=None):
        return self.mangle_name('MPI_Op')


@Type.add_type('RANK')
class TypeRank(Type):

    def type_text(self, count_type=None):
        return 'int'


@Type.add_type('TAG')
class TypeRank(Type):

    def type_text(self, count_type=None):
        return 'int'


@Type.add_type('COMM', abi_type=['ompi'])
class TypeCommunicator(Type):

    def type_text(self, count_type=None):
        return 'MPI_Comm'


@Type.add_type('COMM', abi_type=['standard'])
class TypeCommunicatorStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Comm {self.tmpname} = {ConvertFuncs.COMM}({self.name});']

    def tmp_type_text(self, count_type=None):
        return 'MPI_Comm'

    def return_code(self, name):
        return [f'return {ConvertOMPIToStandard.COMM}({name});']

    def type_text(self, count_type=None):
        return self.mangle_name('MPI_Comm')


@Type.add_type('COMM_OUT', abi_type=['ompi'])
class TypeCommunicator(Type):

    def type_text(self, count_type=None):
        return 'MPI_Comm *'


@Type.add_type('COMM_OUT', abi_type=['standard'])
class TypeCommunicator(Type):

    @property
    def final_code(self):
        return [f'*{self.name} = {ConvertOMPIToStandard.COMM}((MPI_Comm) *{self.name});']

    def type_text(self, count_type=None):
        type_name = self.mangle_name('MPI_Comm')
        return f'{type_name} *'

    @property
    def argument(self):
        return f'(MPI_Comm *) {self.name}'


@Type.add_type('WIN', abi_type=['ompi'])
class TypeWindow(Type):

    def type_text(self, count_type=None):
        return 'MPI_Win'


@Type.add_type('WIN', abi_type=['standard'])
class TypeWindowStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Win {self.tmpname} = {ConvertFuncs.WIN}({self.name});']

    def type_text(self, count_type=None):
        return self.mangle_name('MPI_Win')


@Type.add_type('REQUEST', abi_type=['ompi'])
class TypeRequest(Type):

    def type_text(self, count_type=None):
        return 'MPI_Request'


@Type.add_type('REQUEST', abi_type=['standard'])
class TypeRequestStandard(Type):

    def type_text(self, count_type=None):
        return self.mangle_name('MPI_Request')

    @property
    def argument(self):
        return f'(MPI_Request) {self.name}'


@Type.add_type('REQUEST_INOUT', abi_type=['ompi'])
class TypeRequestInOut(Type):

    def type_text(self, count_type=None):
        return 'MPI_Request *'


@Type.add_type('REQUEST_INOUT', abi_type=['standard'])
class TypeRequestInOutStandard(Type):

    @property
    def final_code(self):
        if self.count_param is None:
            return [f'{ConvertFuncs.REQUEST}({self.name});']
        else:
            return [
                'for (int i = 0; i < %s; ++i) {' % (self.count_param,),
                f'{ConvertFuncs.REQUEST}(&{self.name}[i]);',
                '}',
            ]

    @property
    def argument(self):
        return f'(MPI_Request *) {self.name}'

    def type_text(self, count_type=None):
        type_name = self.mangle_name('MPI_Request')
        return f'{type_name} *'

    def parameter(self, count_type=None, **kwargs):
        type_name = self.mangle_name('MPI_Request')
        if self.count_param is None:
            return f'{type_name} *{self.name}'
        else:
            return f'{type_name} {self.name}[]'


@Type.add_type('STATUS_OUT', abi_type=['ompi'])
class TypeStatusOut(Type):

    def type_text(self, count_type=None):
        return 'MPI_Status *'

    def parameter(self, count_type=None, **kwargs):
        if self.count_param is None:
            return f'MPI_Status *{self.name}'
        else:
            return f'MPI_Status {self.name}[]'


@Type.add_type('STATUS_OUT', abi_type=['standard'])
class TypeStausOutStandard(StandardABIType):

    def if_should_set_status(self):
        """Generate the condition to check if the status(es) should be set."""
        condition = ' && '.join(f'{self.mangle_name(const)} != {self.name}'
                                for const in IGNORED_STATUS_HANDLES)
        return 'if (%s) {' % (condition,)

    @property
    def status_argument(self):
        return f'{self.name}_arg'

    @property
    def init_code(self):
        code = [f'MPI_Status *{self.status_argument} = NULL;']
        if self.count_param is None:
            code.append(f'MPI_Status {self.tmpname};')
        else:
            code.append(f'MPI_Status *{self.tmpname} = NULL;')
        code.append(self.if_should_set_status())
        if self.count_param is not None:
            code.append(f'{self.tmpname} = malloc({self.count_param} * sizeof(MPI_Status));')
            code.append(f'{self.status_argument} = {self.tmpname};')
        else:
            code.append(f'{self.status_argument} = &{self.tmpname};')
        code.append('} else {')
        if self.count_param is not None:
            code.append(f'{self.status_argument} = MPI_STATUSES_IGNORE;')
        else:
            code.append(f'{self.status_argument} = MPI_STATUS_IGNORE;')
        code.append('}')
        return code

    @property
    def final_code(self):
        code = [self.if_should_set_status()]
        if self.count_param is None:
            code.append(f'{ConvertFuncs.STATUS}({self.name}, &{self.tmpname});')
        else:
            code.extend([
                'for (int i = 0; i < %s; ++i) {' % (self.count_param,),
                f'{ConvertFuncs.STATUS}(&{self.name}[i], &{self.tmpname}[i]);',
                '}',
                f'free({self.tmpname});',
            ])
        code.append('}')
        return code

    @property
    def argument(self):
        return self.status_argument

    def type_text(self, count_type=None):
        type_name = self.mangle_name('MPI_Status')
        return f'{type_name} *'

    def parameter(self, count_type=None, **kwargs):
        type_name = self.mangle_name('MPI_Status')
        if self.count_param is None:
            return f'{type_name} *{self.name}'
        else:
            return f'{type_name} {self.name}[]'


# For now this just assumes that MPI_Fint doesn't need any conversions
@Type.add_type('FINT')
class TypeFint(Type):

    def type_text(self, count_type=None):
        return 'MPI_Fint'


@Type.add_type('STRING')
class TypeString(Type):

    def type_text(self, count_type=None):
        return 'const char *'


@Type.add_type('STRING_OUT')
class TypeStringOut(Type):

    def type_text(self, count_type=None):
        return 'char *'


@Type.add_type('INFO', abi_type=['ompi'])
class TypeInfo(Type):

    def type_text(self, count_type=None):
        return 'MPI_Info'


@Type.add_type('INFO', abi_type=['standard'])
class TypeInfoStandard(StandardABIType):

    @property
    def init_code(self):
        return [f'MPI_Info {self.tmpname} = {ConvertFuncs.INFO}({self.name});']

    def type_text(self, count_type=None):
        return self.mangle_name('MPI_Info')


@Type.add_type('FILE_OUT', abi_type=['ompi'])
class TypeFileOut(Type):

    def type_text(self, count_type=None):
        return 'MPI_File *'


@Type.add_type('FILE_OUT', abi_type=['standard'])
class TypeFileOutStandard(Type):

    @property
    def argument(self):
        return f'(MPI_File *) {self.name}'

    @property
    def final_code(self):
        return [f'{ConvertFuncs.FILE}({self.name});']

    def type_text(self, count_type=None):
        type_name = self.mangle_name('MPI_File')
        return f'{type_name} *'


class Prototype:
    """MPI function prototype."""

    def __init__(self, name, return_type, params):
        self.name = name
        self.return_type = return_type
        self.params = params

    def signature(self, abi_type, fn_name, count_type=None, **kwargs):
        """Build a signature with the given name and count_type."""
        params = ', '.join(param.construct(abi_type, **kwargs).parameter(count_type=count_type, **kwargs)
                           for param in self.params)
        if not params:
            params = 'void'
        return_type_text = self.return_type.construct(abi_type, **kwargs).type_text(count_type=count_type)
        return f'{return_type_text} {fn_name}({params})'

    @property
    def need_bigcount(self):
        """Check if a bigcount interface is required for a prototype."""
        return any('COUNT' in param.type_ for param in self.params)


class TemplateParseError(Exception):
    """Error raised during parsing."""
    pass


def validate_body(body):
    """Validate the body of a template."""
    # Just do a simple bracket balance test determine the bounds of the
    # function body. All lines after the function body should be blank. There
    # are cases where this will break, such as if someone puts code all on one
    # line.
    bracket_balance = 0
    line_count = 0
    for line in body:
        line = line.strip()
        if bracket_balance == 0 and line_count > 0 and line:
            raise TemplateParserError('Extra code found in template; only one function body is allowed')

        update = line.count('{') - line.count('}')
        bracket_balance += update
        if bracket_balance != 0:
            line_count += 1


class SourceTemplate:
    """Source template for a single API function."""

    def __init__(self, prototype, header, body):
        self.prototype = prototype
        self.header = header
        self.body = body

    @staticmethod
    def load(fname, prefix=None):
        """Load a template file and return the SourceTemplate."""
        if prefix is not None:
            fname = os.path.join(prefix, fname)
        with open(fname) as fp:
            header = []
            prototype = []
            body = []

            for line in fp:
                line = line.rstrip()
                if prototype and line.startswith('PROTOTYPE'):
                    raise TemplateParseError('more than one prototype found in template file')
                elif ((prototype and not any(')' in s for s in prototype))
                      or line.startswith('PROTOTYPE')):
                    prototype.append(line)
                elif prototype:
                    # Validate bracket balance
                    body.append(line)
                else:
                    header.append(line)

            if not prototype:
                raise RuntimeError('missing prototype')
            # Parse the prototype
            prototype = ''.join(prototype)
            prototype = prototype[len('PROTOTYPE'):]
            i = prototype.index('(')
            j = prototype.index(')')
            return_type, name = prototype[:i].split()
            return_type = ReturnType(return_type)
            params = [param.strip() for param in prototype[i + 1:j].split(',') if param.strip()]
            params = [Parameter(param) for param in params]
            prototype = Prototype(name, return_type, params)
            # Ensure the body contains only one function
            validate_body(body)
            return SourceTemplate(prototype, header, body)

    def print_header(self, file=sys.stdout):
        """Print the source header."""
        for line in self.header:
            print(line, file=file)

    def print_body(self, func_name, file=sys.stdout):
        """Print the body."""
        for line in self.body:
            # FUNC_NAME is used for error messages
            line = line.replace('FUNC_NAME', f'"{func_name}"')
            print(line, file=file)


def print_profiling_header(fn_name, file=sys.stdout):
    """Print the profiling header code."""
    print('#if OMPI_BUILD_MPI_PROFILING')
    print('#if OPAL_HAVE_WEAK_SYMBOLS', file=file)
    print(f'#pragma weak {fn_name} = P{fn_name}', file=file)
    print('#endif', file=file)
    print(f'#define {fn_name} P{fn_name}', file=file)
    print('#endif')


def ompi_abi(base_name, template):
    """Generate the OMPI ABI functions."""
    template.print_header()
    print_profiling_header(base_name)
    print(template.prototype.signature('ompi', base_name))
    template.print_body(func_name=base_name)
    # Check if we need to generate the bigcount interface
    if template.prototype.need_bigcount:
        base_name_c = f'{base_name}_c'
        print_profiling_header(base_name_c)
        print(template.prototype.signature('ompi', base_name_c, count_type='MPI_Count'))
        template.print_body(func_name=base_name_c)


ABI_INTERNAL_HEADER = 'ompi/mpi/c/abi.h'


def indent_lines(lines, tab, start=0):
    """Crude pretty-printing function."""
    new_lines = []
    indent_count = start
    for line in lines:
        # Closing bracket
        if '}' in line:
            indent_count -= 1

        prefix = indent_count * tab
        new_lines.append(f'{prefix}{line}')

        # Opening bracket
        if '{' in line:
            indent_count += 1
    return new_lines


def standard_abi(base_name, template):
    """Generate the standard ABI functions."""
    template.print_header()
    print(f'#include "{ABI_INTERNAL_HEADER}"')

    # Static internal function (add a random component to avoid conflicts)
    internal_name = f'ompi_abi_{template.prototype.name}'
    internal_sig = template.prototype.signature('ompi', internal_name,
                                                count_type='MPI_Count')
    print('static inline', internal_sig)
    template.print_body(func_name=base_name)

    def generate_function(prototype, fn_name, internal_fn, count_type='int'):
        """Generate a function for the standard ABI."""
        print_profiling_header(fn_name)

        # Handle type conversions and arguments
        params = [param.construct('standard') for param in prototype.params]
        print(prototype.signature('standard', fn_name, count_type=count_type))
        print('{')
        lines = []
        return_type = prototype.return_type.construct('standard')
        lines.append(f'{return_type.tmp_type_text()} ret_value;')
        for param in params:
            if param.init_code:
                lines.extend(param.init_code)
        pass_args = ', '.join(param.argument for param in params)
        lines.append(f'ret_value = {internal_fn}({pass_args});')
        for param in params:
            if param.final_code:
                lines.extend(param.final_code)
        lines.extend(return_type.return_code('ret_value'))

        # Indent the lines
        lines = indent_lines(lines, 4 * ' ', start=1)
        for line in lines:
            print(line)
        print('}')

    generate_function(template.prototype, base_name, internal_name)
    if template.prototype.need_bigcount:
        base_name_c = f'{base_name}_c'
        generate_function(template.prototype, base_name_c, internal_name,
                          count_type='MPI_Count')


def gen_header(args):
    """Generate an ABI header and conversion code."""
    prototypes = [SourceTemplate.load(file_, args.srcdir).prototype for file_ in args.file]

    builder = ABIHeaderBuilder(prototypes, external=args.external)
    builder.dump_header()


def gen_source(args):
    """Generate source file."""
    template = SourceTemplate.load(args.source_file)

    base_name = mpi_fn_name_from_base_fn_name(template.prototype.name)
    if args.type == 'ompi':
        ompi_abi(base_name, template)
    else:
        standard_abi(base_name, template)


def main():
    if len(sys.argv) < 2:
        # Fix required for Python 3.6
        print('ERROR: missing subparser argument (see --help)')
        sys.exit(1)

    parser = argparse.ArgumentParser(description='generate ABI header file and conversion code')
    subparsers = parser.add_subparsers()

    parser_header = subparsers.add_parser('header')
    parser_header.add_argument('file', nargs='+', help='list of template source files')
    parser_header.add_argument('--external', action='store_true', help='generate external mpi.h header file')
    parser_header.add_argument('--srcdir', help='source directory')
    parser_header.set_defaults(func=gen_header)

    parser_gen = subparsers.add_parser('source')
    # parser = argparse.ArgumentParser(description='C ABI binding generation code')
    parser_gen.add_argument('type', choices=('ompi', 'standard'),
                            help='generate the OMPI ABI functions or the standard ABI functions')
    parser_gen.add_argument('source_file', help='source template file')
    parser_gen.set_defaults(func=gen_source)

    args = parser.parse_args()

    # Always add the header
    print('/* THIS FILE WAS AUTOGENERATED BY ompi/mpi/c/abi.py. DO NOT EDIT BY HAND. */')
    args.func(args)


if __name__ == '__main__':
    main()
