# Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
# Copyright (c) 2023      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADERS$
#
#
"""MPI C Binding Code.

This file is used for generating C bindings, as well as bigcount interfaces,
from individual *.c.in template files. This also currently includes unused ABI
code, in preparation for the standard ABI.

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
from ompi_bindings import consts, util
from ompi_bindings.consts import ConvertFuncs, ConvertOMPIToStandard
from ompi_bindings.c_type import Type
from ompi_bindings.parser import SourceTemplate

c_intrinsic_types = ['char', 'int', 'long int', 'void']
#OMPI_ABI_HANDLE_BASE_OFFSET = '16385'

class ABIHeaderBuilder:
    """ABI header builder code."""

    def __init__(self, prototypes, out, external=False):
        self.out = out
        self.external = external

        if external:
            mangle_name = lambda name: name
        else:
            mangle_name = util.abi_internal_name

        # Build up the list of standard ABI signatures
        signatures = []
        for prototype in prototypes:
            base_name = util.mpi_fn_name_from_base_fn_name(prototype.name)
            signatures.append(prototype.signature(base_name, abi_type='standard',
                                                  mangle_name=mangle_name))
            # Profiling prototype
            signatures.append(prototype.signature(f'P{base_name}', abi_type='standard',
                                                  mangle_name=mangle_name))
            if util.prototype_has_bigcount(prototype):
                signatures.append(prototype.signature(f'{base_name}_c', abi_type='standard',
                                                      enable_count=True,
                                                      mangle_name=mangle_name))
                # Profiling prototype
                signatures.append(prototype.signature(f'P{base_name}_c', abi_type='standard',
                                                      enable_count=True,
                                                      mangle_name=mangle_name))
        self.signatures = signatures

    def mangle_name(self, extname):
        """Mangle names, depending on whether building external or internal header."""
        if self.external:
            return extname
        return util.abi_internal_name(extname)

    def dump(self, *pargs, **kwargs):
        self.out.dump(*pargs, **kwargs)

    def dump_lines(self, lines):
        lines = util.indent_lines(lines, 4 * ' ', start=1)
        for line in lines:
            self.dump(line)

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

        for i, err in enumerate(consts.ERROR_CLASSES):
            self.dump(f'#define {self.mangle_name(err)} {i + 1}')
        self.dump()

        self.define_all('MPI_Datatype', consts.PREDEFINED_DATATYPES)
        self.define_all('MPI_Op', consts.RESERVED_OPS)
        self.define_all('MPI_Comm', consts.RESERVED_COMMUNICATORS)
        self.define_all('MPI_Errhandler', consts.RESERVED_ERRHANDLERS)
        self.define_all('MPI_Group', consts.RESERVED_GROUPS)
        self.define_all('MPI_Request', consts.RESERVED_REQUESTS)
        self.define_all('MPI_Session', consts.RESERVED_SESSIONS)
        self.define_all('MPI_Win', consts.RESERVED_WINDOWS)
        self.define_all('MPI_Info', consts.RESERVED_INFOS)
        self.define_all('MPI_File', consts.RESERVED_FILES)
        self.define_all('MPI_Message', consts.RESERVED_MESSAGES)

        for name, value in consts.VARIOUS_CONSTANTS.items():
            self.dump(f'#define {self.mangle_name(name)} {value}')
        self.dump()

        status_type = self.mangle_name('MPI_Status')
        for i, name in enumerate(consts.IGNORED_STATUS_HANDLES):
            self.define(f'{status_type} *', self.mangle_name(name), i + 1)
        self.dump()

        for i, name in enumerate(consts.COMMUNICATOR_SPLIT_TYPES):
            self.dump(f'#define {self.mangle_name(name)} {i}')
        self.dump()

        for mpi_type, c_type in consts.C_OPAQUE_TYPES.items():
            self.dump(f'typedef {c_type} {self.mangle_name(mpi_type)};')
        self.dump()

        for handle in consts.C_HANDLES:
            prefix, suffix = handle.split('_')
            name = f'{prefix}_ABI_{suffix}'
            self.dump(f'typedef struct {self.mangle_name(name)} *{self.mangle_name(handle)};')
        self.dump()
        self.dump("""
struct MPI_Status_ABI {
    int MPI_SOURCE;
    int MPI_TAG;
    int MPI_ERROR;
    int MPI_Internal[5];
};""")
        self.dump(f'typedef struct MPI_Status_ABI {self.mangle_name("MPI_Status")};')
        self.dump()
        # user functions
        self.dump('typedef int (MPI_Copy_function)(MPI_Comm_ABI_INTERNAL, int, void *, void *, void *, int *);')
        self.dump('typedef int (MPI_Delete_function)(MPI_Comm_ABI_INTERNAL, int, void *, void *);')
#
#       generate prototypes for user call back functions
#
        for handle in consts.C_ATTRIBUTE_OBJS:
            prefix, suffix = handle.split('_')
            copy_callback_func_name = f'{handle}_copy_attr_function'
            copy_callback_func_name = f'{self.mangle_name(copy_callback_func_name)}'
            delete_callback_func_name = f'{handle}_delete_attr_function'
            delete_callback_func_name = f'{self.mangle_name(delete_callback_func_name)}'
            #
            # stupid MPI standard naming consistency
            #
            if handle == 'MPI_Type':
                obj_arg_type = f'{self.mangle_name("MPI_Datatype")}'
            else:
                obj_arg_type = f'{self.mangle_name(handle)}'
            obj_arg_name = f'old{suffix}'.lower()
            obj_arg = f'{obj_arg_type} {obj_arg_name}'
            keyval_arg = f'int {suffix}_keyval'.lower()
            self.dump(f'typedef int ({copy_callback_func_name})({obj_arg}, {keyval_arg}, void *, void *, void *,int *);')
            self.dump(f'typedef int ({delete_callback_func_name})({obj_arg}, {keyval_arg}, void *, void *);')

        # Function signatures
        for sig in self.signatures:
            self.dump(f'{sig};')
        self.dump('int MPI_Abi_details(int *buflen, char *details, MPI_Info *info);')
        self.dump('int MPI_Abi_version(int *abi_major, int *abi_minor);')

        self.dump("""
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
""")
        self.dump(f'#endif /* {header_guard} */')

class ABIConverterBuilder:
    """ABI converter builder code."""

    def __init__(self, out):
        self.out = out

    def mangle_name(self, extname):
        """Mangle names"""
        return util.abi_internal_name(extname)

    def dump(self, *pargs, **kwargs):
        self.out.dump(*pargs, **kwargs)

    def dump_lines(self, lines):
        lines = util.indent_lines(lines, 4 * ' ', start=1)
        for line in lines:
            self.dump(line)

    def generate_error_convert_fn(self):
        self.dump(f'{consts.INLINE_ATTRS} int {ConvertFuncs.ERROR_CLASS}(int error_class)')
        self.dump('{')
        lines = []
        lines.append('switch (error_class) {')
        for error in consts.ERROR_CLASSES:
            lines.append(f'case {self.mangle_name(error)}:')
            lines.append(f'return {error};')
        lines.append('default:')
        lines.append('return (error_class - %s);' % ('OMPI_ABI_HANDLE_BASE_OFFSET'))
        lines.append('}')
        self.dump_lines(lines)
        self.dump('}')

    def generate_error_convert_fn_intern_to_abi(self):
        self.dump(f'{consts.INLINE_ATTRS} int {ConvertOMPIToStandard.ERROR_CLASS}(int error_class)')
        self.dump('{')
        lines = []
        lines.append('switch (error_class) {')
        for error in consts.ERROR_CLASSES:
            lines.append(f'case {error}:')
            lines.append(f'return {self.mangle_name(error)};')
        lines.append('default:')
        lines.append('return (error_class + %s);' % ('OMPI_ABI_HANDLE_BASE_OFFSET'))
        lines.append('}')
        self.dump_lines(lines)
        self.dump('}')

    def generate_new_datatype_convert_fn(self):
        arg_type = self.mangle_name('MPI_Datatype')
        self.dump(f'{consts.INLINE_ATTRS} MPI_Datatype {ConvertFuncs.DATATYPE}({arg_type} datatype)')
        self.dump('{')
        lines = []
        for i, value_name in enumerate(consts.PREDEFINED_DATATYPES):
            intern_name = self.mangle_name(value_name)
            if i == 0:
                lines.append('if (%s == datatype) {' % (intern_name))
            else:
                lines.append('} else if (%s == datatype) {' % (intern_name))
            lines.append(f'return {value_name};')
        #
        # now shoe-horn in optional fortran predefined types
        #
        lines.append('#if OMPI_BUILD_FORTRAN_BINDINGS')
        for i,value_name in enumerate(consts.PREDEFINED_OPTIONAL_FORTRAN_DATATYPES):
            intern_name = self.mangle_name(value_name)
            base_type = value_name[4:]
            lines.append('} else if (%s == datatype) {' % (intern_name))
            lines.append(f'#if OMPI_HAVE_FORTRAN_{base_type}')
            lines.append(f'return {value_name};')
            lines.append('#else')
            lines.append('return MPI_DATATYPE_NULL;')
            lines.append('#endif')
        lines.append('#endif  /* OMPI_BUILD_FORTRAN_BINDINGS */')
        lines.append('}')
        lines.append(f'return (MPI_Datatype) datatype;')
        self.dump_lines(lines)
        self.dump('}')

    def generate_new_datatype_convert_fn_intern_to_abi(self):
        return_type = self.mangle_name('MPI_Datatype')
        self.dump(f'{consts.INLINE_ATTRS} {return_type} {ConvertOMPIToStandard.DATATYPE}(MPI_Datatype datatype)')
        self.dump('{')
        lines = []
        for i, value_name in enumerate(consts.PREDEFINED_DATATYPES):
            intern_name = self.mangle_name(value_name)
            if i == 0:
                lines.append('if (%s == datatype) {' % (value_name))
            else:
                lines.append('} else if (%s == datatype) {' % (value_name))
            lines.append(f'return {intern_name};')
        #
        # now shoe-horn in optional fortran predefined types
        #
        mangle_null_name = self.mangle_name('MPI_DATATYPE_NULL')
        lines.append('#if OMPI_BUILD_FORTRAN_BINDINGS')
        for i,value_name in enumerate(consts.PREDEFINED_OPTIONAL_FORTRAN_DATATYPES):
            intern_name = self.mangle_name(value_name)
            base_type = value_name[4:]
            lines.append(f'#if OMPI_HAVE_FORTRAN_{base_type}')
            lines.append('} else if (%s == datatype) {' % (value_name))
            lines.append(f'return {intern_name};')
            lines.append('#endif')
        lines.append('#endif  /* OMPI_BUILD_FORTRAN_BINDINGS */')
        lines.append('}')
        lines.append(f'return ({return_type}) datatype;')
        self.dump_lines(lines)
        self.dump('}')

    def generic_convert(self, fn_name, param_name, type_, value_names, offset=None):
        is_ptr_arg = False
        tmp_type = type_
        if (tmp_type[-1] == '*'):
            is_ptr_arg = True
            tmp_type = tmp_type[:-1].strip()
        if tmp_type not in c_intrinsic_types:
            intern_type = self.mangle_name(tmp_type)
        else:
            intern_type = tmp_type
        if (is_ptr_arg == True):
            intern_type = intern_type + ' *'
        self.dump(f'{consts.INLINE_ATTRS} {type_} {fn_name}({intern_type} {param_name})')
        self.dump('{')
        lines = []
        if (offset != None):
            lines.append('if (%s <= %s) {' % (offset, param_name))
            lines.append('return (%s - %s);' % (param_name, offset))
            lines.append('}')
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

    def generic_convert_reverse(self, fn_name, param_name, type_, value_names, offset=None):
        is_ptr_arg = False
        tmp_type = type_
        if (tmp_type[-1] == '*'):
            is_ptr_arg = True
            tmp_type = tmp_type[:-1].strip()
        if tmp_type not in c_intrinsic_types:
            intern_type = self.mangle_name(tmp_type)
        else:
            intern_type = tmp_type
        if (is_ptr_arg == True):
            intern_type = intern_type + ' *'
        self.dump(f'{consts.INLINE_ATTRS} {intern_type} {fn_name}({type_} {param_name})')
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
        if (offset == None):
            lines.append(f'return ({intern_type}) {param_name};')
        else:
            lines.append(f'return ({intern_type}) ({param_name} + {offset});')
        self.dump_lines(lines)
        self.dump('}')

    def generate_comm_convert_fn(self):
        self.generic_convert(ConvertFuncs.COMM, 'comm', 'MPI_Comm', consts.RESERVED_COMMUNICATORS)

    def generate_comm_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.COMM, 'comm', 'MPI_Comm', consts.RESERVED_COMMUNICATORS)

    def generate_info_convert_fn(self):
        self.generic_convert(ConvertFuncs.INFO, 'info', 'MPI_Info', consts.RESERVED_INFOS)

    def generate_info_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.INFO, 'info', 'MPI_Info', consts.RESERVED_INFOS)

    def generate_file_convert_fn(self):
        self.generic_convert(ConvertFuncs.FILE, 'file', 'MPI_File', consts.RESERVED_FILES)

    def generate_file_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.FILE, 'file', 'MPI_File', consts.RESERVED_FILES)

    def generate_datatype_convert_fn(self):
        self.generic_convert(ConvertFuncs.DATATYPE, 'datatype', 'MPI_Datatype', consts.PREDEFINED_DATATYPES)

    def generate_datatype_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.DATATYPE, 'datatype', 'MPI_Datatype', consts.PREDEFINED_DATATYPES)

    def generate_errhandler_convert_fn(self):
        self.generic_convert(ConvertFuncs.ERRHANDLER, 'errorhandler', 'MPI_Errhandler', consts.RESERVED_ERRHANDLERS)

    def generate_errhandler_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.ERRHANDLER, 'errorhandler', 'MPI_Errhandler', consts.RESERVED_ERRHANDLERS)

    def generate_comm_copy_attr_convert_fn(self):
        self.generic_convert(ConvertFuncs.COMM_COPY_ATTR_FUNCTION, 'comm_copy_attr_fn', 'MPI_Comm_copy_attr_function *', consts.RESERVED_COMM_COPY_ATTR_FNS)

    def generate_comm_delete_attr_convert_fn(self):
        self.generic_convert(ConvertFuncs.COMM_DELETE_ATTR_FUNCTION, 'comm_delete_attr_fn', 'MPI_Comm_delete_attr_function *', consts.RESERVED_COMM_DEL_ATTR_FNS)

    def generate_type_copy_attr_convert_fn(self):
        self.generic_convert(ConvertFuncs.TYPE_COPY_ATTR_FUNCTION, 'type_copy_attr_fn', 'MPI_Type_copy_attr_function *', consts.RESERVED_TYPE_COPY_ATTR_FNS)

    def generate_type_delete_attr_convert_fn(self):
        self.generic_convert(ConvertFuncs.TYPE_DELETE_ATTR_FUNCTION, 'type_delete_attr_fn', 'MPI_Type_delete_attr_function *', consts.RESERVED_TYPE_DEL_ATTR_FNS)

    def generate_win_copy_attr_convert_fn(self):
        self.generic_convert(ConvertFuncs.WIN_COPY_ATTR_FUNCTION, 'win_copy_attr_fn', 'MPI_Win_copy_attr_function *', consts.RESERVED_WIN_COPY_ATTR_FNS)

    def generate_win_delete_attr_convert_fn(self):
        self.generic_convert(ConvertFuncs.WIN_DELETE_ATTR_FUNCTION, 'win_delete_attr_fn', 'MPI_Win_delete_attr_function *', consts.RESERVED_WIN_DEL_ATTR_FNS)

    def generate_group_convert_fn(self):
        self.generic_convert(ConvertFuncs.GROUP, 'group', 'MPI_Group', consts.RESERVED_GROUPS)

    def generate_group_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.GROUP, 'group', 'MPI_Group', consts.RESERVED_GROUPS)

    def generate_message_convert_fn(self):
        self.generic_convert(ConvertFuncs.MESSAGE, 'message', 'MPI_Message', consts.RESERVED_MESSAGES)

    def generate_message_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.MESSAGE, 'message', 'MPI_Message', consts.RESERVED_MESSAGES)

    def generate_op_convert_fn(self):
        self.generic_convert(ConvertFuncs.OP, 'op', 'MPI_Op', consts.RESERVED_OPS)

    def generate_op_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.OP, 'op', 'MPI_Op', consts.RESERVED_OPS)

    def generate_session_convert_fn(self):
        self.generic_convert(ConvertFuncs.SESSION, 'session', 'MPI_Session', consts.RESERVED_SESSIONS)

    def generate_session_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.SESSION, 'session', 'MPI_Session', consts.RESERVED_SESSIONS)

    def generate_win_convert_fn(self):
        self.generic_convert(ConvertFuncs.WIN, 'win', 'MPI_Win', consts.RESERVED_WINDOWS)

    def generate_win_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.WIN, 'win', 'MPI_Win', consts.RESERVED_WINDOWS)

    def generate_attr_key_convert_fn(self):
        self.generic_convert(ConvertFuncs.ATTR_KEY, 'key', 'int', consts.RESERVED_ATTR_KEYS, 'OMPI_ABI_HANDLE_BASE_OFFSET')

    def generate_attr_key_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.ATTR_KEY, 'key', 'int', consts.RESERVED_ATTR_KEYS, 'OMPI_ABI_HANDLE_BASE_OFFSET')

    def generate_comm_cmp_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.COMM_CMP, 'result', 'int', consts.COMM_GROUP_COMPARE_VALS)

    def generate_ts_level_convert_fn(self):
        self.generic_convert(ConvertFuncs.TS_LEVEL, 'level', 'int', consts.TS_LEVEL_VALUES)

    def generate_ts_level_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.TS_LEVEL, 'level', 'int', consts.TS_LEVEL_VALUES)

    def generate_tag_convert_fn(self):
        self.generic_convert(ConvertFuncs.TAG, 'tag', 'int', consts.RESERVED_TAGS)

    def generate_tag_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.TAG, 'tag', 'int', consts.RESERVED_TAGS)

    def generate_source_convert_fn(self):
        self.generic_convert(ConvertFuncs.SOURCE, 'source', 'int', consts.RESERVED_SOURCE)

    def generate_source_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.SOURCE, 'tag', 'int', consts.RESERVED_SOURCE)

    def generate_root_convert_fn(self):
        self.generic_convert(ConvertFuncs.ROOT, 'root', 'int', consts.RESERVED_ROOT)

    def generate_pvar_session_convert_fn(self):
        self.generic_convert(ConvertFuncs.PVAR_SESSION, 'pe_session', 'MPI_T_pvar_session', consts.RESERVED_PVAR_SESSIONS)

    def generate_pvar_session_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.PVAR_SESSION, 'pe_session', 'MPI_T_pvar_session', consts.RESERVED_PVAR_SESSIONS)

    def generate_cvar_handle_convert_fn(self):
        self.generic_convert(ConvertFuncs.CVAR_HANDLE, 'cvar_handle', 'MPI_T_cvar_handle', consts.RESERVED_CVAR_HANDLES)

    def generate_cvar_handle_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.CVAR_HANDLE, 'cvar_handle', 'MPI_T_cvar_handle', consts.RESERVED_CVAR_HANDLES)

    def generate_pvar_handle_convert_fn(self):
        self.generic_convert(ConvertFuncs.PVAR_HANDLE, 'pvar_handle', 'MPI_T_pvar_handle', consts.RESERVED_PVAR_HANDLES)

    def generate_pvar_handle_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.PVAR_HANDLE, 'pvar_handle', 'MPI_T_pvar_handle', consts.RESERVED_PVAR_HANDLES)

    def generate_t_enum_convert_fn(self):
        self.generic_convert(ConvertFuncs.T_ENUM, 't_enum', 'MPI_T_enum', consts.RESERVED_T_ENUMS)

    def generate_t_enum_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.T_ENUM, 'pvar_handle', 'MPI_T_enum', consts.RESERVED_T_ENUMS)

    def generate_t_bind_convert_fn(self):
        self.generic_convert(ConvertFuncs.T_BIND, 'bind', 'int', consts.T_BIND_VALUES)

    def generate_t_bind_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.T_BIND, 'bind', 'int', consts.T_BIND_VALUES)

    def generate_t_verbosity_convert_fn(self):
        self.generic_convert(ConvertFuncs.T_VERBOSITY, 'verbosity', 'int', consts.T_VERBOSITY_VALUES)

    def generate_t_verbosity_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.T_VERBOSITY, 'verbosity', 'int', consts.T_VERBOSITY_VALUES)

    def generate_t_source_order_convert_fn(self):
        self.generic_convert(ConvertFuncs.T_SOURCE_ORDER, 'order', 'MPI_T_source_order', consts.T_SOURCE_ORDER_VALUES)

    def generate_t_source_order_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.T_SOURCE_ORDER, 'order', 'MPI_T_source_order', consts.T_SOURCE_ORDER_VALUES)

    def generate_pvar_class_convert_fn(self):
        self.generic_convert(ConvertFuncs.PVAR_CLASS, 'pvar_class', 'int', consts.T_PVAR_CLASS_VALUES)

    def generate_pvar_class_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.PVAR_CLASS, 'order', 'int', consts.T_PVAR_CLASS_VALUES)

    def generate_t_cb_safety_convert_fn(self):
        self.generic_convert(ConvertFuncs.T_CB_SAFETY, 'safety', 'MPI_T_cb_safety', consts.T_CB_SAFETY_VALUES)

    def generate_comm_split_type_convert_fn(self):
        self.generic_convert(ConvertFuncs.SPLIT_TYPE, 'split_type', 'int', consts.COMMUNICATOR_SPLIT_TYPES)

    def generate_weight_convert_fn(self):
        self.generic_convert(ConvertFuncs.WEIGHTS, 'weights', 'int *', consts.RESERVED_WEIGHTS)

    def generate_subarray_order_convert_fn(self):
        self.generic_convert(ConvertFuncs.SUBARRAY_ORDER, 'order', 'int', consts.SUBARRAY_ORDER_TYPES)

    def generate_subarray_distrib_types_convert_fn(self):
        self.generic_convert(ConvertFuncs.SUBARRAY_DISTRIB_TYPES, 'dist', 'int', consts.SUBARRAY_DISTRIB_TYPES)

    def generate_subarray_dargs_types_convert_fn(self):
        self.generic_convert(ConvertFuncs.SUBARRAY_DARGS_TYPES, 'darg', 'int', consts.SUBARRAY_DARGS_TYPES)

    def generate_whence_convert_fn(self):
        self.generic_convert(ConvertFuncs.WHENCE, 'whence', 'int', consts.WHENCE_VALUES)

    def generate_combiner_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.COMBINER, 'combiner', 'int', consts.COMBINER_VALUES)

    def generate_typeclass_convert_fn(self):
        self.generic_convert(ConvertFuncs.TYPECLASS, 'typeclass', 'int', consts.TYPECLASS_VALUES)

    def generate_win_lock_convert_fn(self):
        self.generic_convert(ConvertFuncs.WIN_LOCK, 'lock_assert', 'int', consts.WIN_LOCK_VALUES)

    def generate_topo_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.TOPO, 'status', 'int', consts.TOPO_VALUES)

    def generate_buffer_convert_fn(self):
        self.generic_convert(ConvertFuncs.BUFFER, 'buffer', 'void *', consts.BUFFER_VALUES)

    def generate_buffer_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.BUFFER, 'buffer', 'void *', consts.BUFFER_VALUES)

    def generate_mode_bits_convert_fn(self):
        self.dump(f'{consts.INLINE_ATTRS} int {ConvertFuncs.MODE_BITS}(int mode_bits)')
        self.dump('{')
        lines = []
        lines.append('int ret_value = 0;')
        for value_name in consts.MODE_BITS:
            intern_name = self.mangle_name(value_name)
            lines.append('if (%s & mode_bits ) {' % (intern_name))
            lines.append('ret_value |= %s;' % (value_name))
            lines.append('}')
        lines.append('return ret_value;')
        self.dump_lines(lines)
        self.dump('}')

    def generate_mode_bits_convert_fn_intern_to_abi(self):
        self.dump(f'{consts.INLINE_ATTRS} int {ConvertOMPIToStandard.MODE_BITS}(int mode_bits)')
        self.dump('{')
        lines = []
        lines.append('int ret_value = 0;')
        for value_name in consts.MODE_BITS:
            intern_name = self.mangle_name(value_name)
            lines.append('if (%s & mode_bits ) {' % (value_name))
            lines.append('ret_value |= %s;' % (intern_name))
            lines.append('}')
        lines.append('return ret_value;')
        self.dump_lines(lines)
        self.dump('}')

    def generate_rma_mode_bits_convert_fn(self):
        self.dump(f'{consts.INLINE_ATTRS} int {ConvertFuncs.RMA_MODE_BITS}(int mode_bits)')
        self.dump('{')
        lines = []
        lines.append('int ret_value = 0;')
        for value_name in consts.RMA_MODE_BITS:
            intern_name = self.mangle_name(value_name)
            lines.append('if (%s & mode_bits ) {' % (intern_name))
            lines.append('ret_value |= %s;' % (value_name))
            lines.append('}')
        lines.append('return ret_value;')
        self.dump_lines(lines)
        self.dump('}')

    def generate_rma_mode_bits_convert_fn_intern_to_abi(self):
        self.dump(f'{consts.INLINE_ATTRS} int {ConvertOMPIToStandard.RMA_MODE_BITS}(int mode_bits)')
        self.dump('{')
        lines = []
        lines.append('int ret_value = 0;')
        for value_name in consts.RMA_MODE_BITS:
            intern_name = self.mangle_name(value_name)
            lines.append('if (%s & mode_bits ) {' % (value_name))
            lines.append('ret_value |= %s;' % (intern_name))
            lines.append('}')
        lines.append('return ret_value;')
        self.dump_lines(lines)
        self.dump('}')


    def generate_pointer_convert_fn(self, type_, fn_name, constants):
        abi_type = self.mangle_name(type_)
        self.dump(f'{consts.INLINE_ATTRS} void {fn_name}({abi_type} *ptr)')
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
#       self.generate_pointer_convert_fn('MPI_Request', ConvertFuncs.REQUEST, consts.RESERVED_REQUESTS)
        self.generic_convert(ConvertFuncs.REQUEST, 'request', 'MPI_Request', consts.RESERVED_REQUESTS)

    def generate_request_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.REQUEST, 'request', 'MPI_Request', consts.RESERVED_REQUESTS)

#   def generate_file_convert_fn(self):
#       self.generate_pointer_convert_fn('MPI_File', ConvertFuncs.FILE, consts.RESERVED_FILES)

    def generate_status_convert_fn(self):
        type_ = 'MPI_Status'
        abi_type = self.mangle_name(type_)
        self.dump(f'{consts.INLINE_ATTRS} void {ConvertFuncs.STATUS}({type_} *out, {abi_type} *inp)')
        self.dump('{')
        self.dump('    void *ptr = &out->_ucount;')
        self.dump(f'    out->MPI_SOURCE = {ConvertFuncs.SOURCE}(inp->MPI_SOURCE);')
        self.dump(f'    out->MPI_TAG = {ConvertFuncs.TAG}(inp->MPI_TAG);')
        self.dump('    out->_cancelled = inp->MPI_internal[0];')
        self.dump('    memcpy(ptr, &inp->MPI_internal[1], sizeof(out->_ucount));')
        self.dump(f'    out->MPI_ERROR = {ConvertFuncs.ERROR_CLASS}(inp->MPI_ERROR);')
        # Ignoring the private fields for now
        self.dump('}')

    def generate_status_convert_fn_intern_to_abi(self):
        type_ = 'MPI_Status'
        abi_type = self.mangle_name(type_)
        self.dump(f'{consts.INLINE_ATTRS} void {ConvertOMPIToStandard.STATUS}({abi_type} *out, {type_} *inp)')
        self.dump('{')
        self.dump('    void *ptr = &out->MPI_internal[1];')
        self.dump(f'    out->MPI_SOURCE = {ConvertOMPIToStandard.SOURCE}(inp->MPI_SOURCE);')
        self.dump(f'    out->MPI_TAG = {ConvertOMPIToStandard.TAG}(inp->MPI_TAG);')
        self.dump('    out->MPI_internal[0] = inp->_cancelled;')
        self.dump('    memcpy(ptr, &inp->_ucount, sizeof(inp->_ucount));')
        self.dump(f'    out->MPI_ERROR = {ConvertOMPIToStandard.ERROR_CLASS}(inp->MPI_ERROR);')
        # Ignoring the private fields for now
        self.dump('}')

    def generate_errhandler_args_convert_fn_intern_to_abi(self, header_only=False):
        if header_only == True:
            self.dump(f'void ompi_convert_errhandler_args_intern_to_abi(void *object, int object_type, int *err_code);')
            return
        self.dump(f'void ompi_convert_errhandler_args_intern_to_abi(void *object, int object_type, int *err_code)')
        self.dump('{')
        lines = []
        lines.append('ompi_communicator_t **comm;')
        lines.append('ompi_file_t **file;')
        lines.append('ompi_instance_t **instance;')
        lines.append('ompi_win_t **win;')
        lines.append(f'*err_code = {ConvertOMPIToStandard.ERROR_CLASS}(*err_code);')
        lines.append('switch(object_type) {')
        lines.append('case OMPI_ERRHANDLER_TYPE_COMM:')
        lines.append('comm = (ompi_communicator_t **)object;')
        lines.append(f'*comm = (ompi_communicator_t *){ConvertOMPIToStandard.COMM}(*comm);')
        lines.append('break;')
        lines.append('case OMPI_ERRHANDLER_TYPE_WIN:')
        lines.append('win = (ompi_win_t **)object;')
        lines.append(f'*win = (ompi_win_t *){ConvertOMPIToStandard.WIN}(*win);')
        lines.append('break;')
        lines.append('case OMPI_ERRHANDLER_TYPE_FILE:')
        lines.append('file = (ompi_file_t **)object;')
        lines.append(f'*file = (ompi_file_t *){ConvertOMPIToStandard.FILE}(*file);')
        lines.append('break;')
        lines.append('case OMPI_ERRHANDLER_TYPE_INSTANCE:')
        lines.append('instance = (ompi_instance_t **)object;')
        lines.append(f'*instance = (ompi_instance_t *){ConvertOMPIToStandard.SESSION}(*instance);')
        lines.append('break;')
        lines.append('};')
        self.dump_lines(lines);
        self.dump('}')

    def define(self, type_, name, value):
        self.dump(f'#define {name} OMPI_CAST_CONSTANT({type_}, {value})')

    def define_all(self, type_, constants):
        for i, const in enumerate(constants):
            self.define(self.mangle_name(type_), self.mangle_name(const), i + 1)
        self.dump()

    def dump_include_file_code(self):
        '''generate code to use in include source file for converter functions'''
        header_guard = '_ABI_CONVERTERS_'
        self.dump(f'#ifndef {header_guard}')
        self.dump(f'#define {header_guard}')

        self.dump('#include "stddef.h"')
        self.dump('#include "stdint.h"')

        self.dump("""
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
""")
        self.dump('/*')
        self.dump(' * see section 20.3.4 of the MPI 5.0 standard')
        self.dump(' */')
        self.dump('#define OMPI_ABI_HANDLE_BASE_OFFSET 16385')
        self.dump('\n')
        self.dump('/*')
        self.dump(' * helper functions for abi generated routines ')
        self.dump(' */')
        self.dump(f'{consts.INLINE_ATTRS} void *ompi_abi_malloc(MPI_Count count, size_t elsize)')
        self.dump('{')
        lines = []
        lines.append('void *aptr = NULL;')
        lines.append('if (count > 0) {')
        lines.append('aptr = (void *)malloc(count * elsize);')
        lines.append('assert(NULL != aptr);')
        lines.append('}')
        lines.append('return aptr;')
        lines.append('}')
        lines.append('\n')
        self.dump_lines(lines)

        #
        # generate prototypes for methods included in standalond converter methods file
        #        
        self.dump('\n')
        self.generate_errhandler_args_convert_fn_intern_to_abi(header_only=True)
        self.dump('\n')

        # Now generate the conversion code - there's a reason for the order here
        # some converters depend on others being declared earlier in the include
        # file
        self.generate_error_convert_fn()
        self.generate_error_convert_fn_intern_to_abi()
        self.generate_comm_convert_fn()
        self.generate_comm_convert_fn_intern_to_abi()
        self.generate_info_convert_fn()
        self.generate_info_convert_fn_intern_to_abi()
        self.generate_file_convert_fn()
        self.generate_file_convert_fn_intern_to_abi()
        self.generate_group_convert_fn()
        self.generate_group_convert_fn_intern_to_abi()
#       self.generate_datatype_convert_fn()
#       self.generate_datatype_convert_fn_intern_to_abi()
        self.generate_new_datatype_convert_fn()
        self.generate_new_datatype_convert_fn_intern_to_abi()
        self.generate_errhandler_convert_fn()
        self.generate_errhandler_convert_fn_intern_to_abi()
        self.generate_message_convert_fn()
        self.generate_message_convert_fn_intern_to_abi()
        self.generate_op_convert_fn()
        self.generate_op_convert_fn_intern_to_abi()
        self.generate_session_convert_fn()
        self.generate_session_convert_fn_intern_to_abi()
        self.generate_win_convert_fn()
        self.generate_win_convert_fn_intern_to_abi()
        self.generate_request_convert_fn()
        self.generate_request_convert_fn_intern_to_abi()
        self.generate_tag_convert_fn()
        self.generate_tag_convert_fn_intern_to_abi()
        self.generate_source_convert_fn()
        self.generate_source_convert_fn_intern_to_abi()
        self.generate_status_convert_fn()
        self.generate_status_convert_fn_intern_to_abi()
        self.generate_attr_key_convert_fn()
        self.generate_attr_key_convert_fn_intern_to_abi()
        self.generate_ts_level_convert_fn()
        self.generate_ts_level_convert_fn_intern_to_abi()
        self.generate_pvar_session_convert_fn()
        self.generate_pvar_session_convert_fn_intern_to_abi()
        self.generate_cvar_handle_convert_fn()
        self.generate_cvar_handle_convert_fn_intern_to_abi()
        self.generate_pvar_handle_convert_fn()
        self.generate_pvar_handle_convert_fn_intern_to_abi()
        self.generate_t_enum_convert_fn()
        self.generate_t_enum_convert_fn_intern_to_abi()
        self.generate_t_bind_convert_fn()
        self.generate_t_bind_convert_fn_intern_to_abi()
        self.generate_t_verbosity_convert_fn()
        self.generate_t_verbosity_convert_fn_intern_to_abi()
        self.generate_t_source_order_convert_fn()
        self.generate_t_source_order_convert_fn_intern_to_abi()
        self.generate_pvar_class_convert_fn()
        self.generate_pvar_class_convert_fn_intern_to_abi()
        self.generate_mode_bits_convert_fn()
        self.generate_mode_bits_convert_fn_intern_to_abi()
        self.generate_rma_mode_bits_convert_fn()
        self.generate_rma_mode_bits_convert_fn_intern_to_abi()
        self.generate_buffer_convert_fn()
        self.generate_buffer_convert_fn_intern_to_abi()

        #
        # the following only need abi to intern converters
        #
        self.generate_comm_copy_attr_convert_fn()
        self.generate_comm_delete_attr_convert_fn()
        self.generate_type_copy_attr_convert_fn()
        self.generate_type_delete_attr_convert_fn()
        self.generate_win_copy_attr_convert_fn()
        self.generate_win_delete_attr_convert_fn()
        self.generate_comm_split_type_convert_fn()
        self.generate_weight_convert_fn()
        self.generate_subarray_order_convert_fn()
        self.generate_subarray_distrib_types_convert_fn()
        self.generate_subarray_dargs_types_convert_fn()
        self.generate_root_convert_fn()
        self.generate_t_cb_safety_convert_fn()
        self.generate_win_lock_convert_fn()
        self.generate_whence_convert_fn()
        self.generate_typeclass_convert_fn()

        #
        # the following only need intern to abi converters
        #
        self.generate_comm_cmp_convert_fn_intern_to_abi()
        self.generate_combiner_convert_fn_intern_to_abi()
        self.generate_topo_convert_fn_intern_to_abi()

        self.dump("""
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
""")
        self.dump(f'#endif /* {header_guard} */')

    def dump_code(self):
        '''generate code to use in standalone source file for converter functions'''
        self.dump('#include "stddef.h"')
        self.dump('#include "stdint.h"')
        self.dump('#include "ompi/communicator/communicator.h"')
        self.dump('#include "ompi/win/win.h"')
        self.dump('#include "ompi/file/file.h"')
        self.dump('#include "ompi/instance/instance.h"')
        self.dump('#include "ompi/errhandler/errhandler.h"')
        self.dump('#include "ompi/mpi/c/abi.h"')
        self.dump('#include "ompi/mpi/c/abi_converters.h"')

        #
        # special case converters:
        #    - errhandler functions convert
        #
        self.generate_errhandler_args_convert_fn_intern_to_abi()

def print_profiling_header(fn_name, out):
    """Print the profiling header code."""
    out.dump('#if OMPI_BUILD_MPI_PROFILING')
    out.dump('#if OPAL_HAVE_WEAK_SYMBOLS')
    out.dump(f'#pragma weak {fn_name} = P{fn_name}')
    out.dump('#endif')
    out.dump(f'#define {fn_name} P{fn_name}')
    out.dump('#endif')


def print_cdefs_for_bigcount(out, enable_count=False):
    if enable_count:
        out.dump('#undef OMPI_BIGCOUNT_SRC')
        out.dump('#define OMPI_BIGCOUNT_SRC 1')
    else:
        out.dump('#undef OMPI_BIGCOUNT_SRC')
        out.dump('#define OMPI_BIGCOUNT_SRC 0')

def print_cdefs_for_abi(out, abi_type='ompi'):
    if abi_type == 'ompi':
        out.dump('#undef OMPI_ABI_SRC')
        out.dump('#define OMPI_ABI_SRC 0')
    else:
        out.dump('#undef OMPI_ABI_SRC')
        out.dump('#define OMPI_ABI_SRC 1')

def generate_replacements(mangle_names=False):
    replacements = {}
    for key in consts.MAX_STRING_LEN_CONSTANTS:
        if mangle_names == True:
            val = util.abi_internal_name(key)
        else:
            val = key
        replacements[key] = val
    return replacements

def ompi_abi(base_name, template, out, suppress_bc=False, suppress_nbc=False):
    """Generate the OMPI ABI functions."""
    template.print_header(out)
    if suppress_nbc == False:
        print_profiling_header(base_name, out)
        print_cdefs_for_bigcount(out)
        print_cdefs_for_abi(out)
        out.dump(template.prototype.signature(base_name, abi_type='ompi'))
        template.print_body(func_name=base_name, out=out,
                            replacements=generate_replacements(mangle_names=False))
    # Check if we need to generate the bigcount interface
    if util.prototype_has_bigcount(template.prototype) and suppress_bc == False:
        # there are some special cases where we need to explicitly define the bigcount functions in the template file
        if base_name[-2:] == "_c":
            base_name_c = f'{base_name}'
        else:
            base_name_c = f'{base_name}_c'
        print_profiling_header(base_name_c, out)
        print_cdefs_for_bigcount(out, enable_count=True)
        print_cdefs_for_abi(out)
        out.dump(template.prototype.signature(base_name_c, abi_type='ompi', enable_count=True))
        template.print_body(func_name=base_name_c, out=out,
                            replacements=generate_replacements(mangle_names=False))

ABI_INTERNAL_HEADER = 'ompi/mpi/c/abi.h'
ABI_INTERNAL_CONVERTOR = 'ompi/mpi/c/abi_converters.h'


def standard_abi(base_name, template, out, suppress_bc=False, suppress_nbc=False):
    """Generate the standard ABI functions."""
    template.print_header(out)
    out.dump(f'#include "{ABI_INTERNAL_HEADER}"')
    out.dump(f'#include "{ABI_INTERNAL_CONVERTOR}"')
    print_cdefs_for_abi(out,abi_type='standard')

    # If any parameters are pointers to user callback functions, generate code
    # for callback wrappers
    if util.prototype_needs_callback_wrappers(template.prototype):
        params = [param.construct(abi_type='standard') for param in template.prototype.params]
        for param in params:
            if param.callback_wrapper_code:
                lines = []
                lines.extend(param.callback_wrapper_code)
                for line in lines:
                    out.dump(line)

    # Static internal function (add a random component to avoid conflicts)
    if suppress_nbc == False:
        internal_name = f'ompi_abi_{template.prototype.name}'
        print_cdefs_for_bigcount(out)
        print_cdefs_for_abi(out, abi_type='standard')
        internal_sig = template.prototype.signature(internal_name, abi_type='ompi',
                                                    enable_count=False)
        out.dump(consts.INLINE_ATTRS, internal_sig)
        template.print_body(func_name=base_name, out=out, replacements=generate_replacements(mangle_names=True))
    if util.prototype_has_bigcount(template.prototype) and suppress_bc == False:
        internal_name = f'ompi_abi_{template.prototype.name}_c'
        print_cdefs_for_bigcount(out, enable_count=True)
        print_cdefs_for_abi(out, abi_type='standard')
        internal_sig = template.prototype.signature(internal_name, abi_type='ompi',
                                                    enable_count=True)
        out.dump(consts.INLINE_ATTRS, internal_sig)
        template.print_body(func_name=base_name, out=out, replacements=generate_replacements(mangle_names=True))

    def generate_function(prototype, fn_name, internal_fn, out, enable_count=False):
        """Generate a function for the standard ABI."""
        print_profiling_header(fn_name,out)
#       print_cdefs_for_bigcount(out, enable_count)

        # Handle type conversions and arguments
        params = [param.construct(abi_type='standard') for param in prototype.params]
        out.dump(prototype.signature(fn_name, abi_type='standard', enable_count=enable_count))
        out.dump('{')
        lines = []
        return_type = prototype.return_type.construct(abi_type='standard')
        lines.append(f'{return_type.tmp_type_text()} ret_value;')
        for param in params:
            if param.need_async_cleanup == True:
                lines.append('int idx  = 0;')
                break
        for param in params:
#           print("param = " + str(param) + " " + str(param.argument))
            if param.init_code:
                lines.extend(param.init_code)
        pass_args = ', '.join(param.argument for param in params)
        lines.append(f'ret_value = {internal_fn}({pass_args});')
        for param in params:
            if param.final_code:
                lines.extend(param.final_code)
        lines.extend(return_type.return_code('ret_value'))

        # Indent the lines
        lines = util.indent_lines(lines, 4 * ' ', start=1)
        for line in lines:
            out.dump(line)
        out.dump('}')

    if suppress_nbc == False:
        internal_name = f'ompi_abi_{template.prototype.name}'
        generate_function(template.prototype, base_name, internal_name, out)
    if util.prototype_has_bigcount(template.prototype) and suppress_bc == False:
        if base_name[-2:] == "_c":
            base_name_c = f'{base_name}'
        else:
            base_name_c = f'{base_name}_c'
        internal_name = f'ompi_abi_{template.prototype.name}_c'
        generate_function(template.prototype, base_name_c, internal_name, out,
                          enable_count=True)

def generate_converters(args, out):
    """Generate ABI conversion methods for either the include file or the standalone source file"""
    out.dump(f'/* {consts.GENERATED_MESSAGE} */')
    builder = ABIConverterBuilder(out)
    if args.type  == 'include_file':
        builder.dump_include_file_code()
    else:
        builder.dump_code()

def generate_header(args, out):
    """Generate an ABI header and conversion code."""
    out.dump(f'/* {consts.GENERATED_MESSAGE} */')
    prototypes = [SourceTemplate.load(file_, args.srcdir, type_constructor=Type.construct).prototype
                  for file_ in args.file]
    builder = ABIHeaderBuilder(prototypes, out, external=args.external)
    builder.dump_header()


def generate_source(args, out):
    """Generate source file."""
    out.dump(f'/* {consts.GENERATED_MESSAGE} */')
    template = SourceTemplate.load(args.source_file, type_constructor=Type.construct)
    if args.mpit  == True:
        base_name = util.mpit_fn_name_from_base_fn_name(template.prototype.name)
    else:
        base_name = util.mpi_fn_name_from_base_fn_name(template.prototype.name)
    if args.type == 'ompi':
        ompi_abi(base_name, template, out, args.suppress_bc, args.suppress_nbc)
    else:
        standard_abi(base_name, template, out, args.suppress_bc, args.suppress_nbc)
