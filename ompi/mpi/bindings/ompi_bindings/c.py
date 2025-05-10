# Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
# Copyright (c) 2023      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
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

    def generate_error_convert_fn(self):
        self.dump(f'{consts.INLINE_ATTRS} int {ConvertFuncs.ERROR_CLASS}(int error_class)')
        self.dump('{')
        lines = []
        lines.append('switch (error_class) {')
        for error in consts.ERROR_CLASSES:
            lines.append(f'case {self.mangle_name(error)}:')
            lines.append(f'return {error};')
        lines.append('default:')
        lines.append('return error_class;')
        lines.append('}')
        self.dump_lines(lines)
        self.dump('}')

    def generic_convert(self, fn_name, param_name, type_, value_names):
        intern_type = self.mangle_name(type_)
        self.dump(f'{consts.INLINE_ATTRS} {type_} {fn_name}({intern_type} {param_name})')
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
        lines.append(f'return ({intern_type}) {param_name};')
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

    def generate_group_convert_fn(self):
        self.generic_convert(ConvertFuncs.GROUP, 'group', 'MPI_Group', consts.RESERVED_GROUPS)

    def generate_group_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.GROUP, 'group', 'MPI_Group', consts.RESERVED_GROUPS)

    def generate_message_convert_fn(self):
        self.generic_convert(ConvertFuncs.MESSAGE, 'message', 'MPI_Message', consts.RESERVED_MESSAGES)

    def generate_message_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.MESSAGE, 'message', 'MPI_Message', consts.RESERVED_MESSAGES)

    def generate_op_convert_fn(self):
        self.generic_convert(ConvertFuncs.OP, 'op', 'MPI_Op', consts.COLLECTIVE_OPERATIONS)

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
        self.generate_pointer_convert_fn('MPI_Request', ConvertFuncs.REQUEST, consts.RESERVED_REQUESTS)

    def generate_request_convert_fn_intern_to_abi(self):
        self.generic_convert_reverse(ConvertOMPIToStandard.REQUEST, 'request', 'MPI_Request', consts.RESERVED_REQUESTS)

#   def generate_file_convert_fn(self):
#       self.generate_pointer_convert_fn('MPI_File', ConvertFuncs.FILE, consts.RESERVED_FILES)

    def generate_status_convert_fn(self):
        type_ = 'MPI_Status'
        abi_type = self.mangle_name(type_)
        self.dump(f'{consts.INLINE_ATTRS} void {ConvertFuncs.STATUS}({type_} *out, {abi_type} *inp)')
        self.dump('{')
        self.dump('    out->MPI_SOURCE = inp->MPI_SOURCE;')
        self.dump('    out->MPI_TAG = inp->MPI_TAG;')
        self.dump(f'    out->MPI_ERROR = {ConvertFuncs.ERROR_CLASS}(inp->MPI_ERROR);')
        # Ignoring the private fields for now
        self.dump('}')

    def generate_status_convert_fn_intern_to_abi(self):
        type_ = 'MPI_Status'
        abi_type = self.mangle_name(type_)
        self.dump(f'{consts.INLINE_ATTRS} void {ConvertOMPIToStandard.STATUS}({abi_type} *out, {type_} *inp)')
        self.dump('{')
        self.dump('    out->MPI_SOURCE = inp->MPI_SOURCE;')
        self.dump('    out->MPI_TAG = inp->MPI_TAG;')
#       self.dump(f'    out->MPI_ERROR = {ConvertOMPIToStandard.ERROR_CLASS}(inp->MPI_ERROR);')
        # Ignoring the private fields for now
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

        for i, err in enumerate(consts.ERROR_CLASSES):
            self.dump(f'#define {self.mangle_name(err)} {i + 1}')
        self.dump()

        self.define_all('MPI_Datatype', consts.PREDEFINED_DATATYPES)
        self.define_all('MPI_Op', consts.COLLECTIVE_OPERATIONS)
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
    int mpi_abi_private[5];
};""")
        self.dump(f'typedef struct MPI_Status_ABI {self.mangle_name("MPI_Status")};')
        self.dump()
        # user functions
        self.dump('typedef int (MPI_Copy_function)(MPI_Comm_ABI_INTERNAL, int, void *, void *, void *, int *);')
        self.dump('typedef int (MPI_Delete_function)(MPI_Comm_ABI_INTERNAL, int, void *, void *);')
        # Function signatures
        for sig in self.signatures:
            self.dump(f'{sig};')
#           print("Working on signature " + str(sig))
        self.dump('int MPI_Abi_details(int *buflen, char *details, MPI_Info *info);')
        self.dump('int MPI_Abi_supported(int *flag);')
        self.dump('int MPI_Abi_version(int *abi_major, int *abi_minor);')
        if not self.external:
            # Now generate the conversion code
            self.generate_error_convert_fn()
            self.generate_comm_convert_fn()
            self.generate_comm_convert_fn_intern_to_abi()
            self.generate_info_convert_fn()
            self.generate_info_convert_fn_intern_to_abi()
            self.generate_file_convert_fn()
            self.generate_file_convert_fn_intern_to_abi()
            self.generate_group_convert_fn()
            self.generate_group_convert_fn_intern_to_abi()
            self.generate_datatype_convert_fn()
            self.generate_datatype_convert_fn_intern_to_abi()
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
            self.generate_status_convert_fn()
            self.generate_status_convert_fn_intern_to_abi()

        self.dump("""
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
""")
        self.dump(f'#endif /* {header_guard} */')


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

def ompi_abi(base_name, template, out):
    """Generate the OMPI ABI functions."""
    template.print_header(out)
    print_profiling_header(base_name, out)
    print_cdefs_for_bigcount(out)
    out.dump(template.prototype.signature(base_name, abi_type='ompi'))
    template.print_body(func_name=base_name, out=out)
    # Check if we need to generate the bigcount interface
    if util.prototype_has_bigcount(template.prototype):
        base_name_c = f'{base_name}_c'
        print_profiling_header(base_name_c, out)
        print_cdefs_for_bigcount(out, enable_count=True)
        out.dump(template.prototype.signature(base_name_c, abi_type='ompi', enable_count=True))
        template.print_body(func_name=base_name_c, out=out)


ABI_INTERNAL_HEADER = 'ompi/mpi/c/abi.h'


def standard_abi(base_name, template, out):
    """Generate the standard ABI functions."""
    template.print_header(out)
    out.dump(f'#include "{ABI_INTERNAL_HEADER}"')

    # Static internal function (add a random component to avoid conflicts)
    internal_name = f'ompi_abi_{template.prototype.name}'
    print_cdefs_for_bigcount(out)
    internal_sig = template.prototype.signature(internal_name, abi_type='ompi',
                                                enable_count=False)
    out.dump(consts.INLINE_ATTRS, internal_sig)
    template.print_body(func_name=base_name, out=out)
    if util.prototype_has_bigcount(template.prototype):
        internal_name = f'ompi_abi_{template.prototype.name}_c'
        print_cdefs_for_bigcount(out, enable_count=True)
        internal_sig = template.prototype.signature(internal_name, abi_type='ompi',
                                                    enable_count=True)
        out.dump(consts.INLINE_ATTRS, internal_sig)
        template.print_body(func_name=base_name, out=out)

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

    internal_name = f'ompi_abi_{template.prototype.name}'
    generate_function(template.prototype, base_name, internal_name, out)
    if util.prototype_has_bigcount(template.prototype):
        base_name_c = f'{base_name}_c'
        internal_name = f'ompi_abi_{template.prototype.name}_c'
        generate_function(template.prototype, base_name_c, internal_name, out,
                          enable_count=True)


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
    base_name = util.mpi_fn_name_from_base_fn_name(template.prototype.name)
    if args.type == 'ompi':
        ompi_abi(base_name, template, out)
    else:
        standard_abi(base_name, template, out)
