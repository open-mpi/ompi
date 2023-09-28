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
from individual *.c.in template files.

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
import argparse
import re
import sys
import os
from ompi_bindings import consts, util
from ompi_bindings.c_type import Type
from ompi_bindings.parser import SourceTemplate


def print_profiling_header(fn_name, out):
    """Print the profiling header code."""
    out.dump('#if OMPI_BUILD_MPI_PROFILING')
    out.dump('#if OPAL_HAVE_WEAK_ALIASES')
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

def ompi_abi(base_name, template, out):
    """Generate the OMPI ABI functions."""
    template.print_header(out)
    print_profiling_header(base_name, out)
    print_cdefs_for_bigcount(out)
    print_cdefs_for_abi(out)
    out.dump(template.prototype.signature(base_name, abi_type='ompi'))
    template.print_body(func_name=base_name, out=out)
    # Check if we need to generate the bigcount interface
    if util.prototype_has_bigcount(template.prototype):
        base_name_c = f'{base_name}_c'
        print_profiling_header(base_name_c, out)
        print_cdefs_for_bigcount(out, enable_count=True)
        print_cdefs_for_abi(out)
        out.dump(template.prototype.signature(base_name_c, abi_type='ompi', enable_count=True))
        template.print_body(func_name=base_name_c, out=out)


ABI_INTERNAL_HEADER = 'ompi/mpi/c/abi.h'
ABI_INTERNAL_CONVERTOR = 'ompi/mpi/c/abi_converters.h'


def standard_abi(base_name, template, out):
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
    internal_name = f'ompi_abi_{template.prototype.name}'
    print_cdefs_for_bigcount(out)
    print_cdefs_for_abi(out, abi_type='standard')
    internal_sig = template.prototype.signature(internal_name, abi_type='ompi',
                                                enable_count=False)
    out.dump(consts.INLINE_ATTRS, internal_sig)
    template.print_body(func_name=base_name, out=out)
    if util.prototype_has_bigcount(template.prototype):
        internal_name = f'ompi_abi_{template.prototype.name}_c'
        print_cdefs_for_bigcount(out, enable_count=True)
        print_cdefs_for_abi(out, abi_type='standard')
        internal_sig = template.prototype.signature(internal_name, abi_type='ompi',
                                                    enable_count=True)
        out.dump(consts.INLINE_ATTRS, internal_sig)
        # FUNC_NAME in the bigcount body must report the _c variant (e.g.,
        # "MPI_Bcast_c", not "MPI_Bcast"), mirroring ompi_abi() above.
        if base_name[-2:] == "_c":
            base_name_c = f'{base_name}'
        else:
            base_name_c = f'{base_name}_c'
        template.print_body(func_name=base_name_c, out=out)

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


def generate_source(args, out):
    """Generate source file."""
    out.dump(f'/* {consts.GENERATED_MESSAGE} */')
    template = SourceTemplate.load(args.source_file, type_constructor=Type.construct)
    base_name = util.mpi_fn_name_from_base_fn_name(template.prototype.name)
    if args.type == 'ompi':
        ompi_abi(base_name, template, out)
    else:
        standard_abi(base_name, template, out)
