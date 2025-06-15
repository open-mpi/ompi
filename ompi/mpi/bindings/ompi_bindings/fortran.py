# Copyright (c) 2024-2025 Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
"""Fortran binding generation code.

This takes as input a *.in file containing a list of prototypes for Fortran
subroutines with generic types. Using this file, it can generate the Fortran
subroutines in one file and the C wraping code in another for all prototypes
listed.
"""
from collections import namedtuple
import json
import re
from ompi_bindings import consts, util
from ompi_bindings.fortran_type import FortranType
from ompi_bindings.parser import SourceTemplate


class FortranBinding:
    """Class for generating the binding for a single function."""

    def __init__(self, prototype, out, template=None, bigcount=False, needs_ts=False):
        # Generate bigcount interface version
        self.bigcount = bigcount
        self.fn_name = template.prototype.name
        self.out = out
        self.template = template
        self.needs_ts = needs_ts
        self.parameters = []
        for param in self.template.prototype.params:
            self.parameters.append(param.construct(fn_name=self.fn_name,
                                                   bigcount=bigcount))

    def dump(self, *pargs, **kwargs):
        """Write to the output file."""
        self.out.dump(*pargs, **kwargs)

    def _fn_name_suffix(self):
        """Return a suffix for function names."""
        return '_c' if self.bigcount else ''

    @property
    def c_func_name(self):
        """Produce the final C func name from base_name."""
        return f'ompi_{self.fn_name}_wrapper_f08{self._fn_name_suffix()}'

    @property
    def inner_call(self):
        """Produce the name of the function to call in the body of the C code."""
        return f'PMPI_{self.fn_name.capitalize()}{self._fn_name_suffix()}'

    def _use(self):
        """Determine the Fortran use-statements needed."""
        use = {}
        for param in self.parameters:
            for mod, name in param.use():
                if mod not in use:
                    use[mod] = set()
                use[mod].add(name)
        return use

    def _use_stmts(self):
        """Return a list of required use statments."""
        use = self._use()
        stmts = []
        for mod, names in use.items():
            names = ', '.join(names)
            stmts.append(f'use :: {mod}, only: {names}')
        return stmts

    def _print_fortran_interface(self):
        """Output the C subroutine binding for the Fortran code."""
        name = self.c_func_name
        self.dump('    interface')

        # Print the subroutine and parameter list, breaking parameters across lines
        subroutine_start = f'        subroutine {name}('
        params = [param.name for param in self.parameters]
        params.append(consts.FORTRAN_ERROR_NAME)
        lines = util.break_param_lines_fortran(start=subroutine_start, params=params, end=') &')
        for line in lines:
            self.dump(line)
        self.dump(f'            BIND(C, name="{name}")')

        use_stmts = self._use_stmts()
        for stmt in use_stmts:
            self.dump(f'            {stmt}')
        self.dump('            implicit none')
        for param in self.parameters:
            self.dump(f'            {param.declare_cbinding_fortran()}')
        self.dump(f'            INTEGER, INTENT(OUT) :: {consts.FORTRAN_ERROR_NAME}')
        self.dump(f'        end subroutine {name}')
        self.dump('    end interface')

    def _print_fortran_header(self, is_interface=False):
        """Print the header, including use stmts, dummy variable decls, etc..

        This does not include the subroutine line.
        """
        # Use statements
        use_stmts = self._use_stmts()
        for stmt in use_stmts:
            self.dump(f'    {stmt}')
        self.dump('    implicit none')
        # Parameters/dummy variable declarations
        for param in self.parameters:
            if is_interface:
                self.dump_lines(param.interface_predeclare())
            self.dump_lines(param.declare())
        # Add the integer error manually
        self.dump(f'    INTEGER, OPTIONAL, INTENT(OUT) :: {consts.FORTRAN_ERROR_NAME}')

    def _print_fortran_subroutine(self):
        """Output the Fortran subroutine line."""
        sub_name = util.fortran_f08_name(self.fn_name, bigcount=self.bigcount, needs_ts=self.needs_ts)
        params = [param.name for param in self.parameters]
        params.append(consts.FORTRAN_ERROR_NAME)
        lines = util.break_param_lines_fortran(f'subroutine {sub_name}(', params, ')')
        for line in lines:
            self.dump(line)

    def _print_fortran_subroutine_end(self):
        """Output the Fortran end subroutine line."""
        sub_name = util.fortran_f08_name(self.fn_name, bigcount=self.bigcount, needs_ts=self.needs_ts)
        self.dump(f'end subroutine {sub_name}')

    def dump_lines(self, line_text):
        for line in line_text.split('\n'):
            line = line.rstrip()
            if line:
                self.dump(f'    {line}')

    def print_f_source(self):
        """Output the main MPI Fortran subroutine."""
        self._print_fortran_subroutine()
        self._print_fortran_header()

        # Temporaries
        self.dump(f'    INTEGER :: {consts.C_ERROR_TMP_NAME}')
        for param in self.parameters:
            self.dump_lines(param.declare_tmp())

        # Interface for call to C function
        self.dump()
        self._print_fortran_interface()
        self.dump()

        # Output in pre C function call methods

        for param in self.parameters:
            self.dump_lines(param.pre_c_call())

        # Call into the C function
        call_start = f'    call {self.c_func_name}('
        params = [param.argument() for param in self.parameters]
        params.append(consts.C_ERROR_TMP_NAME)
        lines = util.break_param_lines_fortran(start=call_start, params=params, end=')')
        for line in lines:
            self.dump(line)

        # Convert error type
        self.dump(f'    if (present({consts.FORTRAN_ERROR_NAME})) {consts.FORTRAN_ERROR_NAME} = {consts.C_ERROR_TMP_NAME}')

        for param in self.parameters:
            self.dump_lines(param.post())

        self._print_fortran_subroutine_end()

    def print_c_source(self):
        """Output the C source and function that the Fortran calls into."""
        if self.template is None:
            return
        parameters = [param.c_parameter() for param in self.parameters]
        # Always append the integer error
        parameters.append(f'MPI_Fint *{consts.C_ERROR_NAME}')
        parameters = ', '.join(parameters)
        # Just put the signature here to silence `-Wmissing-prototypes`
        c_func = self.c_func_name
        self.dump(f'void {c_func}({parameters});')
        self.dump(f'void {c_func}({parameters})')
        count_type, disp_type, count_fint_type = ('MPI_Count', 'MPI_Aint', 'MPI_Count') if self.bigcount else ('int', 'int', 'MPI_Fint')
        self.template.print_body(c_func, out=self.out,
                                 replacements={'INNER_CALL': self.inner_call,
                                               'COUNT_TYPE': count_type,
                                               'COUNT_FINT_TYPE': count_fint_type,
                                               'DISP_TYPE': disp_type,
                                               'LOGICAL_TYPE': 'int'})

    def print_interface(self):
        """Output just the Fortran interface for this binding."""
        self._print_fortran_subroutine()
        self._print_fortran_header(is_interface=True)
        self._print_fortran_subroutine_end()


def print_f_source_header(out):
    """Print the fortran f08 file header."""
    out.dump(f'! {consts.GENERATED_MESSAGE}')
    out.dump('#include "ompi/mpi/fortran/configure-fortran-output.h"')


def print_profiling_rename_macros(templates, out, args):
    """Print macros for renaming functions for the profiling interface.

    Previously hardcoded in mpi-f08-rename.h.
    """
    out.dump('#if OMPI_BUILD_MPI_PROFILING')
    for template in templates:
        has_buffers = util.prototype_has_buffers(template.prototype)
        needs_ts = has_buffers and args.generate_ts_suffix
        name = util.fortran_f08_name(template.prototype.name, needs_ts=needs_ts)
        out.dump(f'#define {name} P{name}')
        # Check for bigcount version
        if util.prototype_has_bigcount(template.prototype):
            bigcount_name = util.fortran_f08_name(template.prototype.name, bigcount=True, needs_ts=needs_ts)
            out.dump(f'#define {bigcount_name} P{bigcount_name}')
        name = util.fortran_f08_generic_interface_name(template.prototype.name)
        out.dump(f'#define {name} P{name}')
    out.dump('#endif /* OMPI_BUILD_MPI_PROFILING */')


def print_c_source_header(out):
    """Print the header of the C source file."""
    out.dump(f'/* {consts.GENERATED_MESSAGE} */')
    out.dump('#include "ompi_config.h"')
    out.dump('#include "mpi.h"')
    out.dump('#include "ompi/errhandler/errhandler.h"')
    out.dump('#include "ompi/mpi/fortran/mpif-h/status-conversion.h"')
    out.dump('#include "ompi/mpi/fortran/base/constants.h"')
    out.dump('#include "ompi/mpi/fortran/base/fint_2_int.h"')
    out.dump('#include "ompi/request/request.h"')
    out.dump('#include "ompi/communicator/communicator.h"')
    out.dump('#include "ompi/win/win.h"')
    out.dump('#include "ompi/file/file.h"')
    out.dump('#include "ompi/errhandler/errhandler.h"')
    out.dump('#include "ompi/datatype/ompi_datatype.h"')
    out.dump('#include "ompi/attribute/attribute.h"')
    out.dump('#include "ompi/mca/coll/base/coll_base_util.h"')
    out.dump('#include "ts.h"')
    out.dump('#include "bigcount.h"')


def print_binding(prototype, lang, out, bigcount=False, template=None, needs_ts=False):
    """Print the binding with or without bigcount."""
    binding = FortranBinding(prototype, out=out, bigcount=bigcount, template=template, needs_ts=needs_ts)
    if lang == 'fortran':
        binding.print_f_source()
    else:
        binding.print_c_source()


def load_function_templates(prototype_files):
    """Load the templates from a file list."""
    return [
        SourceTemplate.load(fname, type_constructor=FortranType.construct)
        for fname in prototype_files
    ]


def generate_code(args, out):
    """Generate binding code based on arguments."""
    templates = load_function_templates(args.prototype_files)

    if args.lang == 'fortran':
        print_f_source_header(out)
        out.dump()
        print_profiling_rename_macros(templates, out, args)
        out.dump()
    else:
        print_c_source_header(out)

    for template in templates:
        out.dump()
        has_buffers = util.prototype_has_buffers(template.prototype)
        needs_ts = has_buffers and args.generate_ts_suffix
        print_binding(template.prototype, args.lang, out, template=template, needs_ts=needs_ts)
        if util.prototype_has_bigcount(template.prototype):
            out.dump()
            print_binding(template.prototype, args.lang, bigcount=True, out=out, template=template, needs_ts=needs_ts)


def generate_interface(args, out):
    """Generate the Fortran interface files."""
    out.dump(f'! {consts.GENERATED_MESSAGE}')

    templates = load_function_templates(args.prototype_files)
    print_profiling_rename_macros(templates, out, args)

    for template in templates:
        ext_name = util.ext_api_func_name(template.prototype.name)
        out.dump(f'interface {ext_name}')
        has_buffers = util.prototype_has_buffers(template.prototype)
        needs_ts = has_buffers and args.generate_ts_suffix
        binding = FortranBinding(template.prototype, template=template, needs_ts=needs_ts, out=out)
        binding.print_interface()
        if util.prototype_has_bigcount(template.prototype):
            out.dump()
            binding_c = FortranBinding(template.prototype, out=out, template=template,
                                       needs_ts=needs_ts, bigcount=True)
            binding_c.print_interface()
        out.dump(f'end interface {ext_name}')
