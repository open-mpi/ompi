# Copyright (c) 2024      Triad National Security, LLC. All rights
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
from ompi_bindings import compiler, consts, util
from ompi_bindings.fortran_type import FortranType


FortranParameter = namedtuple('FortranParameter', ['type_name', 'name', 'dep_params'])
FortranPrototype = namedtuple('FortranPrototype', ['fn_name', 'parameters'])


def load_prototypes(fname):
    """Load the prototypes from a JSON file."""
    with open(fname) as fp:
        data = json.load(fp)
        prototypes = []
        for proto in data:
            fn_name = proto['name']
            parameters = []
            for param in proto['parameters']:
                type_name = param['type']
                type_ = FortranType.get(type_name)
                param_name = param['name']
                dep_params = param['dep_params'] if 'dep_params' in param else None
                try:
                    type_.validate_dep_param_keys(param_name, [] if dep_params is None else dep_params.keys())
                except util.BindingError as err:
                    raise util.BindingError(f'Invalid prototype "{fn_name}": {err}') from None
                parameters.append(FortranParameter(type_name, param_name, dep_params))
            prototypes.append(FortranPrototype(fn_name, parameters))
        return prototypes


class FortranBinding:
    """Class for generating the binding for a single function."""

    def __init__(self, prototype, out, bigcount=False, ts=False):
        # Generate bigcount interface version
        self.bigcount = bigcount
        # Generate files with support for TS 29113 or not
        self.ts = ts
        self.fn_name = prototype.fn_name
        self.out = out
        self.parameters = []
        param_map = {}
        dep_params = {}
        for param in prototype.parameters:
            type_ = FortranType.get(param.type_name)
            param_type = type_(param.name, self.fn_name, bigcount=bigcount, ts=ts)
            self.parameters.append(param_type)
            param_map[param.name] = param_type
            if param.dep_params is not None:
                dep_params[param.name] = param.dep_params
        # Set dependent parameters for those that need them
        for name, deps in dep_params.items():
            try:
                param_map[name].dep_params = {key: param_map[dep_name] for key, dep_name in deps.items()}
            except KeyError as err:
                raise util.BindingError(
                    f'Invalid dependent type for parameter "{name}" (prototype "{prototype.fn_name}"): {err}'
                ) from None

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
        sub_name = util.fortran_f08_name(self.fn_name, bigcount=self.bigcount)
        params = [param.name for param in self.parameters]
        params.append(consts.FORTRAN_ERROR_NAME)
        lines = util.break_param_lines_fortran(f'subroutine {sub_name}(', params, ')')
        for line in lines:
            self.dump(line)

    def _print_fortran_subroutine_end(self):
        """Output the Fortran end subroutine line."""
        sub_name = util.fortran_f08_name(self.fn_name, bigcount=self.bigcount)
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
        parameters = [param.c_parameter() for param in self.parameters]
        # Always append the integer error
        parameters.append(f'MPI_Fint *{consts.C_ERROR_NAME}')
        parameters = ', '.join(parameters)
        # Just put the signature here to silence `-Wmissing-prototypes`
        c_func = self.c_func_name
        self.dump(f'void {c_func}({parameters});')
        self.dump(f'void {c_func}({parameters})')
        self.dump('{')
        self.dump(f'    int {consts.C_ERROR_TMP_NAME}; ')

        # First the temporary declarations
        for param in self.parameters:
            self.dump_lines(param.c_declare_tmp())

        # Shortcut conditions, if any
        for param in self.parameters:
            condition = param.c_shortcut_condition()
            if condition is None:
                continue
            self.dump(f'    if ({condition}) {{')
            self.dump(f'        *{consts.C_ERROR_NAME} = OMPI_INT_2_FINT(MPI_SUCCESS);')
            for other_param in self.parameters:
                self.dump_lines(other_param.c_shortcut_code())
            self.dump('        return;')
            self.dump('    }')

        # Prepare code for temporaries, etc.
        for param in self.parameters:
            self.dump_lines(param.c_prepare())

        # Call into the C API
        c_api_func = util.ext_api_func_name_profile(self.fn_name, bigcount=self.bigcount)
        arguments = [param.c_argument() for param in self.parameters]
        arguments = ', '.join(arguments)
        self.dump(f'    {consts.C_ERROR_TMP_NAME} = {c_api_func}({arguments});')

        # Post-processing code
        self.dump(f'    *{consts.C_ERROR_NAME} = OMPI_INT_2_FINT({consts.C_ERROR_TMP_NAME});')
        for param in self.parameters:
            self.dump_lines(param.c_post())
        self.dump('}')

    def print_interface(self):
        """Output just the Fortran interface for this binding."""
        self._print_fortran_subroutine()
        self._print_fortran_header(is_interface=True)
        self._print_fortran_subroutine_end()


def print_f_source_header(out):
    """Print the fortran f08 file header."""
    out.dump(f'! {consts.GENERATED_MESSAGE}')
    out.dump('#include "ompi/mpi/fortran/configure-fortran-output.h"')


def print_profiling_rename_macros(prototypes, out):
    """Print macros for renaming functions for the profiling interface.

    Previously hardcoded in mpi-f08-rename.h.
    """
    out.dump('#if OMPI_BUILD_MPI_PROFILING')
    for prototype in prototypes:
        name = util.fortran_f08_name(prototype.fn_name)
        out.dump(f'#define {name} P{name}')
        # Check for bigcount version
        if util.fortran_prototype_has_bigcount(prototype):
            bigcount_name = util.fortran_f08_name(prototype.fn_name, bigcount=True)
            out.dump(f'#define {bigcount_name} P{bigcount_name}')
    out.dump('#endif /* OMPI_BUILD_MPI_PROFILING */')


def print_c_source_header(out, ts=False):
    """Print the header of the C source file."""
    out.dump(f'/* {consts.GENERATED_MESSAGE} */')
    if ts:
        out.dump('#include <ISO_Fortran_binding.h>')
        out.dump('#include "ts.h"')
    out.dump('#include "ompi_config.h"')
    out.dump('#include "mpi.h"')
    out.dump('#include "ompi/errhandler/errhandler.h"')
    out.dump('#include "ompi/mpi/fortran/mpif-h/status-conversion.h"')
    out.dump('#include "ompi/mpi/fortran/base/constants.h"')
    out.dump('#include "ompi/mpi/fortran/base/fint_2_int.h"')
    out.dump('#include "ompi/request/request.h"')
    out.dump('#include "ompi/communicator/communicator.h"')


def print_binding(prototype, lang, out, bigcount=False, ts=False):
    """Print the binding with or without bigcount."""
    binding = FortranBinding(prototype, out=out, bigcount=bigcount, ts=ts)
    if lang == 'fortran':
        binding.print_f_source()
    else:
        binding.print_c_source()


def generate_code(args, out):
    """Generate binding code based on arguments."""
    prototypes = load_prototypes(args.prototypes)
    if args.lang == 'fortran':
        print_f_source_header(out)
        out.dump()
        print_profiling_rename_macros(prototypes, out)
        out.dump()
    else:
        print_c_source_header(out, ts=args.ts)
    for prototype in prototypes:
        out.dump()
        print_binding(prototype, args.lang, out, ts=args.ts)
        if util.fortran_prototype_has_bigcount(prototype):
            out.dump()
            out.dump('#if OMPI_BIGCOUNT')
            print_binding(prototype, args.lang, bigcount=True, out=out, ts=args.ts)
            out.dump('#endif /* OMPI_BIGCOUNT */')


def generate_interface(args, out):
    """Generate the Fortran interface files."""
    prototypes = load_prototypes(args.prototypes)
    out.dump(f'! {consts.GENERATED_MESSAGE}')
    for prototype in prototypes:
        ext_name = util.ext_api_func_name(prototype.fn_name)
        out.dump(f'interface {ext_name}')
        binding = FortranBinding(prototype, out=out, ts=args.ts)
        binding.print_interface()
        if util.fortran_prototype_has_bigcount(prototype):
            out.dump()
            binding_c = FortranBinding(prototype, out=out, bigcount=True, ts=args.ts)
            out.dump('#if OMPI_BIGCOUNT')
            binding_c.print_interface()
            out.dump('#endif /* OMPI_BIGCOUNT */')
        out.dump(f'end interface {ext_name}')
