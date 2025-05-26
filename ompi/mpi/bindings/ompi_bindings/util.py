# Copyright (c) 2024-2025 Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
"""Utility code for OMPI binding generation."""
import textwrap


class OutputFile:
    """Output file of script."""

    def __init__(self, fp):
        self.fp = fp

    def dump(self, *pargs, **kwargs):
        print(*pargs, **kwargs, file=self.fp)


def prepare_text(text):
    """Prepare text to be output, removing extra lines and whitespace."""
    text = textwrap.dedent(text)
    lines = text.split('\n')
    out_lines = []
    new_lines = 0
    for line in lines:
        line = line.rstrip()
        # Only allow one blank line
        if not line:
            new_lines += 1
        else:
            new_lines = 0
        if new_lines > 1:
            continue
        out_lines.append(line)
    return '\n'.join(line for line in lines if line.strip())


class BindingError(Exception):
    """Thrown when a binding error is encountered."""


def validate_allowed_keys(keys, req_keys, type_name, param_name):
    """Validate allowed keys for a type, raising an error on failure."""
    missing_keys = [key for key in req_keys if key not in keys]
    invalid_keys = [key for key in keys if key not in req_keys]
    init_message = f'Param {param_name} with type {type_name}'
    if missing_keys and invalid_keys:
        raise BindingError(f'{init_message} has missing keys ({missing_keys}) and invalid keys ({invalid_keys})')
    elif missing_keys:
        raise BindingError(f'{init_message} has missing keys: {missing_keys}')
    elif invalid_keys:
        raise BindingError(f'{init_message} has invalid keys: {invalid_keys}')


def ext_api_func_name(fn_name, bigcount=False):
    """Produce the external MPI API function name."""
    suffix = '_c' if bigcount else ''
    return f'MPI_{fn_name.capitalize()}{suffix}'


def ext_api_func_name_profile(fn_name, bigcount=False):
    """Produce the external PMPI API function name."""
    return f'P{ext_api_func_name(fn_name, bigcount)}'


def fortran_name(fn_name, bigcount=False, needs_ts=False, gen_f90=False):
    """Produce the final f08 name from base_name. See section 19.2 of MPI 4.1 standard."""
    name = ''
    if gen_f90 == False:
        suffix = '_c' if bigcount else ''
        ts = 'ts' if needs_ts else ''
        name = f'MPI_{fn_name.capitalize()}{suffix}_f08{ts}'
    else:
        ts = '_FTS' if needs_ts else ''
        name = f'MPI_{fn_name.capitalize()}{ts}'
    return name

def fortran_f08_generic_interface_name(fn_name):
    """Produce the generic interface name from the base_name."""
    return f'MPI_{fn_name.capitalize()}'

def break_param_lines_fortran(start, params, end):
    """Break paramters for a fortran call onto multiple lines.

    This is often necessary to avoid going over the max line length of 132
    characters.
    """
    assert len(params) > 0, 'expected at least one parameter'
#
#   handle special case of just one parameter and return
#
    if len(params) == 1:
        result_lines = [f'{start}{params[0]}{end}']
        return result_lines    
    indent = len(start) * ' '
    lines = [f'{start}{params[0]},']
    for param in params[1:-1]:
        lines.append(f'{indent}{param},')
    last_line = f'{indent}{params[-1]}{end}'
    max_len = max(len(line) for line in lines)
    max_len = max(max_len, len(last_line))
    result_lines = []
    for line in lines:
        spaces = (max_len - len(line) + 1) * ' '
        result_lines.append(f'{line}{spaces}&')
    result_lines.append(last_line)
    return result_lines


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


def mpi_fn_name_from_base_fn_name(name):
    """Convert from a base name to the standard 'MPI_*' name."""
    return f'MPI_{name.capitalize()}'


def abi_internal_name(extname):
    """Convert from the ABI external name to an internal name.

    Used to avoid conflicts with existing MPI names.
    """
    return f'{extname}_ABI_INTERNAL'


BIGCOUNT_TYPE_NAMES = [
    'COUNT',
    'COUNT_ARRAY',
    'DISP',
    'DISP_ARRAY',
    'DISP_OUT',
    'COUNT_INOUT',
    'COUNT_OUT',
    'AINT_COUNT',
    'AINT_COUNT_ARRAY',
    'AINT_COUNT_OUT',
    'AINT_COUNT_INOUT',
    'INT_AINT_OUT',
    'USER_FUNCTION',
    'DATAREP_CONVERSION_FUNCTION',
]


def prototype_has_bigcount(prototype):
    """Should this prototype have a bigcount version?"""
    return any(param.type_name in BIGCOUNT_TYPE_NAMES for param in prototype.params)

BUFFER_TYPE_NAMES = [
    'BUFFER',
    'BUFFER_ASYNC',
    'BUFFER_OUT',
    'BUFFER_ASYNC_OUT',
]

def prototype_has_buffers(prototype):
    """Does the prototype have buffer arguments?"""
    if any(param.type_name in BUFFER_TYPE_NAMES for param in prototype.params):
        return True
    else:
        return False
