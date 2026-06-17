# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
"""Validate the Open MPI mpi_f08 interfaces against the MPI standard.

The mpi_f08 module is the only Fortran binding whose dummy-argument
*names* are part of the user-visible contract (keyword arguments).  This
script loads the MPI Forum's pympistandard metadata and checks that every
mpi_f08 procedure's dummy arguments agree with the standard on three
things:

  * name      -- the dummy-argument name (case-insensitive)
  * intent    -- INTENT(IN|OUT|INOUT)
  * type      -- the declared Fortran type

It is intended to be run at build time over the generated Fortran source
(api_f08_generated.F90 and the interface headers) plus the hand-written
*_f08.F90 files, and exits non-zero -- failing the build -- if any
mpi_f08 interface has drifted from the standard.  Only the mpi_f08
module is in scope; the C back-end and the older mpi (f90) module are
not checked.

Things the standard deliberately leaves to the implementation are not
flagged:

  * choice buffers (standard type 'TYPE(*), ...'); Open MPI renders
    these with its own ignore-TKR macro and may attach INTENT(IN).
  * any argument whose standard F08 intent is unspecified (None), e.g.
    a TYPE(MPI_Status) argument that must also accept MPI_STATUS_IGNORE.
  * a large-count (_c) procedure that the standard does not provide an
    F08 binding for.
"""

import argparse
import os
import re
import sys


# Automake-style result colors (PASS=green, FAIL=red, SKIP=blue), matching
# Automake's color-tests palette.
_STATUS_COLOR = {'PASS': '\033[0;32m', 'FAIL': '\033[0;31m', 'SKIP': '\033[1;34m'}
_COLOR_RESET = '\033[m'


def _use_color():
    """Colorize like Automake: AM_COLOR_TESTS=always forces it; otherwise a TTY."""
    setting = os.environ.get('AM_COLOR_TESTS', '')
    if setting == 'always':
        return True
    if setting == 'no' or os.environ.get('NO_COLOR'):
        return False
    return sys.stdout.isatty()


def emit_status(status, label, reason=None):
    """Print an Automake-style '<STATUS>: <label>' line, colorized if appropriate."""
    tag = status
    if _use_color():
        tag = f'{_STATUS_COLOR[status]}{status}{_COLOR_RESET}'
    line = f'{tag}: {label}'
    if reason:
        line += f' ({reason})'
    print(line)


def load_standard(pympistd_dir):
    """Load {function_key: {'r': params, 'c': params}} from pympistandard.

    Each params entry is a list of (name, intent, type) tuples for the
    regular ('r') and large-count/embiggened ('c') F08 bindings.
    """
    sys.path.insert(0, os.path.join(pympistd_dir, 'src'))
    import pympistandard as std
    std.use_api_version(1)

    def params_of(express):
        f08 = getattr(express, 'f08', None) if express is not None else None
        if f08 is None:
            return None
        return [(p.name.lower(), p.intent, p.type) for p in f08.parameters]

    table = {}
    for key, proc in std.PROCEDURES.items():
        entry = {}
        regular = params_of(proc.express)
        if regular is not None:
            entry['r'] = regular
        embiggened = params_of(proc.express.embiggen)
        if embiggened is not None:
            entry['c'] = embiggened
        if entry:
            table[key.lower()] = entry
    return table


# Procedures whose Open MPI mpi_f08 binding intentionally has a different
# argument count than the standard's F08 expression, so an argument-count
# mismatch is expected rather than drift: MPI_Pcontrol is variadic, and the
# Open MPI bindings for MPI_Type_get_contents / MPI_Type_get_envelope take a
# different argument shape than pympistandard's rendering.
COUNT_MISMATCH_EXEMPT = {
    'mpi_pcontrol',
    'mpi_type_get_contents',
    'mpi_type_get_envelope',
}


def function_key(subroutine_name):
    """Map an mpi_f08 subroutine name to a (pympistandard key, is_large_count) pair.

    Large-count variants use two suffix conventions: the template-generated
    routines use '_c_f08'/'_c_f08ts', while the hand-written ones use
    '_f08_c'/'_f08ts_c'.  Both must be recognized so the large-count routines
    resolve to the embiggened standard entry instead of being skipped.
    """
    is_c = bool(re.search(r'(_c_f08(ts)?|_f08(ts)?_c)$', subroutine_name))
    base = re.sub(r'(_c_f08(ts)?|_f08(ts)?_c|_f08(ts)?)$', '', subroutine_name).lower()
    if not base.startswith('mpi_'):
        base = 'mpi_' + base
    return base, is_c


def normalize_type(text):
    """Normalize a Fortran type for comparison."""
    text = re.sub(r'\s+', '', text).upper().replace('KIND=', '')
    # CHARACTER(LEN=*) and friends are all just CHARACTER for our purposes.
    text = re.sub(r'CHARACTER\(.*\)', 'CHARACTER', text)
    return text


def is_choice_buffer(std_type):
    """A standard choice-buffer argument renders as 'TYPE(*), ...'."""
    return 'TYPE(*)' in std_type


def parse_fortran(path):
    """Yield (subroutine_name, [arg_names], {arg: (intent, base_type)}) per top-level routine."""
    lines = open(path).read().split('\n')
    i, n = 0, len(lines)
    while i < n:
        s = lines[i].strip()
        m = re.match(r'(?:pure\s+|elemental\s+)?subroutine\s+(MPI_\w+)\s*\(', s)
        if not m:
            i += 1
            continue
        name = m.group(1)
        # Gather a possibly continued signature.
        sig, j = s, i
        while sig.rstrip().endswith('&'):
            j += 1
            sig = sig.rstrip()[:-1] + lines[j].strip()
        arglist = re.search(r'\((.*)\)', sig).group(1)
        args = [a.strip().lower() for a in arglist.split(',')
                if a.strip() and a.strip().lower() != 'ierror']
        # Collect dummy declarations up to the first nested interface / end.
        decls = {}
        k = j + 1
        while k < n:
            low = lines[k].strip().lower()
            if re.match(r'^interface\b', low) or re.match(r'^end\s+subroutine', low):
                break
            if '::' in lines[k]:
                upper = lines[k].upper()
                has_intent = 'INTENT(' in upper
                is_buffer = 'TYPE(*)' in upper or 'IGNORE_TKR' in upper
                if has_intent or is_buffer:
                    intent = ('INTENT(INOUT)' if 'INTENT(INOUT)' in upper else
                              'INTENT(OUT)' if 'INTENT(OUT)' in upper else
                              'INTENT(IN)' if 'INTENT(IN)' in upper else None)
                    # Base type: text before the first top-level comma of the LHS.
                    lhs = lines[k].split('::', 1)[0]
                    base, depth = '', 0
                    for ch in lhs:
                        if ch == '(':
                            depth += 1
                        elif ch == ')':
                            depth -= 1
                        elif ch == ',' and depth == 0:
                            break
                        base += ch
                    # Declared names (before any '(' dimension spec, top-level commas).
                    rhs = lines[k].split('::', 1)[1].split('!')[0]
                    buf, depth, names = '', 0, []
                    for ch in rhs:
                        if ch == '(':
                            depth += 1
                            if depth == 1:
                                names.append(buf)
                                buf = ''
                        elif ch == ')':
                            depth -= 1
                        elif ch == ',' and depth == 0:
                            names.append(buf)
                            buf = ''
                        elif depth == 0:
                            buf += ch
                    names.append(buf)
                    for nm in names:
                        nm = nm.strip().lower()
                        if re.match(r'^[a-z]\w*$', nm):
                            decls[nm] = (intent, base.strip())
            k += 1
        yield name, args, decls
        i = j + 1


def check_file(path, table, matched):
    """Return mismatch strings for one file; add fully-checked routines to `matched`."""
    problems = []
    for name, args, decls in parse_fortran(path):
        key, is_c = function_key(name)
        entry = table.get(key)
        if entry is None:
            # No standard metadata for this routine (an Open MPI extension, or
            # a routine newer than the pympistandard in use) -- nothing to
            # check it against.
            continue
        expected = entry.get('c') if is_c else entry.get('r')
        if expected is None:
            # OMPI provides a large-count variant the standard does not.
            continue
        expected = [(n, i, t) for (n, i, t) in expected if n != 'ierror']
        if len(args) != len(expected):
            # An unexpected argument-count difference is real drift; a few
            # routines legitimately differ (see COUNT_MISMATCH_EXEMPT).
            if key not in COUNT_MISMATCH_EXEMPT:
                problems.append(f'{name}: has {len(args)} non-ierror argument(s) but the '
                                f'standard specifies {len(expected)} (argument added or removed?)')
            continue
        matched.add(name)
        for got_name, (std_name, std_intent, std_type) in zip(args, expected):
            if got_name != std_name:
                problems.append(f'{name}: argument {got_name!r} should be named '
                                f'{std_name!r}')
            got_intent, got_type = decls.get(got_name, (None, None))
            # Intent: skip where the standard leaves it unspecified.
            if std_intent is not None and got_intent is not None and got_intent != std_intent:
                problems.append(f'{name}: argument {std_name!r} has {got_intent}, '
                                f'standard requires {std_intent}')
            # Type: skip choice buffers (implementation-defined rendering).
            if (got_type is not None and not is_choice_buffer(std_type)
                    and 'IGNORE_TKR' not in got_type.upper()):
                if normalize_type(got_type) != normalize_type(std_type):
                    problems.append(f'{name}: argument {std_name!r} is declared '
                                    f'{got_type!r}, standard requires {std_type!r}')
    return problems


def main():
    parser = argparse.ArgumentParser(
        description='Validate mpi_f08 interfaces against the MPI standard (pympistandard).')
    parser.add_argument('--pympistd-dir', required=True,
                        help='path to the pympistandard submodule')
    parser.add_argument('--label', default='mpi_f08 interfaces',
                        help='label for the Automake-style PASS/FAIL/SKIP result line')
    parser.add_argument('files', nargs='+',
                        help='Fortran files containing mpi_f08 interfaces to check')
    args = parser.parse_args()
    label = args.label

    # Skip (don't fail) when the mpi_f08 sources are not present -- e.g. the
    # Fortran bindings were not built.  (The Makefiles only run this during
    # "make check" when the f08 bindings are enabled, but stay robust anyway.)
    if not any(os.path.exists(f) for f in args.files):
        emit_status('SKIP', label, 'mpi_f08 sources not built')
        return

    # Skip when pympistandard is unavailable (e.g. the submodule was not
    # checked out).  The generator falls back the same way; a build with
    # pympistandard present is what enforces standard names.
    if not os.path.isdir(os.path.join(args.pympistd_dir, 'src')):
        emit_status('SKIP', label, 'pympistandard not available')
        return
    try:
        table = load_standard(args.pympistd_dir)
    except ImportError as e:
        emit_status('SKIP', label, f'pympistandard could not be loaded: {e}')
        return

    problems = []
    matched = set()
    for path in args.files:
        if os.path.exists(path):
            problems.extend(check_file(path, table, matched))

    # Guard against silently validating nothing: if files were present but not
    # a single routine matched a standard entry, the pympistandard key format
    # has almost certainly drifted from what this script expects, so a "pass"
    # would be meaningless.  Fail loudly instead.
    if not matched:
        emit_status('FAIL', label, 'matched 0 procedures; pympistandard key format may have changed')
        sys.exit(1)

    if problems:
        print('mpi_f08 interfaces disagree with the MPI standard:')
        for p in problems:
            print(f'  {p}')
        print(f'{len(problems)} mismatch(es) found ({len(matched)} procedures validated).')
        emit_status('FAIL', label)
        sys.exit(1)

    emit_status('PASS', label, f'{len(matched)} procedures validated')


if __name__ == '__main__':
    main()
