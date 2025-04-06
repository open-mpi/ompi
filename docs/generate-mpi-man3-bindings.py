#!/usr/bin/env python3
#
# Copyright (c) 2025 Jeffrey M. Squyres.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Script to create RST files containing the C, F90, and F80 bindings
# that will be included in each of the MPI API man pages.  We generate
# the bindings using the official MPI Forum python library to read the
# API JSON that was generated when building the MPI Forum standard
# LaTeX document and then emit the bindings.
#
# Using this method, we can emit both "regular" and "embiggened"
# versions of each API (if an "embiggened" version exists).

import os
import sys
import textwrap
import argparse

from pathlib import Path

def generate(func_name, output_dir):
    global std

    # Sanity check
    func_name = func_name.lower()
    if not func_name in std.PROCEDURES:
        print(f"ERROR: Don't know {func_name}")
        return

    # Get the data for the bindings
    data = std.PROCEDURES[func_name]

    # Make an array of strings to emit into the output RST file.
    blank = ''
    out = []

    out.append('SYNTAX')
    out.append('------')
    out.append(blank)

    # C bindings
    out.append('C Syntax')
    out.append('^^^^^^^^')
    out.append(blank)
    out.append('.. code-block:: c')
    out.append(blank)

    binding = data.express.iso_c
    line = textwrap.fill(str(binding), width=72,
                         initial_indent='    ',
                         subsequent_indent = '        ')
    out.append(line)
    out.append(blank)
    if data.has_embiggenment():
        binding = data.express.embiggen.iso_c
        line = textwrap.fill(str(binding), width=72,
                             initial_indent='    ',
                             subsequent_indent = '        ')
        out.append(line)
        out.append(blank)

    # F90 bindings
    # Note: the f90 bindings were not embiggened
    out.append('Fortran Syntax')
    out.append('^^^^^^^^^^^^^^')
    out.append(blank)
    out.append('.. code-block:: fortran')
    out.append(blank)
    out.append('    USE MPI')
    out.append("    ! or the older form: INCLUDE 'mpif.h'")

    binding = data.express.f90
    lines = str(binding).split('\n')
    for line in lines:
        out.append(f'    {line}')
    out.append(blank)

    # F08 bindings
    out.append('Fortran 2008 Syntax')
    out.append('^^^^^^^^^^^^^^^^^^^')
    out.append(blank)
    out.append('.. code-block:: fortran')
    out.append(blank)
    out.append('    USE mpi_f08')

    binding = data.express.f08
    lines = str(binding).split('\n')
    for line in lines:
        out.append(f'    {line}')
    out.append(blank)

    if data.has_embiggenment():
        binding = data.express.embiggen.f08
        lines = str(binding).split('\n')
        for line in lines:
            out.append(f'    {line}')
        out.append(blank)

    # Write the output file -- but only if it has changed
    old_content = None
    new_content = '\n'.join(out)
    output_file = os.path.join(output_dir, f'{func_name}.rst')
    if os.path.exists(output_file):
        with open(output_file) as fp:
            old_content = fp.read()

    if old_content != new_content:
        with open(output_file, 'w') as fp:
            fp.write('\n'.join(out))
        print(f'Wrote {output_file}')

#----------------

def setup_cli():
    parser = argparse.ArgumentParser(description="Generate C/F90/F08 bindings for RST")

    parser.add_argument('--srcdir',
                        required=True,
                        help='Build source dir')
    parser.add_argument('--builddir',
                        required=True,
                        help='Build build dir')

    args = parser.parse_args()
    return args

#----------------

def main():
    args = setup_cli()

    src_dir = os.path.abspath(args.srcdir)
    build_dir = os.path.abspath(args.builddir)

    # A bit of a hack to load the pympistandard module, which is in
    # the Open MPI '3rd-party" tree.
    pympistandard_dir = Path(os.path.join(src_dir, '..', '3rd-party',
                                          'pympistandard', 'src')).resolve()

    sys.path.insert(0, str(pympistandard_dir))
    global std
    import pympistandard as std

    # This is the JSON file with all the MPI standard APIs.  This is
    # not currently officially distributed by the MPI Forum, so it was
    # obtained by checking out the relevant branch from
    # https://github.com/mpi-forum/mpi-standard/ and doing a build.
    # This will create a file named apis.json.  Copy that here to this
    # tree.
    mpi_standard_json = os.path.abspath(os.path.join(src_dir,
                                                     'mpi-standard-apis.json'))
    std.use_api_version(1, given_path=mpi_standard_json)

    # We need to write all of these into the build tree.  See
    # docs/Makefile.am for a fuller explaination: all RST files are
    # copied to the build tree, and we build there.
    output_dir = os.path.join(build_dir, 'man-openmpi', 'man3', 'bindings')
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # Now we finally generate the files.  Iterate over all the MPI
    # procedures and generate a binding file for each one of them.
    #for func_name in std.PROCEDURES:
    #    generate(func_name, output_dir)

    # PROOF OF CONCEPT: Just generate 2 binding files for now -- to be
    # replaced with the above loop.
    generate("mpi_init", output_dir)
    generate("mpi_send", output_dir)

if __name__ == "__main__":
    main()
