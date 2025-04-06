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

import re
import os
import sys
import textwrap
import argparse

from pathlib import Path
from collections import defaultdict
from pprint import pprint

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

def generate(func_name_arg, output_dir, directives):
    global std

    # Sanity check
    func_name = func_name_arg.lower()
    if not func_name in std.PROCEDURES:
        print(f"ERROR: Don't know {func_name}")
        return

    # Do not generate this file if the binding for this function is
    # going to be included in another file
    for key, data in directives.items():
        if key == func_name:
            continue

        # If this function is in the list of MPI bindings included in
        # another page, then we don't need to generate this page.
        if func_name in data:
            return

    # Also do not generate if we have no man page for this MPI API
    # function.
    if func_name not in directives:
        return

    #----------------

    # If we get here, we're going to generate the bindings for one or
    # more API functions.
    func_names_data = []
    for name in directives[func_name_arg]:
        func_names_data.append(std.PROCEDURES[name])
    data = std.PROCEDURES[func_name_arg]
    if data not in func_names_data:
        func_names_data.insert(0, data)

    # Make an array of strings to emit into the output RST file.
    blank = ''
    out = []

    out.append('SYNTAX')
    out.append('------')
    out.append(blank)

    have_binding = False

    # C bindings
    emitted_header = False
    for data in func_names_data:
        binding = str(data.express.iso_c)
        if binding and len(binding) > 0 and binding != 'None':
            have_binding = True
            if not emitted_header:
                out.append('C Syntax')
                out.append('^^^^^^^^')
                out.append(blank)
                out.append('.. code-block:: c')
                out.append(blank)
                emitted_header = True

            # Per
            # https://github.com/mpi-forum/pympistandard/issues/25,
            # there's a bug that the binding for MPI_Pcontrol returns
            # a string containing "\ldots", not "...".  Do a manual
            # replacement here.
            binding = binding.replace(r'\ldots', '...')

            line = textwrap.fill(binding, width=72,
                                 initial_indent='    ',
                                 subsequent_indent = '        ')
            out.append(line)
            out.append(blank)

            if data.has_embiggenment():
                binding = str(data.express.embiggen.iso_c)
                binding = binding.replace(r'\ldots', '...')
                line = textwrap.fill(binding, width=72,
                                     initial_indent='    ',
                                     subsequent_indent = '        ')
                out.append(line)
                out.append(blank)

    # F90 bindings
    # Note: the f90 bindings were not embiggened
    emitted_header = False
    for data in func_names_data:
        binding = str(data.express.f90)
        if binding and len(binding) > 0 and binding != 'None':
            have_binding = True
            if not emitted_header:
                out.append('Fortran Syntax')
                out.append('^^^^^^^^^^^^^^')
                out.append(blank)
                out.append('.. code-block:: fortran')
                out.append(blank)
                out.append('    USE MPI')
                out.append("    ! or the older form: INCLUDE 'mpif.h'")
                emitted_header = True

            lines = binding.split('\n')
            for line in lines:
                out.append(f'    {line}')
            out.append(blank)

    # F08 bindings
    emitted_header = False
    for data in func_names_data:
        binding = str(data.express.f08)
        if binding and len(binding) > 0 and binding != 'None':
            have_binding = True
            if not emitted_header:
                out.append('Fortran 2008 Syntax')
                out.append('^^^^^^^^^^^^^^^^^^^')
                out.append(blank)
                out.append('.. code-block:: fortran')
                out.append(blank)
                out.append('    USE mpi_f08')
                emitted_header = True

            lines = binding.split('\n')
            for line in lines:
                out.append(f'    {line}')
            out.append(blank)

            if data.has_embiggenment():
                binding = str(data.express.embiggen.f08)
                lines = binding.split('\n')
                for line in lines:
                    out.append(f'    {line}')
                out.append(blank)

    # Sanity check
    if not have_binding:
        print(f"NO BINDINGS for {func_name_arg}")
        return

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

        # This script is run during "make".  So emit a make-style
        # message.
        print(f'  GENERATE RST {func_name_arg} binding')

#----------------

# Some existing .3.rst man pages actually contain the docs for
# multiple MPI_* API functions.  Read the .3.rst files and look for
# directives that mean "this file contains documentation for all these
# MPI API functions".
def read_rst_man_pages(src_dir):
    directives = {}
    prog = re.compile(r'^MPI_.*\.3\.rst$')

    man3_dir = Path(os.path.join(src_dir, 'man-openmpi', 'man3')).resolve()
    for file in os.listdir(man3_dir):
        # Only want MPI man pages
        if not prog.match(file):
            continue

        with open(os.path.join(man3_dir, file)) as fp:
            lines = fp.readlines()

        file_api_name = file.replace('.3.rst', '').lower()

        # Make an initial/empty list for every MPI API man page that
        # we find
        directives[file_api_name] = list()

        prefix = '.. mpi-bindings:'
        for line in lines:
            line = line.strip()
            if not line.startswith(prefix):
                continue

            bindings = line[len(prefix):].split(',')
            for binding in bindings:
                binding = binding.strip()
                directives[file_api_name].append(binding.lower())

    return directives

#----------------

def main():
    args = setup_cli()

    src_dir = os.path.abspath(args.srcdir)
    build_dir = os.path.abspath(args.builddir)

    # Read existing srcdir/man-openmpi/man3/MPI_*.3.rst files and look
    # for directives to guide this generation process.
    directives = read_rst_man_pages(src_dir)

    # A bit of a hack to load the pympistandard module, which is in
    # the Open MPI '3rd-party" tree in the source dir.
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
    for func_name in std.PROCEDURES:
        generate(func_name, output_dir, directives)

if __name__ == "__main__":
    main()
