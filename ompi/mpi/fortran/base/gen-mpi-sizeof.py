#!/usr/bin/env python3
#
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015-2021 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2022      IBM Corporation.  All rights reserved.
# Copyright (c) 2025      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Script to generate the overloaded MPI_SIZEOF interfaces and
# subroutine bodies for both the mpi and mpi_f08 modules.
#
# This script won't really be necessary (i.e., be a whole lot simpler)
# when Fortran compilers uniformly supprort TS 29113 -- i.e., they
# support dimension(..).  Using dimension(..), you can have just *one*
# procedure for every type, and dimension(..) will resolve to both
# scalars and all possible ranks.
#
# But for the meantime, we generate for all ranks so that we support
# as many compilers as possible.  :-\ (we don't check the compiler and
# see if it supports dimension(..) and do a different generation based
# on that, because we already have a zillion different options in the
# Fortran support -- let's just do MPI_Sizeof this one way in the name
# of simplicity...).
#

import argparse
import sys
import os
import copy

from pathlib import Path

# Global dictionary to store subroutine templates
subs = {}
indent = "      "

# --- Helper Functions ---

def queue_sub(f_type, suffix, import_type=None, mpi_version=0, request_deprecate=False):
    # Creates and stores a template for a Fortran subroutine
    global subs
    # Leave off the MPI/PMI prefix; we'll add that when outputting
    sub_name = f"Sizeof_{suffix}"

    # Make a dictionary for this subroutine
    subr = {}
    subr['name'] = sub_name
    start = f"{indent}SUBROUTINE ^PREFIX^{sub_name}^RANK^(x, size, ierror)\n"
    if import_type:
        start += f"{indent}  USE, INTRINSIC :: iso_fortran_env, ONLY: {import_type.upper()}\n"

    # For long type names and large ranks, this first line gets very
    # long and only narrowly squeezed in before 72 columns. Use no
    # whitespace.
    start += f"{indent}{f_type.upper()}^DIMENSION^::x\n"
    start += f"{indent}  INTEGER, INTENT(OUT) :: size\n"
    start += f"{indent}  INTEGER^IERROR_OPTIONAL^, INTENT(OUT) :: ierror"
    subr['start'] = start

    subr['middle'] = f"{indent}  size = storage_size(x) / 8\n"
    subr['middle'] += f"{indent}  ^IERROR_STATEMENT^ierror = 0"

    if mpi_version >= 4 and request_deprecate:
        subr['end'] = f"!GCC$ ATTRIBUTES DEPRECATED :: ^PREFIX^{sub_name}^RANK^\n"
    else:
         subr['end'] = ""
    subr['end'] += f"{indent}END SUBROUTINE ^PREFIX^{sub_name}^RANK^"

    # Save it in the overall dictionary
    subs[sub_name] = subr

def generate(prefix, sub_name, rank, want_body, optional_ierror_param, optional_ierror_statement):
    # Generates the Fortran code for a specific subroutine
    global subs
    if sub_name not in subs:
        # This can happen if a type was conditionally excluded
        return ""

    # Deep copy the template
    subr = copy.deepcopy(subs[sub_name])

    # Make the initial version
    str_out = subr['start'] + "\n"
    if want_body:
        str_out += "\n" + subr['middle'] + "\n"
    str_out += subr['end'] + "\n"

    # Substitute in the relevant parameters
    str_out = str_out.replace('^PREFIX^', prefix)
    str_out = str_out.replace('^IERROR_OPTIONAL^', optional_ierror_param)
    str_out = str_out.replace('^IERROR_STATEMENT^', optional_ierror_statement)


    # If rank is 0, generate a scalar version. Otherwise, generate an
    # array version.
    if rank == 0:
        str_out = str_out.replace('^RANK^', '_scalar')
        str_out = str_out.replace('^DIMENSION^', '')
    else:
        str_out = str_out.replace('^RANK^', f'_r{rank}')
        # Generate dimension string like "1,"*(rank-1) + "*"
        dim = ",".join(['1'] * (rank - 1))
        if rank > 1:
             dim += ","
        dim += "*" # Add the final asterisk
        str_out = str_out.replace('^DIMENSION^', f', DIMENSION({dim})')


    # All done
    return str_out

def output_content(f, prefix, want_bodies, maxrank, optional_ierror_param, optional_ierror_statement):
    # Writes the generated content (interfaces or bodies) to the file object
    global subs
    global indent

    if not want_bodies:
        f.write(f"{indent}INTERFACE {prefix}Sizeof\n\n")

    # Print all the module procedure lines
    for sub_name in sorted(subs.keys()):
        rank = 0
        while rank <= maxrank:
            str_generated = generate(prefix, sub_name, rank, want_bodies,
                                    optional_ierror_param, optional_ierror_statement)
            if str_generated: # Only write if generate didn't return empty
                 f.write(str_generated + "\n")
            rank += 1

    if not want_bodies:
        f.write(f"{indent}END INTERFACE {prefix}Sizeof\n\n")

def output_file(filename, want_bodies, args):
    # Generates and writes the complete output file
    global indent

    # Determine optional ierror parts
    optional_ierror_param = ""
    optional_ierror_statement = ""
    if args.ierror and args.ierror.lower() == "optional":
        optional_ierror_param = ", OPTIONAL"
        optional_ierror_statement = "IF (present(ierror)) "


    try:
        # Delete existing file first, like Perl's unlink
        if os.path.exists(filename):
            os.unlink(filename)

        with open(filename, "w") as f:
            f.write("""! -*- f90 -*-
! WARNING: This is a generated file!  Edits will be lost!
!
! Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2015-2021 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2022      IBM Corporation.  All rights reserved.
! $COPYRIGHT$
!
! This file was generated by gen-mpi-sizeof.py for all the MPI_SIZEOF
! interface possibilities for intrinsic types.  Once TS 29113 is
! supported in all compilers, we can simply have *one* procedure for
! each type and use dimension(..) to indicate scalars+all array ranks.
! But until more compilers support this, we simply generate a
! procedure for scalars and all possible ranks in an attempt to
! support lots of Fortran compilers.

""")

            # Only output if the generate arg is True. Otherwise, output an
            # empty .h file (that is still safe to include by mpif.h, but
            # won't include the MPI_SIZEOF interface block).
            if args.generate:
                # Call queue_sub for all required types *before* outputting
                for size in [8, 16, 32, 64]:
                    queue_sub(f"integer(int{size})", f"int{size}", f"int{size}", args.mpi_version, args.request_deprecate)

                for size in [16, 32, 64, 128]:
                    # Check conditions before queueing real types
                    queue_real = True
                    if size == 16 and not args.real2 and not args.iso_real16:
                         queue_real = False
                    if size == 128 and not args.real16:
                        queue_real = False
                    if queue_real:
                        queue_sub(f"real(real{size})", f"real{size}", f"real{size}", args.mpi_version, args.request_deprecate)

                    # Check conditions before queueing complex types
                    queue_complex = True
                    if size == 16 and not args.complex4 and not args.iso_real16:
                        queue_complex = False
                    if size == 128 and not args.complex32:
                        queue_complex = False
                    if queue_complex:
                        queue_sub(f"complex(real{size})", f"complex{size}", f"real{size}", args.mpi_version, args.request_deprecate)

                # Handle special non-ISO types if needed
                if args.real2 and not args.iso_real16:
                     queue_sub("real*2", "real16", mpi_version=args.mpi_version, request_deprecate=args.request_deprecate) # No import_type for non-standard
                     # Assume complex*4 goes with real*2 if iso_real16 is not used
                     if args.complex4 and not args.iso_real16:
                         queue_sub("complex*4", "complex16", mpi_version=args.mpi_version, request_deprecate=args.request_deprecate) # No import_type for non-standard

                queue_sub("character", "character", mpi_version=args.mpi_version, request_deprecate=args.request_deprecate) # No iso_fortran_env needed
                queue_sub("logical", "logical", mpi_version=args.mpi_version, request_deprecate=args.request_deprecate) # No iso_fortran_env needed


                # --- Generate Content ---
                if not want_bodies or (want_bodies and args.mpi):
                    output_content(f, "MPI_", want_bodies, args.maxrank,
                                   optional_ierror_param, optional_ierror_statement)
                if not want_bodies or (want_bodies and args.pmpi):
                     output_content(f, "PMPI_", want_bodies, args.maxrank,
                                    optional_ierror_param, optional_ierror_statement)
            else:
                # Sad panda message
                f.write("""! *** ATTENTION!
!
! Sad panda.
!
! This compiler does not support the Right Stuff to enable MPI_SIZEOF.
! Specifically: we need support for the INTERFACE keyword,
! ISO_FORTRAN_ENV, and the STORAGE_SIZE() intrinsic on all types.
! Apparently, this compiler does not support both of those things, so
! this file will be (effectively) blank (i.e., we didn't bother
! generating the necessary stuff for MPI_SIZEOF because the compiler
! doesn't support
! it).
!
! If you want support for MPI_SIZEOF, please use a different Fortran
! compiler to build Open MPI.

""")
                if want_bodies:
                    name = "pompi_sad_panda" if args.pmpi else "ompi_sad_panda"
                    f.write(f"""!
! Dummy subroutine, just so that there is *some* Fortran in this file
! (this is defensive programming: since the Fortran compiler doesn't
! support enough mojo, configure should set some AM_CONDITIONALs such
! that this file should not end up being compiled, but just in case
! that logic changes someday and this file *does* end up getting
! compiled, make sure that it's not entirely empty because some
! compilers are unhappy if there are no Fortran statements in this
! file).
subroutine {name}()
  implicit none

  print *, 'Open MPI is a sad panda because your Fortran compiler'
  print *, 'does not support enough Fortran mojo for MPI_SIZEOF'
end subroutine {name}

""")
    except IOError as e:
        print(f"Error writing to file {filename}: {e}", file=sys.stderr)
        sys.exit(1)

# --- Main Execution ---

def main():
    parser = argparse.ArgumentParser(description="Generate Fortran MPI_SIZEOF interfaces and implementations.")

    # File outputs
    parser.add_argument("--header",
                        help="Filename for the interface header output.")
    parser.add_argument("--impl",
                        help="Filename for the implementation subroutine output.")

    # Control flags
    parser.add_argument("--generate", type=int, default=0,
                        help="Actually generate MPI_SIZEOF (otherwise output 'sad panda').")
    parser.add_argument("--ierror", choices=['optional', 'mandatory'],
                        help="How to handle the ierror parameter.")
    parser.add_argument("--maxrank", type=int, default=7,
                        help="Maximum array rank to generate (default: 7, must be >=4, <=15).")
    parser.add_argument("--mpi", action='store_true',
                        help="Generate MPI_ prefixed routines (for --impl).")
    parser.add_argument("--pmpi", action='store_true',
                        help="Generate PMPI_ prefixed routines (for --impl).")
    parser.add_argument("--request_deprecate", type=int, default=0,
                        help="Add GCC DEPRECATED attribute if MPI version >= 4")
    parser.add_argument("--mpi_version", type=int, default=3,
                        help="MPI Standard version (e.g., 3 or 4)")

    # Type support flags
    parser.add_argument("--complex32", type=int, default=0,
                        help="Enable Complex*32 support (real128).")
    parser.add_argument("--complex4", type=int, default=0,
                        help="Enable Complex*4 support (real16).")
    parser.add_argument("--real16", type=int, default=0,
                        help="Enable Real*16 support (real128).")
    parser.add_argument("--real2", type=int, default=0,
                        help="Enable Real*2 support (real16).")
    parser.add_argument("--iso_real16", type=int, default=0,
                        help="Use ISO_FORTRAN_ENV real16 (overrides real2/complex4 if both specified).")

    args = parser.parse_args()

    # --- Argument Validation ---
    if not args.header and not args.impl:
        parser.error("Must specify --header and/or --impl filenames to output.")

    if args.generate:
        if not args.ierror:
             parser.error("--ierror must be specified when --generate is used.")
        if not (4 <= args.maxrank <= 15):
            parser.error("--maxrank must be >= 4 and <= 15.")
        if args.impl and not args.mpi and not args.pmpi:
            parser.error("Must specify --pmpi and/or --mpi if --impl is specified.")
        # Note: The Perl script checked for presence of all type flags,
        # but argparse with action='store_true' defaults them to False if not given.
        # The logic in output_file now handles the conditional generation based
        # on these boolean flags.

    # --- File Generation ---
    if args.header:
        print(f"Generating header file: {args.header}")
        output_file(args.header, want_bodies=False, args=args)

    if args.impl:
        print(f"Generating implementation file: {args.impl}")
        output_file(args.impl, want_bodies=True, args=args)

    print("Done.")
    sys.exit(0)

if __name__ == "__main__":
    main()
