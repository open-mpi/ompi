#!/usr/bin/env python3
#
# Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016-2018 FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2020      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2022      IBM Corporation.  All rights reserved.
# Copyright (c) 2025      Jeffrey M. Squyres.  All rights reserved.
# Copyright (c) 2025      Triad National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This script creates header files to be compiled with the various
# Fortran bindings.  In some cases, we need Fortran PARAMETER values;
# in other cases, we need #define preprocessor macros.
#
# This script generates both cases, and ensures that the values are
# the same between both (e.g., that MPI_COMM_WORLD is both a fortran
# INTEGER PARAMETER of value 0 and is #define'd to be 0).
#
# Additionally, since Open MPI provides the configure ability to
# compile out the entire MPI IO interface, all the IO
# handles/constants are generated in separate .h files in the
# non-preprocessor case, and included in relevant #if's in the
# preprocessor case.
#
# Files are generated in the following directories:
#
#   ompi/include
#   ompi/mpi/fortran/use-mpi-f08
#

import os
import sys
import re
from pathlib import Path

# --- Data Definitions ---

# Standard Handles
handles = {
    'MPI_COMM_WORLD': 0,
    'MPI_COMM_SELF': 1,
    'MPI_GROUP_EMPTY': 1,
    'MPI_ERRORS_ARE_FATAL': 1,
    'MPI_ERRORS_RETURN': 2,
    'MPI_ERRORS_ABORT': 3,
    'MPI_MAX': 1,
    'MPI_MIN': 2,
    'MPI_SUM': 3,
    'MPI_PROD': 4,
    'MPI_LAND': 5,
    'MPI_BAND': 6,
    'MPI_LOR': 7,
    'MPI_BOR': 8,
    'MPI_LXOR': 9,
    'MPI_BXOR': 10,
    'MPI_MAXLOC': 11,
    'MPI_MINLOC': 12,
    'MPI_REPLACE': 13,
    'MPI_NO_OP': 14,
    'MPI_COMM_NULL': 2,
    'MPI_DATATYPE_NULL': 0,
    'MPI_ERRHANDLER_NULL': 0,
    'MPI_GROUP_NULL': 0,
    'MPI_INFO_NULL': 0,
    'MPI_MESSAGE_NULL': 0,
    'MPI_OP_NULL': 0,
    'MPI_REQUEST_NULL': 0,
    'MPI_WIN_NULL': 0,
    'MPI_SESSION_NULL': 0,
    'MPI_BYTE': 1,
    'MPI_PACKED': 2,
    'MPI_UB': 3,
    'MPI_LB': 4,
    'MPI_CHARACTER': 5,
    'MPI_LOGICAL': 6,
    'MPI_INTEGER': 7,
    'MPI_INTEGER1': 8,
    'MPI_INTEGER2': 9,
    'MPI_INTEGER4': 10,
    'MPI_INTEGER8': 11,
    'MPI_INTEGER16': 12,
    'MPI_REAL': 13,
    'MPI_REAL4': 14,
    'MPI_REAL8': 15,
    'MPI_REAL16': 16,
    'MPI_DOUBLE_PRECISION': 17,
    'MPI_COMPLEX': 18,
    'MPI_COMPLEX8': 19,
    'MPI_COMPLEX16': 20,
    'MPI_COMPLEX32': 21,
    'MPI_DOUBLE_COMPLEX': 22,
    'MPI_2REAL': 23,
    'MPI_2DOUBLE_PRECISION': 24,
    'MPI_2INTEGER': 25,
    'MPI_2COMPLEX': 26,
    'MPI_2DOUBLE_COMPLEX': 27,
    'MPI_REAL2': 28,
    'MPI_LOGICAL1': 29,
    'MPI_LOGICAL2': 30,
    'MPI_LOGICAL4': 31,
    'MPI_LOGICAL8': 32,
    'MPI_WCHAR': 33,
    'MPI_CHAR': 34,
    'MPI_UNSIGNED_CHAR': 35,
    'MPI_SIGNED_CHAR': 36,
    'MPI_SHORT': 37,
    'MPI_UNSIGNED_SHORT': 38,
    'MPI_INT': 39,
    'MPI_UNSIGNED': 40,
    'MPI_LONG': 41,
    'MPI_UNSIGNED_LONG': 42,
    'MPI_LONG_LONG_INT': 43,
    'MPI_UNSIGNED_LONG_LONG': 44,
    'MPI_FLOAT': 45,
    'MPI_DOUBLE': 46,
    'MPI_LONG_DOUBLE': 47,
    'MPI_FLOAT_INT': 48,
    'MPI_DOUBLE_INT': 49,
    'MPI_LONG_DOUBLE_INT': 50,
    'MPI_LONG_INT': 51,
    'MPI_2INT': 52,
    'MPI_SHORT_INT': 53,
    'MPI_CXX_BOOL': 54,
    'MPI_CXX_FLOAT_COMPLEX': 55,
    'MPI_CXX_DOUBLE_COMPLEX': 56,
    'MPI_CXX_LONG_DOUBLE_COMPLEX': 57,
    'MPI_INT8_T': 58,
    'MPI_UINT8_T': 59,
    'MPI_INT16_T': 60,
    'MPI_UINT16_T': 61,
    'MPI_INT32_T': 62,
    'MPI_UINT32_T': 63,
    'MPI_INT64_T': 64,
    'MPI_UINT64_T': 65,
    'MPI_AINT': 66,
    'MPI_OFFSET': 67,
    'MPI_C_BOOL': 68,
    'MPI_C_COMPLEX': 69,
    'MPI_C_DOUBLE_COMPLEX': 70,
    'MPI_C_LONG_DOUBLE_COMPLEX': 71,
    'MPI_COUNT': 72,
    'MPI_COMPLEX4': 73,
    # Leave room for 16bit datatypes
    # See https://github.com/mpi-forum/mpi-issues/issues/65
    'MPI_LOGICAL16': 77,
    'MPI_MESSAGE_NO_PROC': 1,
    'MPI_INFO_ENV': 1,
}

# Handle aliases
handles['MPI_LONG_LONG'] = handles['MPI_LONG_LONG_INT']
handles['MPI_CXX_COMPLEX'] = handles['MPI_CXX_FLOAT_COMPLEX']
handles['MPI_C_FLOAT_COMPLEX'] = handles['MPI_C_COMPLEX']

# IO Handles
io_handles = {
    'MPI_FILE_NULL': 0,
}

# Standard Constants (populated later with version info)
constants = {
    'MPI_ANY_SOURCE': -1,
    'MPI_ANY_TAG': -1,
    'MPI_PROC_NULL': -2,
    'MPI_ROOT': -4,
    'MPI_UNDEFINED': -32766,
    'MPI_CART': 1,
    'MPI_GRAPH': 2,
    'MPI_DIST_GRAPH': 3,
    'MPI_KEYVAL_INVALID': -1,
    'MPI_SOURCE': 1,
    'MPI_TAG': 2,
    'MPI_ERROR': 3,
    'MPI_TAG_UB': 0,
    'MPI_HOST': 1,
    'MPI_IO': 2,
    'MPI_WTIME_IS_GLOBAL': 3,
    'MPI_APPNUM': 4,
    'MPI_LASTUSEDCODE': 5,
    'MPI_UNIVERSE_SIZE': 6,
    'MPI_WIN_BASE': 7,
    'MPI_WIN_SIZE': 8,
    'MPI_WIN_DISP_UNIT': 9,
    'MPI_WIN_CREATE_FLAVOR': 10,
    'MPI_WIN_MODEL': 11,
    'MPI_FT': 12,
    'MPI_WIN_FLAVOR_CREATE': 1,
    'MPI_WIN_FLAVOR_ALLOCATE': 2,
    'MPI_WIN_FLAVOR_DYNAMIC': 3,
    'MPI_WIN_FLAVOR_SHARED': 4,
    'MPI_WIN_UNIFIED': 0,
    'MPI_WIN_SEPARATE': 1,
    'MPI_BSEND_OVERHEAD': 128,
    'MPI_ORDER_C': 0,
    'MPI_ORDER_FORTRAN': 1,
    'MPI_DISTRIBUTE_BLOCK': 0,
    'MPI_DISTRIBUTE_CYCLIC': 1,
    'MPI_DISTRIBUTE_NONE': 2,
    'MPI_DISTRIBUTE_DFLT_DARG': -1,
    'MPI_TYPECLASS_INTEGER': 1,
    'MPI_TYPECLASS_REAL': 2,
    'MPI_TYPECLASS_COMPLEX': 3,
    'MPI_MODE_NOCHECK': 1,
    'MPI_MODE_NOPRECEDE': 2,
    'MPI_MODE_NOPUT': 4,
    'MPI_MODE_NOSTORE': 8,
    'MPI_MODE_NOSUCCEED': 16,
    'MPI_LOCK_EXCLUSIVE': 1,
    'MPI_LOCK_SHARED': 2,
    'MPI_THREAD_SINGLE': 0,
    'MPI_THREAD_FUNNELED': 1,
    'MPI_THREAD_SERIALIZED': 2,
    'MPI_THREAD_MULTIPLE': 3,
    'MPI_SUCCESS': 0,
    'MPI_ERR_BUFFER': 1,
    'MPI_ERR_COUNT': 2,
    'MPI_ERR_TYPE': 3,
    'MPI_ERR_TAG': 4,
    'MPI_ERR_COMM': 5,
    'MPI_ERR_RANK': 6,
    'MPI_ERR_REQUEST': 7,
    'MPI_ERR_ROOT': 8,
    'MPI_ERR_GROUP': 9,
    'MPI_ERR_OP': 10,
    'MPI_ERR_TOPOLOGY': 11,
    'MPI_ERR_DIMS': 12,
    'MPI_ERR_ARG': 13,
    'MPI_ERR_UNKNOWN': 14,
    'MPI_ERR_TRUNCATE': 15,
    'MPI_ERR_OTHER': 16,
    'MPI_ERR_INTERN': 17,
    'MPI_ERR_IN_STATUS': 18,
    'MPI_ERR_PENDING': 19,
    'MPI_ERR_ACCESS': 20,
    'MPI_ERR_AMODE': 21,
    'MPI_ERR_ASSERT': 22,
    'MPI_ERR_BAD_FILE': 23,
    'MPI_ERR_BASE': 24,
    'MPI_ERR_CONVERSION': 25,
    'MPI_ERR_DISP': 26,
    'MPI_ERR_DUP_DATAREP': 27,
    'MPI_ERR_FILE_EXISTS': 28,
    'MPI_ERR_FILE_IN_USE': 29,
    'MPI_ERR_FILE': 30,
    'MPI_ERR_INFO_KEY': 31,
    'MPI_ERR_INFO_NOKEY': 32,
    'MPI_ERR_INFO_VALUE': 33,
    'MPI_ERR_INFO': 34,
    'MPI_ERR_IO': 35,
    'MPI_ERR_KEYVAL': 36,
    'MPI_ERR_LOCKTYPE': 37,
    'MPI_ERR_NAME': 38,
    'MPI_ERR_NO_MEM': 39,
    'MPI_ERR_NOT_SAME': 40,
    'MPI_ERR_NO_SPACE': 41,
    'MPI_ERR_NO_SUCH_FILE': 42,
    'MPI_ERR_PORT': 43,
    'MPI_ERR_PROC_ABORTED': 74,
    'MPI_ERR_QUOTA': 44,
    'MPI_ERR_READ_ONLY': 45,
    'MPI_ERR_RMA_CONFLICT': 46,
    'MPI_ERR_RMA_SYNC': 47,
    'MPI_ERR_SERVICE': 48,
    'MPI_ERR_SIZE': 49,
    'MPI_ERR_SPAWN': 50,
    'MPI_ERR_UNSUPPORTED_DATAREP': 51,
    'MPI_ERR_UNSUPPORTED_OPERATION': 52,
    'MPI_ERR_WIN': 53,
    'MPI_ERR_PROC_FAILED': 75,
    'MPI_ERR_PROC_FAILED_PENDING': 76,
    'MPI_ERR_REVOKED': 77,
    'MPI_T_ERR_MEMORY': 54,
    'MPI_T_ERR_NOT_INITIALIZED': 55,
    'MPI_T_ERR_CANNOT_INIT': 56,
    'MPI_T_ERR_INVALID_INDEX': 57,
    'MPI_T_ERR_INVALID_ITEM': 58,
    'MPI_T_ERR_INVALID_HANDLE': 59,
    'MPI_T_ERR_OUT_OF_HANDLES': 60,
    'MPI_T_ERR_OUT_OF_SESSIONS': 61,
    'MPI_T_ERR_INVALID_SESSION': 62,
    'MPI_T_ERR_CVAR_SET_NOT_NOW': 63,
    'MPI_T_ERR_CVAR_SET_NEVER': 64,
    'MPI_T_ERR_PVAR_NO_STARTSTOP': 65,
    'MPI_T_ERR_PVAR_NO_WRITE': 66,
    'MPI_T_ERR_PVAR_NO_ATOMIC': 67,
    'MPI_ERR_RMA_RANGE': 68,
    'MPI_ERR_RMA_ATTACH': 69,
    'MPI_ERR_RMA_FLAVOR': 70,
    'MPI_ERR_RMA_SHARED': 71,
    'MPI_T_ERR_INVALID': 72,
    'MPI_ERR_SESSION': 78,
    'MPI_ERR_VALUE_TOO_LARGE': 79,
    'MPI_ERR_ERRHANDLER': 80,
    'MPI_ERR_NOTIFY_IDX': 81,
    'MPI_ERR_LASTCODE': 92,
    'MPI_IDENT': 0,
    'MPI_CONGRUENT': 1,
    'MPI_SIMILAR': 2,
    'MPI_UNEQUAL': 3,
    'MPI_COMBINER_NAMED': 0,
    'MPI_COMBINER_DUP': 1,
    'MPI_COMBINER_CONTIGUOUS': 2,
    'MPI_COMBINER_VECTOR': 3,
    'MPI_COMBINER_HVECTOR_INTEGER': 4,
    'MPI_COMBINER_HVECTOR': 5,
    'MPI_COMBINER_INDEXED': 6,
    'MPI_COMBINER_HINDEXED_INTEGER': 7,
    'MPI_COMBINER_HINDEXED': 8,
    'MPI_COMBINER_INDEXED_BLOCK': 9,
    'MPI_COMBINER_STRUCT_INTEGER': 10,
    'MPI_COMBINER_STRUCT': 11,
    'MPI_COMBINER_SUBARRAY': 12,
    'MPI_COMBINER_DARRAY': 13,
    'MPI_COMBINER_F90_REAL': 14,
    'MPI_COMBINER_F90_COMPLEX': 15,
    'MPI_COMBINER_F90_INTEGER': 16,
    'MPI_COMBINER_RESIZED': 17,
    'MPI_COMBINER_HINDEXED_BLOCK': 18,
    'MPI_COMBINER_VALUE_INDEX': 19,
    'MPI_COMM_TYPE_SHARED': 0,
    'OMPI_COMM_TYPE_HWTHREAD': 1,
    'OMPI_COMM_TYPE_CORE': 2,
    'OMPI_COMM_TYPE_L1CACHE': 3,
    'OMPI_COMM_TYPE_L2CACHE': 4,
    'OMPI_COMM_TYPE_L3CACHE': 5,
    'OMPI_COMM_TYPE_SOCKET': 6,
    'OMPI_COMM_TYPE_NUMA': 7,
    'OMPI_COMM_TYPE_NODE': 0,
    'OMPI_COMM_TYPE_BOARD': 8,
    'OMPI_COMM_TYPE_HOST': 9,
    'OMPI_COMM_TYPE_CU': 10,
    'OMPI_COMM_TYPE_CLUSTER': 11,
    'MPI_COMM_TYPE_HW_UNGUIDED': 12,
    'MPI_COMM_TYPE_HW_GUIDED': 13,
    'MPI_COMM_TYPE_RESOURCE_GUIDED': 14,
}

# IO Constants
io_constants = {
    'MPI_SEEK_SET': 600,
    'MPI_SEEK_CUR': 602,
    'MPI_SEEK_END': 604,
    'MPI_MODE_CREATE': 1,
    'MPI_MODE_RDONLY': 2,
    'MPI_MODE_WRONLY': 4,
    'MPI_MODE_RDWR': 8,
    'MPI_MODE_DELETE_ON_CLOSE': 16,
    'MPI_MODE_UNIQUE_OPEN': 32,
    'MPI_MODE_EXCL': 64,
    'MPI_MODE_APPEND': 128,
    'MPI_MODE_SEQUENTIAL': 256,
}

# Large IO Constants (using MPI_OFFSET_KIND)
lio_constants = {
    'MPI_DISPLACEMENT_CURRENT': -54278278,
}

# --- File Writing Logic ---

def write_file_if_changed(filename, content):
    # Writes content to filename only if file doesn't exist or content differs
    filepath = Path(filename)
    needs_write = True
    if filepath.is_file():
        try:
            existing_content = filepath.read_text()
            if content == existing_content:
                needs_write = False
        except IOError as e:
            print(f"Warning: Could not read existing file {filename}: {e}", file=sys.stderr)
            # Proceed to write

    if needs_write:
        try:
            # Ensure directory exists
            filepath.parent.mkdir(parents=True, exist_ok=True)
            filepath.write_text(content)
            print(f"created {filename}")
        except IOError as e:
            print(f"Error: Could not write to file {filename}: {e}", file=sys.stderr)
            sys.exit(1) # Mimic Perl's die
    else:
        print(f"{filename} unchanged; not written")

# --- Value Reading Logic ---

def read_value_from_file(filename, key):
    # Reads a value for a specific key=value line from a file
    value = None
    filepath = Path(filename)
    try:
        with open(filepath, 'r') as f:
            for line in f:
                match = re.match(rf"^{re.escape(key)}=(.*)", line)
                if match:
                    value = match.group(1).strip()
                    break
    except IOError as e:
        print(f"Error: Couldn't open {filename}: {e}", file=sys.stderr)
        sys.exit(1) # Mimic Perl's die

    if value is None:
        print(f"Error: Did not find the string \"{key}\" in the file {filename}", file=sys.stderr)
        sys.exit(1) # Mimic Perl's die

    # Attempt to convert to int if possible, otherwise keep as string
    try:
        return int(value)
    except ValueError:
        return value

# --- Fortran File Generation ---

fortran_header_template = """! -*- fortran -*-
! WARNING! THIS IS A GENERATED FILE!!
! ANY EDITS YOU PUT HERE WILL BE LOST!
! ==> Instead, edit topdir/ompi/include/mpif-values.py.

! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2016 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Oak Ridge National Labs.  All rights reserved.
! Copyright (c) 2016      Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! Copyright (c) 2022      IBM Corporation.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

"""

def generate_fortran_file(header, vals, lvals, filename):
    # Generates a Fortran include file with PARAMETER statements
    content = header
    # Declarations
    for key in sorted(vals.keys()):
        content += f"        integer {key}\n"
    for key in sorted(lvals.keys()):
        content += f"        integer(KIND=MPI_OFFSET_KIND) {key}\n"
    content += "\n"
    # Parameters
    for key in sorted(vals.keys()):
        content += f"        parameter ({key}={vals[key]})\n"
    for key in sorted(lvals.keys()):
        content += f"        parameter ({key}={lvals[key]})\n"

    write_file_if_changed(filename, content)

# --- C Preprocessor File Generation ---

c_header_template = """! WARNING! THIS IS A GENERATED FILE!!
! ANY EDITS YOU PUT HERE WILL BE LOST!
! Instead, edit topdir/ompi/include/mpif-values.py.
!

!
! Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2016 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Oak Ridge National Labs.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2016-2019 Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! Copyright (c) 2022      IBM Corporation.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

#ifndef USE_MPI_F08_CONSTANTS_H
#define USE_MPI_F08_CONSTANTS_H

"""

def generate_c_header_file(filename):
    # Generates the mpi-f08-constants.h C preprocessor file
    content = c_header_template

    for key in sorted(constants.keys()):
        content += f"#define OMPI_{key} {constants[key]}\n"
    content += "\n"
    for key in sorted(handles.keys()):
        content += f"#define OMPI_{key} {handles[key]}\n"
    content += "\n" # Explicitly add newline like Perl script

    for key in sorted(io_constants.keys()):
        content += f"#define OMPI_{key} {io_constants[key]}\n"
    for key in sorted(lio_constants.keys()):
        content += f"#define OMPI_{key} {lio_constants[key]}\n"
    content += "\n"
    for key in sorted(io_handles.keys()):
        content += f"#define OMPI_{key} {io_handles[key]}\n"
    content += "\n"
    content += "#endif /* USE_MPI_F08_CONSTANTS_H */\n"

    write_file_if_changed(filename, content)


# --- Main Script Logic ---

def main():
    print("creating Fortran header files (with common constants)...")

    # Find the OMPI topdir
    topdir = None
    if Path("ompi/include/mpi.h.in").is_file():
        topdir = Path(".")
    elif Path("../ompi/include/mpi.h.in").is_file():
        topdir = Path("..")
    elif Path("../../ompi/include/mpi.h.in").is_file():
        topdir = Path("../..")
    else:
        print("Please run this script from the Open MPI topdir or topdir/include/mpi", file=sys.stderr)
        print("Aborting.", file=sys.stderr)
        sys.exit(1)

    # Read version info and update constants dict
    version_file = topdir / "VERSION"
    constants['MPI_VERSION'] = read_value_from_file(version_file, 'mpi_standard_version')
    constants['MPI_SUBVERSION'] = read_value_from_file(version_file, 'mpi_standard_subversion')


    # --- Generate Fortran Files ---
    ompi_include_dir = topdir / "ompi/include"
    generate_fortran_file(fortran_header_template, handles, {},
                          ompi_include_dir / "mpif-handles.h")
    generate_fortran_file(fortran_header_template, constants, {},
                          ompi_include_dir / "mpif-constants.h")
    generate_fortran_file(fortran_header_template, io_handles, {},
                          ompi_include_dir / "mpif-io-handles.h")
    generate_fortran_file(fortran_header_template, io_constants, lio_constants,
                          ompi_include_dir / "mpif-io-constants.h")

    # --- Generate C Preprocessor File ---
    mpi_f08_mod_dir = topdir / "ompi/mpi/fortran/use-mpi-f08/mod"
    generate_c_header_file(mpi_f08_mod_dir / "mpi-f08-constants.h")

    print("Done.")
    sys.exit(0)

if __name__ == "__main__":
    main()
